//! Elaboration of the surface language into the core language.
//!
//! This module is where user-facing type checking happens, along with
//! translating the convenient surface language into a simpler, more explicit
//! core language.
//!
//! The algorithm is structured _bidirectionally_, ie. divided into _checking_
//! and _synthesis_ modes. By supplying type annotations as early as possible
//! using the checking mode, we can improve the locality of type errors, and
//! provide enough _control_ to the algorithm to allow for elaboration even in
//! the presence of ‘fancy’ types.
//!
//! For places where bidirectional typing is not enough, _unification_ is used
//! in an attempt to infer unknown terms and types based on how they are used.
//!
//! ## Resources
//!
//! - [Bidirectional Typing Rules: A Tutorial](https://davidchristiansen.dk/tutorials/bidirectional.pdf)
//! - [Bidirectional Types Checking – Compose NYC 2019](https://www.youtube.com/watch?v=utyBNDj7s2w)
//! - [Lecture Notes on Bidirectional Type Checking](https://www.cs.cmu.edu/~fp/courses/15312-f04/handouts/15-bidirectional.pdf)
//! - [elaboration-zoo](https://github.com/AndrasKovacs/elaboration-zoo/)

use std::str::FromStr;
use std::sync::Arc;

use scoped_arena::Scope;

use self::patterns::{Body, CheckedPattern, PatMatrix, PatternMode};
use crate::alloc::SliceVec;
use crate::core::semantics::{self, ArcValue, Closure, Head, Telescope, Value};
use crate::core::{self, prim, Const, Plicity, Prim, UIntStyle};
use crate::env::{self, EnvLen, Level, SharedEnv, UniqueEnv};
use crate::files::FileId;
use crate::source::{BytePos, ByteRange, FileRange, Span, Spanned};
use crate::surface::elaboration::reporting::Message;
use crate::surface::{
    distillation, pretty, BinOp, FormatField, Item, Module, Param, Pattern, Term,
};
use crate::symbol::Symbol;

mod order;
mod patterns;
mod reporting;
mod unification;

/// Top-level item environment.
pub struct ItemEnv<'arena> {
    /// Names of items.
    names: UniqueEnv<Symbol>,
    /// Types of items.
    types: UniqueEnv<ArcValue<'arena>>,
    /// Expressions of items.
    exprs: UniqueEnv<ArcValue<'arena>>,
}

impl<'arena> ItemEnv<'arena> {
    /// Construct a new, empty environment.
    pub fn new() -> ItemEnv<'arena> {
        ItemEnv {
            names: UniqueEnv::new(),
            types: UniqueEnv::new(),
            exprs: UniqueEnv::new(),
        }
    }

    fn push_definition(&mut self, name: Symbol, r#type: ArcValue<'arena>, expr: ArcValue<'arena>) {
        self.names.push(name);
        self.types.push(r#type);
        self.exprs.push(expr);
    }

    fn reserve(&mut self, additional: usize) {
        self.names.reserve(additional);
        self.types.reserve(additional);
        self.exprs.reserve(additional);
    }
}

/// Local variable environment.
///
/// This is used for keeping track of [local variables] that are bound by the
/// program, for example by function parameters, let bindings, or pattern
/// matching.
///
/// This environment behaves as a stack.
/// - As scopes are entered, it is important to remember to call either
///   [`LocalEnv::push_def`] or [`LocalEnv::push_param`].
/// - On scope exit, it is important to remember to call [`LocalEnv::pop`].
/// - Multiple bindings can be removed at once with [`LocalEnv::truncate`].
///
/// [local variables]: core::Term::LocalVar
struct LocalEnv<'arena> {
    /// Names of local variables.
    names: UniqueEnv<Option<Symbol>>,
    /// Types of local variables.
    types: UniqueEnv<ArcValue<'arena>>,
    /// Information about the local binders. Used when inserting new
    /// metavariables during [evaluation][semantics::EvalEnv::eval].
    infos: UniqueEnv<core::LocalInfo>,
    /// Expressions that will be substituted for local variables during
    /// [evaluation][semantics::EvalEnv::eval].
    exprs: SharedEnv<ArcValue<'arena>>,
}

impl<'arena> LocalEnv<'arena> {
    /// Construct a new, empty environment.
    fn new() -> LocalEnv<'arena> {
        LocalEnv {
            names: UniqueEnv::new(),
            types: UniqueEnv::new(),
            infos: UniqueEnv::new(),
            exprs: SharedEnv::new(),
        }
    }

    /// Get the length of the local environment.
    fn len(&self) -> EnvLen {
        self.names.len()
    }

    fn next_var(&self) -> ArcValue<'arena> {
        Spanned::empty(Arc::new(Value::local_var(self.exprs.len().next_level())))
    }

    fn reserve(&mut self, additional: usize) {
        self.names.reserve(additional);
        self.types.reserve(additional);
        self.infos.reserve(additional);
        self.exprs.reserve(additional);
    }

    /// Push a local definition onto the context.
    fn push_def(&mut self, name: Option<Symbol>, expr: ArcValue<'arena>, r#type: ArcValue<'arena>) {
        self.names.push(name);
        self.types.push(r#type);
        self.infos.push(core::LocalInfo::Def);
        self.exprs.push(expr);
    }

    /// Push a local parameter onto the context.
    fn push_param(&mut self, name: Option<Symbol>, r#type: ArcValue<'arena>) {
        // An expression that refers to itself once it is pushed onto the local
        // expression environment.
        let expr = self.next_var();

        self.names.push(name);
        self.types.push(r#type);
        self.infos.push(core::LocalInfo::Param);
        self.exprs.push(expr);
    }

    /// Pop a local binder off the context.
    fn pop(&mut self) {
        self.names.pop();
        self.types.pop();
        self.infos.pop();
        self.exprs.pop();
    }

    /// Truncate the local environment.
    fn truncate(&mut self, len: EnvLen) {
        self.names.truncate(len);
        self.types.truncate(len);
        self.infos.truncate(len);
        self.exprs.truncate(len);
    }
}

/// The reason why a metavariable was inserted.
#[derive(Debug, Copy, Clone)]
pub enum MetaSource {
    ImplicitArg(FileRange, Option<Symbol>),
    /// The type of a hole.
    HoleType(FileRange, Symbol),
    /// The expression of a hole.
    HoleExpr(FileRange, Symbol),
    /// The type of a placeholder
    PlaceholderType(FileRange),
    /// The expression of a placeholder
    PlaceholderExpr(FileRange),
    /// The type of a placeholder pattern.
    PlaceholderPatternType(FileRange),
    /// The type of a named pattern.
    NamedPatternType(FileRange, Symbol),
    /// The overall type of a match expression
    MatchExprType(FileRange),
    /// The type of a reported error.
    ReportedErrorType(FileRange),
}

impl MetaSource {
    pub fn range(&self) -> FileRange {
        match self {
            MetaSource::ImplicitArg(range, _)
            | MetaSource::HoleType(range, _)
            | MetaSource::HoleExpr(range, _)
            | MetaSource::PlaceholderType(range)
            | MetaSource::PlaceholderExpr(range)
            | MetaSource::PlaceholderPatternType(range)
            | MetaSource::NamedPatternType(range, _)
            | MetaSource::MatchExprType(range)
            | MetaSource::ReportedErrorType(range) => *range,
        }
    }
}

/// Metavariable environment.
///
/// This is used for keeping track of the state of [metavariables] whose
/// definitions are intended to be found through the use of [unification].
///
/// [metavariables]: core::Term::MetaVar
struct MetaEnv<'arena> {
    /// The source of inserted metavariables, used when reporting [unsolved
    /// metavariables][Message::UnsolvedMetaVar].
    sources: UniqueEnv<MetaSource>,
    /// Types of metavariables.
    types: UniqueEnv</* TODO: lazy value */ ArcValue<'arena>>,
    /// Expressions that will be substituted for metavariables during
    /// [evaluation][semantics::EvalEnv::eval].
    ///
    /// These will be set to [`None`] when a metavariable is first
    /// [inserted][Context::push_unsolved_term], then will be set to [`Some`]
    /// if a solution is found during [`unification`].
    exprs: UniqueEnv<Option<ArcValue<'arena>>>,
}

impl<'arena> MetaEnv<'arena> {
    /// Construct a new, empty environment.
    fn new() -> MetaEnv<'arena> {
        MetaEnv {
            sources: UniqueEnv::new(),
            types: UniqueEnv::new(),
            exprs: UniqueEnv::new(),
        }
    }

    /// Push an unsolved metavariable onto the context.
    fn push(&mut self, source: MetaSource, r#type: ArcValue<'arena>) -> Level {
        // TODO: check that hole name is not already in use
        let var = self.exprs.len().next_level();

        self.sources.push(source);
        self.types.push(r#type);
        self.exprs.push(None);

        var
    }
}

/// Elaboration context.
pub struct Context<'arena> {
    file_id: FileId,
    /// Scoped arena for storing elaborated terms.
    //
    // TODO: Make this local to the elaboration context, and reallocate
    //       elaborated terms to an external `Scope` during zonking, resetting
    //       this scope on completion.
    scope: &'arena Scope<'arena>,

    // Commonly used values, cached to increase sharing.
    universe: ArcValue<'static>,
    format_type: ArcValue<'static>,
    bool_type: ArcValue<'static>,

    /// Primitive environment.
    prim_env: prim::Env<'arena>,
    /// Item environment.
    item_env: ItemEnv<'arena>,
    /// Meta environment.
    meta_env: MetaEnv<'arena>,
    /// Local environment.
    local_env: LocalEnv<'arena>,
    /// A partial renaming to be used during [`unification`].
    renaming: unification::PartialRenaming,
    /// Diagnostic messages encountered during elaboration.
    messages: Vec<Message>,
}

fn suggest_name(name: Symbol, candidates: impl Iterator<Item = Symbol>) -> Option<Symbol> {
    let name = name.resolve();
    candidates.min_by_key(|candidate| {
        let candidate = candidate.resolve();
        levenshtein::levenshtein(name, candidate)
    })
}

impl<'arena> Context<'arena> {
    /// Construct a new elaboration context, backed by the supplied arena.
    pub fn new(
        file_id: FileId,
        scope: &'arena Scope<'arena>,
        item_env: ItemEnv<'arena>,
    ) -> Context<'arena> {
        Context {
            file_id,
            scope,

            universe: Spanned::empty(Arc::new(Value::Universe)),
            format_type: Spanned::empty(Arc::new(Value::prim(Prim::FormatType, []))),
            bool_type: Spanned::empty(Arc::new(Value::prim(Prim::BoolType, []))),

            prim_env: prim::Env::default(scope),
            item_env,
            meta_env: MetaEnv::new(),
            local_env: LocalEnv::new(),
            renaming: unification::PartialRenaming::new(),
            messages: Vec::new(),
        }
    }

    pub fn finish(self) -> ItemEnv<'arena> {
        self.item_env
    }

    fn file_range(&self, byte_range: ByteRange) -> FileRange {
        FileRange::new(self.file_id, byte_range)
    }

    /// Lookup an item name in the context.
    fn get_item_name(&self, name: Symbol) -> Option<(Level, &ArcValue<'arena>)> {
        let item_var = self.item_env.names.elem_level(&name)?;
        let item_type = self.item_env.types.get_level(item_var)?;

        Some((item_var, item_type))
    }

    /// Lookup a local name in the context.
    fn get_local_name(&self, name: Symbol) -> Option<(env::Index, &ArcValue<'arena>)> {
        let local_var = self.local_env.names.elem_index(&Some(name))?;
        let local_type = self.local_env.types.get_index(local_var)?;

        Some((local_var, local_type))
    }

    /// Push an unsolved term onto the context, to be updated later during
    /// unification.
    fn push_unsolved_term(
        &mut self,
        source: MetaSource,
        r#type: ArcValue<'arena>,
    ) -> core::Term<'arena> {
        core::Term::InsertedMeta(
            source.range().into(),
            self.meta_env.push(source, r#type),
            (self.scope).to_scope_from_iter(self.local_env.infos.iter().copied()),
        )
    }

    /// Push an unsolved type onto the context, to be updated later during
    /// unification.
    fn push_unsolved_type(&mut self, source: MetaSource) -> ArcValue<'arena> {
        let r#type = self.push_unsolved_term(source, self.universe.clone());
        self.eval_env().eval(&r#type)
    }

    fn push_message(&mut self, message: Message) {
        self.messages.push(message);
    }

    pub fn handle_messages(&mut self, on_message: &mut dyn FnMut(Message)) {
        for message in self.messages.drain(..) {
            on_message(message);
        }

        let meta_env = &self.meta_env;
        for (expr, source) in Iterator::zip(meta_env.exprs.iter(), meta_env.sources.iter()) {
            match (expr, *source) {
                // Avoid producing messages for some unsolved metavariable sources:
                // Should have an unsolved hole expression
                (None, MetaSource::HoleType(_, _)) => {}
                // Should have an unsolved placeholder
                (None, MetaSource::PlaceholderType(_)) => {}
                // Should already have an error
                (None, MetaSource::ReportedErrorType(_)) => {}

                // For other sources, report an unsolved problem message
                (None, source) => on_message(Message::UnsolvedMetaVar { source }),
                // Yield messages of solved named holes
                (Some(expr), MetaSource::HoleExpr(range, name)) => {
                    let expr = self.pretty_value(expr);
                    on_message(Message::HoleSolution { range, name, expr });
                }
                // Ignore solutions of anything else
                (Some(_), _) => {}
            }
        }
    }

    pub fn eval_env(&mut self) -> semantics::EvalEnv<'arena, '_> {
        semantics::ElimEnv::new(&self.item_env.exprs, &self.meta_env.exprs)
            .eval_env(&mut self.local_env.exprs)
    }

    pub fn elim_env(&self) -> semantics::ElimEnv<'arena, '_> {
        semantics::ElimEnv::new(&self.item_env.exprs, &self.meta_env.exprs)
    }

    pub fn quote_env(&self) -> semantics::QuoteEnv<'arena, '_> {
        semantics::QuoteEnv::new(self.elim_env(), self.local_env.len())
    }

    fn unification_context(&mut self) -> unification::Context<'arena, '_> {
        unification::Context::new(
            self.scope,
            &mut self.renaming,
            &self.item_env.exprs,
            self.local_env.len(),
            &mut self.meta_env.exprs,
        )
    }

    pub fn distillation_context<'out_arena>(
        &self,
        scope: &'out_arena Scope<'out_arena>,
    ) -> distillation::Context<'out_arena, '_> {
        distillation::Context::new(
            scope,
            &self.item_env.names,
            self.local_env.names.clone(),
            &self.meta_env.sources,
        )
    }

    fn pretty_value(&self, value: &ArcValue<'_>) -> String {
        let term = self.quote_env().unfolding_metas().quote(self.scope, value);
        let surface_term = self.distillation_context(self.scope).check(&term);

        pretty::Context::new(self.scope)
            .term(&surface_term)
            .pretty(usize::MAX)
            .to_string()
    }

    /// Reports an error if there are duplicate fields found, returning a slice
    /// of the labels unique labels and an iterator over the unique fields.
    fn report_duplicate_labels<'fields, F>(
        &mut self,
        range: ByteRange,
        fields: &'fields [F],
        get_label: fn(&F) -> (ByteRange, Symbol),
    ) -> (&'arena [Symbol], impl Iterator<Item = &'fields F>) {
        let mut labels = SliceVec::new(self.scope, fields.len());
        // Will only allocate when duplicates are encountered
        let mut duplicate_indices = Vec::new();
        let mut duplicate_labels = Vec::new();

        for (index, field) in fields.iter().enumerate() {
            let (range, label) = get_label(field);
            if labels.contains(&label) {
                duplicate_indices.push(index);
                duplicate_labels.push((self.file_range(range), label));
            } else {
                labels.push(label)
            }
        }

        if !duplicate_labels.is_empty() {
            self.push_message(Message::DuplicateFieldLabels {
                range: self.file_range(range),
                labels: duplicate_labels,
            });
        }

        let filtered_fields = (fields.iter().enumerate()).filter_map(move |(index, field)| {
            (!duplicate_indices.contains(&index)).then_some(field)
        });

        (labels.into(), filtered_fields)
    }

    fn check_tuple_fields<F>(
        &mut self,
        range: ByteRange,
        fields: &[F],
        get_range: fn(&F) -> ByteRange,
        expected_labels: &[Symbol],
    ) -> Result<(), ()> {
        if fields.len() == expected_labels.len() {
            return Ok(());
        }

        let mut found_labels = Vec::with_capacity(fields.len());
        let mut fields_iter = fields.iter().enumerate().peekable();
        let mut expected_labels_iter = expected_labels.iter();

        // use the label names from the expected labels
        while let Some(((_, field), label)) =
            Option::zip(fields_iter.peek(), expected_labels_iter.next())
        {
            found_labels.push((self.file_range(get_range(field)), *label));
            fields_iter.next();
        }

        // use numeric labels for excess fields
        for (index, field) in fields_iter {
            found_labels.push((
                self.file_range(get_range(field)),
                Symbol::get_tuple_label(index),
            ));
        }

        self.push_message(Message::MismatchedFieldLabels {
            range: self.file_range(range),
            found_labels,
            expected_labels: expected_labels.to_vec(),
        });
        Err(())
    }

    fn check_record_fields<F>(
        &mut self,
        range: ByteRange,
        fields: &[F],
        get_label: impl Fn(&F) -> (ByteRange, Symbol),
        labels: &'arena [Symbol],
    ) -> Result<(), ()> {
        if fields.len() == labels.len()
            && fields
                .iter()
                .zip(labels.iter())
                .all(|(field, type_label)| get_label(field).1 == *type_label)
        {
            return Ok(());
        }

        // TODO: improve handling of duplicate labels
        self.push_message(Message::MismatchedFieldLabels {
            range: self.file_range(range),
            found_labels: fields
                .iter()
                .map(|field| {
                    let (range, label) = get_label(field);
                    (self.file_range(range), label)
                })
                .collect(),
            expected_labels: labels.to_vec(),
        });
        Err(())
    }

    /// Parse a source string into number, assuming an ASCII encoding.
    fn parse_ascii<T>(
        &mut self,
        range: ByteRange,
        symbol: Symbol,
        make: fn(T, UIntStyle) -> Const,
    ) -> Option<Const>
    where
        T: From<u8> + std::ops::Shl<Output = T> + std::ops::BitOr<Output = T>,
    {
        // TODO: Parse escape codes
        // TODO: Alternate byte orders
        // TODO: Non-ASCII encodings

        let source = symbol.resolve();
        let mut num = Some(T::from(0));
        let mut count: u8 = 0;

        for (offset, ch) in source.char_indices() {
            if !ch.is_ascii() {
                let ch_start = range.start() + 1 + offset as BytePos;
                let ch_end = ch_start + ch.len_utf8() as BytePos;

                self.push_message(Message::NonAsciiStringLiteral {
                    invalid_range: self.file_range(ByteRange::new(ch_start, ch_end)),
                });
                num = None;
            }

            num = num.filter(|_| usize::from(count) < std::mem::size_of::<T>());
            num = num.map(|num| {
                // Yikes this is a tad ugly. Setting the bytes in reverse order...
                let offset = 8 * (std::mem::size_of::<T>() as u8 - (count + 1));
                num | (T::from(ch as u8) << T::from(offset))
            });
            count += 1;
        }

        if count as usize != std::mem::size_of::<T>() {
            self.push_message(Message::MismatchedStringLiteralByteLength {
                range: self.file_range(range),
                expected_len: std::mem::size_of::<T>(),
                found_len: count as usize,
            });
            num = None;
        }

        num.map(|num| make(num, UIntStyle::Ascii))
    }

    /// Parse a source string into a number.
    fn parse_number<T: FromStr>(
        &mut self,
        range: ByteRange,
        symbol: Symbol,
        make: fn(T) -> Const,
    ) -> Option<Const>
    where
        T::Err: std::fmt::Display,
    {
        // TODO: Custom parsing and improved errors
        match symbol.resolve().parse() {
            Ok(data) => Some(make(data)),
            Err(error) => {
                let message = error.to_string();
                self.push_message(Message::InvalidNumericLiteral {
                    range: self.file_range(range),
                    message,
                });
                None
            }
        }
    }

    /// Parse a source string into a number.
    fn parse_number_radix<T: FromStrRadix>(
        &mut self,
        range: ByteRange,
        symbol: Symbol,
        make: fn(T, UIntStyle) -> Const,
    ) -> Option<Const> {
        // TODO: Custom parsing and improved errors
        let s = symbol.resolve();
        let (s, radix, style) = if let Some(s) = s.strip_prefix("0x") {
            (s, 16, UIntStyle::Hexadecimal)
        } else if let Some(s) = s.strip_prefix("0b") {
            (s, 2, UIntStyle::Binary)
        } else {
            (s, 10, UIntStyle::Decimal)
        };
        match T::from_str_radix(s, radix) {
            Ok(data) => Some(make(data, style)),
            Err(error) => {
                let message = error.to_string();
                self.push_message(Message::InvalidNumericLiteral {
                    range: self.file_range(range),
                    message,
                });
                None
            }
        }
    }

    /// Coerce an expression from one type to another type. This will trigger
    /// unification, recording a unification error on failure.
    fn coerce(
        &mut self,
        surface_range: ByteRange, /* TODO: could be removed if we never encounter empty spans in
                                   * the core term */
        expr: core::Term<'arena>,
        from: &ArcValue<'arena>,
        to: &ArcValue<'arena>,
    ) -> core::Term<'arena> {
        let span = expr.span();
        let from = self.elim_env().force(from);
        let to = self.elim_env().force(to);

        match (from.as_ref(), to.as_ref()) {
            // Coerce format descriptions to their representation types by
            // applying `Repr`.
            (Value::Stuck(Head::Prim(Prim::FormatType), elims), Value::Universe)
                if elims.is_empty() =>
            {
                core::Term::FunApp(
                    span,
                    Plicity::Explicit,
                    self.scope
                        .to_scope(core::Term::Prim(span, core::Prim::FormatRepr)),
                    self.scope.to_scope(expr),
                )
            }

            // Otherwise, unify the types
            (_, _) => match self.unification_context().unify(&from, &to) {
                Ok(()) => expr,
                Err(error) => {
                    let range = match span {
                        Span::Range(range) => range,
                        Span::Empty => {
                            let range = self.file_range(surface_range);
                            self.push_message(Message::MissingSpan { range });
                            range
                        }
                    };

                    self.push_message(Message::FailedToUnify {
                        range,
                        found: self.pretty_value(&from),
                        expected: self.pretty_value(&to),
                        error,
                    });
                    core::Term::Prim(span, Prim::ReportedError)
                }
            },
        }
    }

    /// Elaborate a module.
    pub fn elab_module<'out_arena>(
        &mut self,
        scope: &'out_arena Scope<'out_arena>,
        surface_module: &Module<'_, ByteRange>,
        on_message: &mut dyn FnMut(Message),
    ) -> core::Module<'out_arena> {
        let elab_order = order::elaboration_order(self, surface_module);
        let mut items = Vec::with_capacity(surface_module.items.len());
        self.item_env.reserve(surface_module.items.len());

        for item in elab_order.iter().copied().map(|i| &surface_module.items[i]) {
            match item {
                Item::Def(item) => {
                    let (expr, type_value) =
                        self.synth_fun_lit(item.range, item.params, item.expr, item.r#type);
                    let expr_value = self.eval_env().eval(&expr);
                    let type_expr = self.quote_env().quote(self.scope, &type_value);

                    self.item_env
                        .push_definition(item.label.1, type_value, expr_value);

                    items.push(core::Item::Def {
                        label: item.label.1,
                        r#type: self.scope.to_scope(type_expr),
                        expr: self.scope.to_scope(expr),
                    });
                }
                Item::ReportedError(_) => {}
            }
        }

        // Unfold all unification solutions
        let items = scope.to_scope_from_iter(items.into_iter().map(|item| match item {
            core::Item::Def {
                label,
                r#type,
                expr,
            } => {
                // TODO: Unfold unsolved metas to reported errors
                let r#type = self.eval_env().unfold_metas(scope, r#type);
                let expr = self.eval_env().unfold_metas(scope, expr);

                core::Item::Def {
                    label,
                    r#type: scope.to_scope(r#type),
                    expr: scope.to_scope(expr),
                }
            }
        }));

        self.handle_messages(on_message);

        // TODO: Clear environments
        // TODO: Reset scopes

        core::Module { items }
    }

    /// Elaborate a term, returning its synthesized type.
    pub fn elab_term<'out_arena>(
        &mut self,
        scope: &'out_arena Scope<'out_arena>,
        surface_term: &Term<'_, ByteRange>,
        on_message: &mut dyn FnMut(Message),
    ) -> (core::Term<'out_arena>, core::Term<'out_arena>) {
        let (term, r#type) = self.synth(surface_term);
        let term = self.eval_env().unfold_metas(scope, &term);
        let r#type = self.quote_env().unfolding_metas().quote(scope, &r#type);

        self.handle_messages(on_message);

        // TODO: Clear environments
        // TODO: Reset scopes

        (term, r#type)
    }

    /// Elaborate a term, expecting it to be a format.
    pub fn elab_format<'out_arena>(
        &mut self,
        scope: &'out_arena Scope<'out_arena>,
        surface_term: &Term<'_, ByteRange>,
        on_message: &mut dyn FnMut(Message),
    ) -> core::Term<'out_arena> {
        let term = self.check(surface_term, &self.format_type.clone());
        let term = self.eval_env().unfold_metas(scope, &term); // TODO: fuse with above?

        self.handle_messages(on_message);

        // TODO: Clear environments
        // TODO: Reset scopes

        term
    }

    /// Push a local definition onto the context.
    fn push_local_def(
        &mut self,
        pattern: &CheckedPattern<'arena>,
        scrut: Scrutinee<'arena>,
        value: ArcValue<'arena>,
    ) -> Vec<(Option<Symbol>, Scrutinee<'arena>)> {
        self.push_pattern(pattern, scrut, value, PatternMode::Let, true)
    }

    /// Push a local parameter onto the context.
    fn push_local_param(
        &mut self,
        pattern: &CheckedPattern<'arena>,
        scrut: Scrutinee<'arena>,
    ) -> Vec<(Option<Symbol>, Scrutinee<'arena>)> {
        let value = self.local_env.next_var();
        self.push_pattern(pattern, scrut, value, PatternMode::Fun, true)
    }

    /// Check that a surface term conforms to the given type.
    ///
    /// Returns the elaborated term in the core language.
    fn check(
        &mut self,
        surface_term: &Term<'_, ByteRange>,
        expected_type: &ArcValue<'arena>,
    ) -> core::Term<'arena> {
        let file_range = self.file_range(surface_term.range());
        let expected_type = self.elim_env().force(expected_type);

        match (surface_term, expected_type.as_ref()) {
            (Term::Paren(_, term), _) => self.check(term, &expected_type),
            (Term::Let(range, def_pattern, def_type, def_expr, body_expr), _) => {
                let (def_pattern, _, def_type_value) =
                    self.synth_ann_pattern(def_pattern, *def_type);
                let scrut = self.check_scrutinee(def_expr, def_type_value);
                let value = self.eval_env().eval(scrut.expr);
                let (scrut, extra_def) = self.freshen_scrutinee(scrut, &value, [&def_pattern]);

                let initial_len = self.local_env.len();
                let defs = self.push_local_def(&def_pattern, scrut.clone(), value);
                let body_expr = self.check(body_expr, &expected_type);
                self.local_env.truncate(initial_len);

                let matrix = PatMatrix::singleton(scrut, def_pattern);
                let body = self.elab_match(
                    matrix,
                    &[Body::new(body_expr, defs)],
                    *range,
                    def_expr.range(),
                );
                self.insert_extra_let(*range, body, extra_def)
            }
            (Term::If(_, cond_expr, then_expr, else_expr), _) => {
                let cond_expr = self.check(cond_expr, &self.bool_type.clone());
                let then_expr = self.check(then_expr, &expected_type);
                let else_expr = self.check(else_expr, &expected_type);

                core::Term::ConstMatch(
                    file_range.into(),
                    self.scope.to_scope(cond_expr),
                    // NOTE: in lexicographic order: in Rust, `false < true`
                    self.scope.to_scope_from_iter([
                        (Const::Bool(false), else_expr),
                        (Const::Bool(true), then_expr),
                    ]),
                    None,
                )
            }
            (Term::Match(range, scrutinee_expr, equations), _) => {
                self.check_match(*range, scrutinee_expr, equations, &expected_type)
            }
            (Term::FunLiteral(range, params, body_expr), _) => {
                self.check_fun_lit(*range, params, body_expr, &expected_type)
            }
            // Attempt to specialize terms with freshly inserted implicit
            // arguments if an explicit function was expected.
            (_, Value::FunType(Plicity::Explicit, ..)) => {
                let surface_range = surface_term.range();
                let (synth_term, synth_type) = self.synth_and_insert_implicit_apps(surface_term);
                self.coerce(surface_range, synth_term, &synth_type, &expected_type)
            }
            (Term::RecordLiteral(range, expr_fields), Value::RecordType(labels, types)) => {
                if self
                    .check_record_fields(*range, expr_fields, |field| field.label, labels)
                    .is_err()
                {
                    return core::Term::Prim(file_range.into(), Prim::ReportedError);
                }

                let mut types = types.clone();
                let mut expr_fields = expr_fields.iter();
                let mut exprs = SliceVec::new(self.scope, types.len());

                while let Some((expr_field, (r#type, next_types))) =
                    Option::zip(expr_fields.next(), self.elim_env().split_telescope(types))
                {
                    let name_expr = Term::Name(expr_field.label.0, expr_field.label.1);
                    let expr = expr_field.expr.as_ref().unwrap_or(&name_expr);
                    let expr = self.check(expr, &r#type);
                    types = next_types(self.eval_env().eval(&expr));
                    exprs.push(expr);
                }

                core::Term::RecordLit(file_range.into(), labels, exprs.into())
            }
            (Term::Tuple(_, elem_exprs), Value::Universe) => {
                self.local_env.reserve(elem_exprs.len());
                let labels = Symbol::get_tuple_labels(0..elem_exprs.len());
                let labels = self.scope.to_scope_from_iter(labels.iter().copied());

                let initial_local_len = self.local_env.len();
                let universe = &self.universe.clone();
                let types = self.scope.to_scope_from_iter(
                    Iterator::zip(labels.iter(), elem_exprs.iter()).map(|(label, elem_expr)| {
                        let r#type = self.check(elem_expr, universe);
                        let type_value = self.eval_env().eval(&r#type);
                        self.local_env.push_param(Some(*label), type_value);
                        r#type
                    }),
                );

                self.local_env.truncate(initial_local_len);

                core::Term::RecordType(file_range.into(), labels, types)
            }
            (Term::Tuple(_, elem_exprs), Value::Stuck(Head::Prim(Prim::FormatType), args))
                if args.is_empty() =>
            {
                self.local_env.reserve(elem_exprs.len());
                let labels = Symbol::get_tuple_labels(0..elem_exprs.len());
                let labels = self.scope.to_scope_from_iter(labels.iter().copied());

                let initial_local_len = self.local_env.len();
                let format_type = self.format_type.clone();
                let formats = self.scope.to_scope_from_iter(
                    Iterator::zip(labels.iter(), elem_exprs.iter()).map(|(label, elem_expr)| {
                        let format = self.check(elem_expr, &format_type);
                        let format_value = self.eval_env().eval(&format);
                        let r#type = self.elim_env().format_repr(&format_value);
                        self.local_env.push_param(Some(*label), r#type);
                        format
                    }),
                );

                self.local_env.truncate(initial_local_len);

                core::Term::FormatRecord(file_range.into(), labels, formats)
            }
            (Term::Tuple(range, elem_exprs), Value::RecordType(labels, types)) => {
                if self
                    .check_tuple_fields(*range, elem_exprs, |expr| expr.range(), labels)
                    .is_err()
                {
                    return core::Term::error(self.file_range(*range));
                }

                let mut types = types.clone();
                let mut elem_exprs = elem_exprs.iter();
                let mut exprs = SliceVec::new(self.scope, elem_exprs.len());

                while let Some((elem_expr, (r#type, next_types))) =
                    Option::zip(elem_exprs.next(), self.elim_env().split_telescope(types))
                {
                    let expr = self.check(elem_expr, &r#type);
                    types = next_types(self.eval_env().eval(&expr));
                    exprs.push(expr);
                }

                core::Term::RecordLit(file_range.into(), labels, exprs.into())
            }
            (Term::ArrayLiteral(_, elem_exprs), _) => {
                use crate::core::semantics::Elim::FunApp as App;

                let (len_value, elem_type) = match expected_type.match_prim_spine() {
                    Some((Prim::ArrayType, [App(_, elem_type)])) => (None, elem_type),
                    Some((Prim::Array8Type, [App(_, len), App(_, elem_type)])) => {
                        (Some(len), elem_type)
                    }
                    Some((Prim::Array16Type, [App(_, len), App(_, elem_type)])) => {
                        (Some(len), elem_type)
                    }
                    Some((Prim::Array32Type, [App(_, len), App(_, elem_type)])) => {
                        (Some(len), elem_type)
                    }
                    Some((Prim::Array64Type, [App(_, len), App(_, elem_type)])) => {
                        (Some(len), elem_type)
                    }
                    Some((Prim::ReportedError, _)) => {
                        return core::Term::Prim(file_range.into(), Prim::ReportedError)
                    }
                    _ => {
                        self.push_message(Message::ArrayLiteralNotSupported {
                            range: file_range,
                            expected_type: self.pretty_value(&expected_type),
                        });
                        return core::Term::Prim(file_range.into(), Prim::ReportedError);
                    }
                };

                let len = match len_value.map(|val| val.as_ref()) {
                    None => Some(elem_exprs.len() as u64),
                    Some(Value::ConstLit(Const::U8(len, _))) => Some(*len as u64),
                    Some(Value::ConstLit(Const::U16(len, _))) => Some(*len as u64),
                    Some(Value::ConstLit(Const::U32(len, _))) => Some(*len as u64),
                    Some(Value::ConstLit(Const::U64(len, _))) => Some(*len),
                    Some(Value::Stuck(Head::Prim(Prim::ReportedError), _)) => {
                        return core::Term::Prim(file_range.into(), Prim::ReportedError);
                    }
                    _ => None,
                };

                match len {
                    Some(len) if elem_exprs.len() as u64 == len => core::Term::ArrayLit(
                        file_range.into(),
                        self.scope.to_scope_from_iter(
                            (elem_exprs.iter()).map(|elem_expr| self.check(elem_expr, elem_type)),
                        ),
                    ),
                    _ => {
                        // Check the array elements anyway in order to report
                        // any errors inside the literal as well.
                        for elem_expr in *elem_exprs {
                            self.check(elem_expr, elem_type);
                        }

                        self.push_message(Message::MismatchedArrayLength {
                            range: file_range,
                            found_len: elem_exprs.len(),
                            expected_len: self.pretty_value(len_value.unwrap()),
                        });

                        core::Term::Prim(file_range.into(), Prim::ReportedError)
                    }
                }
            }
            (Term::StringLiteral(range, lit), _) => {
                let constant = match expected_type.match_prim_spine() {
                    Some((Prim::U8Type, [])) => self.parse_ascii(*range, *lit, Const::U8),
                    Some((Prim::U16Type, [])) => self.parse_ascii(*range, *lit, Const::U16),
                    Some((Prim::U32Type, [])) => self.parse_ascii(*range, *lit, Const::U32),
                    Some((Prim::U64Type, [])) => self.parse_ascii(*range, *lit, Const::U64),
                    // Some((Prim::Array8Type, [len, _])) => todo!(),
                    // Some((Prim::Array16Type, [len, _])) => todo!(),
                    // Some((Prim::Array32Type, [len, _])) => todo!(),
                    // Some((Prim::Array64Type, [len, _])) => todo!(),
                    Some((Prim::ReportedError, _)) => None,
                    _ => {
                        self.push_message(Message::StringLiteralNotSupported {
                            range: file_range,
                            expected_type: self.pretty_value(&expected_type),
                        });
                        None
                    }
                };

                match constant {
                    Some(constant) => core::Term::ConstLit(file_range.into(), constant),
                    None => core::Term::Prim(file_range.into(), Prim::ReportedError),
                }
            }
            (Term::NumberLiteral(range, lit), _) => {
                let constant = match expected_type.match_prim_spine() {
                    Some((Prim::U8Type, [])) => self.parse_number_radix(*range, *lit, Const::U8),
                    Some((Prim::U16Type, [])) => self.parse_number_radix(*range, *lit, Const::U16),
                    Some((Prim::U32Type, [])) => self.parse_number_radix(*range, *lit, Const::U32),
                    Some((Prim::U64Type, [])) => self.parse_number_radix(*range, *lit, Const::U64),
                    Some((Prim::S8Type, [])) => self.parse_number(*range, *lit, Const::S8),
                    Some((Prim::S16Type, [])) => self.parse_number(*range, *lit, Const::S16),
                    Some((Prim::S32Type, [])) => self.parse_number(*range, *lit, Const::S32),
                    Some((Prim::S64Type, [])) => self.parse_number(*range, *lit, Const::S64),
                    Some((Prim::F32Type, [])) => self.parse_number(*range, *lit, Const::F32),
                    Some((Prim::F64Type, [])) => self.parse_number(*range, *lit, Const::F64),
                    Some((Prim::ReportedError, _)) => None,
                    _ => {
                        self.push_message(Message::NumericLiteralNotSupported {
                            range: file_range,
                            expected_type: self.pretty_value(&expected_type),
                        });
                        return core::Term::Prim(file_range.into(), Prim::ReportedError);
                    }
                };

                match constant {
                    Some(constant) => core::Term::ConstLit(file_range.into(), constant),
                    None => core::Term::Prim(file_range.into(), Prim::ReportedError),
                }
            }
            (Term::BinOp(range, lhs, op, rhs), _) => {
                self.check_bin_op(*range, lhs, *op, rhs, &expected_type)
            }
            (Term::ReportedError(_), _) => core::Term::Prim(file_range.into(), Prim::ReportedError),
            (_, _) => {
                let surface_range = surface_term.range();
                let (synth_term, synth_type) = self.synth(surface_term);
                self.coerce(surface_range, synth_term, &synth_type, &expected_type)
            }
        }
    }

    /// Wrap a term in fresh implicit applications that correspond to implicit
    /// parameters in the type provided.
    fn insert_implicit_apps(
        &mut self,
        range: ByteRange,
        mut term: core::Term<'arena>,
        mut r#type: ArcValue<'arena>,
    ) -> (core::Term<'arena>, ArcValue<'arena>) {
        let file_range = self.file_range(range);
        while let Value::FunType(Plicity::Implicit, name, param_type, body_type) =
            self.elim_env().force(&r#type).as_ref()
        {
            let source = MetaSource::ImplicitArg(file_range, *name);
            let arg_term = self.push_unsolved_term(source, param_type.clone());
            let arg_value = self.eval_env().eval(&arg_term);

            term = core::Term::FunApp(
                file_range.into(),
                Plicity::Implicit,
                self.scope.to_scope(term),
                self.scope.to_scope(arg_term),
            );
            r#type = self.elim_env().apply_closure(body_type, arg_value);
        }
        (term, r#type)
    }

    /// Synthesize the type of `surface_term`, wrapping it in fresh implicit
    /// applications if the term was not an implicit function literal.
    fn synth_and_insert_implicit_apps(
        &mut self,
        surface_term: &Term<'_, ByteRange>,
    ) -> (core::Term<'arena>, ArcValue<'arena>) {
        let (term, r#type) = self.synth(surface_term);
        match term {
            core::Term::FunLit(_, Plicity::Implicit, _, _) => (term, r#type),
            term => self.insert_implicit_apps(surface_term.range(), term, r#type),
        }
    }

    /// Synthesize the type of the given surface term.
    ///
    /// Returns the elaborated term in the core language and its type.
    fn synth(
        &mut self,
        surface_term: &Term<'_, ByteRange>,
    ) -> (core::Term<'arena>, ArcValue<'arena>) {
        let file_range = self.file_range(surface_term.range());
        match surface_term {
            Term::Paren(_, term) => self.synth(term),
            Term::Name(range, name) => {
                if let Some((term, r#type)) = self.get_local_name(*name) {
                    return (
                        core::Term::LocalVar(file_range.into(), term),
                        r#type.clone(),
                    );
                }
                if let Some((term, r#type)) = self.get_item_name(*name) {
                    return (core::Term::ItemVar(file_range.into(), term), r#type.clone());
                }
                if let Some((prim, r#type)) = self.prim_env.get_name(*name) {
                    return (core::Term::Prim(file_range.into(), prim), r#type.clone());
                }

                self.push_message(Message::UnboundName {
                    range: file_range,
                    name: *name,
                    suggested_name: {
                        let item_names = self.item_env.names.iter().copied();
                        let local_names = self.local_env.names.iter().flatten().copied();
                        suggest_name(*name, item_names.chain(local_names))
                    },
                });

                self.synth_reported_error(*range)
            }
            Term::Hole(_, name) => {
                let type_source = MetaSource::HoleType(file_range, *name);
                let expr_source = MetaSource::HoleExpr(file_range, *name);

                let r#type = self.push_unsolved_type(type_source);
                let expr = self.push_unsolved_term(expr_source, r#type.clone());

                (expr, r#type)
            }
            Term::Placeholder(_) => {
                let type_source = MetaSource::PlaceholderType(file_range);
                let expr_source = MetaSource::PlaceholderExpr(file_range);

                let r#type = self.push_unsolved_type(type_source);
                let expr = self.push_unsolved_term(expr_source, r#type.clone());

                (expr, r#type)
            }
            Term::Ann(_, expr, r#type) => {
                let r#type = self.check(r#type, &self.universe.clone());
                let type_value = self.eval_env().eval(&r#type);
                let expr = self.check(expr, &type_value);

                let ann_expr = core::Term::Ann(
                    file_range.into(),
                    self.scope.to_scope(expr),
                    self.scope.to_scope(r#type),
                );

                (ann_expr, type_value)
            }
            Term::Let(range, def_pattern, def_type, def_expr, body_expr) => {
                let (def_pattern, _, def_type_value) =
                    self.synth_ann_pattern(def_pattern, *def_type);
                let scrut = self.check_scrutinee(def_expr, def_type_value);
                let value = self.eval_env().eval(scrut.expr);
                let (scrut, extra_def) = self.freshen_scrutinee(scrut, &value, [&def_pattern]);

                let initial_len = self.local_env.len();
                let defs = self.push_local_def(&def_pattern, scrut.clone(), value);
                let (body_expr, body_type) = self.synth(body_expr);
                self.local_env.truncate(initial_len);

                let matrix = patterns::PatMatrix::singleton(scrut, def_pattern);
                let body = self.elab_match(
                    matrix,
                    &[Body::new(body_expr, defs)],
                    *range,
                    def_expr.range(),
                );
                let let_expr = self.insert_extra_let(*range, body, extra_def);
                (let_expr, body_type)
            }
            Term::If(_, cond_expr, then_expr, else_expr) => {
                let cond_expr = self.check(cond_expr, &self.bool_type.clone());
                let (then_expr, r#type) = self.synth(then_expr);
                let else_expr = self.check(else_expr, &r#type);

                let match_expr = core::Term::ConstMatch(
                    file_range.into(),
                    self.scope.to_scope(cond_expr),
                    // NOTE: in lexicographic order: in Rust, `false < true`
                    self.scope.to_scope_from_iter([
                        (Const::Bool(false), else_expr),
                        (Const::Bool(true), then_expr),
                    ]),
                    None,
                );

                (match_expr, r#type)
            }
            Term::Match(range, scrutinee_expr, equations) => {
                // Create a single metavariable representing the overall
                // type of the match expression, allowing us to unify this with
                // the types of the match equations together.
                let r#type = self.push_unsolved_type(MetaSource::MatchExprType(file_range));
                let expr = self.check_match(*range, scrutinee_expr, equations, &r#type);
                (expr, r#type)
            }
            Term::Universe(_) => (
                core::Term::Universe(file_range.into()),
                self.universe.clone(),
            ),
            Term::Arrow(_, plicity, param_type, body_type) => {
                let universe = self.universe.clone();
                let param_type = self.check(param_type, &universe);
                let param_type_value = self.eval_env().eval(&param_type);

                self.local_env.push_param(None, param_type_value);
                let body_type = self.check(body_type, &universe);
                self.local_env.pop();

                let fun_type = core::Term::FunType(
                    file_range.into(),
                    *plicity,
                    None,
                    self.scope.to_scope(param_type),
                    self.scope.to_scope(body_type),
                );

                (fun_type, universe)
            }
            Term::FunType(range, patterns, body_type) => {
                self.synth_fun_type(*range, patterns, body_type)
            }
            Term::FunLiteral(range, params, body_expr) => {
                self.synth_fun_lit(*range, params, body_expr, None)
            }
            Term::App(range, head_expr, args) => {
                let mut head_range = head_expr.range();
                let (mut head_expr, mut head_type) = self.synth(head_expr);

                for arg in *args {
                    head_type = self.elim_env().force(&head_type);

                    match arg.plicity {
                        Plicity::Implicit => {}
                        Plicity::Explicit => {
                            (head_expr, head_type) =
                                self.insert_implicit_apps(head_range, head_expr, head_type);
                        }
                    }

                    let (param_type, body_type) = match head_type.as_ref() {
                        Value::FunType(plicity, _, param_type, body_type) => {
                            if arg.plicity == *plicity {
                                (param_type, body_type)
                            } else {
                                self.messages.push(Message::PlicityArgumentMismatch {
                                    head_range: self.file_range(head_range),
                                    head_plicity: Plicity::Explicit,
                                    head_type: self.pretty_value(&head_type),
                                    arg_range: self.file_range(arg.term.range()),
                                    arg_plicity: arg.plicity,
                                });
                                return self.synth_reported_error(*range);
                            }
                        }

                        // There's been an error when elaborating the head of
                        // the application, so avoid trying to elaborate any
                        // further to prevent cascading type errors.
                        _ if head_expr.is_error() || head_type.is_error() => {
                            return self.synth_reported_error(*range);
                        }
                        _ => {
                            // NOTE: We could try to infer that this is a function type,
                            // but this takes more work to prevent cascading type errors
                            self.push_message(Message::UnexpectedArgument {
                                head_range: self.file_range(head_range),
                                head_type: self.pretty_value(&head_type),
                                arg_range: self.file_range(arg.term.range()),
                            });
                            return self.synth_reported_error(*range);
                        }
                    };

                    let arg_range = arg.term.range();
                    head_range = ByteRange::merge(head_range, arg_range);

                    let arg_expr = self.check(&arg.term, param_type);
                    let arg_expr_value = self.eval_env().eval(&arg_expr);

                    head_expr = core::Term::FunApp(
                        self.file_range(head_range).into(),
                        arg.plicity,
                        self.scope.to_scope(head_expr),
                        self.scope.to_scope(arg_expr),
                    );
                    head_type = self.elim_env().apply_closure(body_type, arg_expr_value);
                }
                (head_expr, head_type)
            }
            Term::RecordType(range, type_fields) => {
                let universe = self.universe.clone();
                let initial_local_len = self.local_env.len();
                let (labels, type_fields) =
                    self.report_duplicate_labels(*range, type_fields, |f| f.label);
                let mut types = SliceVec::new(self.scope, labels.len());

                for type_field in type_fields {
                    let r#type = self.check(&type_field.r#type, &universe);
                    let type_value = self.eval_env().eval(&r#type);
                    self.local_env
                        .push_param(Some(type_field.label.1), type_value);
                    types.push(r#type);
                }
                self.local_env.truncate(initial_local_len);

                let record_type = core::Term::RecordType(file_range.into(), labels, types.into());

                (record_type, universe)
            }
            Term::RecordLiteral(range, expr_fields) => {
                let (labels, expr_fields) =
                    self.report_duplicate_labels(*range, expr_fields, |f| f.label);
                let mut types = SliceVec::new(self.scope, labels.len());
                let mut exprs = SliceVec::new(self.scope, labels.len());

                for expr_field in expr_fields {
                    let name_expr = Term::Name(expr_field.label.0, expr_field.label.1);
                    let expr = expr_field.expr.as_ref().unwrap_or(&name_expr);
                    let (expr, r#type) = self.synth(expr);
                    types.push(self.quote_env().quote(self.scope, &r#type));
                    exprs.push(expr);
                }

                let types = Telescope::new(self.local_env.exprs.clone(), types.into());

                (
                    core::Term::RecordLit(file_range.into(), labels, exprs.into()),
                    Spanned::empty(Arc::new(Value::RecordType(labels, types))),
                )
            }
            Term::Tuple(_, elem_exprs) => {
                let labels = Symbol::get_tuple_labels(0..elem_exprs.len());
                let labels = self.scope.to_scope_from_iter(labels.iter().copied());

                let mut exprs = SliceVec::new(self.scope, labels.len());
                let mut types = SliceVec::new(self.scope, labels.len());

                for elem_exprs in elem_exprs.iter() {
                    let (expr, r#type) = self.synth(elem_exprs);
                    types.push(self.quote_env().quote(self.scope, &r#type));
                    exprs.push(expr);
                }

                let types = Telescope::new(self.local_env.exprs.clone(), types.into());
                let term = core::Term::RecordLit(file_range.into(), labels, exprs.into());
                let r#type = Spanned::empty(Arc::new(Value::RecordType(labels, types)));
                (term, r#type)
            }
            Term::Proj(range, head_expr, labels) => {
                let head_range = head_expr.range();
                let (mut head_expr, mut head_type) = self.synth_and_insert_implicit_apps(head_expr);

                'labels: for (label_range, proj_label) in *labels {
                    head_type = self.elim_env().force(&head_type);
                    match (&head_expr, head_type.as_ref()) {
                        // Ensure that the head of the projection is a record
                        (_, Value::RecordType(labels, types)) => {
                            let mut labels = labels.iter().copied();
                            let mut types = types.clone();

                            let head_expr_value = self.eval_env().eval(&head_expr);

                            // Look for a field matching the label of the current
                            // projection in the record type.
                            while let Some((label, (r#type, next_types))) =
                                Option::zip(labels.next(), self.elim_env().split_telescope(types))
                            {
                                if *proj_label == label {
                                    // The field was found. Update the head expression
                                    // and continue elaborating the next projection.
                                    head_expr = core::Term::RecordProj(
                                        self.file_range(ByteRange::merge(head_range, *label_range))
                                            .into(),
                                        self.scope.to_scope(head_expr),
                                        *proj_label,
                                    );
                                    head_type = r#type;
                                    continue 'labels;
                                } else {
                                    // This is not the field we are looking for. Substitute the
                                    // value of this field in the rest of the types and continue
                                    // looking for the field.
                                    let head_expr = head_expr_value.clone();
                                    let expr = self.elim_env().record_proj(head_expr, label);
                                    types = next_types(expr);
                                }
                            }
                            // Couldn't find the field in the record type.
                            // Fallthrough with an error.
                        }
                        // There's been an error when elaborating the head of
                        // the projection, so avoid trying to elaborate any
                        // further to prevent cascading type errors.
                        (core::Term::Prim(_, Prim::ReportedError), _)
                        | (_, Value::Stuck(Head::Prim(Prim::ReportedError), _)) => {
                            return self.synth_reported_error(*range);
                        }
                        // The head expression was not a record type.
                        // Fallthrough with an error.
                        _ => {}
                    }

                    self.push_message(Message::UnknownField {
                        head_range: self.file_range(head_range),
                        head_type: self.pretty_value(&head_type),
                        label_range: self.file_range(*label_range),
                        label: *proj_label,
                        suggested_label: suggest_name(*proj_label, labels.iter().map(|(_, l)| *l)),
                    });
                    return self.synth_reported_error(*range);
                }

                (head_expr, head_type)
            }
            Term::ArrayLiteral(range, _) => {
                self.push_message(Message::AmbiguousArrayLiteral { range: file_range });
                self.synth_reported_error(*range)
            }
            // TODO: Stuck macros + unification like in Klister?
            Term::StringLiteral(range, _) => {
                self.push_message(Message::AmbiguousStringLiteral { range: file_range });
                self.synth_reported_error(*range)
            }
            // TODO: Stuck macros + unification like in Klister?
            Term::NumberLiteral(range, _) => {
                self.push_message(Message::AmbiguousNumericLiteral { range: file_range });
                self.synth_reported_error(*range)
            }
            Term::BooleanLiteral(_, val) => {
                let expr = core::Term::ConstLit(file_range.into(), Const::Bool(*val));
                (expr, self.bool_type.clone())
            }
            Term::FormatRecord(range, format_fields) => {
                let (labels, formats) = self.check_format_fields(*range, format_fields);
                let format_record = core::Term::FormatRecord(file_range.into(), labels, formats);
                (format_record, self.format_type.clone())
            }
            Term::FormatCond(_, (_, name), format, pred) => {
                let format_type = self.format_type.clone();
                let format = self.check(format, &format_type);
                let format_value = self.eval_env().eval(&format);
                let repr_type = self.elim_env().format_repr(&format_value);

                self.local_env.push_param(Some(*name), repr_type);
                let bool_type = self.bool_type.clone();
                let pred_expr = self.check(pred, &bool_type);
                self.local_env.pop();

                let cond_format = core::Term::FormatCond(
                    file_range.into(),
                    *name,
                    self.scope.to_scope(format),
                    self.scope.to_scope(pred_expr),
                );

                (cond_format, format_type)
            }
            Term::FormatOverlap(range, format_fields) => {
                let (labels, formats) = self.check_format_fields(*range, format_fields);
                let overlap_format = core::Term::FormatOverlap(file_range.into(), labels, formats);

                (overlap_format, self.format_type.clone())
            }
            Term::BinOp(range, lhs, op, rhs) => self.synth_bin_op(*range, lhs, *op, rhs),
            Term::ReportedError(range) => self.synth_reported_error(*range),
        }
    }

    // TODO: use iteration instead of recursion
    fn check_fun_lit(
        &mut self,
        range: ByteRange,
        params: &[Param<'_, ByteRange>],
        body_expr: &Term<'_, ByteRange>,
        expected_type: &ArcValue<'arena>,
    ) -> core::Term<'arena> {
        let file_range = self.file_range(range);
        match params.split_first() {
            None => self.check(body_expr, expected_type),
            Some((param, next_params)) => {
                let expected_type = self.elim_env().force(expected_type);
                match expected_type.as_ref() {
                    Value::FunType(expected_plicity, _, expected_type, next_body_type)
                        if param.plicity == *expected_plicity =>
                    {
                        let initial_len = self.local_env.len();
                        let (pattern, scrut) = self.check_param(param, expected_type);
                        let name = pattern.name();
                        let scrut_type = scrut.r#type.clone();
                        let scrut_range = scrut.range;

                        let (defs, body_term) = {
                            let arg_expr = self.local_env.next_var();
                            let expected_type =
                                self.elim_env().apply_closure(next_body_type, arg_expr);

                            let defs = self.push_local_param(&pattern, scrut.clone());
                            let body_term =
                                self.check_fun_lit(range, next_params, body_expr, &expected_type);
                            self.local_env.truncate(initial_len);
                            (defs, body_term)
                        };

                        let matrix = PatMatrix::singleton(scrut, pattern);

                        let body_term = {
                            self.local_env.push_param(name, scrut_type);
                            let body_term = self.elab_match(
                                matrix,
                                &[Body::new(body_term, defs)],
                                range,
                                scrut_range,
                            );
                            self.local_env.truncate(initial_len);
                            body_term
                        };

                        core::Term::FunLit(
                            file_range.into(),
                            param.plicity,
                            name,
                            self.scope.to_scope(body_term),
                        )
                    }

                    // If an implicit function is expected, try to generalise the
                    // function literal by wrapping it in an implicit function
                    Value::FunType(
                        Plicity::Implicit,
                        param_name,
                        expected_type,
                        next_body_type,
                    ) if param.plicity == Plicity::Explicit => {
                        let arg_expr = self.local_env.next_var();
                        (self.local_env).push_param(*param_name, expected_type.clone());
                        let body_type = self.elim_env().apply_closure(next_body_type, arg_expr);
                        let body_expr = self.check_fun_lit(range, params, body_expr, &body_type);
                        self.local_env.pop();
                        core::Term::FunLit(
                            file_range.into(),
                            Plicity::Implicit,
                            *param_name,
                            self.scope.to_scope(body_expr),
                        )
                    }

                    // Attempt to elaborate the the body of the function in synthesis
                    // mode if we are checking against a metavariable.
                    Value::Stuck(Head::MetaVar(_), _) => {
                        let range = ByteRange::merge(param.pattern.range(), body_expr.range());
                        let (expr, r#type) = self.synth_fun_lit(range, params, body_expr, None);
                        self.coerce(range, expr, &r#type, &expected_type)
                    }
                    r#type if r#type.is_error() => core::Term::error(file_range),
                    _ => {
                        self.push_message(Message::UnexpectedParameter {
                            param_range: self.file_range(param.pattern.range()),
                        });
                        // TODO: For improved error recovery, bind the rest of
                        // the parameters, and check the body of the function
                        // literal using the expected body type.
                        core::Term::error(file_range)
                    }
                }
            }
        }
    }

    // TODO: use iteration instead of recursion
    fn synth_fun_lit(
        &mut self,
        range: ByteRange,
        params: &[Param<'_, ByteRange>],
        body_expr: &Term<'_, ByteRange>,
        body_type: Option<&Term<'_, ByteRange>>,
    ) -> (core::Term<'arena>, ArcValue<'arena>) {
        match params.split_first() {
            None => match body_type {
                None => self.synth(body_expr),
                Some(body_type) => {
                    let body_type = self.check(body_type, &self.universe.clone());
                    let body_type = self.eval_env().eval(&body_type);
                    let term = self.check(body_expr, &body_type);
                    (term, body_type)
                }
            },
            Some((param, next_params)) => {
                let initial_len = self.local_env.len();
                let (pattern, scrut) = self.synth_param(param);
                let name = pattern.name();
                let scrut_type = scrut.r#type.clone();
                let scrut_range = scrut.range;

                let (defs, body_term, body_type) = {
                    let defs = self.push_local_param(&pattern, scrut.clone());
                    let (body_term, body_type_value) =
                        self.synth_fun_lit(range, next_params, body_expr, body_type);
                    let body_type = self.quote_env().quote(self.scope, &body_type_value);
                    self.local_env.truncate(initial_len);
                    (defs, body_term, body_type)
                };

                let matrix = PatMatrix::singleton(scrut, pattern);

                let term = {
                    self.local_env.push_param(name, scrut_type.clone());
                    let body_term = self.elab_match(
                        matrix.clone(),
                        &[Body::new(body_term, defs.clone())],
                        range,
                        scrut_range,
                    );
                    self.local_env.truncate(initial_len);
                    core::Term::FunLit(
                        self.file_range(range).into(),
                        param.plicity,
                        name,
                        self.scope.to_scope(body_term),
                    )
                };

                let r#type = {
                    self.local_env.push_param(name, scrut_type.clone());
                    let body_type = self.elab_match(
                        matrix,
                        &[Body::new(body_type, defs.clone())],
                        range,
                        scrut_range,
                    );
                    self.local_env.truncate(initial_len);
                    Spanned::empty(Arc::new(Value::FunType(
                        param.plicity,
                        name,
                        scrut_type,
                        Closure::new(self.local_env.exprs.clone(), self.scope.to_scope(body_type)),
                    )))
                };

                (term, r#type)
            }
        }
    }

    // TODO: use iteration instead of recursion
    fn synth_fun_type(
        &mut self,
        range: ByteRange,
        params: &[Param<'_, ByteRange>],
        body_type: &Term<'_, ByteRange>,
    ) -> (core::Term<'arena>, ArcValue<'arena>) {
        let universe = self.universe.clone();
        match params.split_first() {
            None => {
                let term = self.check(body_type, &universe);
                (term, universe)
            }
            Some((param, next_params)) => {
                let (pattern, scrut) = self.synth_param(param);
                let name = pattern.name();
                let input_type_expr = self.quote_env().quote(self.scope, &scrut.r#type);

                let initial_len = self.local_env.len();
                let defs = self.push_local_param(&pattern, scrut.clone());
                let (body_expr, _) = self.synth_fun_type(range, next_params, body_type);
                self.local_env.truncate(initial_len);

                let body_expr = {
                    self.local_env.push_param(name, scrut.r#type.clone());
                    let matrix = PatMatrix::singleton(scrut, pattern);
                    let body_expr =
                        self.elab_match(matrix, &[Body::new(body_expr, defs)], range, range);
                    self.local_env.truncate(initial_len);
                    body_expr
                };

                let term = core::Term::FunType(
                    self.file_range(range).into(),
                    param.plicity,
                    name,
                    self.scope.to_scope(input_type_expr),
                    self.scope.to_scope(body_expr),
                );

                (term, universe)
            }
        }
    }

    fn synth_bin_op(
        &mut self,
        range: ByteRange,
        lhs: &Term<'_, ByteRange>,
        op: BinOp<ByteRange>,
        rhs: &Term<'_, ByteRange>,
    ) -> (core::Term<'arena>, ArcValue<'arena>) {
        use BinOp::*;
        use Prim::*;

        // de-sugar into function application
        let (lhs_expr, lhs_type) = self.synth_and_insert_implicit_apps(lhs);
        let (rhs_expr, rhs_type) = self.synth_and_insert_implicit_apps(rhs);
        let lhs_type = self.elim_env().force(&lhs_type);
        let rhs_type = self.elim_env().force(&rhs_type);
        let operand_types = Option::zip(lhs_type.match_prim_spine(), rhs_type.match_prim_spine());

        let (fun, body_type) = match (op, operand_types) {
            (Mul(_), Some(((U8Type, []), (U8Type, [])))) => (U8Mul, U8Type),
            (Mul(_), Some(((U16Type, []), (U16Type, [])))) => (U16Mul, U16Type),
            (Mul(_), Some(((U32Type, []), (U32Type, [])))) => (U32Mul, U32Type),
            (Mul(_), Some(((U64Type, []), (U64Type, [])))) => (U64Mul, U64Type),

            (Mul(_), Some(((S8Type, []), (S8Type, [])))) => (S8Mul, S8Type),
            (Mul(_), Some(((S16Type, []), (S16Type, [])))) => (S16Mul, S16Type),
            (Mul(_), Some(((S32Type, []), (S32Type, [])))) => (S32Mul, S32Type),
            (Mul(_), Some(((S64Type, []), (S64Type, [])))) => (S64Mul, S64Type),

            (Div(_), Some(((U8Type, []), (U8Type, [])))) => (U8Div, U8Type),
            (Div(_), Some(((U16Type, []), (U16Type, [])))) => (U16Div, U16Type),
            (Div(_), Some(((U32Type, []), (U32Type, [])))) => (U32Div, U32Type),
            (Div(_), Some(((U64Type, []), (U64Type, [])))) => (U64Div, U64Type),

            (Div(_), Some(((S8Type, []), (S8Type, [])))) => (S8Div, S8Type),
            (Div(_), Some(((S16Type, []), (S16Type, [])))) => (S16Div, S16Type),
            (Div(_), Some(((S32Type, []), (S32Type, [])))) => (S32Div, S32Type),
            (Div(_), Some(((S64Type, []), (S64Type, [])))) => (S64Div, S64Type),

            (Add(_), Some(((U8Type, []), (U8Type, [])))) => (U8Add, U8Type),
            (Add(_), Some(((U16Type, []), (U16Type, [])))) => (U16Add, U16Type),
            (Add(_), Some(((U32Type, []), (U32Type, [])))) => (U32Add, U32Type),
            (Add(_), Some(((U64Type, []), (U64Type, [])))) => (U64Add, U64Type),

            (Add(_), Some(((S8Type, []), (S8Type, [])))) => (S8Add, S8Type),
            (Add(_), Some(((S16Type, []), (S16Type, [])))) => (S16Add, S16Type),
            (Add(_), Some(((S32Type, []), (S32Type, [])))) => (S32Add, S32Type),
            (Add(_), Some(((S64Type, []), (S64Type, [])))) => (S64Add, S64Type),

            (Add(_), Some(((PosType, []), (U8Type, [])))) => (PosAddU8, PosType),
            (Add(_), Some(((PosType, []), (U16Type, [])))) => (PosAddU16, PosType),
            (Add(_), Some(((PosType, []), (U32Type, [])))) => (PosAddU32, PosType),
            (Add(_), Some(((PosType, []), (U64Type, [])))) => (PosAddU64, PosType),

            (Sub(_), Some(((U8Type, []), (U8Type, [])))) => (U8Sub, U8Type),
            (Sub(_), Some(((U16Type, []), (U16Type, [])))) => (U16Sub, U16Type),
            (Sub(_), Some(((U32Type, []), (U32Type, [])))) => (U32Sub, U32Type),
            (Sub(_), Some(((U64Type, []), (U64Type, [])))) => (U64Sub, U64Type),

            (Sub(_), Some(((S8Type, []), (S8Type, [])))) => (S8Sub, S8Type),
            (Sub(_), Some(((S16Type, []), (S16Type, [])))) => (S16Sub, S16Type),
            (Sub(_), Some(((S32Type, []), (S32Type, [])))) => (S32Sub, S32Type),
            (Sub(_), Some(((S64Type, []), (S64Type, [])))) => (S64Sub, S64Type),

            (Eq(_), Some(((BoolType, []), (BoolType, [])))) => (BoolEq, BoolType),
            (Neq(_), Some(((BoolType, []), (BoolType, [])))) => (BoolNeq, BoolType),

            (Eq(_), Some(((U8Type, []), (U8Type, [])))) => (U8Eq, BoolType),
            (Eq(_), Some(((U16Type, []), (U16Type, [])))) => (U16Eq, BoolType),
            (Eq(_), Some(((U32Type, []), (U32Type, [])))) => (U32Eq, BoolType),
            (Eq(_), Some(((U64Type, []), (U64Type, [])))) => (U64Eq, BoolType),

            (Eq(_), Some(((S8Type, []), (S8Type, [])))) => (S8Eq, BoolType),
            (Eq(_), Some(((S16Type, []), (S16Type, [])))) => (S16Eq, BoolType),
            (Eq(_), Some(((S32Type, []), (S32Type, [])))) => (S32Eq, BoolType),
            (Eq(_), Some(((S64Type, []), (S64Type, [])))) => (S64Eq, BoolType),

            (Neq(_), Some(((U8Type, []), (U8Type, [])))) => (U8Neq, BoolType),
            (Neq(_), Some(((U16Type, []), (U16Type, [])))) => (U16Neq, BoolType),
            (Neq(_), Some(((U32Type, []), (U32Type, [])))) => (U32Neq, BoolType),
            (Neq(_), Some(((U64Type, []), (U64Type, [])))) => (U64Neq, BoolType),

            (Neq(_), Some(((S8Type, []), (S8Type, [])))) => (S8Neq, BoolType),
            (Neq(_), Some(((S16Type, []), (S16Type, [])))) => (S16Neq, BoolType),
            (Neq(_), Some(((S32Type, []), (S32Type, [])))) => (S32Neq, BoolType),
            (Neq(_), Some(((S64Type, []), (S64Type, [])))) => (S64Neq, BoolType),

            (Lt(_), Some(((U8Type, []), (U8Type, [])))) => (U8Lt, BoolType),
            (Lt(_), Some(((U16Type, []), (U16Type, [])))) => (U16Lt, BoolType),
            (Lt(_), Some(((U32Type, []), (U32Type, [])))) => (U32Lt, BoolType),
            (Lt(_), Some(((U64Type, []), (U64Type, [])))) => (U64Lt, BoolType),

            (Lt(_), Some(((S8Type, []), (S8Type, [])))) => (S8Lt, BoolType),
            (Lt(_), Some(((S16Type, []), (S16Type, [])))) => (S16Lt, BoolType),
            (Lt(_), Some(((S32Type, []), (S32Type, [])))) => (S32Lt, BoolType),
            (Lt(_), Some(((S64Type, []), (S64Type, [])))) => (S64Lt, BoolType),

            (Lte(_), Some(((U8Type, []), (U8Type, [])))) => (U8Lte, BoolType),
            (Lte(_), Some(((U16Type, []), (U16Type, [])))) => (U16Lte, BoolType),
            (Lte(_), Some(((U32Type, []), (U32Type, [])))) => (U32Lte, BoolType),
            (Lte(_), Some(((U64Type, []), (U64Type, [])))) => (U64Lte, BoolType),

            (Lte(_), Some(((S8Type, []), (S8Type, [])))) => (S8Lte, BoolType),
            (Lte(_), Some(((S16Type, []), (S16Type, [])))) => (S16Lte, BoolType),
            (Lte(_), Some(((S32Type, []), (S32Type, [])))) => (S32Lte, BoolType),
            (Lte(_), Some(((S64Type, []), (S64Type, [])))) => (S64Lte, BoolType),

            (Gt(_), Some(((U8Type, []), (U8Type, [])))) => (U8Gt, BoolType),
            (Gt(_), Some(((U16Type, []), (U16Type, [])))) => (U16Gt, BoolType),
            (Gt(_), Some(((U32Type, []), (U32Type, [])))) => (U32Gt, BoolType),
            (Gt(_), Some(((U64Type, []), (U64Type, [])))) => (U64Gt, BoolType),

            (Gt(_), Some(((S8Type, []), (S8Type, [])))) => (S8Gt, BoolType),
            (Gt(_), Some(((S16Type, []), (S16Type, [])))) => (S16Gt, BoolType),
            (Gt(_), Some(((S32Type, []), (S32Type, [])))) => (S32Gt, BoolType),
            (Gt(_), Some(((S64Type, []), (S64Type, [])))) => (S64Gt, BoolType),

            (Gte(_), Some(((U8Type, []), (U8Type, [])))) => (U8Gte, BoolType),
            (Gte(_), Some(((U16Type, []), (U16Type, [])))) => (U16Gte, BoolType),
            (Gte(_), Some(((U32Type, []), (U32Type, [])))) => (U32Gte, BoolType),
            (Gte(_), Some(((U64Type, []), (U64Type, [])))) => (U64Gte, BoolType),

            (Gte(_), Some(((S8Type, []), (S8Type, [])))) => (S8Gte, BoolType),
            (Gte(_), Some(((S16Type, []), (S16Type, [])))) => (S16Gte, BoolType),
            (Gte(_), Some(((S32Type, []), (S32Type, [])))) => (S32Gte, BoolType),
            (Gte(_), Some(((S64Type, []), (S64Type, [])))) => (S64Gte, BoolType),

            _ => {
                self.push_message(Message::BinOpMismatchedTypes {
                    range: self.file_range(range),
                    lhs_range: self.file_range(lhs.range()),
                    rhs_range: self.file_range(rhs.range()),
                    op: op.map_range(|range| self.file_range(range)),
                    lhs: self.pretty_value(&lhs_type),
                    rhs: self.pretty_value(&rhs_type),
                });
                return self.synth_reported_error(range);
            }
        };

        let fun_head = core::Term::Prim(self.file_range(op.range()).into(), fun);
        let fun_app = core::Term::FunApp(
            self.file_range(range).into(),
            Plicity::Explicit,
            self.scope.to_scope(core::Term::FunApp(
                Span::merge(&lhs_expr.span(), &rhs_expr.span()),
                Plicity::Explicit,
                self.scope.to_scope(fun_head),
                self.scope.to_scope(lhs_expr),
            )),
            self.scope.to_scope(rhs_expr),
        );

        // TODO: Maybe it would be good to reuse lhs_type here if body_type is the same
        (
            fun_app,
            Spanned::empty(Arc::new(Value::prim(body_type, []))),
        )
    }

    fn check_bin_op(
        &mut self,
        range: ByteRange,
        lhs: &Term<'_, ByteRange>,
        op: BinOp<ByteRange>,
        rhs: &Term<'_, ByteRange>,
        expected_type: &ArcValue<'arena>,
    ) -> core::Term<'arena> {
        use BinOp::*;
        use Prim::*;

        let prim = match expected_type.as_ref() {
            Value::Stuck(Head::Prim(prim), spine) if spine.is_empty() => prim,
            // TODO: handle metavars?
            _ => {
                let (expr, synth_type) = self.synth_bin_op(range, lhs, op, rhs);
                return self.coerce(range, expr, &synth_type, expected_type);
            }
        };

        let (fun, op_type) = match (op, prim) {
            (Add(_), U8Type) => (U8Add, U8Type),
            (Add(_), U16Type) => (U16Add, U16Type),
            (Add(_), U32Type) => (U32Add, U32Type),
            (Add(_), U64Type) => (U64Add, U64Type),

            (Add(_), S8Type) => (S8Add, S8Type),
            (Add(_), S16Type) => (S16Add, S16Type),
            (Add(_), S32Type) => (S32Add, S32Type),
            (Add(_), S64Type) => (S64Add, S64Type),

            (Sub(_), U8Type) => (U8Sub, U8Type),
            (Sub(_), U16Type) => (U16Sub, U16Type),
            (Sub(_), U32Type) => (U32Sub, U32Type),
            (Sub(_), U64Type) => (U64Sub, U64Type),

            (Sub(_), S8Type) => (S8Sub, S8Type),
            (Sub(_), S16Type) => (S16Sub, S16Type),
            (Sub(_), S32Type) => (S32Sub, S32Type),
            (Sub(_), S64Type) => (S64Sub, S64Type),

            (Mul(_), U8Type) => (U8Mul, U8Type),
            (Mul(_), U16Type) => (U16Mul, U16Type),
            (Mul(_), U32Type) => (U32Mul, U32Type),
            (Mul(_), U64Type) => (U64Mul, U64Type),

            (Mul(_), S8Type) => (S8Mul, S8Type),
            (Mul(_), S16Type) => (S16Mul, S16Type),
            (Mul(_), S32Type) => (S32Mul, S32Type),
            (Mul(_), S64Type) => (S64Mul, S64Type),

            (Div(_), U8Type) => (U8Div, U8Type),
            (Div(_), U16Type) => (U16Div, U16Type),
            (Div(_), U32Type) => (U32Div, U32Type),
            (Div(_), U64Type) => (U64Div, U64Type),

            (Div(_), S8Type) => (S8Div, S8Type),
            (Div(_), S16Type) => (S16Div, S16Type),
            (Div(_), S32Type) => (S32Div, S32Type),
            (Div(_), S64Type) => (S64Div, S64Type),

            _ => {
                let (expr, synth_type) = self.synth_bin_op(range, lhs, op, rhs);
                return self.coerce(range, expr, &synth_type, expected_type);
            }
        };

        let expected_type = Spanned::empty(Arc::new(Value::prim(op_type, [])));

        let lhs_expr = self.check(lhs, &expected_type);
        let rhs_expr = self.check(rhs, &expected_type);

        let fun_head = core::Term::Prim(self.file_range(op.range()).into(), fun);
        core::Term::FunApp(
            self.file_range(range).into(),
            Plicity::Explicit,
            self.scope.to_scope(core::Term::FunApp(
                Span::merge(&lhs_expr.span(), &rhs_expr.span()),
                Plicity::Explicit,
                self.scope.to_scope(fun_head),
                self.scope.to_scope(lhs_expr),
            )),
            self.scope.to_scope(rhs_expr),
        )
    }

    fn synth_reported_error(&mut self, range: ByteRange) -> (core::Term<'arena>, ArcValue<'arena>) {
        let file_range = self.file_range(range);
        let expr = core::Term::Prim(file_range.into(), Prim::ReportedError);
        let r#type = self.push_unsolved_type(MetaSource::ReportedErrorType(file_range));
        (expr, r#type)
    }

    /// Check a series of format fields
    fn check_format_fields(
        &mut self,
        range: ByteRange,
        format_fields: &[FormatField<'_, ByteRange>],
    ) -> (&'arena [Symbol], &'arena [core::Term<'arena>]) {
        let universe = self.universe.clone();
        let format_type = self.format_type.clone();

        let initial_local_len = self.local_env.len();
        let (labels, format_fields) =
            self.report_duplicate_labels(range, format_fields, |f| match f {
                FormatField::Format { label, .. } | FormatField::Computed { label, .. } => *label,
            });
        let mut formats = SliceVec::new(self.scope, labels.len());

        for format_field in format_fields {
            match format_field {
                FormatField::Format {
                    label: (label_range, label),
                    format,
                    pred,
                } => {
                    let label_range = self.file_range(*label_range);
                    let format = self.check(format, &format_type);
                    let format_value = self.eval_env().eval(&format);
                    let r#type = self.elim_env().format_repr(&format_value);

                    self.local_env.push_param(Some(*label), r#type);

                    match pred {
                        None => formats.push(format),
                        // Elaborate refined fields to conditional formats
                        Some(pred) => {
                            // Note: No need to push a param, as this was done above,
                            // in preparation for checking the the next format field.
                            let cond_expr = self.check(pred, &self.bool_type.clone());

                            let field_span = Span::merge(&label_range.into(), &cond_expr.span());
                            formats.push(core::Term::FormatCond(
                                field_span,
                                *label,
                                self.scope.to_scope(format),
                                self.scope.to_scope(cond_expr),
                            ));
                        }
                    }
                }
                FormatField::Computed {
                    label: (label_range, label),
                    r#type,
                    expr,
                } => {
                    let label_range = self.file_range(*label_range);
                    let (expr, r#type, type_value) = match r#type {
                        Some(r#type) => {
                            let r#type = self.check(r#type, &universe);
                            let type_value = self.eval_env().eval(&r#type);
                            (self.check(expr, &type_value), r#type, type_value)
                        }
                        None => {
                            let (expr, type_value) = self.synth_and_insert_implicit_apps(expr);
                            let r#type = self.quote_env().quote(self.scope, &type_value);
                            (expr, r#type, type_value)
                        }
                    };

                    let field_span = Span::merge(&label_range.into(), &expr.span());
                    let format = core::Term::FunApp(
                        field_span,
                        Plicity::Explicit,
                        self.scope.to_scope(core::Term::FunApp(
                            field_span,
                            Plicity::Explicit,
                            self.scope
                                .to_scope(core::Term::Prim(field_span, Prim::FormatSucceed)),
                            self.scope.to_scope(r#type),
                        )),
                        self.scope.to_scope(expr),
                    );

                    // Assume that `Repr ${type_value} ${expr} = ${type_value}`
                    self.local_env.push_param(Some(*label), type_value);
                    formats.push(format);
                }
            }
        }

        self.local_env.truncate(initial_local_len);

        (labels, formats.into())
    }

    /// Elaborate a match expression in checking mode
    fn check_match(
        &mut self,
        range: ByteRange,
        scrutinee_expr: &Term<'_, ByteRange>,
        equations: &[(Pattern<ByteRange>, Term<'_, ByteRange>)],
        expected_type: &ArcValue<'arena>,
    ) -> core::Term<'arena> {
        let expected_type = self.elim_env().force(expected_type);
        let scrut = self.synth_scrutinee(scrutinee_expr);
        let value = self.eval_env().eval(scrut.expr);

        let patterns: Vec<_> = equations
            .iter()
            .map(|(pat, _)| self.check_pattern(pat, &scrut.r#type))
            .collect();
        let (scrut, extra_def) = self.freshen_scrutinee(scrut, &value, &patterns);

        let mut rows = Vec::with_capacity(equations.len());
        let mut bodies = Vec::with_capacity(equations.len());

        for (pattern, (_, expr)) in patterns.into_iter().zip(equations) {
            let initial_len = self.local_env.len();
            let defs = self.push_pattern(
                &pattern,
                scrut.clone(),
                value.clone(),
                PatternMode::Match,
                true,
            );
            let expr = self.check(expr, &expected_type);
            self.local_env.truncate(initial_len);

            rows.push(patterns::PatRow::singleton((pattern, scrut.clone())));
            bodies.push(Body::new(expr, defs));
        }

        let matrix = patterns::PatMatrix::new(rows);
        let body = self.elab_match(matrix, &bodies, range, scrut.range);
        self.insert_extra_let(range, body, extra_def)
    }

    fn synth_scrutinee(&mut self, scrutinee_expr: &Term<'_, ByteRange>) -> Scrutinee<'arena> {
        let (expr, r#type) = self.synth_and_insert_implicit_apps(scrutinee_expr);

        Scrutinee {
            range: scrutinee_expr.range(),
            expr: self.scope.to_scope(expr),
            r#type,
        }
    }

    fn check_scrutinee(
        &mut self,
        scrutinee_expr: &Term<'_, ByteRange>,
        expected_type: ArcValue<'arena>,
    ) -> Scrutinee<'arena> {
        let expr = self.check(scrutinee_expr, &expected_type);
        Scrutinee {
            range: scrutinee_expr.range(),
            expr: self.scope.to_scope(expr),
            r#type: expected_type,
        }
    }

    // Bind `scrut` to a fresh variable if it is unsafe to evaluate multiple times,
    // and may be evaluated multiple times by any of `patterns`
    // Don't forget to wrap the body in a `Term::Let` with `insert_extra_let`!
    fn freshen_scrutinee<'a>(
        &mut self,
        mut scrut: Scrutinee<'arena>,
        value: &ArcValue<'arena>,
        patterns: impl IntoIterator<Item = &'a CheckedPattern<'a>>,
    ) -> (
        Scrutinee<'arena>,
        Option<(Option<Symbol>, core::Term<'arena>, core::Term<'arena>)>,
    ) {
        if scrut.expr.is_atomic() || patterns.into_iter().all(|pat| pat.is_atomic()) {
            return (scrut, None);
        }

        let def_name = None; // TODO: generate a fresh name
        let def_type = self.quote_env().quote(self.scope, &scrut.r#type);
        let def_expr = scrut.expr.clone();

        let var = core::Term::LocalVar(def_expr.span(), env::Index::last());
        scrut.expr = self.scope.to_scope(var);
        (self.local_env).push_def(def_name, value.clone(), scrut.r#type.clone());
        let extra_def = Some((def_name, def_type, def_expr));
        (scrut, extra_def)
    }

    /// Wrap `body` in a `Term::Let` binding `extra_def` if it is `Some`.
    /// Used in conjunction with `freshen_scrutinee`
    fn insert_extra_let(
        &mut self,
        range: ByteRange,
        body: core::Term<'arena>,
        extra_def: Option<(Option<Symbol>, core::Term<'arena>, core::Term<'arena>)>,
    ) -> core::Term<'arena> {
        match extra_def {
            None => body,
            Some((def_name, def_type, def_expr)) => {
                self.local_env.pop();
                core::Term::Let(
                    self.file_range(range).into(),
                    def_name,
                    self.scope.to_scope(def_type),
                    self.scope.to_scope(def_expr),
                    self.scope.to_scope(body),
                )
            }
        }
    }

    fn synth_param(
        &mut self,
        param: &Param<'_, ByteRange>,
    ) -> (CheckedPattern<'arena>, Scrutinee<'arena>) {
        let (pattern, _, r#type) = self.synth_ann_pattern(&param.pattern, param.r#type.as_ref());
        let expr =
            core::Term::LocalVar(self.file_range(pattern.range()).into(), env::Index::last());
        let scrut = Scrutinee {
            range: pattern.range(),
            expr: self.scope.to_scope(expr),
            r#type,
        };
        (pattern, scrut)
    }

    fn check_param(
        &mut self,
        param: &Param<'_, ByteRange>,
        expected_type: &ArcValue<'arena>,
    ) -> (CheckedPattern<'arena>, Scrutinee<'arena>) {
        let pattern = self.check_ann_pattern(&param.pattern, param.r#type.as_ref(), expected_type);
        let expr =
            core::Term::LocalVar(self.file_range(pattern.range()).into(), env::Index::last());
        let scrut = Scrutinee {
            range: pattern.range(),
            expr: self.scope.to_scope(expr),
            r#type: expected_type.clone(),
        };
        (pattern, scrut)
    }

    fn elab_match(
        &mut self,
        mut matrix: PatMatrix<'arena>,
        bodies: &[Body<'arena>],
        match_range: ByteRange,
        scrut_range: ByteRange,
    ) -> core::Term<'arena> {
        debug_assert_eq!(
            matrix.num_rows(),
            bodies.len(),
            "Must have one body for each row"
        );
        patterns::check_coverage(self, &matrix, match_range, scrut_range);
        patterns::compile_match(self, &mut matrix, bodies, EnvLen::new())
    }
}

trait FromStrRadix: Sized {
    fn from_str_radix(src: &str, radix: u32) -> Result<Self, std::num::ParseIntError>;
}

macro_rules! impl_from_str_radix {
    ($t:ty) => {
        impl FromStrRadix for $t {
            fn from_str_radix(src: &str, radix: u32) -> Result<Self, std::num::ParseIntError> {
                // calls base implementation, not trait function
                Self::from_str_radix(src, radix)
            }
        }
    };
}

impl_from_str_radix!(u8);
impl_from_str_radix!(u16);
impl_from_str_radix!(u32);
impl_from_str_radix!(u64);

/// Scrutinee of a match expression
#[derive(Debug, Clone)]
pub struct Scrutinee<'arena> {
    range: ByteRange,
    expr: &'arena core::Term<'arena>,
    r#type: ArcValue<'arena>,
}
