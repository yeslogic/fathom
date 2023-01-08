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

use scoped_arena::Scope;
use std::cell::RefCell;
use std::str::FromStr;
use std::sync::Arc;

use crate::alloc::SliceVec;
use crate::core::semantics::{self, ArcValue, Head, Telescope, Value};
use crate::core::{self, prim, Const, Prim, UIntStyle};
use crate::env::{self, EnvLen, Level, SharedEnv, UniqueEnv};
use crate::source::{ByteRange, Span, Spanned, StringId, StringInterner};
use crate::surface::elaboration::reporting::Message;
use crate::surface::{distillation, pretty, BinOp, FormatField, Item, Module, Pattern, Term};

mod order;
mod reporting;
mod unification;

/// Top-level item environment.
struct ItemEnv<'arena> {
    /// Names of items.
    names: UniqueEnv<StringId>,
    /// Types of items.
    types: UniqueEnv<ArcValue<'arena>>,
    /// Expressions of items.
    exprs: UniqueEnv<ArcValue<'arena>>,
}

impl<'arena> ItemEnv<'arena> {
    /// Construct a new, empty environment.
    fn new() -> ItemEnv<'arena> {
        ItemEnv {
            names: UniqueEnv::new(),
            types: UniqueEnv::new(),
            exprs: UniqueEnv::new(),
        }
    }

    fn push_definition(
        &mut self,
        name: StringId,
        r#type: ArcValue<'arena>,
        expr: ArcValue<'arena>,
    ) {
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
/// This environment behaves as a stack. As scopes are entered, it is important
/// to remember to call either [`LocalEnv::push_def`] or [`LocalEnv::push_param`].
/// On scope exit, it is important to remember to call [`LocalEnv::pop`].
/// Multiple bindings can be removed at once with [`LocalEnv::truncate`].
///
/// [local variables]: core::Term::LocalVar
struct LocalEnv<'arena> {
    /// Names of local variables.
    names: UniqueEnv<Option<StringId>>,
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

    fn reserve(&mut self, additional: usize) {
        self.names.reserve(additional);
        self.types.reserve(additional);
        self.infos.reserve(additional);
        self.exprs.reserve(additional);
    }

    /// Push a local definition onto the context.
    fn push_def(
        &mut self,
        name: Option<StringId>,
        expr: ArcValue<'arena>,
        r#type: ArcValue<'arena>,
    ) {
        self.names.push(name);
        self.types.push(r#type);
        self.infos.push(core::LocalInfo::Def);
        self.exprs.push(expr);
    }

    /// Push a local parameter onto the context.
    fn push_param(&mut self, name: Option<StringId>, r#type: ArcValue<'arena>) -> ArcValue<'arena> {
        // An expression that refers to itself once it is pushed onto the local
        // expression environment.
        let expr = Spanned::empty(Arc::new(Value::local_var(self.exprs.len().next_level())));

        self.names.push(name);
        self.types.push(r#type);
        self.infos.push(core::LocalInfo::Param);
        self.exprs.push(expr.clone());

        expr
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
    /// The type of a hole.
    HoleType(ByteRange, StringId),
    /// The expression of a hole.
    HoleExpr(ByteRange, StringId),
    /// The type of a placeholder
    PlaceholderType(ByteRange),
    /// The expression of a placeholder
    PlaceholderExpr(ByteRange),
    /// The type of a placeholder pattern.
    PlaceholderPatternType(ByteRange),
    /// The type of a named pattern.
    NamedPatternType(ByteRange, StringId),
    /// The overall type of a match expression
    MatchExprType(ByteRange),
    /// The type of a reported error.
    ReportedErrorType(ByteRange),
}

impl MetaSource {
    pub fn range(&self) -> ByteRange {
        match self {
            MetaSource::HoleType(range, _)
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
pub struct Context<'interner, 'arena> {
    /// Global string interner.
    interner: &'interner RefCell<StringInterner>,
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

impl<'interner, 'arena> Context<'interner, 'arena> {
    /// Construct a new elaboration context, backed by the supplied arena.
    pub fn new(
        interner: &'interner RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
    ) -> Context<'interner, 'arena> {
        Context {
            interner,
            scope,

            universe: Spanned::empty(Arc::new(Value::Universe)),
            format_type: Spanned::empty(Arc::new(Value::prim(Prim::FormatType, []))),
            bool_type: Spanned::empty(Arc::new(Value::prim(Prim::BoolType, []))),

            prim_env: prim::Env::default(interner, scope),
            item_env: ItemEnv::new(),
            meta_env: MetaEnv::new(),
            local_env: LocalEnv::new(),
            renaming: unification::PartialRenaming::new(),
            messages: Vec::new(),
        }
    }

    /// Lookup an item name in the context.
    fn get_item_name(&self, name: StringId) -> Option<(env::Level, &ArcValue<'arena>)> {
        let item_var = self.item_env.names.elem_level(&name)?;
        let item_type = self.item_env.types.get_level(item_var)?;

        Some((item_var, item_type))
    }

    /// Lookup a local name in the context.
    fn get_local_name(&self, name: StringId) -> Option<(env::Index, &ArcValue<'arena>)> {
        let local_var = self.local_env.names.elem_index(&Some(name))?;
        let local_type = self.local_env.types.get_index(local_var)?;

        Some((local_var, local_type))
    }

    /// Push an unsolved term onto the context, to be updated later during unification.
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

    /// Push an unsolved type onto the context, to be updated later during unification.
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
                (None, MetaSource::HoleType(_, _)) => {} // should have an unsolved hole expression
                (None, MetaSource::PlaceholderType(_)) => {} // should have an unsolved placeholder expression
                (None, MetaSource::ReportedErrorType(_)) => {} // should already have an error reported

                // For other sources, report an unsolved problem message
                (None, source) => on_message(Message::UnsolvedMetaVar { source }),
                // Yield messages of solved named holes
                (Some(expr), MetaSource::HoleExpr(range, name)) => {
                    let term = self.quote_env().quote(self.scope, expr);
                    let surface_term = distillation::Context::new(
                        self.interner,
                        self.scope,
                        &self.item_env.names,
                        &mut self.local_env.names,
                        &self.meta_env.sources,
                    )
                    .check(&term);

                    let pretty_context = pretty::Context::new(self.interner, self.scope);
                    let doc = pretty_context.term(&surface_term).into_doc();
                    let expr = doc.pretty(usize::MAX).to_string();

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
        &mut self,
        scope: &'out_arena Scope<'out_arena>,
    ) -> distillation::Context<'interner, 'out_arena, '_> {
        distillation::Context::new(
            self.interner,
            scope,
            &self.item_env.names,
            &mut self.local_env.names,
            &self.meta_env.sources,
        )
    }

    fn pretty_print_value(&mut self, value: &ArcValue<'_>) -> String {
        let scope = self.scope;

        let term = self.quote_env().unfolding_metas().quote(scope, value);
        let surface_term = self.distillation_context(scope).check(&term);

        pretty::Context::new(self.interner, scope)
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
        get_label: fn(&F) -> (ByteRange, StringId),
    ) -> (&'arena [StringId], impl Iterator<Item = &'fields F>) {
        let mut labels = SliceVec::new(self.scope, fields.len());
        // Will only allocate when duplicates are encountered
        let mut duplicate_indices = Vec::new();
        let mut duplicate_labels = Vec::new();

        for (index, field) in fields.iter().enumerate() {
            let (range, label) = get_label(field);
            if labels.contains(&label) {
                duplicate_indices.push(index);
                duplicate_labels.push((range, label));
            } else {
                labels.push(label)
            }
        }

        if !duplicate_labels.is_empty() {
            self.push_message(Message::DuplicateFieldLabels {
                range,
                labels: duplicate_labels,
            });
        }

        let filtered_fields = (fields.iter().enumerate())
            .filter_map(move |(index, field)| (!duplicate_indices.contains(&index)).then_some(field));

        (labels.into(), filtered_fields)
    }

    /// Parse a source string into number, assuming an ASCII encoding.
    fn parse_ascii<T>(
        &mut self,
        range: ByteRange,
        string_id: StringId,
        make: fn(T, UIntStyle) -> Const,
    ) -> Option<Const>
    where
        T: From<u8> + std::ops::Shl<Output = T> + std::ops::BitOr<Output = T>,
    {
        // TODO: Parse escape codes
        // TODO: Alternate byte orders
        // TODO: Non-ASCII encodings

        let interner = self.interner.borrow();
        let source = interner.resolve(string_id).unwrap();
        let mut num = Some(T::from(0));
        let mut count: u8 = 0;

        for (offset, ch) in source.char_indices() {
            if !ch.is_ascii() {
                let ch_start = range.start() + 1 + offset;
                let ch_end = ch_start + ch.len_utf8();

                self.push_message(Message::NonAsciiStringLiteral {
                    invalid_range: ByteRange::new(range.file_id(), ch_start, ch_end),
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
                range,
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
        string_id: StringId,
        make: fn(T) -> Const,
    ) -> Option<Const>
    where
        T::Err: std::fmt::Display,
    {
        // TODO: Custom parsing and improved errors
        match self.interner.borrow().resolve(string_id).unwrap().parse() {
            Ok(data) => Some(make(data)),
            Err(error) => {
                let message = error.to_string();
                self.push_message(Message::InvalidNumericLiteral { range, message });
                None
            }
        }
    }

    /// Parse a source string into a number.
    fn parse_number_radix<T: FromStrRadix>(
        &mut self,
        range: ByteRange,
        string_id: StringId,
        make: fn(T, UIntStyle) -> Const,
    ) -> Option<Const> {
        // TODO: Custom parsing and improved errors
        let interner = self.interner.borrow();
        let s = interner.resolve(string_id).unwrap();
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
                self.push_message(Message::InvalidNumericLiteral { range, message });
                None
            }
        }
    }

    /// Conversion checking for `expr` under the types `type0` and `type1`.
    /// This will trigger unification, recording a unification error on failure.
    //
    // NOTE: We could eventually call this method `coerce` if we end up adding
    //       coercions to the core language.
    fn convert(
        &mut self,
        surface_range: ByteRange, // TODO: could be removed if we never encounter empty spans in the core term
        expr: core::Term<'arena>,
        type0: &ArcValue<'arena>,
        type1: &ArcValue<'arena>,
    ) -> core::Term<'arena> {
        let span = expr.span();
        let range = match span {
            Span::Range(range) => range,
            Span::Empty => {
                self.push_message(Message::MissingSpan {
                    range: surface_range,
                });
                surface_range
            }
        };
        match self.unification_context().unify(type0, type1) {
            Ok(()) => expr,
            Err(error) => {
                let lhs = self.pretty_print_value(type0);
                let rhs = self.pretty_print_value(type1);
                self.push_message(Message::FailedToUnify {
                    range,
                    lhs,
                    rhs,
                    error,
                });
                core::Term::Prim(span, Prim::ReportedError)
            }
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
                        self.synth_fun_lit(item.range, item.patterns, item.expr, item.type_);
                    let expr_value = self.eval_env().eval(&expr);
                    let r#type = self.quote_env().quote(self.scope, &type_value);

                    self.item_env
                        .push_definition(item.label.1, type_value, expr_value);

                    items.push(core::Item::Def {
                        label: item.label.1,
                        r#type: self.scope.to_scope(r#type),
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

    /// Check that a pattern matches an expected type.
    fn check_pattern(
        &mut self,
        pattern: &Pattern<ByteRange>,
        expected_type: &ArcValue<'arena>,
    ) -> CheckedPattern {
        match pattern {
            Pattern::Name(range, name) => CheckedPattern::Binder(*range, *name),
            Pattern::Placeholder(range) => CheckedPattern::Placeholder(*range),
            Pattern::StringLiteral(range, lit) => {
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
                        let expected_type = self.pretty_print_value(expected_type);
                        self.push_message(Message::StringLiteralNotSupported {
                            range: *range,
                            expected_type,
                        });
                        None
                    }
                };

                match constant {
                    Some(constant) => CheckedPattern::ConstLit(*range, constant),
                    None => CheckedPattern::ReportedError(*range),
                }
            }
            Pattern::NumberLiteral(range, lit) => {
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
                        let expected_type = self.pretty_print_value(expected_type);
                        self.push_message(Message::NumericLiteralNotSupported {
                            range: *range,
                            expected_type,
                        });
                        None
                    }
                };

                match constant {
                    Some(constant) => CheckedPattern::ConstLit(*range, constant),
                    None => CheckedPattern::ReportedError(*range),
                }
            }
            Pattern::BooleanLiteral(range, boolean) => {
                let constant = match expected_type.match_prim_spine() {
                    Some((Prim::BoolType, [])) => match *boolean {
                        true => Some(Const::Bool(true)),
                        false => Some(Const::Bool(false)),
                    },
                    _ => {
                        self.push_message(Message::BooleanLiteralNotSupported { range: *range });
                        None
                    }
                };

                match constant {
                    Some(constant) => CheckedPattern::ConstLit(*range, constant),
                    None => CheckedPattern::ReportedError(*range),
                }
            }
        }
    }

    /// Synthesize the type of a pattern.
    fn synth_pattern(
        &mut self,
        pattern: &Pattern<ByteRange>,
    ) -> (CheckedPattern, ArcValue<'arena>) {
        match pattern {
            Pattern::Name(range, name) => {
                let source = MetaSource::NamedPatternType(*range, *name);
                let r#type = self.push_unsolved_type(source);
                (CheckedPattern::Binder(*range, *name), r#type)
            }
            Pattern::Placeholder(range) => {
                let source = MetaSource::PlaceholderPatternType(*range);
                let r#type = self.push_unsolved_type(source);
                (CheckedPattern::Placeholder(*range), r#type)
            }
            Pattern::StringLiteral(range, _) => {
                self.push_message(Message::AmbiguousStringLiteral { range: *range });
                let source = MetaSource::ReportedErrorType(*range);
                let r#type = self.push_unsolved_type(source);
                (CheckedPattern::ReportedError(*range), r#type)
            }
            Pattern::NumberLiteral(range, _) => {
                self.push_message(Message::AmbiguousNumericLiteral { range: *range });
                let source = MetaSource::ReportedErrorType(*range);
                let r#type = self.push_unsolved_type(source);
                (CheckedPattern::ReportedError(*range), r#type)
            }
            Pattern::BooleanLiteral(range, val) => {
                let r#const = Const::Bool(*val);
                let r#type = self.bool_type.clone();
                (CheckedPattern::ConstLit(*range, r#const), r#type)
            }
        }
    }

    /// Check that the type of an annotated pattern matches an expected type.
    fn check_ann_pattern(
        &mut self,
        pattern: &Pattern<ByteRange>,
        r#type: Option<&Term<'_, ByteRange>>,
        expected_type: &ArcValue<'arena>,
    ) -> CheckedPattern {
        match r#type {
            None => self.check_pattern(pattern, expected_type),
            Some(r#type) => {
                let range = r#type.range();
                let r#type = self.check(r#type, &self.universe.clone());
                let r#type = self.eval_env().eval(&r#type);

                match self.unification_context().unify(&r#type, expected_type) {
                    Ok(()) => self.check_pattern(pattern, &r#type),
                    Err(error) => {
                        let lhs = self.pretty_print_value(&r#type);
                        let rhs = self.pretty_print_value(expected_type);
                        self.push_message(Message::FailedToUnify {
                            range,
                            lhs,
                            rhs,
                            error,
                        });
                        CheckedPattern::ReportedError(range)
                    }
                }
            }
        }
    }

    /// Synthesize the type of an annotated pattern.
    fn synth_ann_pattern(
        &mut self,
        pattern: &Pattern<ByteRange>,
        r#type: Option<&Term<'_, ByteRange>>,
    ) -> (CheckedPattern, ArcValue<'arena>) {
        match r#type {
            None => self.synth_pattern(pattern),
            Some(r#type) => {
                let r#type = self.check(r#type, &self.universe.clone());
                let type_value = self.eval_env().eval(&r#type);
                (self.check_pattern(pattern, &type_value), type_value)
            }
        }
    }

    /// Push a local definition onto the context.
    /// The supplied `pattern` is expected to be irrefutable.
    fn push_local_def(
        &mut self,
        pattern: CheckedPattern,
        expr: ArcValue<'arena>,
        r#type: ArcValue<'arena>,
    ) -> Option<StringId> {
        let name = match pattern {
            CheckedPattern::Binder(_, name) => Some(name),
            CheckedPattern::Placeholder(_) => None,
            // FIXME: generate failing parameter expressions?
            CheckedPattern::ConstLit(range, _) => {
                self.push_message(Message::RefutablePattern {
                    pattern_range: range,
                });
                None
            }
            CheckedPattern::ReportedError(_) => None,
        };

        self.local_env.push_def(name, expr, r#type);

        name
    }

    /// Push a local parameter onto the context.
    /// The supplied `pattern` is expected to be irrefutable.
    fn push_local_param(
        &mut self,
        pattern: CheckedPattern,
        r#type: ArcValue<'arena>,
    ) -> (Option<StringId>, ArcValue<'arena>) {
        let name = match pattern {
            CheckedPattern::Binder(_, name) => Some(name),
            CheckedPattern::Placeholder(_) => None,
            // FIXME: generate failing parameter expressions?
            CheckedPattern::ConstLit(range, _) => {
                self.push_message(Message::RefutablePattern {
                    pattern_range: range,
                });
                None
            }
            CheckedPattern::ReportedError(_) => None,
        };

        let expr = self.local_env.push_param(name, r#type);

        (name, expr)
    }

    /// Check that a surface term conforms to the given type.
    ///
    /// Returns the elaborated term in the core language.
    fn check(
        &mut self,
        surface_term: &Term<'_, ByteRange>,
        expected_type: &ArcValue<'arena>,
    ) -> core::Term<'arena> {
        let expected_type = self.elim_env().force(expected_type);

        match (surface_term, expected_type.as_ref()) {
            (Term::Let(range, def_pattern, def_type, def_expr, body_expr), _) => {
                let (def_pattern, def_type_value) = self.synth_ann_pattern(def_pattern, *def_type);
                let def_type = self.quote_env().quote(self.scope, &def_type_value);
                let def_expr = self.check(def_expr, &def_type_value);
                let def_expr_value = self.eval_env().eval(&def_expr);

                let def_name = self.push_local_def(def_pattern, def_expr_value, def_type_value); // TODO: split on constants
                let body_expr = self.check(body_expr, &expected_type);
                self.local_env.pop();

                core::Term::Let(
                    range.into(),
                    def_name,
                    self.scope.to_scope(def_type),
                    self.scope.to_scope(def_expr),
                    self.scope.to_scope(body_expr),
                )
            }
            (Term::If(range, cond_expr, then_expr, else_expr), _) => {
                let cond_expr = self.check(cond_expr, &self.bool_type.clone());
                let then_expr = self.check(then_expr, &expected_type);
                let else_expr = self.check(else_expr, &expected_type);

                core::Term::ConstMatch(
                    range.into(),
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
            (Term::FunLiteral(range, patterns, body_expr), _) => {
                self.check_fun_lit(*range, patterns, body_expr, &expected_type)
            }
            (Term::RecordLiteral(range, expr_fields), Value::RecordType(labels, types)) => {
                // TODO: improve handling of duplicate labels
                if expr_fields.len() != labels.len()
                    || Iterator::zip(expr_fields.iter(), labels.iter())
                        .any(|(expr_field, type_label)| expr_field.label.1 != *type_label)
                {
                    self.push_message(Message::MismatchedFieldLabels {
                        range: *range,
                        expr_labels: (expr_fields.iter())
                            .map(|expr_field| expr_field.label)
                            .collect(),
                        type_labels: labels.to_vec(),
                    });
                    return core::Term::Prim(range.into(), Prim::ReportedError);
                }

                let mut types = types.clone();
                let mut expr_fields = expr_fields.iter();
                let mut exprs = SliceVec::new(self.scope, types.len());

                while let Some((expr_field, (r#type, next_types))) =
                    Option::zip(expr_fields.next(), self.elim_env().split_telescope(types))
                {
                    let expr = self.check(&expr_field.expr, &r#type);
                    types = next_types(self.eval_env().eval(&expr));
                    exprs.push(expr);
                }

                core::Term::RecordLit(range.into(), labels, exprs.into())
            }
            (Term::Tuple(range, elem_exprs), Value::Universe) => {
                self.local_env.reserve(elem_exprs.len());
                let mut interner = self.interner.borrow_mut();
                let labels = interner.get_tuple_labels(0..elem_exprs.len());
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

                core::Term::RecordType(range.into(), labels, types)
            }
            (Term::Tuple(range, elem_exprs), Value::Stuck(Head::Prim(Prim::FormatType), args))
                if args.is_empty() =>
            {
                self.local_env.reserve(elem_exprs.len());
                let mut interner = self.interner.borrow_mut();
                let labels = interner.get_tuple_labels(0..elem_exprs.len());
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

                core::Term::FormatRecord(range.into(), labels, formats)
            }
            (Term::Tuple(range, elem_exprs), Value::RecordType(labels, types)) => {
                if elem_exprs.len() != labels.len() {
                    let mut expr_labels = Vec::with_capacity(elem_exprs.len());
                    let mut elem_exprs = elem_exprs.iter().enumerate().peekable();
                    let mut label_iter = labels.iter();

                    // use the label names from the expected type
                    while let Some(((_, elem_expr), label)) =
                        Option::zip(elem_exprs.peek(), label_iter.next())
                    {
                        expr_labels.push((elem_expr.range(), *label));
                        elem_exprs.next();
                    }

                    // use numeric labels for excess elems
                    for (index, elem_expr) in elem_exprs {
                        expr_labels.push((
                            elem_expr.range(),
                            self.interner.borrow_mut().get_tuple_label(index),
                        ));
                    }

                    self.push_message(Message::MismatchedFieldLabels {
                        range: *range,
                        expr_labels,
                        type_labels: labels.to_vec(),
                    });
                    return core::Term::Prim(range.into(), Prim::ReportedError);
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

                core::Term::RecordLit(range.into(), labels, exprs.into())
            }
            (Term::ArrayLiteral(range, elem_exprs), _) => {
                use crate::core::semantics::Elim::FunApp as App;

                let (len_value, elem_type) = match expected_type.match_prim_spine() {
                    Some((Prim::ArrayType, [App(elem_type)])) => (None, elem_type),
                    Some((Prim::Array8Type, [App(len), App(elem_type)])) => (Some(len), elem_type),
                    Some((Prim::Array16Type, [App(len), App(elem_type)])) => (Some(len), elem_type),
                    Some((Prim::Array32Type, [App(len), App(elem_type)])) => (Some(len), elem_type),
                    Some((Prim::Array64Type, [App(len), App(elem_type)])) => (Some(len), elem_type),
                    Some((Prim::ReportedError, _)) => {
                        return core::Term::Prim(range.into(), Prim::ReportedError)
                    }
                    _ => {
                        let expected_type = self.pretty_print_value(&expected_type);
                        self.push_message(Message::ArrayLiteralNotSupported {
                            range: *range,
                            expected_type,
                        });
                        return core::Term::Prim(range.into(), Prim::ReportedError);
                    }
                };

                let len = match len_value.map(|val| val.as_ref()) {
                    None => Some(elem_exprs.len() as u64),
                    Some(Value::ConstLit(Const::U8(len, _))) => Some(*len as u64),
                    Some(Value::ConstLit(Const::U16(len, _))) => Some(*len as u64),
                    Some(Value::ConstLit(Const::U32(len, _))) => Some(*len as u64),
                    Some(Value::ConstLit(Const::U64(len, _))) => Some(*len),
                    Some(Value::Stuck(Head::Prim(Prim::ReportedError), _)) => {
                        return core::Term::Prim(range.into(), Prim::ReportedError);
                    }
                    _ => None,
                };

                match len {
                    Some(len) if elem_exprs.len() as u64 == len => core::Term::ArrayLit(
                        range.into(),
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

                        let expected_len = self.pretty_print_value(len_value.unwrap());
                        self.push_message(Message::MismatchedArrayLength {
                            range: *range,
                            found_len: elem_exprs.len(),
                            expected_len,
                        });

                        core::Term::Prim(range.into(), Prim::ReportedError)
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
                        let expected_type = self.pretty_print_value(&expected_type);
                        self.push_message(Message::StringLiteralNotSupported {
                            range: *range,
                            expected_type,
                        });
                        None
                    }
                };

                match constant {
                    Some(constant) => core::Term::ConstLit(range.into(), constant),
                    None => core::Term::Prim(range.into(), Prim::ReportedError),
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
                        let expected_type = self.pretty_print_value(&expected_type);
                        self.push_message(Message::NumericLiteralNotSupported {
                            range: *range,
                            expected_type,
                        });
                        return core::Term::Prim(range.into(), Prim::ReportedError);
                    }
                };

                match constant {
                    Some(constant) => core::Term::ConstLit(range.into(), constant),
                    None => core::Term::Prim(range.into(), Prim::ReportedError),
                }
            }
            (Term::ReportedError(range), _) => core::Term::Prim(range.into(), Prim::ReportedError),
            (_, _) => {
                let (core_term, synth_type) = self.synth(surface_term);
                self.convert(surface_term.range(), core_term, &synth_type, &expected_type)
            }
        }
    }

    /// Synthesize the type of the given surface term.
    ///
    /// Returns the elaborated term in the core language and its type.
    fn synth(
        &mut self,
        surface_term: &Term<'_, ByteRange>,
    ) -> (core::Term<'arena>, ArcValue<'arena>) {
        match surface_term {
            Term::Name(range, name) => {
                if let Some((term, r#type)) = self.get_local_name(*name) {
                    return (core::Term::LocalVar(range.into(), term), r#type.clone());
                }
                if let Some((term, r#type)) = self.get_item_name(*name) {
                    return (core::Term::ItemVar(range.into(), term), r#type.clone());
                }
                if let Some((prim, r#type)) = self.prim_env.get_name(*name) {
                    return (core::Term::Prim(range.into(), prim), r#type.clone());
                }

                self.push_message(Message::UnboundName {
                    range: *range,
                    name: *name,
                });
                self.synth_reported_error(*range)
            }
            Term::Hole(range, name) => {
                let type_source = MetaSource::HoleType(*range, *name);
                let expr_source = MetaSource::HoleExpr(*range, *name);

                let r#type = self.push_unsolved_type(type_source);
                let expr = self.push_unsolved_term(expr_source, r#type.clone());

                (expr, r#type)
            }
            Term::Placeholder(range) => {
                let type_source = MetaSource::PlaceholderType(*range);
                let expr_source = MetaSource::PlaceholderExpr(*range);

                let r#type = self.push_unsolved_type(type_source);
                let expr = self.push_unsolved_term(expr_source, r#type.clone());

                (expr, r#type)
            }
            Term::Ann(range, expr, r#type) => {
                let r#type = self.check(r#type, &self.universe.clone());
                let type_value = self.eval_env().eval(&r#type);
                let expr = self.check(expr, &type_value);

                let ann_expr = core::Term::Ann(
                    range.into(),
                    self.scope.to_scope(expr),
                    self.scope.to_scope(r#type),
                );

                (ann_expr, type_value)
            }
            Term::Let(range, def_pattern, def_type, def_expr, body_expr) => {
                let (def_pattern, def_type_value) = self.synth_ann_pattern(def_pattern, *def_type);
                let def_type = self.quote_env().quote(self.scope, &def_type_value);
                let def_expr = self.check(def_expr, &def_type_value);
                let def_expr_value = self.eval_env().eval(&def_expr);

                let def_name = self.push_local_def(def_pattern, def_expr_value, def_type_value);
                let (body_expr, body_type) = self.synth(body_expr);
                self.local_env.pop();

                let let_expr = core::Term::Let(
                    range.into(),
                    def_name,
                    self.scope.to_scope(def_type),
                    self.scope.to_scope(def_expr),
                    self.scope.to_scope(body_expr),
                );

                (let_expr, body_type)
            }
            Term::If(range, cond_expr, then_expr, else_expr) => {
                let cond_expr = self.check(cond_expr, &self.bool_type.clone());
                let (then_expr, r#type) = self.synth(then_expr);
                let else_expr = self.check(else_expr, &r#type);

                let match_expr = core::Term::ConstMatch(
                    range.into(),
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
                let r#type = self.push_unsolved_type(MetaSource::MatchExprType(*range));
                let expr = self.check_match(*range, scrutinee_expr, equations, &r#type);
                (expr, r#type)
            }
            Term::Universe(range) => (core::Term::Universe(range.into()), self.universe.clone()),
            Term::Arrow(range, param_type, body_type) => {
                let universe = self.universe.clone();
                let param_type = self.check(param_type, &universe);
                let param_type_value = self.eval_env().eval(&param_type);

                self.local_env.push_param(None, param_type_value);
                let body_type = self.check(body_type, &universe);
                self.local_env.pop();

                let fun_type = core::Term::FunType(
                    range.into(),
                    None,
                    self.scope.to_scope(param_type),
                    self.scope.to_scope(body_type),
                );

                (fun_type, self.universe.clone())
            }
            Term::FunType(range, patterns, body_type) => {
                self.local_env.reserve(patterns.len());
                let initial_local_len = self.local_env.len();

                // Elaborate the parameters, collecting them in a stack
                let mut params = Vec::with_capacity(patterns.len());
                for (pattern, r#type) in *patterns {
                    let (pattern, type_value) = self.synth_ann_pattern(pattern, *r#type);
                    let r#type = self.quote_env().quote(self.scope, &type_value);
                    let (name, _) = self.push_local_param(pattern, type_value);
                    params.push((name, r#type));
                }

                let mut fun_type = self.check(body_type, &self.universe.clone());
                self.local_env.truncate(initial_local_len);

                // Construct the function type from the parameters in reverse
                for (name, r#type) in params.into_iter().rev() {
                    fun_type = core::Term::FunType(
                        range.into(), // FIXME
                        name,
                        self.scope.to_scope(r#type),
                        self.scope.to_scope(fun_type),
                    );
                }

                (fun_type, self.universe.clone())
            }
            Term::FunLiteral(range, patterns, body_expr) => {
                self.synth_fun_lit(*range, patterns, body_expr, None)
            }
            Term::App(range, head_expr, arg_exprs) => {
                let head_range = head_expr.range();
                let (mut head_expr, mut head_type) = self.synth(head_expr);

                for arg_expr in *arg_exprs {
                    head_type = self.elim_env().force(&head_type);
                    match (&head_expr, head_type.as_ref()) {
                        // Ensure that the head of the application is a function
                        (_, Value::FunType(_, param_type, body_type)) => {
                            // Check the argument and apply it to the body type
                            let param_range = arg_expr.range();
                            let arg_expr = self.check(arg_expr, param_type);
                            let arg_expr_value = self.eval_env().eval(&arg_expr);

                            head_expr = core::Term::FunApp(
                                ByteRange::merge(&head_range, &param_range).into(),
                                self.scope.to_scope(head_expr),
                                self.scope.to_scope(arg_expr),
                            );
                            head_type = self.elim_env().apply_closure(body_type, arg_expr_value);
                        }
                        // There's been an error when elaborating the head of
                        // the application, so avoid trying to elaborate any
                        // further to prevent cascading type errors.
                        (core::Term::Prim(_, Prim::ReportedError), _)
                        | (_, Value::Stuck(Head::Prim(Prim::ReportedError), _)) => {
                            return self.synth_reported_error(*range);
                        }
                        _ => {
                            // NOTE: We could try to infer that this is a function type,
                            // but this takes more work to prevent cascading type errors
                            let head_type = self.pretty_print_value(&head_type);
                            self.push_message(Message::UnexpectedArgument {
                                head_range,
                                head_type,
                                arg_range: arg_expr.range(),
                            });
                            return self.synth_reported_error(*range);
                        }
                    }
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
                    let r#type = self.check(&type_field.type_, &universe);
                    let type_value = self.eval_env().eval(&r#type);
                    self.local_env
                        .push_param(Some(type_field.label.1), type_value);
                    types.push(r#type);
                }
                self.local_env.truncate(initial_local_len);

                let record_type = core::Term::RecordType(range.into(), labels, types.into());

                (record_type, universe)
            }
            Term::RecordLiteral(range, expr_fields) => {
                let (labels, expr_fields) =
                    self.report_duplicate_labels(*range, expr_fields, |f| f.label);
                let mut types = SliceVec::new(self.scope, labels.len());
                let mut exprs = SliceVec::new(self.scope, labels.len());

                for expr_field in expr_fields {
                    let (expr, r#type) = self.synth(&expr_field.expr);
                    types.push(self.quote_env().quote(self.scope, &r#type));
                    exprs.push(expr);
                }

                let types = Telescope::new(self.local_env.exprs.clone(), types.into());

                (
                    core::Term::RecordLit(range.into(), labels, exprs.into()),
                    Spanned::empty(Arc::new(Value::RecordType(labels, types))),
                )
            }
            Term::Tuple(range, elem_exprs) => {
                let mut interner = self.interner.borrow_mut();
                let labels = interner.get_tuple_labels(0..elem_exprs.len());
                let labels = self.scope.to_scope_from_iter(labels.iter().copied());

                let mut exprs = SliceVec::new(self.scope, labels.len());
                let mut types = SliceVec::new(self.scope, labels.len());

                for elem_exprs in elem_exprs.iter() {
                    let (expr, r#type) = self.synth(elem_exprs);
                    types.push(self.quote_env().quote(self.scope, &r#type));
                    exprs.push(expr);
                }

                let types = Telescope::new(self.local_env.exprs.clone(), types.into());
                let term = core::Term::RecordLit(range.into(), labels, exprs.into());
                let r#type = Spanned::empty(Arc::new(Value::RecordType(labels, types)));
                (term, r#type)
            }
            Term::Proj(range, head_expr, labels) => {
                let head_range = head_expr.range();
                let (mut head_expr, mut head_type) = self.synth(head_expr);

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
                                        ByteRange::merge(&head_range, label_range).into(),
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

                    let head_type = self.pretty_print_value(&head_type);
                    self.push_message(Message::UnknownField {
                        head_range,
                        head_type,
                        label_range: *label_range,
                        label: *proj_label,
                    });
                    return self.synth_reported_error(*range);
                }

                (head_expr, head_type)
            }
            Term::ArrayLiteral(range, _) => {
                self.push_message(Message::AmbiguousArrayLiteral { range: *range });
                self.synth_reported_error(*range)
            }
            // TODO: Stuck macros + unification like in Klister?
            Term::StringLiteral(range, _) => {
                self.push_message(Message::AmbiguousStringLiteral { range: *range });
                self.synth_reported_error(*range)
            }
            // TODO: Stuck macros + unification like in Klister?
            Term::NumberLiteral(range, _) => {
                self.push_message(Message::AmbiguousNumericLiteral { range: *range });
                self.synth_reported_error(*range)
            }
            Term::BooleanLiteral(range, val) => {
                let expr = core::Term::ConstLit(range.into(), Const::Bool(*val));
                (expr, self.bool_type.clone())
            }
            Term::FormatRecord(range, format_fields) => {
                let (labels, formats) = self.check_format_fields(*range, format_fields);
                let format_record = core::Term::FormatRecord(range.into(), labels, formats);
                (format_record, self.format_type.clone())
            }
            Term::FormatCond(range, (_, name), format, pred) => {
                let format_type = self.format_type.clone();
                let format = self.check(format, &format_type);
                let format_value = self.eval_env().eval(&format);
                let repr_type = self.elim_env().format_repr(&format_value);

                self.local_env.push_param(Some(*name), repr_type);
                let bool_type = self.bool_type.clone();
                let pred_expr = self.check(pred, &bool_type);
                self.local_env.pop();

                let cond_format = core::Term::FormatCond(
                    range.into(),
                    *name,
                    self.scope.to_scope(format),
                    self.scope.to_scope(pred_expr),
                );

                (cond_format, format_type)
            }
            Term::FormatOverlap(range, format_fields) => {
                let (labels, formats) = self.check_format_fields(*range, format_fields);
                let overlap_format = core::Term::FormatOverlap(range.into(), labels, formats);

                (overlap_format, self.format_type.clone())
            }
            Term::BinOp(range, lhs, op, rhs) => self.synth_bin_op(*range, lhs, *op, rhs),
            Term::ReportedError(range) => self.synth_reported_error(*range),
        }
    }

    fn check_fun_lit(
        &mut self,
        range: ByteRange,
        patterns: &[(Pattern<ByteRange>, Option<&Term<'_, ByteRange>>)],
        body_expr: &Term<'_, ByteRange>,
        expected_type: &ArcValue<'arena>,
    ) -> core::Term<'arena> {
        match patterns.split_first() {
            Some(((pattern, r#type), next_patterns)) => {
                let body_type = self.elim_env().force(expected_type);
                match body_type.as_ref() {
                    Value::FunType(_, param_type, next_body_type) => {
                        let range = ByteRange::merge(&pattern.range(), &body_expr.range()).unwrap();
                        let pattern = self.check_ann_pattern(pattern, *r#type, param_type);
                        let (name, arg_expr) = self.push_local_param(pattern, param_type.clone());

                        let body_type = self.elim_env().apply_closure(next_body_type, arg_expr);
                        let body_expr =
                            self.check_fun_lit(range, next_patterns, body_expr, &body_type);
                        self.local_env.pop();

                        core::Term::FunLit(range.into(), name, self.scope.to_scope(body_expr))
                    }
                    // Attempt to elaborate the the body of the function in synthesis
                    // mode if we are checking against a metavariable.
                    Value::Stuck(Head::MetaVar(_), _) => {
                        let range = ByteRange::merge(&pattern.range(), &body_expr.range()).unwrap();
                        let (expr, r#type) = self.synth_fun_lit(range, patterns, body_expr, None);
                        self.convert(range, expr, &r#type, expected_type)
                    }
                    Value::Stuck(Head::Prim(Prim::ReportedError), _) => {
                        core::Term::Prim(range.into(), Prim::ReportedError)
                    }
                    _ => {
                        self.push_message(Message::UnexpectedParameter {
                            param_range: pattern.range(),
                        });
                        // TODO: For improved error recovery, bind the rest of
                        // the parameters, and check the body of the function
                        // literal using the expected body type.
                        core::Term::Prim(range.into(), Prim::ReportedError)
                    }
                }
            }
            None => self.check(body_expr, expected_type),
        }
    }

    fn synth_fun_lit(
        &mut self,
        range: ByteRange,
        patterns: &[(Pattern<ByteRange>, Option<&Term<ByteRange>>)],
        body_expr: &Term<'_, ByteRange>,
        body_type: Option<&Term<'_, ByteRange>>,
    ) -> (core::Term<'arena>, ArcValue<'arena>) {
        self.local_env.reserve(patterns.len());
        let initial_local_len = self.local_env.len();

        // Elaborate the parameters, collecting them into a stack
        let mut params = Vec::with_capacity(patterns.len());
        for (i, (pattern, r#type)) in patterns.iter().enumerate() {
            let range = match i {
                0 => range, // The first pattern uses the range of the full function literal
                _ => ByteRange::merge(&pattern.range(), &body_expr.range()).unwrap(),
            };
            let (pattern, type_value) = self.synth_ann_pattern(pattern, *r#type);
            let r#type = self.quote_env().quote(self.scope, &type_value);
            let (name, _) = self.push_local_param(pattern, type_value);
            params.push((range, name, r#type));
        }

        let (mut fun_lit, mut fun_type) = match body_type {
            Some(body_type) => {
                let body_type = self.check(body_type, &self.universe.clone());
                let body_type_value = self.eval_env().eval(&body_type);
                (self.check(body_expr, &body_type_value), body_type)
            }
            None => {
                let (body_expr, body_type) = self.synth(body_expr);
                (body_expr, self.quote_env().quote(self.scope, &body_type))
            }
        };

        self.local_env.truncate(initial_local_len);

        // Construct the function literal and type from the parameters in reverse
        for (range, name, r#type) in params.into_iter().rev() {
            fun_lit = core::Term::FunLit(range.into(), name, self.scope.to_scope(fun_lit));
            fun_type = core::Term::FunType(
                Span::Empty,
                name,
                self.scope.to_scope(r#type),
                self.scope.to_scope(fun_type),
            );
        }

        (fun_lit, self.eval_env().eval(&fun_type))
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
        let (lhs_expr, lhs_type) = self.synth(lhs);
        let (rhs_expr, rhs_type) = self.synth(rhs);
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

            (Eq(_), Some(((BoolType, []), (BoolType, [])))) => (U8Eq, BoolType),
            (Eq(_), Some(((U8Type, []), (U8Type, [])))) => (U8Eq, BoolType),
            (Eq(_), Some(((U16Type, []), (U16Type, [])))) => (U16Eq, BoolType),
            (Eq(_), Some(((U32Type, []), (U32Type, [])))) => (U32Eq, BoolType),
            (Eq(_), Some(((U64Type, []), (U64Type, [])))) => (U64Eq, BoolType),

            (Eq(_), Some(((S8Type, []), (S8Type, [])))) => (S8Eq, BoolType),
            (Eq(_), Some(((S16Type, []), (S16Type, [])))) => (S16Eq, BoolType),
            (Eq(_), Some(((S32Type, []), (S32Type, [])))) => (S32Eq, BoolType),
            (Eq(_), Some(((S64Type, []), (S64Type, [])))) => (S64Eq, BoolType),

            (Neq(_), Some(((BoolType, []), (BoolType, [])))) => (U8Neq, BoolType),
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
                let lhs_pretty = self.pretty_print_value(&lhs_type);
                let rhs_pretty = self.pretty_print_value(&rhs_type);
                self.push_message(Message::BinOpMismatchedTypes {
                    range,
                    lhs_range: lhs.range(),
                    rhs_range: rhs.range(),
                    op,
                    lhs: lhs_pretty,
                    rhs: rhs_pretty,
                });
                return self.synth_reported_error(range);
            }
        };

        let fun_head = core::Term::Prim(op.range().into(), fun);
        let fun_app = core::Term::FunApp(
            range.into(),
            self.scope.to_scope(core::Term::FunApp(
                Span::merge(&lhs_expr.span(), &rhs_expr.span()),
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

    fn synth_reported_error(&mut self, range: ByteRange) -> (core::Term<'arena>, ArcValue<'arena>) {
        let expr = core::Term::Prim(range.into(), Prim::ReportedError);
        let r#type = self.push_unsolved_type(MetaSource::ReportedErrorType(range));
        (expr, r#type)
    }

    /// Check a series of format fields
    fn check_format_fields(
        &mut self,
        range: ByteRange,
        format_fields: &[FormatField<'_, ByteRange>],
    ) -> (&'arena [StringId], &'arena [core::Term<'arena>]) {
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
                    type_: r#type,
                    expr,
                } => {
                    let (expr, r#type, type_value) = match r#type {
                        Some(r#type) => {
                            let r#type = self.check(r#type, &universe);
                            let type_value = self.eval_env().eval(&r#type);
                            (self.check(expr, &type_value), r#type, type_value)
                        }
                        None => {
                            let (expr, type_value) = self.synth(expr);
                            let r#type = self.quote_env().quote(self.scope, &type_value);
                            (expr, r#type, type_value)
                        }
                    };

                    let field_span = Span::merge(&label_range.into(), &expr.span());
                    let format = core::Term::FunApp(
                        Span::merge(&label_range.into(), &expr.span()),
                        self.scope.to_scope(core::Term::FunApp(
                            field_span,
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
        let match_info = MatchInfo {
            range,
            scrutinee: self.synth_scrutinee(scrutinee_expr),
            expected_type: self.elim_env().force(expected_type),
        };

        self.elab_match(&match_info, true, equations.iter())
    }

    fn synth_scrutinee(&mut self, scrutinee_expr: &Term<'_, ByteRange>) -> Scrutinee<'arena> {
        let (expr, r#type) = self.synth(scrutinee_expr);
        Scrutinee {
            range: scrutinee_expr.range(),
            expr: self.scope.to_scope(expr),
            r#type,
        }
    }

    /// Elaborate a pattern match into a case tree in the core language.
    ///
    /// The implementation is based on the algorithm described in Section 5 of
    /// [“The Implementation of Functional Programming Languages”][impl-fpl].
    ///
    /// [impl-fpl]: https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages/
    fn elab_match<'a>(
        &mut self,
        match_info: &MatchInfo<'arena>,
        is_reachable: bool,
        mut equations: impl Iterator<Item = &'a (Pattern<ByteRange>, Term<'a, ByteRange>)>,
    ) -> core::Term<'arena> {
        match equations.next() {
            Some((pattern, body_expr)) => {
                match self.check_pattern(pattern, &match_info.scrutinee.r#type) {
                    // Named patterns are elaborated to let bindings, where the
                    // scrutinee is bound as a definition in the body expression.
                    // Subsequent patterns are unreachable.
                    CheckedPattern::Binder(range, name) => {
                        self.check_match_reachable(is_reachable, range);

                        let def_name = Some(name);
                        let def_expr = self.eval_env().eval(match_info.scrutinee.expr);
                        let def_type_value = match_info.scrutinee.r#type.clone();
                        let def_type = self.quote_env().quote(self.scope, &def_type_value);

                        self.local_env.push_def(def_name, def_expr, def_type_value);
                        let body_expr = self.check(body_expr, &match_info.expected_type);
                        self.local_env.pop();

                        self.elab_match_unreachable(match_info, equations);

                        core::Term::Let(
                            Span::merge(&range.into(), &body_expr.span()),
                            def_name,
                            self.scope.to_scope(def_type),
                            match_info.scrutinee.expr,
                            self.scope.to_scope(body_expr),
                        )
                    }
                    // Placeholder patterns just elaborate to the body
                    // expression. Subsequent patterns are unreachable.
                    CheckedPattern::Placeholder(range) => {
                        self.check_match_reachable(is_reachable, range);

                        let body_expr = self.check(body_expr, &match_info.expected_type);
                        self.elab_match_unreachable(match_info, equations);

                        body_expr
                    }
                    // If we see a constant pattern we should expect a run of
                    // constants, elaborating to a constant elimination.
                    CheckedPattern::ConstLit(range, r#const) => {
                        self.check_match_reachable(is_reachable, range);

                        let body_expr = self.check(body_expr, &match_info.expected_type);
                        let const_equation = (range, r#const, body_expr);

                        self.elab_match_const(match_info, is_reachable, const_equation, equations)
                    }
                    // If we hit an error, propagate it, while still checking
                    // the body expression and the subsequent branches.
                    CheckedPattern::ReportedError(range) => {
                        self.check(body_expr, &match_info.expected_type);
                        self.elab_match_unreachable(match_info, equations);
                        core::Term::Prim(range.into(), Prim::ReportedError)
                    }
                }
            }
            None => self.elab_match_absurd(is_reachable, match_info),
        }
    }

    /// Ensure that this part of a match expression is reachable, reporting
    /// a message if it is not.
    fn check_match_reachable(&mut self, is_reachable: bool, range: ByteRange) {
        if !is_reachable {
            self.push_message(Message::UnreachablePattern { range });
        }
    }

    /// Elaborate the equations, expecting a series of constant patterns
    fn elab_match_const<'a>(
        &mut self,
        match_info: &MatchInfo<'arena>,
        is_reachable: bool,
        (const_range, r#const, body_expr): (ByteRange, Const, core::Term<'arena>),
        mut equations: impl Iterator<Item = &'a (Pattern<ByteRange>, Term<'a, ByteRange>)>,
    ) -> core::Term<'arena> {
        // The full range of this series of patterns
        let mut full_span = Span::merge(&const_range.into(), &body_expr.span());
        // Temporary vector for accumulating branches
        let mut branches = vec![(r#const, body_expr)];

        // Elaborate a run of constant patterns.
        'patterns: while let Some((pattern, body_expr)) = equations.next() {
            // Update the range up to the end of the next body expression
            full_span = Span::merge(&full_span, &body_expr.range().into());

            // Default expression, defined if we arrive at a default case
            let default_branch;

            match self.check_pattern(pattern, &match_info.scrutinee.r#type) {
                // Accumulate constant pattern. Search for it in the accumulated
                // branches and insert it in order.
                CheckedPattern::ConstLit(range, r#const) => {
                    let body_expr = self.check(body_expr, &match_info.expected_type);

                    // Find insertion index of the branch
                    let insertion_index = branches.binary_search_by(|(probe_const, _)| {
                        Const::partial_cmp(probe_const, &r#const)
                            .expect("attempt to compare non-ordered value")
                    });

                    match insertion_index {
                        Ok(_) => self.push_message(Message::UnreachablePattern { range }),
                        Err(index) => {
                            // This has not yet been covered, so it should be reachable.
                            self.check_match_reachable(is_reachable, range);
                            branches.insert(index, (r#const, body_expr));
                        }
                    }

                    // No default case yet, continue looking for constant patterns.
                    continue 'patterns;
                }

                // Time to elaborate the default pattern. The default case of
                // `core::Term::ConstMatch` binds a variable, so both
                // the named and  placeholder patterns should bind this.
                CheckedPattern::Binder(range, name) => {
                    self.check_match_reachable(is_reachable, range);

                    // TODO: If we know this is an exhaustive match, bind the
                    // scrutinee to a let binding with the elaborated body, and
                    // add it to the branches. This will simplify the
                    // distillation of if expressions.
                    (self.local_env).push_param(Some(name), match_info.scrutinee.r#type.clone());
                    let default_expr = self.check(body_expr, &match_info.expected_type);
                    default_branch = (Some(name), self.scope.to_scope(default_expr) as &_);
                    self.local_env.pop();
                }
                CheckedPattern::Placeholder(range) => {
                    self.check_match_reachable(is_reachable, range);

                    (self.local_env).push_param(None, match_info.scrutinee.r#type.clone());
                    let default_expr = self.check(body_expr, &match_info.expected_type);
                    default_branch = (None, self.scope.to_scope(default_expr) as &_);
                    self.local_env.pop();
                }
                CheckedPattern::ReportedError(range) => {
                    (self.local_env).push_param(None, match_info.scrutinee.r#type.clone());
                    let default_expr = core::Term::Prim(range.into(), Prim::ReportedError);
                    default_branch = (None, self.scope.to_scope(default_expr) as &_);
                    self.local_env.pop();
                }
            };

            // A default pattern was found, check any unreachable patterns.
            self.elab_match_unreachable(match_info, equations);

            return core::Term::ConstMatch(
                full_span,
                match_info.scrutinee.expr,
                self.scope.to_scope_from_iter(branches.into_iter()),
                Some(default_branch),
            );
        }

        // Finished all the constant patterns without encountering a default
        // case. This should have been an exhaustive match, so check to see if
        // all the cases were covered.
        let default_expr = match match_info.scrutinee.r#type.match_prim_spine() {
            // No need for a default case if all the values were covered
            Some((Prim::BoolType, [])) if branches.len() >= 2 => None,
            _ => Some(self.elab_match_absurd(is_reachable, match_info)),
        };

        core::Term::ConstMatch(
            full_span,
            match_info.scrutinee.expr,
            self.scope.to_scope_from_iter(branches.into_iter()),
            default_expr.map(|expr| (None, self.scope.to_scope(expr) as &_)),
        )
    }

    /// Elaborate unreachable match cases. This is useful for that these cases
    /// are correctly typed, even if they are never actually needed.
    fn elab_match_unreachable<'a>(
        &mut self,
        match_info: &MatchInfo<'arena>,
        equations: impl Iterator<Item = &'a (Pattern<ByteRange>, Term<'a, ByteRange>)>,
    ) {
        self.elab_match(match_info, false, equations);
    }

    /// All the equations have been consumed.
    fn elab_match_absurd(
        &mut self,
        is_reachable: bool,
        match_info: &MatchInfo<'arena>,
    ) -> core::Term<'arena> {
        // Report if we can still reach this point
        if is_reachable {
            // TODO: this should be admitted if the scrutinee type is uninhabited
            self.push_message(Message::NonExhaustiveMatchExpr {
                match_expr_range: match_info.range,
                scrutinee_expr_range: match_info.scrutinee.range,
            });
        }
        core::Term::Prim(match_info.range.into(), Prim::ReportedError)
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

/// Simple patterns that have had some initial elaboration performed on them
#[derive(Debug)]
enum CheckedPattern {
    /// Pattern that binds local variable
    Binder(ByteRange, StringId),
    /// Placeholder patterns that match everything
    Placeholder(ByteRange),
    /// Constant literals
    ConstLit(ByteRange, Const),
    /// Error sentinel
    ReportedError(ByteRange),
}

/// Scrutinee of a match expression
struct Scrutinee<'arena> {
    range: ByteRange,
    expr: &'arena core::Term<'arena>,
    r#type: ArcValue<'arena>,
}

struct MatchInfo<'arena> {
    /// The full range of the match expression
    range: ByteRange,
    /// The expression being matched on
    scrutinee: Scrutinee<'arena>,
    /// The expected type of the match arms
    expected_type: ArcValue<'arena>,
}
