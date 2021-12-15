//! Bidirectional elaboration of the surface language into the core language.

use scoped_arena::Scope;
use std::cell::RefCell;
use std::str::FromStr;
use std::sync::Arc;

use crate::alloc::SliceVec;
use crate::core::semantics::{self, ArcValue, Closure, Head, Telescope, Value};
use crate::core::{self, binary, Const, Prim};
use crate::env::{self, EnvLen, GlobalVar, SharedEnv, UniqueEnv};
use crate::source::ByteRange;
use crate::surface::elaboration::reporting::Message;
use crate::surface::{distillation, Pattern, Term};
use crate::{StringId, StringInterner};

mod reporting;
mod unification;

/// Rigid environment.
pub struct RigidEnv<'arena> {
    /// Names of rigid variables.
    names: UniqueEnv<Option<StringId>>,
    /// Types of rigid variables.
    types: UniqueEnv<ArcValue<'arena>>,
    /// Information about the binders. Used when inserting new flexible
    /// variables during [evaluation][semantics::EvalContext::eval].
    infos: UniqueEnv<core::EntryInfo>,
    /// Expressions that will be substituted for rigid variables during
    /// [evaluation][semantics::EvalContext::eval].
    exprs: SharedEnv<ArcValue<'arena>>,
}

impl<'arena> RigidEnv<'arena> {
    /// Construct a new, empty environment.
    pub fn new() -> RigidEnv<'arena> {
        RigidEnv {
            names: UniqueEnv::new(),
            types: UniqueEnv::new(),
            infos: UniqueEnv::new(),
            exprs: SharedEnv::new(),
        }
    }

    pub fn default(
        interner: &RefCell<StringInterner>,
        _scope: &'arena Scope<'arena>,
    ) -> RigidEnv<'arena> {
        use crate::core::Term;
        use crate::env::LocalVar;

        const UNIVERSE: Term<'_> = Term::Universe;
        const VAR0: Term<'_> = Term::RigidVar(LocalVar::last());
        const FORMAT_TYPE: Term<'_> = Term::Prim(Prim::FormatType);

        let mut env = RigidEnv::new();

        let name = |name| Some(interner.borrow_mut().get_or_intern_static(name));
        let close = |term| Closure::new(SharedEnv::new(), term);
        let shared = |value: ArcValue<'static>| move || value.clone();
        let universe = shared(Arc::new(Value::Universe));
        let format_type = shared(Arc::new(Value::prim(Prim::FormatType, [])));

        let array_type = |index_type: Prim| {
            let index_type = Arc::new(Value::prim(index_type, []));
            let output_type = close(&Term::FunType(None, &UNIVERSE, &UNIVERSE));

            Arc::new(Value::FunType(None, index_type, output_type))
        };
        let format_array = |index_type: Prim| {
            let index_type = Arc::new(Value::prim(index_type, []));
            let output_type = close(&Term::FunType(None, &FORMAT_TYPE, &FORMAT_TYPE));

            Arc::new(Value::FunType(None, index_type, output_type))
        };

        let mut define_prim = |prim: Prim, r#type| {
            env.push_def(name(prim.name()), Arc::new(Value::prim(prim, [])), r#type);
        };

        define_prim(Prim::VoidType, universe());

        define_prim(Prim::U8Type, universe());
        define_prim(Prim::U16Type, universe());
        define_prim(Prim::U32Type, universe());
        define_prim(Prim::U64Type, universe());
        define_prim(Prim::S8Type, universe());
        define_prim(Prim::S16Type, universe());
        define_prim(Prim::S32Type, universe());
        define_prim(Prim::S64Type, universe());
        define_prim(Prim::F32Type, universe());
        define_prim(Prim::F64Type, universe());
        define_prim(Prim::Array8Type, array_type(Prim::U8Type));
        define_prim(Prim::Array16Type, array_type(Prim::U16Type));
        define_prim(Prim::Array32Type, array_type(Prim::U32Type));
        define_prim(Prim::Array64Type, array_type(Prim::U64Type));
        define_prim(Prim::PosType, universe());

        define_prim(Prim::FormatType, universe());
        define_prim(
            Prim::FormatSucceed,
            Arc::new(Value::FunType(
                name("Elem"),
                universe(),
                close(&Term::FunType(None, &VAR0, &FORMAT_TYPE)),
            )),
        );
        define_prim(Prim::FormatFail, format_type());
        define_prim(Prim::FormatU8, format_type());
        define_prim(Prim::FormatU16Be, format_type());
        define_prim(Prim::FormatU16Le, format_type());
        define_prim(Prim::FormatU32Be, format_type());
        define_prim(Prim::FormatU32Le, format_type());
        define_prim(Prim::FormatU64Be, format_type());
        define_prim(Prim::FormatU64Le, format_type());
        define_prim(Prim::FormatS8, format_type());
        define_prim(Prim::FormatS16Be, format_type());
        define_prim(Prim::FormatS16Le, format_type());
        define_prim(Prim::FormatS32Be, format_type());
        define_prim(Prim::FormatS32Le, format_type());
        define_prim(Prim::FormatS64Be, format_type());
        define_prim(Prim::FormatS64Le, format_type());
        define_prim(Prim::FormatF32Be, format_type());
        define_prim(Prim::FormatF32Le, format_type());
        define_prim(Prim::FormatF64Be, format_type());
        define_prim(Prim::FormatF64Le, format_type());
        define_prim(Prim::FormatArray8, format_array(Prim::U8Type));
        define_prim(Prim::FormatArray16, format_array(Prim::U16Type));
        define_prim(Prim::FormatArray32, format_array(Prim::U32Type));
        define_prim(Prim::FormatArray64, format_array(Prim::U64Type));
        define_prim(Prim::FormatStreamPos, format_type());
        define_prim(
            Prim::FormatRepr,
            Arc::new(Value::FunType(None, format_type(), close(&UNIVERSE))),
        );

        env
    }

    /// Get the length of the rigid environment.
    fn len(&self) -> EnvLen {
        self.names.len()
    }

    /// Push a rigid definition onto the context.
    fn push_def(
        &mut self,
        name: Option<StringId>,
        expr: ArcValue<'arena>,
        r#type: ArcValue<'arena>,
    ) {
        self.names.push(name);
        self.types.push(r#type);
        self.infos.push(core::EntryInfo::Concrete);
        self.exprs.push(expr);
    }

    /// Push a rigid parameter onto the context.
    fn push_param(&mut self, name: Option<StringId>, r#type: ArcValue<'arena>) -> ArcValue<'arena> {
        // An expression that refers to itself once it is pushed onto the rigid
        // expression environment.
        let expr = Arc::new(Value::rigid_var(self.exprs.len().next_global()));

        self.names.push(name);
        self.types.push(r#type);
        self.infos.push(core::EntryInfo::Abstract);
        self.exprs.push(expr.clone());

        expr
    }

    /// Pop a rigid binder off the context.
    fn pop(&mut self) {
        self.names.pop();
        self.types.pop();
        self.infos.pop();
        self.exprs.pop();
    }

    /// Truncate the rigid environment.
    fn truncate(&mut self, len: EnvLen) {
        self.names.truncate(len);
        self.types.truncate(len);
        self.infos.truncate(len);
        self.exprs.truncate(len);
    }
}

/// The reason why a flexible variable was inserted.
#[derive(Debug, Copy, Clone)]
pub enum FlexSource {
    /// The type of a hole.
    HoleType(ByteRange, StringId),
    /// The expression of a hole.
    HoleExpr(ByteRange, StringId),
    /// The type of a placeholder
    PlaceholderType(ByteRange),
    /// The expression of a placeholder
    PlaceholderExpr(ByteRange),
    /// The type of a placholder pattern.
    PlaceholderPatternType(ByteRange),
    /// The type of a named pattern.
    NamedPatternType(ByteRange, StringId),
    /// The output type of a match expression
    MatchOutputType(ByteRange),
    /// The input type of a function.
    FunInputType(ByteRange),
    /// The output type of a function.
    FunOutputType(ByteRange),
    /// The type of a reported error.
    ReportedErrorType(ByteRange),
}

/// Flexible environment.
pub struct FlexibleEnv<'arena> {
    /// The source of inserted flexible variables, used when reporting [unsolved
    /// flexible variables][Message::UnsolvedFlexibleVar].
    sources: UniqueEnv<FlexSource>,
    /// Types of flexible variables.
    types: UniqueEnv</* TODO: lazy value */ ArcValue<'arena>>,
    /// Expressions that will be substituted for flexible variables during
    /// [evaluation][semantics::EvalContext::eval].
    ///
    /// These will be set to [`None`] when a flexible variable is first
    /// [inserted][Context::push_flexible_term], then will be set to [`Some`]
    /// if a solution is found during [`unification`].
    exprs: UniqueEnv<Option<ArcValue<'arena>>>,
}

impl<'arena> FlexibleEnv<'arena> {
    /// Construct a new, empty environment.
    pub fn new() -> FlexibleEnv<'arena> {
        FlexibleEnv {
            sources: UniqueEnv::new(),
            types: UniqueEnv::new(),
            exprs: UniqueEnv::new(),
        }
    }

    /// Push an unsolved flexible binder onto the context.
    fn push(&mut self, source: FlexSource, r#type: ArcValue<'arena>) -> GlobalVar {
        // TODO: check that hole name is not already in use
        let var = self.exprs.len().next_global();

        self.sources.push(source);
        self.types.push(r#type);
        self.exprs.push(None);

        var
    }

    /// Report on any unsolved flexible variables, or solved named holes.
    fn report<'this>(&'this self) -> impl 'this + Iterator<Item = Message> {
        Iterator::zip(self.sources.iter(), self.exprs.iter()).filter_map(|(&source, expr)| {
            match (expr, source) {
                // Avoid producing messages for some unsolved flexible sources:
                (None, FlexSource::HoleType(_, _)) => None, // should have an unsolved hole expression
                (None, FlexSource::PlaceholderType(_)) => None, // should have an unsolved placeholder expression
                (None, FlexSource::ReportedErrorType(_)) => None, // should already have an error reported
                // For other sources, report an unsolved problem message
                (None, source) => Some(Message::UnsolvedFlexibleVar { source }),
                // Yield messages of solved named holes
                (Some(_), FlexSource::HoleExpr(range, name)) => {
                    Some(Message::HoleSolution { range, name })
                }
                // Ignore solutions of anything else
                (Some(_), _) => None,
            }
        })
    }
}

enum CheckedPattern {
    Name(ByteRange, StringId),
    Placeholder(ByteRange),
    Const(ByteRange, Const),
    ReportedError(ByteRange),
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
    /// Rigid environment.
    rigid_env: RigidEnv<'arena>,
    /// Flexible environment.
    flexible_env: FlexibleEnv<'arena>,
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
            rigid_env: RigidEnv::default(interner, scope),
            flexible_env: FlexibleEnv::new(),
            renaming: unification::PartialRenaming::new(),
            messages: Vec::new(),
        }
    }

    /// Lookup a name in the context.
    fn get_name(&self, name: StringId) -> Option<(core::Term<'arena>, &ArcValue<'arena>)> {
        let rigid_types = Iterator::zip(env::local_vars(), self.rigid_env.types.iter().rev());

        Iterator::zip(self.rigid_env.names.iter().copied().rev(), rigid_types).find_map(
            |(n, (var, r#type))| (Some(name) == n).then(|| (core::Term::RigidVar(var), r#type)),
        )
    }

    /// Push an unsolved flexible binder onto the context.
    fn push_flexible_term(
        &mut self,
        source: FlexSource,
        r#type: ArcValue<'arena>,
    ) -> core::Term<'arena> {
        let rigid_infos = (self.scope).to_scope_from_iter(self.rigid_env.infos.iter().copied());
        core::Term::FlexibleInsertion(self.flexible_env.push(source, r#type), rigid_infos)
    }

    /// Push an unsolved flexible binder onto the context.
    fn push_flexible_value(
        &mut self,
        source: FlexSource,
        r#type: ArcValue<'arena>,
    ) -> ArcValue<'arena> {
        let term = self.push_flexible_term(source, r#type); // TODO: Could avoid allocating the rigid infos
        self.eval_context().eval(&term)
    }

    fn push_message(&mut self, message: impl Into<Message>) {
        self.messages.push(message.into());
    }

    pub fn drain_messages<'this>(&'this mut self) -> impl 'this + Iterator<Item = Message> {
        self.messages.drain(..).chain(self.flexible_env.report())
    }

    pub fn eval_context(&mut self) -> semantics::EvalContext<'arena, '_> {
        semantics::EvalContext::new(&mut self.rigid_env.exprs, &self.flexible_env.exprs)
    }

    pub fn elim_context(&self) -> semantics::ElimContext<'arena, '_> {
        semantics::ElimContext::new(&self.flexible_env.exprs)
    }

    pub fn quote_context<'out_arena>(
        &self,
        scope: &'out_arena Scope<'out_arena>,
    ) -> semantics::QuoteContext<'arena, 'out_arena, '_> {
        semantics::QuoteContext::new(scope, self.rigid_env.len(), &self.flexible_env.exprs)
    }

    fn unification_context(&mut self) -> unification::Context<'arena, '_> {
        unification::Context::new(
            &self.scope,
            &mut self.renaming,
            self.rigid_env.len(),
            &mut self.flexible_env.exprs,
        )
    }

    pub fn distillation_context<'out_arena>(
        &mut self,
        scope: &'out_arena Scope<'out_arena>,
    ) -> distillation::Context<'interner, 'out_arena, '_> {
        distillation::Context::new(
            self.interner,
            scope,
            &mut self.rigid_env.names,
            &self.flexible_env.sources,
        )
    }

    pub fn binary_context(&self) -> binary::Context<'arena, '_> {
        binary::Context::new(&self.flexible_env.exprs)
    }

    /// Reports an error if there are duplicate fields found, returning a slice
    /// of the labels unique labels and an iterator over the unique fields.
    fn report_duplicate_labels<'fields, 'a>(
        &mut self,
        range: ByteRange,
        fields: &'fields [((ByteRange, StringId), Term<'a, ByteRange>)],
    ) -> (
        &'arena [StringId],
        impl Iterator<Item = &'fields ((ByteRange, StringId), Term<'a, ByteRange>)>,
    ) {
        let mut labels = SliceVec::new(self.scope, fields.len());
        // Will only allocate when duplicates are encountered
        let mut duplicate_indices = Vec::new();
        let mut duplicate_labels = Vec::new();

        for (index, ((range, label), _)) in fields.iter().enumerate() {
            if labels.contains(label) {
                duplicate_indices.push(index);
                duplicate_labels.push((*range, *label));
            } else {
                labels.push(*label)
            }
        }

        if !duplicate_labels.is_empty() {
            self.push_message(Message::DuplicateFieldLabels {
                range,
                labels: duplicate_labels,
            });
        }

        let filtered_fields = (fields.iter().enumerate())
            .filter_map(move |(index, field)| (!duplicate_indices.contains(&index)).then(|| field));

        (labels.into(), filtered_fields)
    }

    /// Parse a source string into number, assuming an ASCII encoding.
    fn parse_ascii<T>(&mut self, range: ByteRange, string_id: StringId) -> Option<T>
    where
        T: From<u8> + std::ops::Shl<Output = T> + std::ops::BitOr<Output = T>,
    {
        // TODO: Parse escape codes
        // TODO: Alternate byte orders
        // TODO: Non-ASCII encodings

        let interner = self.interner.borrow();
        let mut data = Some(T::from(0));
        let mut count: u8 = 0;

        for (offset, ch) in interner.resolve(string_id).unwrap().char_indices() {
            if !ch.is_ascii() {
                let ch_start = range.start() + 1 + offset;
                let ch_end = ch_start + ch.len_utf8();

                self.push_message(Message::NonAsciiStringLiteral {
                    invalid_range: ByteRange::new(ch_start, ch_end),
                });
                data = None;
            }

            data = data.filter(|_| usize::from(count) < std::mem::size_of::<T>());
            data = data.map(|data| {
                // Yikes this is a tad ugly. Setting the bytes in reverse order...
                let offset = 8 * (std::mem::size_of::<T>() as u8 - (count + 1));
                data | (T::from(ch as u8) << T::from(offset))
            });
            count += 1;
        }

        if count as usize != std::mem::size_of::<T>() {
            self.push_message(Message::MismatchedStringLiteralByteLength {
                range,
                expected_len: std::mem::size_of::<T>(),
                found_len: count as usize,
            });
            data = None;
        }

        data
    }

    /// Parse a source string into a number.
    fn parse_number<T: FromStr>(&mut self, range: ByteRange, string_id: StringId) -> Option<T>
    where
        T::Err: std::fmt::Display,
    {
        // TODO: Custom parsing and improved errors
        match self.interner.borrow().resolve(string_id).unwrap().parse() {
            Ok(data) => Some(data),
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
        range: ByteRange, // NOTE: could be removed if source info is added to `core::Term`
        expr: core::Term<'arena>,
        type0: &ArcValue<'arena>,
        type1: &ArcValue<'arena>,
    ) -> core::Term<'arena> {
        match self.unification_context().unify(type0, type1) {
            Ok(()) => expr,
            Err(error) => {
                self.push_message(Message::FailedToUnify { range, error });
                core::Term::Prim(Prim::ReportedError)
            }
        }
    }

    /// Check that a pattern matches an expected type.
    ///
    /// Returns a tuple containing:
    ///
    /// - the name of the bound variable
    /// - the type of the bound variable
    fn check_pattern(
        &mut self,
        pattern: &Pattern<'_, ByteRange>,
        expected_type: &ArcValue<'arena>,
    ) -> (CheckedPattern, ArcValue<'arena>) {
        match pattern {
            Pattern::Name(range, name) => {
                (CheckedPattern::Name(*range, *name), expected_type.clone())
            }
            Pattern::Placeholder(range) => {
                (CheckedPattern::Placeholder(*range), expected_type.clone())
            }
            Pattern::Ann(range, pattern, r#type) => {
                let r#type = self.check(r#type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                let r#type = self.eval_context().eval(&r#type);

                let (pattern_name, pattern_type) = self.synth_ann_pattern(pattern, r#type);

                match (self.unification_context()).unify(&pattern_type, &expected_type) {
                    Ok(()) => (pattern_name, pattern_type),
                    Err(error) => {
                        self.push_message(Message::FailedToUnify {
                            range: *range,
                            error,
                        });

                        let source = FlexSource::ReportedErrorType(*range);
                        let r#type = self.push_flexible_value(source, Arc::new(Value::Universe));

                        (CheckedPattern::ReportedError(*range), r#type)
                    }
                }
            }
            Pattern::StringLiteral(range, string) => {
                let constant = match expected_type.match_prim_spine() {
                    Some((Prim::U8Type, [])) => self.parse_ascii(*range, *string).map(Const::U8),
                    Some((Prim::U16Type, [])) => self.parse_ascii(*range, *string).map(Const::U16),
                    Some((Prim::U32Type, [])) => self.parse_ascii(*range, *string).map(Const::U32),
                    Some((Prim::U64Type, [])) => self.parse_ascii(*range, *string).map(Const::U64),
                    // Some((Prim::Array8Type, [len, _])) => todo!(),
                    // Some((Prim::Array16Type, [len, _])) => todo!(),
                    // Some((Prim::Array32Type, [len, _])) => todo!(),
                    // Some((Prim::Array64Type, [len, _])) => todo!(),
                    Some((Prim::ReportedError, _)) => None,
                    _ => {
                        self.push_message(Message::StringLiteralNotSupported { range: *range });
                        None
                    }
                };

                match constant {
                    Some(constant) => (
                        CheckedPattern::Const(*range, constant),
                        expected_type.clone(),
                    ),
                    None => {
                        let source = FlexSource::ReportedErrorType(*range);
                        let r#type = self.push_flexible_value(source, Arc::new(Value::Universe));

                        (CheckedPattern::ReportedError(*range), r#type)
                    }
                }
            }
            Pattern::NumberLiteral(range, number) => {
                let constant = match expected_type.match_prim_spine() {
                    Some((Prim::U8Type, [])) => self.parse_number(*range, *number).map(Const::U8),
                    Some((Prim::U16Type, [])) => self.parse_number(*range, *number).map(Const::U16),
                    Some((Prim::U32Type, [])) => self.parse_number(*range, *number).map(Const::U32),
                    Some((Prim::U64Type, [])) => self.parse_number(*range, *number).map(Const::U64),
                    Some((Prim::S8Type, [])) => self.parse_number(*range, *number).map(Const::S8),
                    Some((Prim::S16Type, [])) => self.parse_number(*range, *number).map(Const::S16),
                    Some((Prim::S32Type, [])) => self.parse_number(*range, *number).map(Const::S32),
                    Some((Prim::S64Type, [])) => self.parse_number(*range, *number).map(Const::S64),
                    Some((Prim::F32Type, [])) => self.parse_number(*range, *number).map(Const::F32),
                    Some((Prim::F64Type, [])) => self.parse_number(*range, *number).map(Const::F64),
                    Some((Prim::ReportedError, _)) => None,
                    _ => {
                        self.push_message(Message::NumericLiteralNotSupported { range: *range });
                        None
                    }
                };

                match constant {
                    Some(constant) => (
                        CheckedPattern::Const(*range, constant),
                        expected_type.clone(),
                    ),
                    None => {
                        let source = FlexSource::ReportedErrorType(*range);
                        let r#type = self.push_flexible_value(source, Arc::new(Value::Universe));

                        (CheckedPattern::ReportedError(*range), r#type)
                    }
                }
            }
        }
    }

    /// Synthesize the type of an annotated pattern.
    ///
    /// Returns a tuple containing:
    ///
    /// - the name of the bound variable
    /// - the type of the bound variable
    fn synth_ann_pattern(
        &mut self,
        pattern: &Pattern<'_, ByteRange>,
        pattern_type: ArcValue<'arena>,
    ) -> (CheckedPattern, ArcValue<'arena>) {
        match pattern {
            Pattern::Ann(_, pattern, r#type) => {
                let r#type = self.check(r#type, &pattern_type);
                let r#type = self.eval_context().eval(&r#type);
                self.synth_ann_pattern(pattern, r#type)
            }
            _ => self.check_pattern(pattern, &pattern_type),
        }
    }

    /// Synthesize the type of a pattern.
    ///
    /// Returns a tuple containing:
    ///
    /// - the name of the bound variable
    /// - the type of the bound variable
    fn synth_pattern(
        &mut self,
        pattern: &Pattern<'_, ByteRange>,
    ) -> (CheckedPattern, ArcValue<'arena>) {
        match pattern {
            Pattern::Name(range, name) => {
                let source = FlexSource::NamedPatternType(*range, *name);
                let r#type = self.push_flexible_value(source, Arc::new(Value::Universe));
                (CheckedPattern::Name(*range, *name), r#type)
            }
            Pattern::Placeholder(range) => {
                let source = FlexSource::PlaceholderPatternType(*range);
                let r#type = self.push_flexible_value(source, Arc::new(Value::Universe));
                (CheckedPattern::Placeholder(*range), r#type)
            }
            Pattern::Ann(_, pattern, r#type) => {
                let r#type = self.check(r#type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                let r#type = self.eval_context().eval(&r#type);
                self.synth_ann_pattern(pattern, r#type)
            }
            Pattern::StringLiteral(range, _) => {
                self.push_message(Message::AmbiguousStringLiteral { range: *range });
                let source = FlexSource::ReportedErrorType(*range);
                let r#type = self.push_flexible_value(source, Arc::new(Value::Universe));
                (CheckedPattern::ReportedError(*range), r#type)
            }
            Pattern::NumberLiteral(range, _) => {
                self.push_message(Message::AmbiguousNumericLiteral { range: *range });
                let source = FlexSource::ReportedErrorType(*range);
                let r#type = self.push_flexible_value(source, Arc::new(Value::Universe));
                (CheckedPattern::ReportedError(*range), r#type)
            }
        }
    }

    /// Push a rigid definition onto the context.
    /// The supplied `pattern` is expected to be irrefutable.
    fn push_rigid_def(
        &mut self,
        pattern: CheckedPattern,
        expr: ArcValue<'arena>,
        r#type: ArcValue<'arena>,
    ) -> Option<StringId> {
        let name = match pattern {
            CheckedPattern::Name(_, name) => Some(name),
            CheckedPattern::Placeholder(_) => None,
            // FIXME: generate failing output expressions?
            CheckedPattern::Const(range, _) => {
                self.push_message(Message::RefutablePattern {
                    pattern_range: range,
                });
                None
            }
            CheckedPattern::ReportedError(_) => None,
        };

        self.rigid_env.push_def(name, expr, r#type);

        name
    }

    /// Push a rigid parameter onto the context.
    /// The supplied `pattern` is expected to be irrefutable.
    fn push_rigid_param(
        &mut self,
        pattern: CheckedPattern,
        r#type: ArcValue<'arena>,
    ) -> (Option<StringId>, ArcValue<'arena>) {
        let name = match pattern {
            CheckedPattern::Name(_, name) => Some(name),
            CheckedPattern::Placeholder(_) => None,
            // FIXME: generate failing output expressions?
            CheckedPattern::Const(range, _) => {
                self.push_message(Message::RefutablePattern {
                    pattern_range: range,
                });
                None
            }
            CheckedPattern::ReportedError(_) => None,
        };

        let expr = self.rigid_env.push_param(name, r#type);

        (name, expr)
    }

    /// Check that a surface term conforms to the given type.
    ///
    /// Returns the elaborated term in the core language.
    pub fn check(
        &mut self,
        surface_term: &Term<'_, ByteRange>,
        expected_type: &ArcValue<'arena>,
    ) -> core::Term<'arena> {
        let expected_type = self.elim_context().force(expected_type);

        match (surface_term, expected_type.as_ref()) {
            (Term::Let(_, def_pattern, def_expr, output_expr), _) => {
                let (def_pattern, def_type_value) = self.synth_pattern(def_pattern);
                let def_type = self.quote_context(self.scope).quote(&def_type_value); // FIXME: avoid requote if possible?
                let def_expr = self.check(def_expr, &def_type_value);
                let def_expr_value = self.eval_context().eval(&def_expr);

                let def_name = self.push_rigid_def(def_pattern, def_expr_value, def_type_value); // TODO: split on constants
                let output_expr = self.check(output_expr, &expected_type);
                self.rigid_env.pop();

                core::Term::Let(
                    def_name,
                    self.scope.to_scope(def_type),
                    self.scope.to_scope(def_expr),
                    self.scope.to_scope(output_expr),
                )
            }
            (Term::Match(range, scrutinee_expr, equations), _) => {
                let scrutinee_range = scrutinee_expr.range();
                let (scrutinee_expr, scrutinee_type) = self.synth(scrutinee_expr);

                self.check_match(
                    true,
                    *range,
                    scrutinee_range,
                    self.scope.to_scope(scrutinee_expr),
                    &scrutinee_type,
                    equations,
                    &expected_type,
                )
            }
            (
                Term::FunLiteral(_, input_pattern, output_expr),
                Value::FunType(_, input_type, output_type),
            ) => {
                let (input_name, input_type) = self.check_pattern(input_pattern, input_type);

                let (input_name, input_expr) = self.push_rigid_param(input_name, input_type);
                let output_type = self.elim_context().apply_closure(output_type, input_expr);
                let output_expr = self.check(output_expr, &output_type);

                self.rigid_env.pop();

                core::Term::FunIntro(input_name, self.scope.to_scope(output_expr))
            }
            (Term::RecordLiteral(range, expr_fields), Value::RecordType(labels, types)) => {
                // TODO: improve handling of duplicate labels
                if expr_fields.len() != labels.len()
                    || Iterator::zip(expr_fields.iter(), labels.iter())
                        .any(|(((_, expr_label), _), type_label)| expr_label != type_label)
                {
                    self.push_message(Message::MismatchedFieldLabels {
                        range: *range,
                        expr_labels: (expr_fields.iter())
                            .map(|(ranged_label, _)| *ranged_label)
                            .collect(),
                        type_labels: labels.iter().copied().collect(),
                    });
                    return core::Term::Prim(Prim::ReportedError);
                }

                let mut types = types.clone();
                let mut expr_fields = expr_fields.iter();
                let mut exprs = SliceVec::new(self.scope, types.len());

                while let Some(((_, expr), (r#type, next_types))) = Option::zip(
                    expr_fields.next(),
                    self.elim_context().split_telescope(types),
                ) {
                    let expr = self.check(expr, &r#type);
                    types = next_types(self.eval_context().eval(&expr));
                    exprs.push(expr);
                }

                core::Term::RecordIntro(labels, exprs.into())
            }
            (Term::UnitLiteral(_), Value::Universe) => core::Term::RecordType(&[], &[]),
            (Term::UnitLiteral(_), _)
                if matches!(
                    expected_type.match_prim_spine(),
                    Some((Prim::FormatType, [])),
                ) =>
            {
                core::Term::FormatRecord(&[], &[])
            }
            (Term::ArrayLiteral(range, elem_exprs), _) => {
                use crate::core::semantics::Elim::Fun;

                let (len, elem_type) = match expected_type.match_prim_spine() {
                    Some((Prim::Array8Type, [Fun(len), Fun(elem_type)])) => (len, elem_type),
                    Some((Prim::Array16Type, [Fun(len), Fun(elem_type)])) => (len, elem_type),
                    Some((Prim::Array32Type, [Fun(len), Fun(elem_type)])) => (len, elem_type),
                    Some((Prim::Array64Type, [Fun(len), Fun(elem_type)])) => (len, elem_type),
                    Some((Prim::ReportedError, _)) => return core::Term::Prim(Prim::ReportedError),
                    _ => {
                        self.push_message(Message::ArrayLiteralNotSupported { range: *range });
                        return core::Term::Prim(Prim::ReportedError);
                    }
                };

                match len.as_ref() {
                    Value::Const(Const::U8(len)) if elem_exprs.len() as u64 == *len as u64 => {}
                    Value::Const(Const::U16(len)) if elem_exprs.len() as u64 == *len as u64 => {}
                    Value::Const(Const::U32(len)) if elem_exprs.len() as u64 == *len as u64 => {}
                    Value::Const(Const::U64(len)) if elem_exprs.len() as u64 == *len as u64 => {}
                    Value::Stuck(Head::Prim(Prim::ReportedError), _) => {
                        return core::Term::Prim(Prim::ReportedError);
                    }
                    _ => {
                        // Check the array elements anyway in order to report
                        // any errors inside the literal as well.
                        for elem_expr in *elem_exprs {
                            self.check(elem_expr, elem_type);
                        }

                        self.push_message(Message::MismatchedArrayLength {
                            range: *range,
                            found_len: elem_exprs.len(),
                        });

                        return core::Term::Prim(Prim::ReportedError);
                    }
                }

                let elem_exprs = self.scope.to_scope_from_iter(
                    (elem_exprs.iter()).map(|elem_expr| self.check(elem_expr, elem_type)),
                );

                core::Term::ArrayIntro(elem_exprs)
            }
            (Term::StringLiteral(range, string), _) => {
                let constant = match expected_type.match_prim_spine() {
                    Some((Prim::U8Type, [])) => self.parse_ascii(*range, *string).map(Const::U8),
                    Some((Prim::U16Type, [])) => self.parse_ascii(*range, *string).map(Const::U16),
                    Some((Prim::U32Type, [])) => self.parse_ascii(*range, *string).map(Const::U32),
                    Some((Prim::U64Type, [])) => self.parse_ascii(*range, *string).map(Const::U64),
                    // Some((Prim::Array8Type, [len, _])) => todo!(),
                    // Some((Prim::Array16Type, [len, _])) => todo!(),
                    // Some((Prim::Array32Type, [len, _])) => todo!(),
                    // Some((Prim::Array64Type, [len, _])) => todo!(),
                    Some((Prim::ReportedError, _)) => None,
                    _ => {
                        self.push_message(Message::StringLiteralNotSupported { range: *range });
                        None
                    }
                };

                match constant {
                    Some(constant) => core::Term::Const(constant),
                    None => core::Term::Prim(Prim::ReportedError),
                }
            }
            (Term::NumberLiteral(range, number), _) => {
                let constant = match expected_type.match_prim_spine() {
                    Some((Prim::U8Type, [])) => self.parse_number(*range, *number).map(Const::U8),
                    Some((Prim::U16Type, [])) => self.parse_number(*range, *number).map(Const::U16),
                    Some((Prim::U32Type, [])) => self.parse_number(*range, *number).map(Const::U32),
                    Some((Prim::U64Type, [])) => self.parse_number(*range, *number).map(Const::U64),
                    Some((Prim::S8Type, [])) => self.parse_number(*range, *number).map(Const::S8),
                    Some((Prim::S16Type, [])) => self.parse_number(*range, *number).map(Const::S16),
                    Some((Prim::S32Type, [])) => self.parse_number(*range, *number).map(Const::S32),
                    Some((Prim::S64Type, [])) => self.parse_number(*range, *number).map(Const::S64),
                    Some((Prim::F32Type, [])) => self.parse_number(*range, *number).map(Const::F32),
                    Some((Prim::F64Type, [])) => self.parse_number(*range, *number).map(Const::F64),
                    Some((Prim::ReportedError, _)) => None,
                    _ => {
                        self.push_message(Message::NumericLiteralNotSupported { range: *range });
                        return core::Term::Prim(Prim::ReportedError);
                    }
                };

                match constant {
                    Some(constant) => core::Term::Const(constant),
                    None => core::Term::Prim(Prim::ReportedError),
                }
            }
            (Term::ReportedError(_), _) => core::Term::Prim(Prim::ReportedError),
            (_, _) => {
                let (core_term, synth_type) = self.synth(surface_term);
                self.convert(surface_term.range(), core_term, &synth_type, &expected_type)
            }
        }
    }

    /// Synthesize the type of the given surface term.
    ///
    /// Returns the elaborated term in the core language and its type.
    pub fn synth(
        &mut self,
        surface_term: &Term<'_, ByteRange>,
    ) -> (core::Term<'arena>, ArcValue<'arena>) {
        match surface_term {
            Term::Name(range, name) => match self.get_name(*name) {
                Some((term, r#type)) => (term, r#type.clone()),
                None => {
                    self.push_message(Message::UnboundName {
                        range: *range,
                        name: *name,
                    });
                    self.synth_reported_error(*range)
                }
            },
            Term::Hole(range, name) => {
                let type_source = FlexSource::HoleType(*range, *name);
                let expr_source = FlexSource::HoleExpr(*range, *name);

                let r#type = self.push_flexible_value(type_source, Arc::new(Value::Universe));
                let expr = self.push_flexible_term(expr_source, r#type.clone());

                (expr, r#type)
            }
            Term::Placeholder(range) => {
                let type_source = FlexSource::PlaceholderType(*range);
                let expr_source = FlexSource::PlaceholderExpr(*range);

                let r#type = self.push_flexible_value(type_source, Arc::new(Value::Universe));
                let expr = self.push_flexible_term(expr_source, r#type.clone());

                (expr, r#type)
            }
            Term::Ann(_, expr, r#type) => {
                let r#type = self.check(r#type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                let type_value = self.eval_context().eval(&r#type);
                let expr = self.check(expr, &type_value);

                let ann_expr =
                    core::Term::Ann(self.scope.to_scope(expr), self.scope.to_scope(r#type));

                (ann_expr, type_value)
            }
            Term::Let(_, def_pattern, def_expr, output_expr) => {
                let (def_pattern, def_type_value) = self.synth_pattern(def_pattern);
                let def_type = self.quote_context(self.scope).quote(&def_type_value); // FIXME: avoid requote if possible?
                let def_expr = self.check(def_expr, &def_type_value);
                let def_expr_value = self.eval_context().eval(&def_expr);

                let def_name = self.push_rigid_def(def_pattern, def_expr_value, def_type_value);
                let (output_expr, output_type) = self.synth(output_expr);
                self.rigid_env.pop();

                let let_expr = core::Term::Let(
                    def_name,
                    self.scope.to_scope(def_type),
                    self.scope.to_scope(def_expr),
                    self.scope.to_scope(output_expr),
                );

                (let_expr, output_type)
            }
            Term::Match(range, scrutinee_expr, equations) => {
                let scrutinee_range = scrutinee_expr.range();
                let (scrutinee_expr, scrutinee_type) = self.synth(scrutinee_expr);

                // Create a single flexible variable representing the type of
                // the match expression's output expressions, allowing us to
                // unify them together.
                let source = FlexSource::MatchOutputType(*range);
                let universe = Arc::new(Value::Universe);
                let output_type = self.push_flexible_value(source, universe);

                let match_expr = self.check_match(
                    true,
                    *range,
                    scrutinee_range,
                    self.scope.to_scope(scrutinee_expr),
                    &scrutinee_type,
                    equations,
                    &output_type,
                );

                (match_expr, output_type)
            }
            Term::Universe(_) => (core::Term::Universe, Arc::new(Value::Universe)),
            Term::Arrow(_, input_type, output_type) => {
                let universe = Arc::new(Value::Universe); // FIXME: avoid temporary Arc
                let input_type = self.check(input_type, &universe);
                let input_type_value = self.eval_context().eval(&input_type);

                self.rigid_env.push_param(None, input_type_value);
                let output_type = self.check(output_type, &universe);
                self.rigid_env.pop();

                let fun_type = core::Term::FunType(
                    None,
                    self.scope.to_scope(input_type),
                    self.scope.to_scope(output_type),
                );

                (fun_type, universe)
            }
            Term::FunType(_, input_pattern, output_type) => {
                let universe = Arc::new(Value::Universe); // FIXME: avoid temporary Arc
                let (input_pattern, input_type_value) = self.synth_pattern(input_pattern);
                let input_type = self.quote_context(self.scope).quote(&input_type_value); // FIXME: avoid requote if possible?

                let (input_name, _) = self.push_rigid_param(input_pattern, input_type_value);
                let output_type = self.check(output_type, &universe);
                self.rigid_env.pop();

                let fun_type = core::Term::FunType(
                    input_name,
                    self.scope.to_scope(input_type),
                    self.scope.to_scope(output_type),
                );

                (fun_type, universe)
            }
            Term::FunLiteral(_, input_pattern, output_expr) => {
                let (input_pattern, input_type) = self.synth_pattern(input_pattern);

                let (input_name, _) = self.push_rigid_param(input_pattern, input_type.clone());
                let (output_expr, output_type) = self.synth(output_expr);
                let output_type = self.quote_context(self.scope).quote(&output_type);
                self.rigid_env.pop();

                (
                    core::Term::FunIntro(input_name, self.scope.to_scope(output_expr)),
                    Arc::new(Value::FunType(
                        input_name,
                        input_type,
                        Closure::new(
                            self.rigid_env.exprs.clone(),
                            self.scope.to_scope(output_type),
                        ),
                    )),
                )
            }
            Term::FunElim(range, head_expr, input_expr) => {
                let head_range = head_expr.range();
                let (head_expr, head_type) = self.synth(head_expr);

                // Ensure that the head type is a function type
                let head_type = self.elim_context().force(&head_type);
                let (head_expr, input_type, output_type) = match head_type.as_ref() {
                    // The simple case - it's easy to see that it is a function type!
                    Value::FunType(_, input_type, output_type) => {
                        (head_expr, input_type.clone(), output_type.clone())
                    }
                    Value::Stuck(Head::Prim(Prim::ReportedError), _) => {
                        return self.synth_reported_error(*range);
                    }
                    // It's not immediately obvious that the head type is a
                    // function type, so instead we construct a function type
                    // with flexible variables standing-in for the input and
                    // output types, and then we attempt to unify the head type
                    // against it.
                    _ => {
                        let universe = Arc::new(Value::Universe);
                        // Create a flexible input type
                        let input_source = FlexSource::FunInputType(head_range);
                        let input_type = self.push_flexible_value(input_source, universe.clone());

                        // Create a flexible output type, with the input bound
                        self.rigid_env.push_param(None, input_type.clone());
                        let output_source = FlexSource::FunOutputType(head_range);
                        let output_type = self.push_flexible_term(output_source, universe);
                        self.rigid_env.pop();

                        // Create a function type between the flexible variables.
                        let output_type = Closure::new(
                            self.rigid_env.exprs.clone(),
                            self.scope.to_scope(output_type),
                        );
                        let fun_type = Arc::new(Value::FunType(
                            None,
                            input_type.clone(),
                            output_type.clone(),
                        ));

                        // Unify the type of the head expression with the function type
                        let head_expr = self.convert(head_range, head_expr, &head_type, &fun_type);

                        (head_expr, input_type, output_type)
                    }
                };

                // Check the input expression and apply it to the output type
                let input_expr = self.check(input_expr, &input_type);
                let input_expr_value = self.eval_context().eval(&input_expr);
                let output_type = self
                    .elim_context()
                    .apply_closure(&output_type, input_expr_value);

                // Construct the final elimination
                let fun_elim = core::Term::FunElim(
                    self.scope.to_scope(head_expr),
                    self.scope.to_scope(input_expr),
                );

                (fun_elim, output_type)
            }
            Term::RecordType(range, type_fields) => {
                let universe = Arc::new(Value::Universe);
                let initial_rigid_len = self.rigid_env.len();
                let (labels, type_fields) = self.report_duplicate_labels(*range, type_fields);
                let mut types = SliceVec::new(self.scope, labels.len());

                for ((_, label), r#type) in type_fields {
                    let r#type = self.check(r#type, &universe);
                    let type_value = self.eval_context().eval(&r#type);
                    self.rigid_env.push_param(Some(*label), type_value);
                    types.push(r#type);
                }

                self.rigid_env.truncate(initial_rigid_len);

                (core::Term::RecordType(labels, types.into()), universe)
            }
            Term::RecordLiteral(range, expr_fields) => {
                let (labels, expr_fields) = self.report_duplicate_labels(*range, expr_fields);
                let mut types = SliceVec::new(self.scope, labels.len());
                let mut exprs = SliceVec::new(self.scope, labels.len());

                for (_, expr) in expr_fields {
                    let (expr, r#type) = self.synth(expr);
                    types.push(self.quote_context(self.scope).quote(&r#type)); // NOTE: Unsure if these are correctly bound!
                    exprs.push(expr);
                }

                let types = Telescope::new(self.rigid_env.exprs.clone(), types.into());

                (
                    core::Term::RecordIntro(labels, exprs.into()),
                    Arc::new(Value::RecordType(labels, types)),
                )
            }
            Term::UnitLiteral(_) => (
                core::Term::RecordIntro(&[], &[]),
                Arc::new(Value::RecordType(
                    &[],
                    Telescope::new(SharedEnv::new(), &[]),
                )),
            ),
            Term::RecordElim(range, head_expr, (label_range, label)) => {
                let head_range = head_expr.range();
                let (head_expr, head_type) = self.synth(head_expr);
                let head_expr_value = self.eval_context().eval(&head_expr);

                let head_type = self.elim_context().force(&head_type);
                match head_type.as_ref() {
                    Value::RecordType(labels, types) => {
                        let mut labels = labels.iter();
                        let mut types = types.clone();

                        while let Some((type_label, (r#type, next_types))) =
                            Option::zip(labels.next(), self.elim_context().split_telescope(types))
                        {
                            if label == type_label {
                                let head_expr = self.scope.to_scope(head_expr);
                                let expr = core::Term::RecordElim(head_expr, *label);
                                return (expr, r#type);
                            } else {
                                let head_expr = head_expr_value.clone();
                                let expr = self.elim_context().apply_record(head_expr, *type_label);
                                types = next_types(expr);
                            }
                        }
                    }
                    Value::Stuck(Head::Prim(Prim::ReportedError), _) => {
                        return self.synth_reported_error(*range);
                    }
                    _ => {}
                }

                self.push_message(Message::UnknownField {
                    head_range,
                    label_range: *label_range,
                    label: *label,
                });
                self.synth_reported_error(*range)
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
            Term::FormatRecord(range, format_fields) => {
                let format_type = Arc::new(Value::prim(Prim::FormatType, []));
                let initial_rigid_len = self.rigid_env.len();
                let (labels, format_fields) = self.report_duplicate_labels(*range, format_fields);
                let mut formats = SliceVec::new(self.scope, labels.len());

                for ((_, label), format) in format_fields {
                    let format = self.check(format, &format_type);
                    let format_value = self.eval_context().eval(&format);
                    let r#type = self.elim_context().apply_repr(&format_value);
                    self.rigid_env.push_param(Some(*label), r#type);
                    formats.push(format);
                }

                self.rigid_env.truncate(initial_rigid_len);

                (
                    core::Term::FormatRecord(labels, formats.into()),
                    format_type,
                )
            }
            Term::ReportedError(range) => self.synth_reported_error(*range),
        }
    }

    fn synth_reported_error(&mut self, range: ByteRange) -> (core::Term<'arena>, ArcValue<'arena>) {
        let type_source = FlexSource::ReportedErrorType(range);
        let r#type = self.push_flexible_value(type_source, Arc::new(Value::Universe));
        (core::Term::Prim(Prim::ReportedError), r#type)
    }

    /// Elaborate a pattern match.
    fn check_match(
        &mut self,
        is_reachable: bool,
        match_range: ByteRange,
        scrutinee_range: ByteRange,
        scrutinee_expr: &'arena core::Term<'arena>,
        scrutinee_type: &ArcValue<'arena>,
        mut equations: &[(Pattern<'_, ByteRange>, Term<'_, ByteRange>)],
        /* default expression: &Fn() -> Term<'arena> */
        expected_type: &ArcValue<'arena>,
    ) -> core::Term<'arena> {
        match equations.split_first() {
            Some(((pattern, output_expr), next_equations)) => {
                let (def_pattern, def_type_value) = self.check_pattern(pattern, &scrutinee_type);
                let def_type = self.quote_context(self.scope).quote(&def_type_value); // FIXME: avoid requote if possible?

                // Warn about unreachable patterns, only when checking the pattern was a success
                if !is_reachable && !matches!(def_pattern, CheckedPattern::ReportedError(_)) {
                    self.push_message(Message::UnreachablePattern {
                        range: pattern.range(),
                    });
                }

                match def_pattern {
                    CheckedPattern::Name(_, name) => {
                        let def_name = Some(name);
                        let def_expr = self.eval_context().eval(&scrutinee_expr);
                        self.rigid_env.push_def(def_name, def_expr, def_type_value);
                        let output_expr = self.check(output_expr, &expected_type);
                        self.rigid_env.pop();

                        // These patterns are unreachable, but check them anyway!
                        self.check_match(
                            false,
                            match_range,
                            scrutinee_range,
                            scrutinee_expr,
                            scrutinee_type,
                            next_equations,
                            expected_type,
                        );

                        core::Term::Let(
                            def_name,
                            self.scope.to_scope(def_type),
                            scrutinee_expr,
                            self.scope.to_scope(output_expr),
                        )
                    }
                    CheckedPattern::Placeholder(_) => {
                        let output_expr = self.check(output_expr, &expected_type);

                        // These patterns are unreachable, but check them anyway!
                        self.check_match(
                            false,
                            match_range,
                            scrutinee_range,
                            scrutinee_expr,
                            scrutinee_type,
                            next_equations,
                            expected_type,
                        );

                        output_expr
                    }
                    CheckedPattern::Const(_, _) => {
                        // Temporary vector for accumulating branches
                        let mut branches = Vec::new();

                        // Collect a run of constant patterns
                        while let Some(((pattern, output_expr), next_equations)) =
                            equations.split_first()
                        {
                            let (def_pattern, _) = self.check_pattern(pattern, &scrutinee_type);
                            match def_pattern {
                                // Accumulate constant pattern
                                CheckedPattern::Const(_, r#const) => {
                                    let output_term = self.check(output_expr, expected_type);
                                    // TODO: push in order
                                    // TODO: check for/avoid duplicates
                                    branches.push((r#const, output_term));
                                    equations = next_equations;
                                }

                                // Time for the default pattern
                                CheckedPattern::Name(_, _)
                                | CheckedPattern::Placeholder(_)
                                | CheckedPattern::ReportedError(_) => {
                                    // Check the default expression and any other
                                    // unreachable equaltions following that.
                                    let default_expr = self.check_match(
                                        true,
                                        match_range,
                                        scrutinee_range,
                                        scrutinee_expr,
                                        scrutinee_type,
                                        equations,
                                        expected_type,
                                    );

                                    return core::Term::ConstElim(
                                        scrutinee_expr,
                                        self.scope.to_scope_from_iter(branches.into_iter()),
                                        self.scope.to_scope(default_expr),
                                    );
                                }
                            }
                        }

                        if is_reachable {
                            // TODO: this should be admitted if the scrutinee type is uninhabited
                            self.push_message(Message::NonExhaustiveMatchExpr {
                                match_expr_range: match_range,
                                scrutinee_expr_range: scrutinee_range,
                            });
                        }
                        core::Term::Prim(Prim::ReportedError)
                    }
                    CheckedPattern::ReportedError(_) => {
                        // Check for any further errors in the first equation's output expression.
                        self.check(output_expr, expected_type);
                        self.check_match(
                            false,
                            match_range,
                            scrutinee_range,
                            scrutinee_expr,
                            scrutinee_type,
                            next_equations,
                            expected_type,
                        );

                        core::Term::Prim(Prim::ReportedError)
                    }
                }
            }
            None => {
                if is_reachable {
                    // TODO: this should be admitted if the scrutinee type is uninhabited
                    self.push_message(Message::NonExhaustiveMatchExpr {
                        match_expr_range: match_range,
                        scrutinee_expr_range: scrutinee_range,
                    });
                }
                core::Term::Prim(Prim::ReportedError)
            }
        }
    }
}
