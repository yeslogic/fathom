//! Bidirectional elaboration of the surface language into the core language.

use scoped_arena::Scope;
use std::sync::Arc;

use crate::core::semantics::{self, ArcValue, Closure, Telescope, Value};
use crate::env::{self, EnvLen, SharedEnv, UniqueEnv};
use crate::surface::elaboration::reporting::Message;
use crate::surface::Term;
use crate::{core, ByteRange, SliceBuilder, StringId};

mod reporting;
mod unification;

/// The reason why a flexible variable was inserted.
#[derive(Debug, Copy, Clone)]
pub enum FlexSource {
    /// The type of a hole.
    HoleType(Option<StringId>),
    /// The expression of a hole.
    HoleExpr(Option<StringId>),
    /// The input type of a function.
    FunInputType(Option<StringId>),
    /// The output type of a function.
    FunOutputType,
    /// The type of a reported error.
    ReportedErrorType,
}

/// Elaboration context.
pub struct Context<'arena> {
    /// Scoped arena for storing elaborated terms.
    //
    // TODO: Make this local to the elaboration context, and reallocate
    //       elaborated terms to an external `Scope` during zonking, resetting
    //       this scope on completion.
    scope: &'arena Scope<'arena>,
    /// A partial renaming to be used during [`unification`].
    renaming: unification::PartialRenaming,

    /// Names of rigid variables.
    rigid_names: UniqueEnv<Option<StringId>>,
    /// Types of rigid variables.
    rigid_types: UniqueEnv<ArcValue<'arena>>,
    /// Information about the binders. Used when inserting new flexible variables.
    rigid_infos: UniqueEnv<core::EntryInfo>,
    /// Expressions that will be substituted for rigid variables during
    /// [evaluation][EvalContext::eval].
    rigid_exprs: SharedEnv<ArcValue<'arena>>,

    /// The source of inserted flexible variables, used when reporting [unsolved
    /// flexible variables][Message::UnsolvedFlexibleVar].
    flexible_sources: UniqueEnv<(ByteRange, FlexSource)>,
    /// Expressions that will be substituted for flexible variables during
    /// [evaluation][EvalContext::eval].
    ///
    /// These will be set to [`None`] when a flexible variable is first
    /// [inserted][Context::push_flexible_term], then will be set to [`Some`]
    /// if a solution is found during [`unification`].
    flexible_exprs: UniqueEnv<Option<ArcValue<'arena>>>,

    /// Diagnostic messages encountered during elaboration.
    messages: Vec<Message>,
}

impl<'arena> Context<'arena> {
    /// Construct a new elaboration context, backed by the supplied arena.
    pub fn new(scope: &'arena Scope<'arena>) -> Context<'arena> {
        Context {
            scope,
            renaming: unification::PartialRenaming::new(),

            rigid_names: UniqueEnv::new(),
            rigid_types: UniqueEnv::new(),
            rigid_infos: UniqueEnv::new(),
            rigid_exprs: SharedEnv::new(),

            flexible_sources: UniqueEnv::new(),
            flexible_exprs: UniqueEnv::new(),

            messages: Vec::new(),
        }
    }

    /// Lookup a name in the context.
    fn get_name(&self, name: StringId) -> Option<(core::Term<'arena>, &ArcValue<'arena>)> {
        let rigid_types = Iterator::zip(env::local_vars(), self.rigid_types.iter().rev());

        Iterator::zip(self.rigid_names.iter().copied().rev(), rigid_types).find_map(
            |(n, (var, r#type))| (Some(name) == n).then(|| (core::Term::RigidVar(var), r#type)),
        )
    }

    /// Get the length of the rigid environment.
    fn rigid_len(&mut self) -> EnvLen {
        self.rigid_names.len()
    }

    /// Push a rigid definition onto the context.
    fn push_rigid_definition(
        &mut self,
        name: Option<StringId>,
        expr: ArcValue<'arena>,
        r#type: ArcValue<'arena>,
    ) {
        self.rigid_names.push(name);
        self.rigid_types.push(r#type);
        self.rigid_infos.push(core::EntryInfo::Concrete);
        self.rigid_exprs.push(expr);
    }

    /// Push a rigid parameter onto the context.
    fn push_rigid_parameter(
        &mut self,
        name: Option<StringId>,
        r#type: ArcValue<'arena>,
    ) -> ArcValue<'arena> {
        // An expression that refers to itself once it is pushed onto the rigid
        // expression environment.
        let expr = Arc::new(Value::rigid_var(self.rigid_exprs.len().next_global()));

        self.rigid_names.push(name);
        self.rigid_types.push(r#type);
        self.rigid_infos.push(core::EntryInfo::Abstract);
        self.rigid_exprs.push(expr.clone());

        expr
    }

    /// Pop a rigid binder off the context.
    fn pop_rigid(&mut self) {
        self.rigid_names.pop();
        self.rigid_types.pop();
        self.rigid_infos.pop();
        self.rigid_exprs.pop();
    }

    /// Truncate the rigid environment.
    fn truncate_rigid(&mut self, len: EnvLen) {
        self.rigid_names.truncate(len);
        self.rigid_types.truncate(len);
        self.rigid_infos.truncate(len);
        self.rigid_exprs.truncate(len);
    }

    /// Push an unsolved flexible binder onto the context.
    fn push_flexible_term(&mut self, range: ByteRange, source: FlexSource) -> core::Term<'arena> {
        // TODO: check that hole name is not already in use
        let var = self.flexible_exprs.len().next_global();

        self.flexible_sources.push((range, source));
        self.flexible_exprs.push(None);

        core::Term::FlexibleInsertion(var, self.rigid_infos.clone())
    }

    /// Push an unsolved flexible binder onto the context.
    fn push_flexible_value(&mut self, range: ByteRange, source: FlexSource) -> ArcValue<'arena> {
        let term = self.push_flexible_term(range, source);
        self.eval_context().eval(&term)
    }

    fn push_message(&mut self, message: impl Into<Message>) {
        self.messages.push(message.into());
    }

    pub fn drain_messages<'this>(&'this mut self) -> impl 'this + Iterator<Item = Message> {
        self.messages.drain(..).chain(
            Iterator::zip(self.flexible_sources.iter(), self.flexible_exprs.iter()).filter_map(
                |(&(range, source), expr)| match (expr, source) {
                    // Avoid producing messages for some unsolved flexible sources:
                    (None, FlexSource::HoleType(_)) => None, // should have an unsolved hole expression
                    (None, FlexSource::ReportedErrorType) => None, // should already have an error reported
                    // For other sources, report an unsolved problem message
                    (None, source) => Some(Message::UnsolvedFlexibleVar { range, source }),
                    // Yield messages of solved named holes
                    (Some(_), FlexSource::HoleExpr(Some(name))) => {
                        Some(Message::HoleSolution { range, name })
                    }
                    // Ignore solutions of anything else
                    (Some(_), _) => None,
                },
            ),
        )
    }

    pub fn force(&mut self, term: &ArcValue<'arena>) -> ArcValue<'arena> {
        semantics::ElimContext::new(&self.flexible_exprs).force(term)
    }

    pub fn eval_context<'this>(&'this mut self) -> semantics::EvalContext<'arena, 'this> {
        semantics::EvalContext::new(&mut self.rigid_exprs, &self.flexible_exprs)
    }

    pub fn elim_context<'this>(&'this self) -> semantics::ElimContext<'arena, 'this> {
        semantics::ElimContext::new(&self.flexible_exprs)
    }

    pub fn quote_context<'out_arena>(
        &self,
        scope: &'out_arena Scope<'out_arena>,
    ) -> semantics::QuoteContext<'arena, 'out_arena, '_> {
        semantics::QuoteContext::new(scope, self.rigid_exprs.len(), &self.flexible_exprs)
    }

    fn close_term(&self, term: core::Term<'arena>) -> Closure<'arena> {
        Closure::new(self.rigid_exprs.clone(), self.scope.to_scope(term))
    }

    fn apply_closure(
        &mut self,
        closure: &Closure<'arena>,
        input_expr: ArcValue<'arena>,
    ) -> ArcValue<'arena> {
        semantics::ElimContext::new(&self.flexible_exprs).apply_closure(closure, input_expr)
    }

    fn unification_context<'this>(&'this mut self) -> unification::Context<'arena, 'this> {
        unification::Context::new(
            &self.scope,
            &mut self.renaming,
            self.rigid_exprs.len(),
            &mut self.flexible_exprs,
        )
    }

    /// Reports an error if there are duplicate fields found, returning a vector
    /// containing the positions of the of the duplicates.
    fn report_duplicate_labels(
        &mut self,
        range: ByteRange,
        fields: &[((ByteRange, StringId), Term<'_>)],
    ) -> Vec<usize> {
        use itertools::Itertools;

        // Will only allocate when duplicates are encountered
        let mut duplicate_indices = Vec::new();
        let mut duplicate_labels = Vec::new();

        for (index, ranged_label) in (fields.iter())
            .map(|(label, _)| *label)
            .enumerate()
            // TODO: avoid transient hashmap allocation
            .duplicates_by(|(_, (_, label))| *label)
        {
            duplicate_indices.push(index);
            duplicate_labels.push(ranged_label);
        }

        if !duplicate_labels.is_empty() {
            self.push_message(Message::DuplicateFieldLabels {
                range,
                labels: duplicate_labels,
            });
        }

        duplicate_indices
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
        self.unification_context()
            .unify(type0, type1)
            .map(|()| expr)
            .map_err(|error| self.push_message(Message::FailedToUnify { range, error }))
            .unwrap_or(core::Term::ReportedError)
    }

    /// Check that a surface term conforms to the given type.
    ///
    /// Returns the elaborated term in the core language.
    pub fn check(
        &mut self,
        surface_term: &Term<'_>,
        expected_type: &ArcValue<'arena>,
    ) -> core::Term<'arena> {
        let expected_type = self.force(expected_type);

        match (surface_term, expected_type.as_ref()) {
            (Term::Let(_, (_, def_name), def_type, def_expr, output_expr), _) => {
                let (def_expr, def_type_value) = match def_type {
                    None => self.synth(def_expr),
                    Some(def_type) => {
                        let def_type = self.check(def_type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                        let def_type_value = self.eval_context().eval(&def_type);

                        let def_expr = self.check(def_expr, &def_type_value);
                        let def_expr = core::Term::Ann(
                            self.scope.to_scope(def_expr),
                            self.scope.to_scope(def_type),
                        );

                        (def_expr, def_type_value)
                    }
                };

                let def_expr_value = self.eval_context().eval(&def_expr);

                self.push_rigid_definition(Some(*def_name), def_expr_value, def_type_value);
                let output_expr = self.check(output_expr, &expected_type);
                self.pop_rigid();

                core::Term::Let(
                    *def_name,
                    self.scope.to_scope(def_expr),
                    self.scope.to_scope(output_expr),
                )
            }
            (
                Term::FunIntro(_, (_, input_name), output_expr),
                Value::FunType(_, input_type, output_type),
            ) => {
                let input_expr = self.push_rigid_parameter(Some(*input_name), input_type.clone());
                let output_type = self.apply_closure(output_type, input_expr);
                let output_expr = self.check(output_expr, &output_type);
                self.pop_rigid();

                core::Term::FunIntro(Some(*input_name), self.scope.to_scope(output_expr))
            }
            (Term::RecordIntro(range, expr_fields), Value::RecordType(labels, types)) => {
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
                    return core::Term::ReportedError;
                }

                let initial_rigid_len = self.rigid_len();
                let mut types = types.clone();
                let mut expr_fields = expr_fields.iter();
                let mut exprs =
                    SliceBuilder::new(self.scope, types.len(), core::Term::ReportedError);

                while let Some(((_, expr), (r#type, next_types))) = Option::zip(
                    expr_fields.next(),
                    self.elim_context().split_telescope(types),
                ) {
                    let expr = self.check(expr, &r#type);
                    let expr_value = self.eval_context().eval(&expr);
                    types = next_types.resume(expr_value);
                    exprs.push(expr);
                }

                self.truncate_rigid(initial_rigid_len);
                core::Term::RecordIntro(labels, exprs.into())
            }
            (Term::RecordEmpty(_), Value::Universe) => core::Term::RecordType(&[], &[]),
            (Term::ReportedError(_), _) => core::Term::ReportedError,
            (_, _) => {
                let (core_term, synth_type) = self.synth(surface_term);
                self.convert(surface_term.range(), core_term, &synth_type, &expected_type)
            }
        }
    }

    /// Synthesize the type of the given surface term.
    ///
    /// Returns the elaborated term in the core language and its type.
    pub fn synth(&mut self, surface_term: &Term<'_>) -> (core::Term<'arena>, ArcValue<'arena>) {
        match surface_term {
            Term::Name(range, name) => match self.get_name(*name) {
                Some((term, r#type)) => (term, r#type.clone()),
                None => {
                    self.push_message(Message::UnboundName {
                        range: *range,
                        name: *name,
                    });
                    let r#type = self.push_flexible_value(*range, FlexSource::ReportedErrorType);
                    (core::Term::ReportedError, r#type)
                }
            },
            Term::Hole(range, name) => (
                self.push_flexible_term(*range, FlexSource::HoleExpr(*name)),
                self.push_flexible_value(*range, FlexSource::HoleType(*name)),
            ),
            Term::Ann(expr, r#type) => {
                let r#type = self.check(r#type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                let type_value = self.eval_context().eval(&r#type);
                let expr = self.check(expr, &type_value);

                let ann_expr =
                    core::Term::Ann(self.scope.to_scope(expr), self.scope.to_scope(r#type));

                (ann_expr, type_value)
            }
            Term::Let(_, (_, def_name), def_type, def_expr, output_expr) => {
                let (def_expr, def_type_value) = match def_type {
                    None => self.synth(def_expr),
                    Some(def_type) => {
                        let def_type = self.check(def_type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                        let def_type_value = self.eval_context().eval(&def_type);

                        let def_expr = self.check(def_expr, &def_type_value);
                        let def_expr = core::Term::Ann(
                            self.scope.to_scope(def_expr),
                            self.scope.to_scope(def_type),
                        );

                        (def_expr, def_type_value)
                    }
                };

                let def_expr_value = self.eval_context().eval(&def_expr);

                self.push_rigid_definition(Some(*def_name), def_expr_value, def_type_value);
                let (output_expr, output_type) = self.synth(output_expr);
                self.pop_rigid();

                let let_expr = core::Term::Let(
                    *def_name,
                    self.scope.to_scope(def_expr),
                    self.scope.to_scope(output_expr),
                );

                (let_expr, output_type)
            }
            Term::Universe(_) => (core::Term::Universe, Arc::new(Value::Universe)),
            Term::FunArrow(input_type, output_type) => {
                let input_type = self.check(input_type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                let input_type_value = self.eval_context().eval(&input_type);

                self.push_rigid_parameter(None, input_type_value);
                let output_type = self.check(output_type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                self.pop_rigid();

                let fun_type = core::Term::FunType(
                    None,
                    self.scope.to_scope(input_type),
                    self.scope.to_scope(output_type),
                );

                (fun_type, Arc::new(Value::Universe))
            }
            Term::FunType(_, (_, input_name), input_type, output_type) => {
                let input_type = self.check(input_type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                let input_type_value = self.eval_context().eval(&input_type);

                self.push_rigid_parameter(Some(*input_name), input_type_value);
                let output_type = self.check(output_type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                self.pop_rigid();

                let fun_type = core::Term::FunType(
                    Some(*input_name),
                    self.scope.to_scope(input_type),
                    self.scope.to_scope(output_type),
                );

                (fun_type, Arc::new(Value::Universe))
            }
            Term::FunIntro(_, (input_range, input_name), output_expr) => {
                let input_name = Some(*input_name);
                let input_type =
                    self.push_flexible_value(*input_range, FlexSource::FunInputType(input_name));

                self.push_rigid_parameter(input_name, input_type.clone());
                let (output_expr, output_type) = self.synth(output_expr);
                let output_type = self.quote_context(self.scope).quote(&output_type);
                self.pop_rigid();

                let output_type = self.close_term(output_type);

                (
                    core::Term::FunIntro(input_name, self.scope.to_scope(output_expr)),
                    Arc::new(Value::FunType(input_name, input_type, output_type)),
                )
            }
            Term::FunElim(head_expr, input_expr) => {
                let head_range = head_expr.range();
                let (head_expr, head_type) = self.synth(head_expr);

                // Ensure that the head type is a function type
                let head_type = self.force(&head_type);
                match head_type.as_ref() {
                    // The simple case - it's easy to see that it is a function type!
                    Value::FunType(_, input_type, output_type) => {
                        // Check the input expression and apply it to the output type
                        let input_expr = self.check(input_expr, &input_type);
                        let input_expr_value = self.eval_context().eval(&input_expr);
                        let output_type = self.apply_closure(&output_type, input_expr_value);

                        // Construct the final elimination
                        let fun_elim = core::Term::FunElim(
                            self.scope.to_scope(head_expr),
                            self.scope.to_scope(input_expr),
                        );

                        (fun_elim, output_type)
                    }
                    // It's not immediately obvious that the head type is a
                    // function type, so instead we construct a function type
                    // with flexible variables standing-in for the input and
                    // output types, and then we attempt to unify the head type
                    // against it.
                    _ => {
                        // Create a function type between flexible variables.
                        let input_type =
                            self.push_flexible_value(head_range, FlexSource::FunInputType(None));

                        self.push_rigid_parameter(None, input_type.clone());
                        let output_type =
                            self.push_flexible_term(head_range, FlexSource::FunOutputType);
                        self.pop_rigid();

                        let output_type = self.close_term(output_type);
                        let fun_type = Arc::new(Value::FunType(
                            None,
                            input_type.clone(),
                            output_type.clone(),
                        ));

                        // Unify the type of the head expression with the function type
                        let head_expr = self.convert(head_range, head_expr, &head_type, &fun_type);

                        // Check the input expression and apply it to the output type
                        let input_expr = self.check(input_expr, &input_type);
                        let input_expr_value = self.eval_context().eval(&input_expr);
                        let output_type = self.apply_closure(&output_type, input_expr_value);

                        // Construct the final elimination
                        let fun_elim = core::Term::FunElim(
                            self.scope.to_scope(head_expr),
                            self.scope.to_scope(input_expr),
                        );

                        (fun_elim, output_type)
                    }
                }
            }
            Term::RecordType(range, type_fields) => {
                let duplicate_indices = self.report_duplicate_labels(*range, type_fields);
                let type_fields = (type_fields.iter().enumerate())
                    .filter_map(|(i, field)| (!duplicate_indices.contains(&i)).then(|| field));

                let labels = (self.scope)
                    .to_scope_from_iter(type_fields.clone().map(|((_, label), _)| *label));
                let type_fields =
                    (self.scope).to_scope_from_iter(type_fields.map(|((_, label), r#type)| {
                        let r#type = self.check(r#type, &Arc::new(Value::Universe));
                        let type_value = self.eval_context().eval(&r#type);
                        self.push_rigid_parameter(Some(*label), type_value);
                        r#type
                    }));

                (
                    core::Term::RecordType(labels, type_fields),
                    Arc::new(Value::Universe),
                )
            }
            Term::RecordIntro(range, expr_fields) => {
                let duplicate_indices = self.report_duplicate_labels(*range, expr_fields);
                let expr_fields = (expr_fields.iter().enumerate())
                    .filter_map(|(i, field)| (!duplicate_indices.contains(&i)).then(|| field));

                let labels = (self.scope)
                    .to_scope_from_iter(expr_fields.clone().map(|((_, label), _)| *label));
                let len = labels.len();
                let mut types = SliceBuilder::new(self.scope, len, core::Term::ReportedError);
                let mut exprs = SliceBuilder::new(self.scope, len, core::Term::ReportedError);

                for (_, expr) in expr_fields {
                    let (expr, r#type) = self.synth(expr);
                    types.push(self.quote_context(self.scope).quote(&r#type));
                    exprs.push(expr);
                }

                (
                    core::Term::RecordIntro(labels as &[_], exprs.into()),
                    Arc::new(Value::RecordType(
                        labels as &[_],
                        Telescope::new(self.rigid_exprs.clone(), types.into()),
                    )),
                )
            }
            Term::RecordEmpty(_) => (
                core::Term::RecordIntro(&[], &[]),
                Arc::new(Value::RecordType(
                    &[],
                    Telescope::new(SharedEnv::new(), &[]),
                )),
            ),
            Term::RecordElim(head_expr, (label_range, label)) => {
                let head_range = head_expr.range();
                let (head_expr, head_type) = self.synth(head_expr);
                let head_expr_value = self.eval_context().eval(&head_expr);

                // TODO: check for reported error?
                if let Value::RecordType(labels, types) = self.force(&head_type).as_ref() {
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
                            let value = self.elim_context().apply_record(head_expr, *type_label);
                            types = next_types.resume(value);
                        }
                    }
                }

                self.push_message(Message::UnknownField {
                    head_range,
                    label_range: *label_range,
                    label: *label,
                });

                (
                    core::Term::ReportedError,
                    self.push_flexible_value(surface_term.range(), FlexSource::ReportedErrorType),
                )
            }
            Term::ReportedError(range) => (
                core::Term::ReportedError,
                self.push_flexible_value(*range, FlexSource::ReportedErrorType),
            ),
        }
    }
}
