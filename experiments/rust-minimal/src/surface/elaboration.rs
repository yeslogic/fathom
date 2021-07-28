//! Bidirectional elaboration of the surface language into the core language.

use std::sync::Arc;

use crate::core::semantics::{self, Closure, ElimContext, EvalContext, ReadbackContext, Value};
use crate::env::{self, LocalVar, SharedEnv, UniqueEnv};
use crate::{core, surface, ByteRange, StringId};

mod unification;

/// The reason why a fresh problem variable was [inserted][`Context::fresh_problem_term`]
/// into the elaboration context.
#[derive(Debug, Copy, Clone)]
pub enum ProblemSource {
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

/// Elaboration diagnostic messages.
#[derive(Debug, Clone)]
pub enum Message {
    UnboundName {
        range: ByteRange,
        name: StringId,
    },
    FailedToUnify {
        range: ByteRange,
        // TODO: add lhs and rhs values
        // lhs: Doc<_>,
        // rhs: Doc<_>,
        error: unification::Error,
    },
    UnsolvedProblem {
        range: ByteRange,
        source: ProblemSource,
    },
    Semantics(semantics::Error),
}

impl From<semantics::Error> for Message {
    fn from(error: semantics::Error) -> Message {
        Message::Semantics(error)
    }
}

/// Elaboration context.
pub struct Context<'arena> {
    /// Arena used for storing elaborated terms.
    arena: &'arena core::Arena<'arena>,
    /// A partial renaming to be used during [`unification`].
    renaming: unification::PartialRenaming,

    /// Names of bound variables.
    binding_names: UniqueEnv<Option<StringId>>,
    /// Types of bound variables.
    binding_types: UniqueEnv<Arc<Value<'arena>>>,
    /// Modes of bound variables. This is used when inserting new problems.
    binding_modes: UniqueEnv<core::BindingMode>,
    /// Expressions that will be substituted for bound variables during
    /// [evaluation][`EvalContext::eval`].
    binding_exprs: SharedEnv<Arc<Value<'arena>>>,

    /// The source of freshly inserted problems. This is used when reporting
    /// unsolved problems.
    problem_sources: UniqueEnv<(ByteRange, ProblemSource)>,
    /// Expressions that will be substituted for problem variables during
    /// [evaluation][`EvalContext::eval`].
    ///
    /// These will be set to [`None`] when a fresh problem variable is first
    /// [inserted][`Context::fresh_problem_term`], then will be set to [`Some`]
    /// if a solution is found during [`unification`].
    problem_exprs: UniqueEnv<Option<Arc<Value<'arena>>>>,

    /// Diagnostic messages encountered during elaboration.
    messages: Vec<Message>,
}

impl<'arena> Context<'arena> {
    /// Construct a new elaboration context, backed by the supplied arena.
    pub fn new(arena: &'arena core::Arena<'arena>) -> Context<'arena> {
        Context {
            arena,
            renaming: unification::PartialRenaming::new(),

            binding_names: UniqueEnv::new(),
            binding_types: UniqueEnv::new(),
            binding_modes: UniqueEnv::new(),
            binding_exprs: SharedEnv::new(),

            problem_sources: UniqueEnv::new(),
            problem_exprs: UniqueEnv::new(),

            messages: Vec::new(),
        }
    }

    fn get_binding(&self, name: StringId) -> Option<(LocalVar, &Arc<Value<'arena>>)> {
        let bindings = Iterator::zip(env::local_vars(), self.binding_types.iter().rev());

        Iterator::zip(self.binding_names.iter().copied().rev(), bindings)
            .find_map(|(n, binding)| (Some(name) == n).then(|| binding))
    }

    /// Push a binding onto the context.
    fn push_definition(
        &mut self,
        name: Option<StringId>,
        expr: Arc<Value<'arena>>,
        r#type: Arc<Value<'arena>>,
    ) {
        self.binding_names.push(name);
        self.binding_types.push(r#type);
        self.binding_modes.push(core::BindingMode::Defined);
        self.binding_exprs.push(expr);
    }

    /// Push an assumption onto the context.
    fn push_assumption(
        &mut self,
        name: Option<StringId>,
        r#type: Arc<Value<'arena>>,
    ) -> Arc<Value<'arena>> {
        // A bound variable expression that refers to itself, once it is pushed
        // onto the expression environment.
        let expr = Arc::new(Value::bound_var(self.binding_exprs.len().next_global()));

        self.binding_names.push(name);
        self.binding_types.push(r#type);
        self.binding_modes.push(core::BindingMode::Assumed);
        self.binding_exprs.push(expr.clone());

        expr
    }

    /// Pop a binding off the context.
    fn pop_binding(&mut self) {
        self.binding_names.pop();
        self.binding_types.pop();
        self.binding_modes.pop();
        self.binding_exprs.pop();
    }

    /// Push a fresh problem onto the context.
    fn fresh_problem_term(
        &mut self,
        range: ByteRange,
        source: ProblemSource,
    ) -> core::Term<'arena> {
        // TODO: check that hole name is not already in use
        let var = self.problem_exprs.len().next_global();

        self.problem_sources.push((range, source));
        self.problem_exprs.push(None);

        core::Term::InsertedProblem(var, self.binding_modes.clone())
    }

    /// Push a fresh problem onto the context, and evaluate it.
    fn fresh_problem_value(
        &mut self,
        range: ByteRange,
        source: ProblemSource,
    ) -> Arc<Value<'arena>> {
        let term = self.fresh_problem_term(range, source);
        self.eval(&term)
    }

    fn push_message(&mut self, message: impl Into<Message>) {
        self.messages.push(message.into());
    }

    fn report_term<'out_arena>(&mut self, message: impl Into<Message>) -> core::Term<'out_arena> {
        self.push_message(message);
        core::Term::ReportedError
    }

    fn report_value<'out_arena>(&mut self, message: impl Into<Message>) -> Arc<Value<'out_arena>> {
        self.push_message(message);
        Arc::new(Value::reported_error())
    }

    pub fn drain_messages<'this>(&'this mut self) -> impl 'this + Iterator<Item = Message> {
        self.messages.drain(..).chain(
            Iterator::zip(self.problem_sources.iter(), self.problem_exprs.iter()).filter_map(
                |((range, source), expr)| {
                    expr.is_none().then(|| Message::UnsolvedProblem {
                        range: *range,
                        source: *source,
                    })
                },
            ),
        )
    }

    pub fn force(&mut self, term: &Arc<Value<'arena>>) -> Arc<Value<'arena>> {
        ElimContext::new(&self.problem_exprs)
            .force(term)
            .unwrap_or_else(|error| self.report_value(error))
    }

    pub fn normalize<'out_arena>(
        &mut self,
        arena: &'out_arena core::Arena<'out_arena>,
        term: &core::Term<'arena>,
    ) -> core::Term<'out_arena> {
        EvalContext::new(&mut self.binding_exprs, &self.problem_exprs)
            .normalise(arena, term)
            .unwrap_or_else(|error| self.report_term(error))
    }

    pub fn eval(&mut self, term: &core::Term<'arena>) -> Arc<Value<'arena>> {
        EvalContext::new(&mut self.binding_exprs, &self.problem_exprs)
            .eval(term)
            .unwrap_or_else(|error| self.report_value(error))
    }

    pub fn readback<'out_arena>(
        &mut self,
        arena: &'out_arena core::Arena<'out_arena>,
        value: &Arc<Value<'arena>>,
    ) -> core::Term<'out_arena> {
        ReadbackContext::new(arena, self.binding_exprs.len(), &self.problem_exprs)
            .readback(value)
            .unwrap_or_else(|error| self.report_term(error))
    }

    fn close_term(&self, term: core::Term<'arena>) -> Closure<'arena> {
        Closure::new(self.binding_exprs.clone(), self.arena.alloc_term(term))
    }

    fn apply_closure(
        &mut self,
        closure: &Closure<'arena>,
        input_expr: Arc<Value<'arena>>,
    ) -> Arc<Value<'arena>> {
        ElimContext::new(&self.problem_exprs)
            .apply_closure(closure, input_expr)
            .unwrap_or_else(|error| self.report_value(error))
    }

    fn unify(
        &mut self,
        value0: &Arc<Value<'arena>>,
        value1: &Arc<Value<'arena>>,
    ) -> unification::Result<()> {
        let mut context = unification::Context::new(
            &self.arena,
            &mut self.renaming,
            self.binding_exprs.len(),
            &mut self.problem_exprs,
        );

        context.unify(value0, value1)
    }

    /// Check that a surface term conforms to the given type.
    ///
    /// Returns the elaborated term in the core language.
    pub fn check(
        &mut self,
        surface_term: &surface::Term<'_>,
        expected_type: &Arc<Value<'arena>>,
    ) -> core::Term<'arena> {
        let expected_type = self.force(expected_type);

        match (surface_term, expected_type.as_ref()) {
            (surface::Term::Let(_, (_, def_name), def_type, def_expr, output_expr), _) => {
                let (def_expr, def_type_value) = match def_type {
                    None => self.synth(def_expr),
                    Some(def_type) => {
                        let def_type = self.check(def_type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                        let def_type_value = self.eval(&def_type);

                        let def_expr = self.check(def_expr, &def_type_value);
                        let def_expr = core::Term::Ann(
                            self.arena.alloc_term(def_expr),
                            self.arena.alloc_term(def_type),
                        );

                        (def_expr, def_type_value)
                    }
                };

                let def_expr_value = self.eval(&def_expr);

                self.push_definition(Some(*def_name), def_expr_value, def_type_value);
                let output_expr = self.check(output_expr, &expected_type);
                self.pop_binding();

                core::Term::Let(
                    *def_name,
                    self.arena.alloc_term(def_expr),
                    self.arena.alloc_term(output_expr),
                )
            }
            (
                surface::Term::FunIntro(_, (_, input_name), output_expr),
                Value::FunType(_, input_type, output_type),
            ) => {
                let input_expr = self.push_assumption(Some(*input_name), input_type.clone());
                let output_type = self.apply_closure(output_type, input_expr);
                let output_expr = self.check(output_expr, &output_type);
                self.pop_binding();

                core::Term::FunIntro(Some(*input_name), self.arena.alloc_term(output_expr))
            }
            (surface::Term::ReportedError(_), _) => core::Term::ReportedError,
            (_, _) => {
                let (core_term, synth_type) = self.synth(surface_term);

                match self.unify(&synth_type, &expected_type) {
                    Ok(()) => core_term,
                    Err(error) => {
                        let range = surface_term.range();
                        self.report_term(Message::FailedToUnify { range, error })
                    }
                }
            }
        }
    }

    /// Synthesize the type of the given surface term.
    ///
    /// Returns the elaborated term in the core language and its type.
    pub fn synth(
        &mut self,
        surface_term: &surface::Term<'_>,
    ) -> (core::Term<'arena>, Arc<Value<'arena>>) {
        match surface_term {
            surface::Term::Name(range, name) => match self.get_binding(*name) {
                Some((local_var, r#type)) => (core::Term::BoundVar(local_var), r#type.clone()),
                None => {
                    self.push_message(Message::UnboundName {
                        range: *range,
                        name: *name,
                    });
                    let r#type = self.fresh_problem_value(*range, ProblemSource::ReportedErrorType);
                    (core::Term::ReportedError, r#type)
                }
            },
            surface::Term::Hole(range, name) => (
                self.fresh_problem_term(*range, ProblemSource::HoleExpr(*name)),
                self.fresh_problem_value(*range, ProblemSource::HoleType(*name)),
            ),
            surface::Term::Ann(expr, r#type) => {
                let r#type = self.check(r#type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                let type_value = self.eval(&r#type);
                let expr = self.check(expr, &type_value);

                let ann_expr =
                    core::Term::Ann(self.arena.alloc_term(expr), self.arena.alloc_term(r#type));

                (ann_expr, type_value)
            }
            surface::Term::Let(_, (_, def_name), def_type, def_expr, output_expr) => {
                let (def_expr, def_type_value) = match def_type {
                    None => self.synth(def_expr),
                    Some(def_type) => {
                        let def_type = self.check(def_type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                        let def_type_value = self.eval(&def_type);

                        let def_expr = self.check(def_expr, &def_type_value);
                        let def_expr = core::Term::Ann(
                            self.arena.alloc_term(def_expr),
                            self.arena.alloc_term(def_type),
                        );

                        (def_expr, def_type_value)
                    }
                };

                let def_expr_value = self.eval(&def_expr);

                self.push_definition(Some(*def_name), def_expr_value, def_type_value);
                let (output_expr, output_type) = self.synth(output_expr);
                self.pop_binding();

                let let_expr = core::Term::Let(
                    *def_name,
                    self.arena.alloc_term(def_expr),
                    self.arena.alloc_term(output_expr),
                );

                (let_expr, output_type)
            }
            surface::Term::Universe(_) => (core::Term::Universe, Arc::new(Value::Universe)),
            surface::Term::FunArrow(input_type, output_type) => {
                let input_type = self.check(input_type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                let input_type_value = self.eval(&input_type);

                self.push_assumption(None, input_type_value);
                let output_type = self.check(output_type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                self.pop_binding();

                let fun_type = core::Term::FunType(
                    None,
                    self.arena.alloc_term(input_type),
                    self.arena.alloc_term(output_type),
                );

                (fun_type, Arc::new(Value::Universe))
            }
            surface::Term::FunType(_, (_, input_name), input_type, output_type) => {
                let input_type = self.check(input_type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                let input_type_value = self.eval(&input_type);

                self.push_assumption(Some(*input_name), input_type_value);
                let output_type = self.check(output_type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                self.pop_binding();

                let fun_type = core::Term::FunType(
                    Some(*input_name),
                    self.arena.alloc_term(input_type),
                    self.arena.alloc_term(output_type),
                );

                (fun_type, Arc::new(Value::Universe))
            }
            surface::Term::FunIntro(_, (input_range, input_name), output_expr) => {
                let input_name = Some(*input_name);
                let input_type =
                    self.fresh_problem_value(*input_range, ProblemSource::FunInputType(input_name));

                self.push_assumption(input_name, input_type.clone());
                let (output_expr, output_type) = self.synth(output_expr);
                let output_type = self.readback(self.arena, &output_type);
                self.pop_binding();

                let output_type = self.close_term(output_type);

                (
                    core::Term::FunIntro(input_name, self.arena.alloc_term(output_expr)),
                    Arc::new(Value::FunType(input_name, input_type, output_type)),
                )
            }
            surface::Term::FunElim(head_expr, input_expr) => {
                let head_range = head_expr.range();
                let (head_expr, head_type) = self.synth(head_expr);

                // Ensure that the head type is a function type
                let head_type = self.force(&head_type);
                match head_type.as_ref() {
                    // The simple case - it's easy to see that it is a function type!
                    Value::FunType(_, input_type, output_type) => {
                        // Check the input expression and apply it to the output type
                        let input_expr = self.check(input_expr, &input_type);
                        let input_expr_value = self.eval(&input_expr);
                        let output_type = self.apply_closure(&output_type, input_expr_value);

                        // Construct the final elimination
                        let fun_elim = core::Term::FunElim(
                            self.arena.alloc_term(head_expr),
                            self.arena.alloc_term(input_expr),
                        );

                        (fun_elim, output_type)
                    }
                    // It's not immediately obvious that the head type is a
                    // function type, so instead we construct a function type
                    // with fresh problem variables standing in for the input
                    // and output types, and then attempt to unify the head type
                    // against it.
                    _ => {
                        // Create a function type between problem variables.
                        let input_type =
                            self.fresh_problem_value(head_range, ProblemSource::FunInputType(None));

                        self.push_assumption(None, input_type.clone());
                        let output_type =
                            self.fresh_problem_term(head_range, ProblemSource::FunOutputType);
                        self.pop_binding();

                        let output_type = self.close_term(output_type);
                        let fun_type = Arc::new(Value::FunType(
                            None,
                            input_type.clone(),
                            output_type.clone(),
                        ));

                        // Attempt to unify the type of the head expression with
                        // the function type.
                        let head_expr = match self.unify(&head_type, &fun_type) {
                            Ok(()) => head_expr,
                            Err(error) => {
                                let range = surface_term.range();
                                self.report_term(Message::FailedToUnify { range, error })
                            }
                        };

                        // Check the input expression and apply it to the output type
                        let input_expr = self.check(input_expr, &input_type);
                        let input_expr_value = self.eval(&input_expr);
                        let output_type = self.apply_closure(&output_type, input_expr_value);

                        // Construct the final elimination
                        let fun_elim = core::Term::FunElim(
                            self.arena.alloc_term(head_expr),
                            self.arena.alloc_term(input_expr),
                        );

                        (fun_elim, output_type)
                    }
                }
            }
            surface::Term::ReportedError(range) => (
                core::Term::ReportedError,
                self.fresh_problem_value(*range, ProblemSource::ReportedErrorType),
            ),
        }
    }
}
