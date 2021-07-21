//! Bidirectional elaboration of the surface language into the core language.

use std::sync::Arc;

use crate::core::semantics::{Closure, ElimContext, EvalContext, ReadbackContext, Value};
use crate::env::{self, LocalVar, SharedEnv, UniqueEnv};
use crate::{core, surface, ByteRange, StringId};

mod unification;

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
    /// [evaluation](`EvalContext::eval`).
    binding_exprs: SharedEnv<Arc<Value<'arena>>>,

    /// Problem names, added by [named holes][`surface::Term::Hole`].
    problem_names: UniqueEnv<Option<StringId>>,
    /// Expressions that will be substituted for problem variables during
    /// [evaluation](`EvalContext::eval`).
    ///
    /// These will be set to [`None`] when a fresh problem variable is
    /// introduced, then will be set to [`Some`] once a solution is found
    /// during unification.
    problem_exprs: UniqueEnv<Option<Arc<Value<'arena>>>>,

    /// Diagnostic messages encountered during elaboration.
    messages: Vec<(ByteRange, String)>,
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

            problem_names: UniqueEnv::new(),
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
    fn fresh_problem_term(&mut self, name: Option<StringId>) -> core::Term<'arena> {
        // TODO: check that hole name is not already in use
        let var = self.problem_exprs.len().next_global();

        self.problem_names.push(name);
        self.problem_exprs.push(None);

        core::Term::InsertedProblem(var, self.binding_modes.clone())
    }

    /// Push a fresh problem onto the context, and evaluate it.
    fn fresh_problem_value(&mut self, name: Option<StringId>) -> Arc<Value<'arena>> {
        let term = self.fresh_problem_term(name);
        self.eval(&term)
    }

    fn push_message(&mut self, range: ByteRange, message: impl Into<String>) {
        self.messages.push((range, message.into()));
    }

    pub fn drain_messages<'this>(
        &'this mut self,
    ) -> impl 'this + Iterator<Item = (ByteRange, String)> {
        self.messages.drain(..)
    }

    pub fn force(&self, term: &Arc<Value<'arena>>) -> Arc<Value<'arena>> {
        ElimContext::new(&self.problem_exprs)
            .force(term)
            .unwrap_or_else(|_| todo!("report error"))
    }

    pub fn normalize<'out_arena>(
        &mut self,
        arena: &'out_arena core::Arena<'out_arena>,
        term: &core::Term<'arena>,
    ) -> core::Term<'out_arena> {
        EvalContext::new(&mut self.binding_exprs, &self.problem_exprs)
            .normalise(arena, term)
            .unwrap_or_else(|_| todo!("report error"))
    }

    pub fn eval(&mut self, term: &core::Term<'arena>) -> Arc<Value<'arena>> {
        EvalContext::new(&mut self.binding_exprs, &self.problem_exprs)
            .eval(term)
            .unwrap_or_else(|_| todo!("report error"))
    }

    pub fn readback<'out_arena>(
        &mut self,
        arena: &'out_arena core::Arena<'out_arena>,
        value: &Arc<Value<'arena>>,
    ) -> core::Term<'out_arena> {
        ReadbackContext::new(arena, self.binding_exprs.len(), &self.problem_exprs)
            .readback(value)
            .unwrap_or_else(|_| todo!("report error"))
    }

    fn unify(
        &mut self,
        range: ByteRange,
        value0: &Arc<Value<'arena>>,
        value1: &Arc<Value<'arena>>,
        on_success: impl FnOnce() -> core::Term<'arena>,
    ) -> core::Term<'arena> {
        let mut context = unification::Context::new(
            &self.arena,
            &mut self.renaming,
            self.binding_exprs.len(),
            &mut self.problem_exprs,
        );

        match context.unify(value0, value1) {
            Ok(()) => on_success(),
            Err(unification::Error::FailedToUnify) => {
                self.push_message(range, "error: type mismatch");
                core::Term::ReportedError
            }
            Err(unification::Error::ScopeError) => {
                self.push_message(range, "error: escaping variable");
                core::Term::ReportedError
            }
            Err(unification::Error::OccursCheck) => {
                self.push_message(range, "error: occurs check");
                core::Term::ReportedError
            }
            Err(unification::Error::Semantics(_)) => todo!("report error"),
        }
    }

    fn apply_closure(
        &mut self,
        closure: &Closure<'arena>,
        input_expr: Arc<Value<'arena>>,
    ) -> Arc<Value<'arena>> {
        ElimContext::new(&self.problem_exprs)
            .closure_elim(closure, input_expr)
            .unwrap_or_else(|_| todo!("report error"))
    }

    /// Check that a surface term conforms to the given type.
    ///
    /// Returns the elaborated term in the core language.
    pub fn check(
        &mut self,
        surface_term: &surface::Term<'_>,
        expected_type: &Arc<Value<'arena>>,
    ) -> core::Term<'arena> {
        match (surface_term, self.force(expected_type).as_ref()) {
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
                let output_expr = self.check(output_expr, expected_type);
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

                self.unify(surface_term.range(), &synth_type, expected_type, || {
                    core_term
                })
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
            surface::Term::Name(_, name) => match self.get_binding(*name) {
                Some((local_var, r#type)) => (core::Term::BoundVar(local_var), r#type.clone()),
                None => {
                    self.push_message(surface_term.range(), "error: unknown variable");
                    (core::Term::ReportedError, self.fresh_problem_value(None))
                }
            },
            surface::Term::Hole(_, name) => {
                let r#type = self.fresh_problem_value(None);
                let expr = self.fresh_problem_term(*name);
                (expr, r#type)
            }
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
            surface::Term::FunIntro(_, (_, input_name), output_expr) => {
                let input_name = Some(*input_name);
                let input_type = self.fresh_problem_value(None);

                self.push_assumption(input_name, input_type.clone());
                let (output_expr, output_type) = self.synth(output_expr);
                let output_type = self.readback(self.arena, &output_type);
                self.pop_binding();

                let output_type = Closure::new(
                    self.binding_exprs.clone(),
                    self.arena.alloc_term(output_type),
                );

                (
                    core::Term::FunIntro(input_name, self.arena.alloc_term(output_expr)),
                    Arc::new(Value::FunType(input_name, input_type, output_type)),
                )
            }
            surface::Term::FunElim(head_expr, input_expr) => {
                let head_expr_range = head_expr.range();
                let (head_expr, head_type) = self.synth(head_expr);

                // Ensure that `head_type` is a function type
                match self.force(&head_type).as_ref() {
                    // The simple case - it's easy to see that it is a function type!
                    Value::FunType(_, input_type, output_type) => {
                        // Check the input and apply it toy the output type
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
                    // It's not immediately obvious that `head_type` is a
                    // function type, so instead we construct a function type
                    // with fresh problem variables standing in for the input
                    // and output types, and then we attempt to unify
                    // `head_type` against it.
                    _ => {
                        // Create a function type between problem variables.
                        let input_type = self.fresh_problem_value(None);
                        let output_type = Closure::new(self.binding_exprs.clone(), {
                            self.push_assumption(None, input_type.clone());
                            let output_type = self.fresh_problem_term(None);
                            self.pop_binding();

                            self.arena.alloc_term(output_type)
                        });
                        let fun_type = Arc::new(Value::FunType(
                            None,
                            input_type.clone(),
                            output_type.clone(),
                        ));

                        // Attempt to unify `head_type` with the function type.
                        let head_expr =
                            self.unify(head_expr_range, &head_type, &fun_type, || head_expr);

                        // Check the input and apply it toy the output type
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
            surface::Term::ReportedError(_) => {
                let r#type = self.fresh_problem_term(None);
                (core::Term::ReportedError, self.eval(&r#type))
            }
        }
    }
}
