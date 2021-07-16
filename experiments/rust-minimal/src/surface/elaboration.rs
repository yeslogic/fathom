//! Bidirectional elaboration of the surface language into the core language.

use std::sync::Arc;

use crate::core::semantics::{
    Closure, ConversionContext, ElimContext, EvalContext, ReadbackContext, Value,
};
use crate::{core, surface, ByteRange, StringId};

/// Elaboration context.
pub struct Context<'arena> {
    /// Arena used for storing elaborated terms.
    arena: &'arena core::Arena<'arena>,
    /// Names of bound variables.
    binding_names: core::UniqueEnv<StringId>,
    /// Types of bound variables.
    binding_types: core::UniqueEnv<Arc<Value<'arena>>>,
    /// Expressions that will be substituted for bound variables during
    /// [evaluation](`crate::core::semantics::EvalContext::eval`).
    binding_exprs: core::SharedEnv<Arc<Value<'arena>>>,
    /// Unification variable names (added by named holes).
    unification_names: core::UniqueEnv<Option<StringId>>,
    /// Unification variable solutions.
    unification_solutions: core::UniqueEnv<Option<Arc<Value<'arena>>>>,
    /// Diagnostic messages encountered during elaboration.
    messages: Vec<(ByteRange, String)>,
}

impl<'arena> Context<'arena> {
    /// Construct a new elaboration context, backed by the supplied arena.
    pub fn new(arena: &'arena core::Arena<'arena>) -> Context<'arena> {
        Context {
            arena,
            binding_names: core::UniqueEnv::new(),
            binding_types: core::UniqueEnv::new(),
            binding_exprs: core::SharedEnv::new(),
            unification_names: core::UniqueEnv::new(),
            unification_solutions: core::UniqueEnv::new(),
            messages: Vec::new(),
        }
    }

    fn get_binding(&self, name: StringId) -> Option<(core::LocalVar, &Arc<Value<'arena>>)> {
        let bindings = Iterator::zip(core::local_vars(), self.binding_types.iter().rev());

        Iterator::zip(self.binding_names.iter().rev(), bindings)
            .find_map(|(n, binding)| (name == *n).then(|| binding))
    }

    /// Push a binding onto the context.
    fn push_binding(
        &mut self,
        name: StringId,
        expr: Arc<Value<'arena>>,
        r#type: Arc<Value<'arena>>,
    ) {
        self.binding_names.push(name);
        self.binding_types.push(r#type);
        self.binding_exprs.push(expr);
    }

    /// Push an assumption onto the context.
    fn push_assumption(
        &mut self,
        name: StringId,
        r#type: Arc<Value<'arena>>,
    ) -> Arc<Value<'arena>> {
        // Create a bound variable that refers to itself, once it is
        // pushed onto the context.
        let expr = Arc::new(Value::bound_var(self.binding_exprs.len().next_global()));
        self.push_binding(name, expr.clone(), r#type);
        expr
    }

    /// Pop a binding off the context.
    fn pop_binding(&mut self) {
        self.binding_names.pop();
        self.binding_types.pop();
        self.binding_exprs.pop();
    }

    /// Push a fresh unification problem onto the context.
    fn push_unification_problem(&mut self, name: Option<StringId>) -> core::Term<'arena> {
        // TODO: check that hole name is not already in use
        let fresh_var = self.unification_solutions.len().next_global();
        self.unification_names.push(name);
        self.unification_solutions.push(None);
        core::Term::UnificationVar(fresh_var)
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
        ElimContext::new(&self.unification_solutions)
            .force(term)
            .unwrap_or_else(|_| todo!("report error"))
    }

    pub fn normalize<'out_arena>(
        &mut self,
        arena: &'out_arena core::Arena<'out_arena>,
        term: &core::Term<'arena>,
    ) -> core::Term<'out_arena> {
        EvalContext::new(&mut self.binding_exprs, &self.unification_solutions)
            .normalise(arena, term)
            .unwrap_or_else(|_| todo!("report error"))
    }

    pub fn eval(&mut self, term: &core::Term<'arena>) -> Arc<Value<'arena>> {
        EvalContext::new(&mut self.binding_exprs, &self.unification_solutions)
            .eval(term)
            .unwrap_or_else(|_| todo!("report error"))
    }

    pub fn readback<'out_arena>(
        &mut self,
        arena: &'out_arena core::Arena<'out_arena>,
        value: &Arc<Value<'arena>>,
    ) -> core::Term<'out_arena> {
        ReadbackContext::new(arena, self.binding_exprs.len(), &self.unification_solutions)
            .readback(value)
            .unwrap_or_else(|_| todo!("report error"))
    }

    fn is_equal(&mut self, value0: &Arc<Value<'_>>, value1: &Arc<Value<'_>>) -> bool {
        ConversionContext::new(self.binding_exprs.len(), &self.unification_solutions)
            .is_equal(value0, value1)
            .unwrap_or_else(|_| todo!("report error"))
    }

    fn apply_closure(
        &mut self,
        closure: &Closure<'arena>,
        input_expr: Arc<Value<'arena>>,
    ) -> Arc<Value<'arena>> {
        ElimContext::new(&self.unification_solutions)
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

                self.push_binding(*def_name, def_expr_value, def_type_value);
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
                let input_expr = self.push_assumption(*input_name, input_type.clone());
                let output_type = self.apply_closure(output_type, input_expr);
                let output_expr = self.check(output_expr, &output_type);
                self.pop_binding();

                core::Term::FunIntro(*input_name, self.arena.alloc_term(output_expr))
            }
            (surface::Term::ReportedError(_), _) => core::Term::ReportedError,
            (_, _) => {
                let (core_term, synth_type) = self.synth(surface_term);

                if self.is_equal(&synth_type, expected_type) {
                    core_term
                } else {
                    self.push_message(surface_term.range(), "error: type mismatch");
                    core::Term::ReportedError
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
            surface::Term::Name(_, name) => match self.get_binding(*name) {
                Some((local_var, r#type)) => (core::Term::BoundVar(local_var), r#type.clone()),
                None => {
                    self.push_message(surface_term.range(), "error: unknown variable");
                    let r#type = self.push_unification_problem(None);
                    (core::Term::ReportedError, self.eval(&r#type))
                }
            },
            surface::Term::Hole(_, name) => {
                let r#type = self.push_unification_problem(None);
                let expr = self.push_unification_problem(*name);
                (expr, self.eval(&r#type))
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

                self.push_binding(*def_name, def_expr_value, def_type_value);
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

                self.push_assumption(*input_name, input_type_value);
                let output_type = self.check(output_type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                self.pop_binding();

                let fun_type = core::Term::FunType(
                    *input_name,
                    self.arena.alloc_term(input_type),
                    self.arena.alloc_term(output_type),
                );

                (fun_type, Arc::new(Value::Universe))
            }
            surface::Term::FunIntro(_, _, _) => {
                self.push_message(
                    surface_term.range(),
                    "error: ambiguous function introduction",
                );
                let r#type = self.push_unification_problem(None);
                (core::Term::ReportedError, self.eval(&r#type))
            }
            surface::Term::FunElim(head_expr, input_expr) => {
                let (head_expr, head_type) = self.synth(head_expr);
                match self.force(&head_type).as_ref() {
                    Value::FunType(_, input_type, output_type) => {
                        let input_expr = self.check(input_expr, input_type);
                        let input_expr_value = self.eval(&input_expr);

                        let output_type = self.apply_closure(output_type, input_expr_value);

                        let fun_elim = core::Term::FunElim(
                            self.arena.alloc_term(head_expr),
                            self.arena.alloc_term(input_expr),
                        );

                        (fun_elim, output_type)
                    }
                    _ => {
                        self.push_message(
                            surface_term.range(),
                            "error: argument to applied non-function",
                        );
                        let r#type = self.push_unification_problem(None);
                        (core::Term::ReportedError, self.eval(&r#type))
                    }
                }
            }
            surface::Term::ReportedError(_) => {
                let r#type = self.push_unification_problem(None);
                (core::Term::ReportedError, self.eval(&r#type))
            }
        }
    }
}
