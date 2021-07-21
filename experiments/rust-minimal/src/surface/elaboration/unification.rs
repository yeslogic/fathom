//! Implementation of problem unification for [`Value`]s.
//!
//! In this module we implemented a limited form of higher order unification,
//! called 'pattern unification'. More details on the algorithm we use can be
//! found in the [elaboration-zoo](https://github.com/AndrasKovacs/elaboration-zoo/),
//! in particular in [elaboration-zoo/03-holes](https://github.com/AndrasKovacs/elaboration-zoo/blob/master/03-holes/).

use std::sync::Arc;

use crate::core::semantics::{self, Elim, ElimContext, EvalContext, Head, Value};
use crate::core::{Arena, Term};
use crate::env::{EnvLen, GlobalVar, LocalVar, SharedEnv, SliceEnv, UniqueEnv};

pub enum Error {
    FailedToUnify,
    ScopeError,
    OccursCheck,
    Semantics(semantics::Error),
}

impl From<semantics::Error> for Error {
    fn from(err: semantics::Error) -> Error {
        Error::Semantics(err)
    }
}

/// Unification context.
pub struct Context<'arena, 'env> {
    /// Arena to store readback terms during renaming.
    arena: &'arena Arena<'arena>,
    /// A renaming that is used when solving pattern unification problems. We
    /// store it in the parent context, re-initialising it on each call to
    /// [`Context::solve`].
    renaming: &'env mut PartialRenaming,
    /// The length of the binding environment.
    bindings: EnvLen,
    /// Solutions to problem variables.
    problems: &'env mut SliceEnv<Option<Arc<Value<'arena>>>>,
}

impl<'arena, 'env> Context<'arena, 'env> {
    pub fn new(
        arena: &'arena Arena<'arena>,
        renaming: &'env mut PartialRenaming,
        bindings: EnvLen,
        problems: &'env mut SliceEnv<Option<Arc<Value<'arena>>>>,
    ) -> Context<'arena, 'env> {
        Context {
            arena,
            renaming,
            bindings,
            problems,
        }
    }

    fn elim_context<'this: 'env>(&'this self) -> ElimContext<'arena, 'env> {
        ElimContext::new(self.problems)
    }

    fn push_binding(&mut self) {
        self.bindings.push();
    }

    fn pop_binding(&mut self) {
        self.bindings.pop();
    }

    /// Unify two values, updating the solution environment if necessary.
    pub fn unify(
        &mut self,
        value0: &Arc<Value<'arena>>,
        value1: &Arc<Value<'arena>>,
    ) -> Result<(), Error> {
        match (
            self.elim_context().force(value0)?.as_ref(),
            self.elim_context().force(value1)?.as_ref(),
        ) {
            // `ReportedError`s result from errors that have already been
            // reported, so we say that they are equal to any other value to
            // prevent them from triggering more errors.
            (Value::Stuck(Head::ReportedError, _), _)
            | (_, Value::Stuck(Head::ReportedError, _)) => Ok(()),

            (
                Value::Stuck(Head::BoundVar(var0), spine0),
                Value::Stuck(Head::BoundVar(var1), spine1),
            ) if var0 == var1 => self.unify_spines(spine0, spine1),
            (
                Value::Stuck(Head::ProblemVar(var0), spine0),
                Value::Stuck(Head::ProblemVar(var1), spine1),
            ) if var0 == var1 => self.unify_spines(spine0, spine1),
            (Value::Stuck(Head::ProblemVar(var0), spine0), _) => self.solve(*var0, spine0, value1),
            (_, Value::Stuck(Head::ProblemVar(var1), spine1)) => self.solve(*var1, spine1, value0),

            (Value::Universe, Value::Universe) => Ok(()),

            (
                Value::FunType(_, input_type0, output_type0),
                Value::FunType(_, input_type1, output_type1),
            ) => {
                self.unify(input_type0, input_type1)?;

                let var = Arc::new(Value::bound_var(self.bindings.next_global()));
                let output_type0 = self
                    .elim_context()
                    .closure_elim(output_type0, var.clone())?;
                let output_type1 = self.elim_context().closure_elim(output_type1, var)?;

                self.push_binding();
                let result = self.unify(&output_type0, &output_type1);
                self.pop_binding();

                result
            }

            (Value::FunIntro(_, output_expr0), Value::FunIntro(_, output_expr1)) => {
                let var = Arc::new(Value::bound_var(self.bindings.next_global()));
                let output_expr0 = self
                    .elim_context()
                    .closure_elim(output_expr0, var.clone())?;
                let output_expr1 = self.elim_context().closure_elim(output_expr1, var)?;

                self.push_binding();
                let result = self.unify(&output_expr0, &output_expr1);
                self.pop_binding();

                result
            }
            // Eta-conversion
            (Value::FunIntro(_, output_expr), _) => {
                let var = Arc::new(Value::bound_var(self.bindings.next_global()));
                let value0 = self.elim_context().closure_elim(output_expr, var.clone())?;
                let value1 = self.elim_context().fun_elim(value1.clone(), var)?;

                self.push_binding();
                let result = self.unify(&value0, &value1);
                self.pop_binding();

                result
            }
            (_, Value::FunIntro(_, output_expr)) => {
                let var = Arc::new(Value::bound_var(self.bindings.next_global()));
                let value0 = self.elim_context().fun_elim(value0.clone(), var.clone())?;
                let value1 = self.elim_context().closure_elim(output_expr, var)?;

                self.push_binding();
                let result = self.unify(&value0, &value1);
                self.pop_binding();

                result
            }

            // Rigid mismatch
            (_, _) => Err(Error::FailedToUnify),
        }
    }

    /// Unify two elimination spines.
    fn unify_spines(
        &mut self,
        spine0: &[Elim<'arena>],
        spine1: &[Elim<'arena>],
    ) -> Result<(), Error> {
        if spine0.len() != spine1.len() {
            return Err(Error::FailedToUnify); // Rigid mismatch
        }
        for (elim0, elim1) in Iterator::zip(spine0.iter(), spine1.iter()) {
            match (elim0, elim1) {
                (Elim::Fun(input_expr0), Elim::Fun(input_expr1)) => {
                    self.unify(input_expr0, input_expr1)?;
                }
            }
        }
        Ok(())
    }

    /// Solve the unification problem `?problem_var spine =? value`, updating
    /// the solution environment with the unification solution if successful.
    fn solve(
        &mut self,
        problem_var: GlobalVar,
        spine: &[Elim<'arena>],
        value: &Arc<Value<'arena>>,
    ) -> Result<(), Error> {
        self.init_renaming(spine)?;
        let term = self.rename(problem_var, value)?;
        let fun_term = self.fun_intros(spine, term);
        let solution = EvalContext::new(&mut SharedEnv::new(), self.problems).eval(&fun_term)?;

        self.problems.set_global(problem_var, Some(solution));

        Ok(())
    }

    /// Re-initialise the [`Context::renaming`] by mapping the bound variables
    /// in the spine to the bound variables in the solution. This can fail if
    /// the spine does not contain distinct bound variables.
    fn init_renaming(&mut self, spine: &[Elim<'arena>]) -> Result<(), Error> {
        self.renaming.init(self.bindings);

        for elim in spine {
            match elim {
                Elim::Fun(input_expr) => match self.elim_context().force(input_expr)?.as_ref() {
                    Value::Stuck(Head::BoundVar(source_var), spine)
                        if spine.is_empty() && self.renaming.set_binding(*source_var) => {}
                    // Spine does not contain distinct bound variables
                    _ => return Err(Error::FailedToUnify),
                },
            }
        }

        Ok(())
    }

    /// Wrap a `term` in [function introductions][`Term::FunIntro`] that
    /// correspond to the given `spine`.
    fn fun_intros(&self, spine: &[Elim<'arena>], term: Term<'arena>) -> Term<'arena> {
        spine.iter().fold(term, |term, _| {
            Term::FunIntro(None, self.arena.alloc_term(term))
        })
    }

    /// Readback `value` to a [`Term`], while at the same time using the current
    /// renaming to update local variables, failing if the partial renaming is
    /// not defined (resulting in an [scope error][`Error::ScopeError`]), and
    /// also checking for occurrences of the `problem_var` (resulting in an
    /// [occurs check error][`Error::OccursCheck`]).
    ///
    /// This allows us to subsequently wrap the returned term in function
    /// introductions, using [`Context::function_intros`].
    fn rename(
        &mut self,
        problem_var: GlobalVar,
        value: &Arc<Value<'arena>>,
    ) -> Result<Term<'arena>, Error> {
        match self.elim_context().force(value)?.as_ref() {
            Value::Stuck(head, spine) => {
                let mut head_expr = match head {
                    Head::BoundVar(source_var) => match self.renaming.rename(*source_var) {
                        None => return Err(Error::ScopeError),
                        Some(target_var) => Term::BoundVar(target_var),
                    },
                    Head::ProblemVar(var) => match *var {
                        var if problem_var == var => return Err(Error::OccursCheck),
                        var => Term::ProblemVar(var),
                    },
                    Head::ReportedError => Term::ReportedError,
                };

                for elim in spine {
                    head_expr = match elim {
                        Elim::Fun(input_expr) => {
                            let input_expr = self.rename(problem_var, input_expr)?;
                            Term::FunElim(
                                self.arena.alloc_term(head_expr),
                                self.arena.alloc_term(input_expr),
                            )
                        }
                    };
                }

                Ok(head_expr)
            }
            Value::Universe => Ok(Term::Universe),
            Value::FunType(input_name, input_type, output_type) => {
                let input_type = self.rename(problem_var, input_type)?;

                let source_var = self.renaming.push_binding();
                let output_type = match self.elim_context().closure_elim(output_type, source_var) {
                    Ok(output_type) => self.rename(problem_var, &output_type),
                    Err(err) => Err(Error::from(err)),
                };
                self.renaming.pop_binding();

                Ok(Term::FunType(
                    *input_name,
                    self.arena.alloc_term(input_type),
                    self.arena.alloc_term(output_type?),
                ))
            }
            Value::FunIntro(input_name, output_expr) => {
                let source_var = self.renaming.push_binding();
                let output_expr = match self.elim_context().closure_elim(output_expr, source_var) {
                    Ok(output_expr) => self.rename(problem_var, &output_expr),
                    Err(err) => Err(Error::from(err)),
                };
                self.renaming.pop_binding();

                Ok(Term::FunIntro(
                    *input_name,
                    self.arena.alloc_term(output_expr?),
                ))
            }
        }
    }
}

/// A partial renaming from a source environment to a target environment.
pub struct PartialRenaming {
    /// The length of the target binding environment
    target_bindings: EnvLen,
    /// Mapping from variables in the source environment to variables in the
    /// target environment.
    source_bindings: UniqueEnv<Option<GlobalVar>>,
}

impl PartialRenaming {
    /// Create a new, empty renaming.
    pub fn new() -> PartialRenaming {
        PartialRenaming {
            target_bindings: EnvLen::new(),
            source_bindings: UniqueEnv::new(),
        }
    }

    /// Re-initialise the renaming to the requested `source_len`, reusing the
    /// previous allocation.
    fn init(&mut self, source_len: EnvLen) {
        self.target_bindings.clear();
        self.source_bindings.clear();
        self.source_bindings.resize(source_len, None);
    }

    /// Set a source variable to target variable mapping, ensuring that the
    /// variable appears uniquely.
    ///
    /// # Returns
    ///
    /// - `true` if the binding was set successfully.
    /// - `false` if the binding was already been set.
    fn set_binding(&mut self, source_var: GlobalVar) -> bool {
        let is_unique = self.get_binding(source_var).is_none();

        if is_unique {
            let target_var = Some(self.target_bindings.next_global());
            self.source_bindings.set_global(source_var, target_var);
            self.target_bindings.push();
        }

        is_unique
    }

    /// Push an extra binding onto the renaming, returning a variable
    /// that is appropriately bound in the source environment
    fn push_binding<'arena>(&mut self) -> Arc<Value<'arena>> {
        let target_var = self.target_bindings.next_global();
        let source_var = self.source_bindings.len().next_global();

        self.target_bindings.push();
        self.source_bindings.push(Some(target_var));

        Arc::new(Value::bound_var(source_var))
    }

    /// Pop a binding off the renaming
    fn pop_binding(&mut self) {
        self.target_bindings.pop();
        self.source_bindings.pop();
    }

    /// Get the bound variable in the target environment that will be used in
    /// place of the `source_var`.
    fn get_binding(&self, source_var: GlobalVar) -> Option<GlobalVar> {
        self.source_bindings
            .get_global(source_var)
            .copied()
            .flatten()
    }

    /// Readback a bound variable in the source environment to a locally bound
    /// variable in the target environment.
    fn rename(&self, source_var: GlobalVar) -> Option<LocalVar> {
        let target_var = self.get_binding(source_var)?;
        Some(self.target_bindings.global_to_local(target_var).unwrap())
    }
}
