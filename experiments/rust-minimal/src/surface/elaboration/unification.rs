//! [Unification] is a process of checking if two [values][Value] are the same,
//! where there might be 'unknown' parts in either value. During this process
//! we attempt to fill in those missing bits of information, and record the
//! solutions we find for future use.
//!
//! We implement a limited form of higher order unification, called 'pattern
//! unification'. More details about the algorithm we use can be found in the
//! [elaboration-zoo], in particular in [elaboration-zoo/03-holes].
//!
//! [Unification]: https://en.wikipedia.org/wiki/Unification_(computer_science)
//! [elaboration-zoo]: https://github.com/AndrasKovacs/elaboration-zoo/
//! [elaboration-zoo/03-holes]: https://github.com/AndrasKovacs/elaboration-zoo/

use std::sync::Arc;

use crate::core::semantics::{self, Closure, Elim, ElimContext, EvalContext, Head, Value};
use crate::core::{Arena, Term};
use crate::env::{EnvLen, GlobalVar, LocalVar, SharedEnv, SliceEnv, UniqueEnv};

/// Errors encountered during unification.
///
/// The documentation for the various pattern unification errors were adapted
/// from the [comments about pattern unification] in Andras Kovacs' excellent
/// [elaboration-zoo] repository.
///
/// [elaboration-zoo]: https://github.com/AndrasKovacs/elaboration-zoo/
/// [comments about pattern unification]: https://github.com/AndrasKovacs/elaboration-zoo/blob/d38b695d5177352501463fab2ac6d0929ba4472b/03-holes/Main.hs#L118-L169
#[derive(Debug, Clone)]
pub enum Error {
    /// A known part of one value failed to match with a known part of the other
    /// value that we are comparing against.
    Mismatched,
    /// A rigid variable appeared multiple times in a flexible spine.
    ///
    /// For example:
    ///
    /// ```text
    /// ?α x x =? x`
    /// ```
    ///
    /// This results in two distinct solutions:
    ///
    /// - `?α := fun x _ => x`
    /// - `?α := fun _ x => x`
    ///
    /// We only want unification to result in a unique solution, so we fail
    /// to unify in this case.
    ///
    /// Another example, assuming `true : Bool`, is:
    ///
    /// ```text
    /// ?α true =? true
    /// ```
    ///
    /// This also has multiple solutions, for example:
    ///
    /// - `?α := fun _ => true`
    /// - `?α := fun x => x`
    /// - `?α := fun x => if x then true else false`
    ///
    /// It's also possible that the return type of `?α` is not always `Bool`,
    /// for example:
    ///
    /// ```text
    /// ?α : fun (b : Bool) -> if b then Bool else (Bool -> Bool)
    /// ```
    ///
    /// In this case the example solution `?α := fun _ => true` is not even
    /// well-typed! In contrast, if the flexible spine only has distinct rigid
    /// variables, even if the return type is dependent, rigid variables block
    /// all computation in the return type, and the pattern solution is
    /// guaranteed to be well-typed.
    NonLinearSpine,
    /// A free rigid variable in the compared value does not occur in the
    /// flexible spine.
    ///
    /// For example, where `z : U` is a rigid variable:
    ///
    /// ```text
    /// ?α x y =? z -> z
    /// ```
    ///
    /// There is no solution for this flexible variable because `?α` is the
    /// topmost-level scope, so it can only abstract over `x` and `y`, but
    /// these don't occur in `z -> z`.
    EscapingRigidVar,
    /// The flexible variable occurs in the value being compared against.
    /// This is sometimes referred to as an 'occurs check' failure.
    ///
    /// For example:
    ///
    /// ```text
    /// ?α =? ?α -> ?α
    /// ```
    ///
    /// Here `?α` occurs in the right hand side, so in order to solve this
    /// flexible variable we would end up going into an infinite loop,
    /// attempting to construct larger and larger solutions:
    ///
    /// - `?α =? ?α -> ?α`
    /// - `?α =? (?α -> ?α) -> (?α -> ?α)`
    /// - `?α =? ((?α -> ?α) -> (?α -> ?α)) -> ((?α -> ?α) -> (?α -> ?α))`
    /// - etc.
    ///
    /// In some type systems we could solve this using [equi-recursive types].
    ///
    /// [equi-recursive types]: https://www.cs.cornell.edu/courses/cs4110/2012fa/lectures/lecture27.pdf
    InfiniteSolution,
    /// An error occurred during evaluation, and is almost certainly a bug.
    Semantics(semantics::Error),
}

impl From<semantics::Error> for Error {
    fn from(err: semantics::Error) -> Error {
        Error::Semantics(err)
    }
}

pub type Result<T> = std::result::Result<T, Error>;

/// Unification context.
pub struct Context<'arena, 'env> {
    /// Arena to store terms during [renaming][Context::rename].
    arena: &'arena Arena<'arena>,
    /// A renaming that is used when solving flexible variables using pattern
    /// unification. We store it in the parent context, re-initialising it on
    /// each call to [`Context::solve`] in order to reuse previous allocations.
    renaming: &'env mut PartialRenaming,
    /// The length of the rigid environment.
    rigid_exprs: EnvLen,
    /// Solutions for flexible variables.
    flexible_exprs: &'env mut SliceEnv<Option<Arc<Value<'arena>>>>,
}

impl<'arena, 'env> Context<'arena, 'env> {
    pub fn new(
        arena: &'arena Arena<'arena>,
        renaming: &'env mut PartialRenaming,
        rigid_exprs: EnvLen,
        flexible_exprs: &'env mut SliceEnv<Option<Arc<Value<'arena>>>>,
    ) -> Context<'arena, 'env> {
        Context {
            arena,
            renaming,
            rigid_exprs,
            flexible_exprs,
        }
    }

    fn elim_context<'this: 'env>(&'this self) -> ElimContext<'arena, 'env> {
        ElimContext::new(self.flexible_exprs)
    }

    fn push_rigid(&mut self) {
        self.rigid_exprs.push();
    }

    fn pop_rigid(&mut self) {
        self.rigid_exprs.pop();
    }

    /// Unify two values, updating the solution environment if necessary.
    pub fn unify(
        &mut self,
        value0: &Arc<Value<'arena>>,
        value1: &Arc<Value<'arena>>,
    ) -> Result<()> {
        // Check for pointer equality before trying to force the values
        if Arc::ptr_eq(value0, value1) {
            return Ok(());
        }

        let value0 = self.elim_context().force(value0)?;
        let value1 = self.elim_context().force(value1)?;

        match (value0.as_ref(), value1.as_ref()) {
            // `ReportedError`s result from errors that have already been
            // reported, so we prevent them from triggering more errors.
            (Value::Stuck(Head::ReportedError, _), _)
            | (_, Value::Stuck(Head::ReportedError, _)) => Ok(()),

            // Rigid-rigid and flexible-flexible case
            //
            // Both values have head variables in common, so all we need to do
            // is unify the elimination spines.
            (
                Value::Stuck(Head::RigidVar(var0), spine0),
                Value::Stuck(Head::RigidVar(var1), spine1),
            ) if var0 == var1 => self.unify_spines(spine0, spine1),
            (
                Value::Stuck(Head::FlexibleVar(var0), spine0),
                Value::Stuck(Head::FlexibleVar(var1), spine1),
            ) if var0 == var1 => self.unify_spines(spine0, spine1),

            // Flexible-rigid case
            //
            // One of the values has a flexible variable at its head, so we
            // attempt to solve it using pattern unification.
            (Value::Stuck(Head::FlexibleVar(var0), spine0), _) => {
                self.solve(*var0, spine0, &value1)
            }
            (_, Value::Stuck(Head::FlexibleVar(var1), spine1)) => {
                self.solve(*var1, spine1, &value0)
            }

            (Value::Universe, Value::Universe) => Ok(()),

            (
                Value::FunType(_, input_type0, output_type0),
                Value::FunType(_, input_type1, output_type1),
            ) => {
                self.unify(input_type0, input_type1)?;
                self.unify_closures(output_type0, output_type1)
            }

            (Value::FunIntro(_, output_expr0), Value::FunIntro(_, output_expr1)) => {
                self.unify_closures(output_expr0, output_expr1)
            }
            (Value::FunIntro(_, output_expr), _) => self.unify_fun_intro(output_expr, &value1),
            (_, Value::FunIntro(_, output_expr)) => self.unify_fun_intro(output_expr, &value0),

            (_, _) => Err(Error::Mismatched),
        }
    }

    /// Unify two elimination spines.
    fn unify_spines(&mut self, spine0: &[Elim<'arena>], spine1: &[Elim<'arena>]) -> Result<()> {
        if spine0.len() != spine1.len() {
            return Err(Error::Mismatched);
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

    /// Unify two [closures][Closure].
    fn unify_closures(
        &mut self,
        closure0: &Closure<'arena>,
        closure1: &Closure<'arena>,
    ) -> Result<()> {
        let var = Arc::new(Value::rigid_var(self.rigid_exprs.next_global()));
        let value0 = self.elim_context().apply_closure(closure0, var.clone())?;
        let value1 = self.elim_context().apply_closure(closure1, var)?;

        self.push_rigid();
        let result = self.unify(&value0, &value1);
        self.pop_rigid();

        result
    }

    /// Unify a closure with a value, using function eta-conversion.
    fn unify_fun_intro(
        &mut self,
        output_expr: &Closure<'arena>,
        value: &Arc<Value<'arena>>,
    ) -> Result<()> {
        let var = Arc::new(Value::rigid_var(self.rigid_exprs.next_global()));
        let value = self.elim_context().apply_fun(value.clone(), var.clone())?;
        let output_expr = self.elim_context().apply_closure(output_expr, var)?;

        self.push_rigid();
        let result = self.unify(&output_expr, &value);
        self.pop_rigid();

        result
    }

    /// Solve a pattern unification problem that looks like:
    ///
    /// ```text
    /// ?α spine =? value`
    /// ```
    ///
    /// If successful, the flexible environment will be updated with a solution
    /// that looks something like:
    ///
    /// ```text
    /// ?α := fun spine => value
    /// ```
    fn solve(
        &mut self,
        flexible_var: GlobalVar,
        spine: &[Elim<'arena>],
        value: &Arc<Value<'arena>>,
    ) -> Result<()> {
        self.init_renaming(spine)?;
        let term = self.rename(flexible_var, value)?;
        let fun_term = self.fun_intros(spine, term);
        let solution =
            EvalContext::new(&mut SharedEnv::new(), self.flexible_exprs).eval(&fun_term)?;

        self.flexible_exprs.set_global(flexible_var, Some(solution));

        Ok(())
    }

    /// Re-initialise the [`Context::renaming`] by mapping the rigid variables
    /// in the spine to the rigid variables in the solution. This can fail if
    /// the spine does not contain distinct rigid variables.
    fn init_renaming(&mut self, spine: &[Elim<'arena>]) -> Result<()> {
        self.renaming.init(self.rigid_exprs);

        for elim in spine {
            match elim {
                Elim::Fun(input_expr) => match self.elim_context().force(input_expr)?.as_ref() {
                    Value::Stuck(Head::RigidVar(source_var), spine)
                        if spine.is_empty() && self.renaming.set_rigid(*source_var) => {}
                    _ => return Err(Error::NonLinearSpine),
                },
            }
        }

        Ok(())
    }

    /// Wrap a `term` in [function introductions][Term::FunIntro] that
    /// correspond to the given `spine`.
    fn fun_intros(&self, spine: &[Elim<'arena>], term: Term<'arena>) -> Term<'arena> {
        spine.iter().fold(term, |term, _| {
            Term::FunIntro(None, self.arena.alloc_term(term))
        })
    }

    /// Rename `value` to a [`Term`], while at the same time using the current
    /// renaming to update local variables, failing if the partial renaming is
    /// not defined (resulting in an [scope error][Error::ScopeError]), and also
    /// checking for occurrences of the `flexible_var` (resulting in an [occurs
    /// check error][Error::InfiniteSolution]).
    ///
    /// This allows us to subsequently wrap the returned term in function
    /// introductions, using [`Context::function_intros`].
    fn rename(
        &mut self,
        flexible_var: GlobalVar,
        value: &Arc<Value<'arena>>,
    ) -> Result<Term<'arena>> {
        match self.elim_context().force(value)?.as_ref() {
            Value::Stuck(head, spine) => {
                let mut head_expr = match head {
                    Head::RigidVar(source_var) => match self.renaming.get_as_local(*source_var) {
                        None => return Err(Error::EscapingRigidVar),
                        Some(target_var) => Term::RigidVar(target_var),
                    },
                    Head::FlexibleVar(var) => match *var {
                        var if flexible_var == var => return Err(Error::InfiniteSolution),
                        var => Term::FlexibleVar(var),
                    },
                    Head::ReportedError => Term::ReportedError,
                };

                for elim in spine {
                    head_expr = match elim {
                        Elim::Fun(input_expr) => {
                            let input_expr = self.rename(flexible_var, input_expr)?;
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
                let input_type = self.rename(flexible_var, input_type)?;
                let output_type = self.rename_closure(flexible_var, output_type)?;

                Ok(Term::FunType(
                    *input_name,
                    self.arena.alloc_term(input_type),
                    self.arena.alloc_term(output_type),
                ))
            }
            Value::FunIntro(input_name, output_expr) => {
                let output_expr = self.rename_closure(flexible_var, output_expr)?;

                Ok(Term::FunIntro(
                    *input_name,
                    self.arena.alloc_term(output_expr),
                ))
            }
        }
    }

    /// Rename a closure back into a [`Term`].
    fn rename_closure(
        &mut self,
        flexible_var: GlobalVar,
        closure: &Closure<'arena>,
    ) -> Result<Term<'arena>> {
        let source_var = self.renaming.next_rigid_var();
        let value = self.elim_context().apply_closure(closure, source_var)?;

        self.renaming.push_rigid();
        let term = self.rename(flexible_var, &value);
        self.renaming.pop_rigid();

        term
    }
}

/// A partial renaming from a source environment to a target environment.
pub struct PartialRenaming {
    /// Mapping from rigid variables in the source environment to rigid
    /// variables in the target environment.
    source: UniqueEnv<Option<GlobalVar>>,
    /// The length of the target binding environment
    target: EnvLen,
}

impl PartialRenaming {
    /// Create a new, empty renaming.
    pub fn new() -> PartialRenaming {
        PartialRenaming {
            source: UniqueEnv::new(),
            target: EnvLen::new(),
        }
    }

    /// Re-initialise the renaming to the requested `source_len`, reusing the
    /// previous allocation.
    fn init(&mut self, source_len: EnvLen) {
        self.source.clear();
        self.source.resize(source_len, None);
        self.target.clear();
    }

    fn next_rigid_var<'arena>(&self) -> Arc<Value<'arena>> {
        Arc::new(Value::rigid_var(self.source.len().next_global()))
    }

    /// Set a rigid source variable to rigid target variable mapping, ensuring
    /// that the variable appears uniquely.
    ///
    /// # Returns
    ///
    /// - `true` if the rigid binding was set successfully.
    /// - `false` if the rigid binding was already set.
    fn set_rigid(&mut self, source_var: GlobalVar) -> bool {
        let is_unique = self.get_as_global(source_var).is_none();

        if is_unique {
            let target_var = Some(self.target.next_global());
            self.source.set_global(source_var, target_var);
            self.target.push();
        }

        is_unique
    }

    /// Push an extra rigid binding onto the renaming.
    fn push_rigid(&mut self) {
        let target_var = self.target.next_global();
        self.source.push(Some(target_var));
        self.target.push();
    }

    /// Pop a rigid binding off the renaming.
    fn pop_rigid(&mut self) {
        self.source.pop();
        self.target.pop();
    }

    /// Get the rigid variable in the target environment that will be used in
    /// place of the `source_var`.
    fn get_as_global(&self, source_var: GlobalVar) -> Option<GlobalVar> {
        self.source.get_global(source_var).copied().flatten()
    }

    /// Rename a rigid variable in the source environment to a rigid variable in
    /// the target environment.
    fn get_as_local(&self, source_var: GlobalVar) -> Option<LocalVar> {
        let target_var = self.get_as_global(source_var)?;
        Some(self.target.global_to_local(target_var).unwrap())
    }
}