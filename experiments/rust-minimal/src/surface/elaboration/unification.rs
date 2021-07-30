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
    /// A bound variable appeared multiple times in a problem spine.
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
    /// well-typed! In contrast, if the problem spine only has distinct bound
    /// variables, even if the return type is dependent, bound variables block
    /// all computation in the return type, and the pattern solution is
    /// guaranteed to be well-typed.
    NonLinearSpine,
    /// A free variable in the compared value does not occur in the problem spine.
    ///
    /// For example, where `z : U` is a bound variable:
    ///
    /// ```text
    /// ?α x y =? z -> z
    /// ```
    ///
    /// There is no solution to this problem, because `?α` is the topmost-level
    /// scope, so it can only abstract over `x` and `y`, but these don't occur
    /// in `z -> z`.
    //
    // FIXME: We could call this a 'FreeBoundVar` error, but this seems like a
    //        bit of an oxymoron! Perhaps this points to some issues in our
    //        naming scheme for variables... D: D:
    EscapingVar,
    /// The problem variable occurs in the value being compared against.
    /// This is sometimes referred to as an 'occurs check' failure.
    ///
    /// For example:
    ///
    /// ```text
    /// ?α =? ?α -> ?α
    /// ```
    ///
    /// Here `?α` occurs in the right hand side, so in order to solve this
    /// problem we would end up going into an infinite loop, attempting to
    /// construct larger and larger solutions:
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
    ) -> Result<()> {
        // Check for pointer equality before trying to force the values
        if Arc::ptr_eq(value0, value1) {
            return Ok(());
        }

        let value0 = self.elim_context().force(value0)?;
        let value1 = self.elim_context().force(value1)?;

        match (value0.as_ref(), value1.as_ref()) {
            // `ReportedError`s result from errors that have already been
            // reported, so we say that they are equal to any other value to
            // prevent them from triggering more errors.
            (Value::Stuck(Head::ReportedError, _), _)
            | (_, Value::Stuck(Head::ReportedError, _)) => Ok(()),

            // Both values have head variables in common, so all we need to do
            // is unify the elimination spines.
            (
                Value::Stuck(Head::BoundVar(var0), spine0),
                Value::Stuck(Head::BoundVar(var1), spine1),
            ) if var0 == var1 => self.unify_spines(spine0, spine1),
            (
                Value::Stuck(Head::ProblemVar(var0), spine0),
                Value::Stuck(Head::ProblemVar(var1), spine1),
            ) if var0 == var1 => self.unify_spines(spine0, spine1),

            // One of the values has a problem variable at its head, so we can
            // attempt to solve it using pattern unification.
            (Value::Stuck(Head::ProblemVar(var0), spine0), _) => self.solve(*var0, spine0, &value1),
            (_, Value::Stuck(Head::ProblemVar(var1), spine1)) => self.solve(*var1, spine1, &value0),

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
        let var = Arc::new(Value::bound_var(self.bindings.next_global()));
        let value0 = self.elim_context().apply_closure(closure0, var.clone())?;
        let value1 = self.elim_context().apply_closure(closure1, var)?;

        self.push_binding();
        let result = self.unify(&value0, &value1);
        self.pop_binding();

        result
    }

    /// Unify a closure with a value, using function eta-conversion.
    fn unify_fun_intro(
        &mut self,
        output_expr: &Closure<'arena>,
        value: &Arc<Value<'arena>>,
    ) -> Result<()> {
        let var = Arc::new(Value::bound_var(self.bindings.next_global()));
        let value = self.elim_context().apply_fun(value.clone(), var.clone())?;
        let output_expr = self.elim_context().apply_closure(output_expr, var)?;

        self.push_binding();
        let result = self.unify(&output_expr, &value);
        self.pop_binding();

        result
    }

    /// Solve a pattern unification problem that looks like:
    ///
    /// ```text
    /// ?α spine =? value`
    /// ```
    ///
    /// If successful, the problem environment  will be updated with a solution
    /// that looks something like:
    ///
    /// ```text
    /// ?α := fun spine => value
    /// ```
    fn solve(
        &mut self,
        problem_var: GlobalVar,
        spine: &[Elim<'arena>],
        value: &Arc<Value<'arena>>,
    ) -> Result<()> {
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
    fn init_renaming(&mut self, spine: &[Elim<'arena>]) -> Result<()> {
        self.renaming.init(self.bindings);

        for elim in spine {
            match elim {
                Elim::Fun(input_expr) => match self.elim_context().force(input_expr)?.as_ref() {
                    Value::Stuck(Head::BoundVar(source_var), spine)
                        if spine.is_empty() && self.renaming.set_binding(*source_var) => {}
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
    /// not defined (resulting in an [scope error][Error::ScopeError]), and
    /// also checking for occurrences of the `problem_var` (resulting in an
    /// [occurs check error][Error::InfiniteSolution]).
    ///
    /// This allows us to subsequently wrap the returned term in function
    /// introductions, using [`Context::function_intros`].
    fn rename(
        &mut self,
        problem_var: GlobalVar,
        value: &Arc<Value<'arena>>,
    ) -> Result<Term<'arena>> {
        match self.elim_context().force(value)?.as_ref() {
            Value::Stuck(head, spine) => {
                let mut head_expr = match head {
                    Head::BoundVar(source_var) => match self.renaming.rename(*source_var) {
                        None => return Err(Error::EscapingVar),
                        Some(target_var) => Term::BoundVar(target_var),
                    },
                    Head::ProblemVar(var) => match *var {
                        var if problem_var == var => return Err(Error::InfiniteSolution),
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
                let output_type = self.rename_closure(problem_var, output_type)?;

                Ok(Term::FunType(
                    *input_name,
                    self.arena.alloc_term(input_type),
                    self.arena.alloc_term(output_type),
                ))
            }
            Value::FunIntro(input_name, output_expr) => {
                let output_expr = self.rename_closure(problem_var, output_expr)?;

                Ok(Term::FunIntro(
                    *input_name,
                    self.arena.alloc_term(output_expr),
                ))
            }
        }
    }

    /// Rename a closure back into [`Term`].
    fn rename_closure(
        &mut self,
        problem_var: GlobalVar,
        closure: &Closure<'arena>,
    ) -> Result<Term<'arena>> {
        let source_var = self.renaming.next_source_var();
        let value = self.elim_context().apply_closure(closure, source_var)?;

        self.renaming.push_binding();
        let term = self.rename(problem_var, &value);
        self.renaming.pop_binding();

        term
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

    fn next_source_var<'arena>(&self) -> Arc<Value<'arena>> {
        Arc::new(Value::bound_var(self.source_bindings.len().next_global()))
    }

    /// Push an extra binding onto the renaming
    fn push_binding(&mut self) {
        let target_var = self.target_bindings.next_global();
        self.target_bindings.push();
        self.source_bindings.push(Some(target_var));
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
