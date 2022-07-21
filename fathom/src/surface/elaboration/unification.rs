//! [Unification] is a process of checking if two [values][Value] are the same,
//! where there might be 'unknown' parts in either value. During this process
//! we attempt to fill in those missing bits of information, and record the
//! solutions we find for future use.
//!
//! We implement a limited form of higher order unification, called 'higher-
//! order pattern unification', which was first described by Dale Miller
//! in ["A Logic Programming Language with Lambda-Abstraction, Function
//! Variables, and Simple Unification”][dale-miller-1991]. More details about
//! the algorithm we use can be found in the [elaboration-zoo], in particular
//! in [elaboration-zoo/03-holes].
//!
//! [Unification]: https://en.wikipedia.org/wiki/Unification_(computer_science)
//! [dale-miller-1991]: https://doi.org/10.1093/logcom/1.4.497
//! [elaboration-zoo]: https://github.com/AndrasKovacs/elaboration-zoo/
//! [elaboration-zoo/03-holes]: https://github.com/AndrasKovacs/elaboration-zoo/tree/master/03-holes

use scoped_arena::Scope;
use std::sync::Arc;

use crate::alloc::SliceVec;
use crate::core::semantics::{
    self, ArcValue, Closure, Elim, Head, SplitBranches, Telescope, Value,
};
use crate::core::{Prim, Span, Term};
use crate::env::{EnvLen, GlobalVar, LocalVar, SharedEnv, SliceEnv, UniqueEnv};
use crate::StringId;

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
    //
    // TODO: Return some sort of type-diff
    Mismatch,
    /// An error that was found in the problem spine.
    Spine(SpineError),
    /// An error that occurred when renaming the solution.
    Rename(RenameError),
}

impl From<SpineError> for Error {
    fn from(error: SpineError) -> Error {
        Error::Spine(error)
    }
}

impl From<RenameError> for Error {
    fn from(error: RenameError) -> Error {
        Error::Rename(error)
    }
}

/// An error that was found in the problem spine.
#[derive(Debug, Clone)]
pub enum SpineError {
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
    NonLinearSpine(GlobalVar),
    /// A flexible variable was found in the problem spine.
    NonRigidFunApp,
    /// A record projection was found in the problem spine.
    RecordProj(StringId),
    /// A constant match was found in the problem spine.
    ConstMatch,
}

/// An error that occurred when renaming the solution.
#[derive(Debug, Clone)]
pub enum RenameError {
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
    EscapingRigidVar(GlobalVar),
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
}

/// Unification context.
pub struct Context<'arena, 'env> {
    /// Scoped arena for storing [renamed][Context::rename] terms.
    scope: &'arena Scope<'arena>,
    /// A renaming that is used when solving flexible variables using pattern
    /// unification. We store it in the parent context, re-initialising it on
    /// each call to [`Context::solve`] in order to reuse previous allocations.
    renaming: &'env mut PartialRenaming,
    /// Item expressions.
    item_exprs: &'env SliceEnv<ArcValue<'arena>>,
    /// The length of the rigid environment.
    rigid_exprs: EnvLen,
    /// Solutions for flexible variables.
    flexible_exprs: &'env mut SliceEnv<Option<ArcValue<'arena>>>,
}

impl<'arena, 'env> Context<'arena, 'env> {
    pub fn new(
        scope: &'arena Scope<'arena>,
        renaming: &'env mut PartialRenaming,
        item_exprs: &'env SliceEnv<ArcValue<'arena>>,
        rigid_exprs: EnvLen,
        flexible_exprs: &'env mut SliceEnv<Option<ArcValue<'arena>>>,
    ) -> Context<'arena, 'env> {
        Context {
            scope,
            renaming,
            item_exprs,
            rigid_exprs,
            flexible_exprs,
        }
    }

    fn elim_context(&self) -> semantics::ElimContext<'arena, '_> {
        semantics::ElimContext::new(self.item_exprs, self.flexible_exprs)
    }

    /// Unify two values, updating the solution environment if necessary.
    pub fn unify(
        &mut self,
        value0: &ArcValue<'arena>,
        value1: &ArcValue<'arena>,
    ) -> Result<(), Error> {
        // Check for pointer equality before trying to force the values
        if Arc::ptr_eq(value0, value1) {
            return Ok(());
        }

        let value0 = self.elim_context().force(value0);
        let value1 = self.elim_context().force(value1);

        match (value0.as_ref(), value1.as_ref()) {
            // `ReportedError`s result from errors that have already been
            // reported, so we prevent them from triggering more errors.
            (Value::Stuck(_, Head::Prim(Prim::ReportedError), _), _)
            | (_, Value::Stuck(_, Head::Prim(Prim::ReportedError), _)) => Ok(()),

            // Rigid-rigid and flexible-flexible cases
            //
            // Both values have head variables in common, so all we need to do
            // is unify the elimination spines.
            (
                Value::Stuck(_, Head::Prim(prim0), spine0),
                Value::Stuck(_, Head::Prim(prim1), spine1),
            ) if prim0 == prim1 => self.unify_spines(spine0, spine1),
            (
                Value::Stuck(_, Head::RigidVar(var0), spine0),
                Value::Stuck(_, Head::RigidVar(var1), spine1),
            ) if var0 == var1 => self.unify_spines(spine0, spine1),
            (
                Value::Stuck(_, Head::FlexibleVar(var0), spine0),
                Value::Stuck(_, Head::FlexibleVar(var1), spine1),
            ) if var0 == var1 => self.unify_spines(spine0, spine1),

            (Value::Universe(_), Value::Universe(_)) => Ok(()),

            (
                Value::FunType(_, _, input_type0, output_type0),
                Value::FunType(_, _, input_type1, output_type1),
            ) => {
                self.unify(input_type0, input_type1)?;
                self.unify_closures(output_type0, output_type1)
            }
            (Value::FunLit(_, _, output_expr0), Value::FunLit(_, _, output_expr1)) => {
                self.unify_closures(output_expr0, output_expr1)
            }
            (Value::FunLit(_, _, output_expr), _) => self.unify_fun_lit(output_expr, &value1),
            (_, Value::FunLit(_, _, output_expr)) => self.unify_fun_lit(output_expr, &value0),

            (Value::RecordType(_, labels0, types0), Value::RecordType(_, labels1, types1)) => {
                if labels0 != labels1 {
                    return Err(Error::Mismatch);
                }
                self.unify_telescopes(types0, types1)
            }
            (Value::RecordLit(_, labels0, exprs0), Value::RecordLit(_, labels1, exprs1)) => {
                if labels0 != labels1 {
                    return Err(Error::Mismatch);
                }
                for (expr0, expr1) in Iterator::zip(exprs0.iter(), exprs1.iter()) {
                    self.unify(&expr0, &expr1)?;
                }
                Ok(())
            }
            (Value::RecordLit(_, labels, exprs), _) => {
                self.unify_record_lit(labels, exprs, &value1)
            }
            (_, Value::RecordLit(_, labels, exprs)) => {
                self.unify_record_lit(labels, exprs, &value0)
            }

            (Value::ArrayLit(_, elem_exprs0), Value::ArrayLit(_, elem_exprs1)) => {
                for (elem_expr0, elem_expr1) in
                    Iterator::zip(elem_exprs0.iter(), elem_exprs1.iter())
                {
                    self.unify(&elem_expr0, &elem_expr1)?;
                }
                Ok(())
            }

            (
                Value::FormatRecord(_, labels0, formats0),
                Value::FormatRecord(_, labels1, formats1),
            ) => {
                if labels0 != labels1 {
                    return Err(Error::Mismatch);
                }
                self.unify_telescopes(formats0, formats1)
            }

            (
                Value::FormatCond(_, label0, format0, cond0),
                Value::FormatCond(_, label1, format1, cond1),
            ) => {
                if label0 != label1 {
                    return Err(Error::Mismatch);
                }
                self.unify(format0, format1)?;
                self.unify_closures(cond0, cond1)
            }

            (Value::ConstLit(const0), Value::ConstLit(const1)) if const0 == const1 => Ok(()),

            // Flexible-rigid cases
            //
            // One of the values has a flexible variable at its head, so we
            // attempt to solve it using pattern unification.
            (Value::Stuck(_, Head::FlexibleVar(var0), spine0), _) => {
                self.solve(*var0, spine0, &value1)
            }
            (_, Value::Stuck(_, Head::FlexibleVar(var1), spine1)) => {
                self.solve(*var1, spine1, &value0)
            }

            (_, _) => Err(Error::Mismatch),
        }
    }

    /// Unify two elimination spines.
    fn unify_spines(
        &mut self,
        spine0: &[Elim<'arena>],
        spine1: &[Elim<'arena>],
    ) -> Result<(), Error> {
        if spine0.len() != spine1.len() {
            return Err(Error::Mismatch);
        }
        for (elim0, elim1) in Iterator::zip(spine0.iter(), spine1.iter()) {
            match (elim0, elim1) {
                (Elim::FunApp(input_expr0), Elim::FunApp(input_expr1)) => {
                    self.unify(input_expr0, input_expr1)?;
                }
                (Elim::RecordProj(label0), Elim::RecordProj(label1)) if label0 == label1 => {}
                (_, _) => {
                    return Err(Error::Mismatch);
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
    ) -> Result<(), Error> {
        let var = Arc::new(Value::rigid_var(self.rigid_exprs.next_global()));
        let value0 = self.elim_context().apply_closure(closure0, var.clone());
        let value1 = self.elim_context().apply_closure(closure1, var);

        self.rigid_exprs.push();
        let result = self.unify(&value0, &value1);
        self.rigid_exprs.pop();

        result
    }

    /// Unify two [telescopes][Telescope].
    fn unify_telescopes(
        &mut self,
        telescope0: &Telescope<'arena>,
        telescope1: &Telescope<'arena>,
    ) -> Result<(), Error> {
        if telescope0.len() != telescope1.len() {
            return Err(Error::Mismatch);
        }

        let initial_rigid_len = self.rigid_exprs;
        let mut telescope0 = telescope0.clone();
        let mut telescope1 = telescope1.clone();

        while let Some(((value0, next_telescope0), (value1, next_telescope1))) = Option::zip(
            self.elim_context().split_telescope(telescope0),
            self.elim_context().split_telescope(telescope1),
        ) {
            if let Err(error) = self.unify(&value0, &value1) {
                self.rigid_exprs.truncate(initial_rigid_len);
                return Err(error);
            }

            let var = Arc::new(Value::rigid_var(self.rigid_exprs.next_global()));
            telescope0 = next_telescope0(var.clone());
            telescope1 = next_telescope1(var);
            self.rigid_exprs.push();
        }

        self.rigid_exprs.truncate(initial_rigid_len);
        Ok(())
    }

    /// Unify a function literal with a value, using eta-conversion.
    ///
    /// ```fathom
    /// (fun x => f x) = f
    /// ```
    fn unify_fun_lit(
        &mut self,
        output_expr: &Closure<'arena>,
        value: &ArcValue<'arena>,
    ) -> Result<(), Error> {
        let var = Arc::new(Value::rigid_var(self.rigid_exprs.next_global()));
        let value = self.elim_context().fun_app(value.clone(), var.clone());
        let output_expr = self.elim_context().apply_closure(output_expr, var);

        self.rigid_exprs.push();
        let result = self.unify(&output_expr, &value);
        self.rigid_exprs.pop();

        result
    }

    /// Unify a record literal with a value, using eta-conversion.
    ///
    /// ```fathom
    /// { x = r.x, y = r.y, .. } = r
    /// ```
    fn unify_record_lit(
        &mut self,
        labels: &[StringId],
        exprs: &[ArcValue<'arena>],
        value: &ArcValue<'arena>,
    ) -> Result<(), Error> {
        for (label, expr) in Iterator::zip(labels.iter(), exprs.iter()) {
            let field_value = self.elim_context().record_proj(value.clone(), *label);
            self.unify(expr, &field_value)?;
        }
        Ok(())
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
        value: &ArcValue<'arena>,
    ) -> Result<(), Error> {
        self.init_renaming(spine)?;
        let term = self.rename(flexible_var, value)?;
        let fun_term = self.fun_intros(spine, term);
        let solution = self
            .elim_context()
            .eval_context(&mut SharedEnv::new())
            .eval(&fun_term);

        self.flexible_exprs.set_global(flexible_var, Some(solution));

        Ok(())
    }

    /// Re-initialise the [`Context::renaming`] by mapping the rigid variables
    /// in the spine to the rigid variables in the solution. This can fail if
    /// the spine does not contain distinct rigid variables.
    fn init_renaming(&mut self, spine: &[Elim<'arena>]) -> Result<(), SpineError> {
        self.renaming.init(self.rigid_exprs);

        for elim in spine {
            match elim {
                Elim::FunApp(input_expr) => match self.elim_context().force(input_expr).as_ref() {
                    Value::Stuck(_, Head::RigidVar(source_var), spine)
                        if spine.is_empty() && self.renaming.set_rigid(*source_var) => {}
                    Value::Stuck(_, Head::RigidVar(source_var), _) => {
                        return Err(SpineError::NonLinearSpine(*source_var))
                    }
                    _ => return Err(SpineError::NonRigidFunApp),
                },
                Elim::RecordProj(label) => return Err(SpineError::RecordProj(*label)),
                Elim::ConstMatch(_) => return Err(SpineError::ConstMatch),
            }
        }

        Ok(())
    }

    /// Wrap a `term` in [function literals][Term::FunLit] that
    /// correspond to the given `spine`.
    fn fun_intros(&self, spine: &[Elim<'arena>], term: Term<'arena>) -> Term<'arena> {
        spine.iter().fold(term, |term, elim| match elim {
            Elim::FunApp(_) => Term::FunLit(Span::fixme(), None, self.scope.to_scope(term)),
            Elim::RecordProj(_) | Elim::ConstMatch(_) => {
                unreachable!("should have been caught by `init_renaming`")
            }
        })
    }

    /// Rename `value` to a [`Term`], while at the same time using the current
    /// renaming to update local variables, failing if the partial renaming is
    /// not defined (resulting in an [scope error][Error::ScopeError]), and also
    /// checking for occurrences of the `flexible_var` (resulting in an [occurs
    /// check error][Error::InfiniteSolution]).
    ///
    /// This allows us to subsequently wrap the returned term in function
    /// literals, using [`Context::function_intros`].
    fn rename(
        &mut self,
        flexible_var: GlobalVar,
        value: &ArcValue<'arena>,
    ) -> Result<Term<'arena>, RenameError> {
        match self.elim_context().force(value).as_ref() {
            Value::Stuck(span, head, spine) => {
                let head_expr = match head {
                    Head::Prim(prim) => Term::Prim(*span, *prim),
                    Head::RigidVar(source_var) => match self.renaming.get_as_local(*source_var) {
                        None => return Err(RenameError::EscapingRigidVar(*source_var)),
                        Some(target_var) => Term::RigidVar(*span, target_var),
                    },
                    Head::FlexibleVar(var) => match *var {
                        var if flexible_var == var => return Err(RenameError::InfiniteSolution),
                        var => Term::FlexibleVar(*span, var),
                    },
                };

                spine.iter().fold(Ok(head_expr), |head_expr, elim| {
                    Ok(match elim {
                        Elim::FunApp(input_expr) => Term::FunApp(
                            *span,
                            self.scope.to_scope(head_expr?),
                            self.scope.to_scope(self.rename(flexible_var, input_expr)?),
                        ),
                        Elim::RecordProj(label) => {
                            Term::RecordProj(*span, self.scope.to_scope(head_expr?), *label)
                        }
                        Elim::ConstMatch(branches) => {
                            let mut branches = branches.clone();
                            let mut pattern_branches =
                                SliceVec::new(self.scope, branches.num_patterns());

                            let default_expr = loop {
                                match self.elim_context().split_branches(branches) {
                                    SplitBranches::Branch((r#const, output_expr), next_branch) => {
                                        pattern_branches.push((
                                            r#const,
                                            self.rename(flexible_var, &output_expr)?,
                                        ));
                                        branches = next_branch;
                                    }
                                    SplitBranches::Default(default_expr) => {
                                        break Some(
                                            self.rename_closure(flexible_var, &default_expr)?,
                                        );
                                    }
                                    SplitBranches::None => {
                                        break None;
                                    }
                                }
                            };

                            Term::ConstMatch(
                                *span,
                                self.scope.to_scope(head_expr?),
                                pattern_branches.into(),
                                default_expr.map(|expr| self.scope.to_scope(expr) as &_),
                            )
                        }
                    })
                })
            }

            Value::Universe(span) => Ok(Term::Universe(*span)),

            Value::FunType(span, input_name, input_type, output_type) => {
                let input_type = self.rename(flexible_var, input_type)?;
                let output_type = self.rename_closure(flexible_var, output_type)?;

                Ok(Term::FunType(
                    *span,
                    *input_name,
                    self.scope.to_scope(input_type),
                    self.scope.to_scope(output_type),
                ))
            }
            Value::FunLit(span, input_name, output_expr) => {
                let output_expr = self.rename_closure(flexible_var, output_expr)?;

                Ok(Term::FunLit(
                    *span,
                    *input_name,
                    self.scope.to_scope(output_expr),
                ))
            }

            Value::RecordType(span, labels, types) => {
                let labels = self.scope.to_scope(labels); // FIXME: avoid copy if this is the same arena?
                let types = self.rename_telescope(flexible_var, types)?;

                Ok(Term::RecordType(*span, labels, types))
            }
            Value::RecordLit(span, labels, exprs) => {
                let labels = self.scope.to_scope(labels); // FIXME: avoid copy if this is the same arena?
                let mut new_exprs = SliceVec::new(self.scope, exprs.len());
                for expr in exprs {
                    new_exprs.push(self.rename(flexible_var, expr)?);
                }

                Ok(Term::RecordLit(*span, labels, new_exprs.into()))
            }

            Value::ArrayLit(span, elem_exprs) => {
                let mut new_elem_exprs = SliceVec::new(self.scope, elem_exprs.len());
                for elem_expr in elem_exprs {
                    new_elem_exprs.push(self.rename(flexible_var, elem_expr)?);
                }

                Ok(Term::ArrayLit(*span, new_elem_exprs.into()))
            }

            Value::FormatRecord(span, labels, formats) => {
                let labels = self.scope.to_scope(labels); // FIXME: avoid copy if this is the same arena?
                let formats = self.rename_telescope(flexible_var, formats)?;

                Ok(Term::FormatRecord(*span, labels, formats))
            }
            Value::FormatCond(span, label, format, cond) => {
                let format = self.rename(flexible_var, format)?;
                let cond = self.rename_closure(flexible_var, cond)?;
                Ok(Term::FormatCond(
                    *span,
                    *label,
                    self.scope.to_scope(format),
                    self.scope.to_scope(cond),
                ))
            }
            Value::FormatOverlap(labels, formats) => {
                let labels = self.scope.to_scope(labels); // FIXME: avoid copy if this is the same arena?
                let formats = self.rename_telescope(flexible_var, formats)?;

                Ok(Term::FormatOverlap(Span::fixme(), labels, formats))
            }

            Value::ConstLit(constant) => Ok(Term::ConstLit(Span::fixme(), *constant)),
        }
    }

    /// Rename a closure back into a [`Term`].
    fn rename_closure(
        &mut self,
        flexible_var: GlobalVar,
        closure: &Closure<'arena>,
    ) -> Result<Term<'arena>, RenameError> {
        let source_var = self.renaming.next_rigid_var();
        let value = self.elim_context().apply_closure(closure, source_var);

        self.renaming.push_rigid();
        let term = self.rename(flexible_var, &value);
        self.renaming.pop_rigid();

        term
    }

    /// Rename a telescope back into a [`Term`].
    fn rename_telescope(
        &mut self,
        flexible_var: GlobalVar,
        telescope: &Telescope<'arena>,
    ) -> Result<&'arena [Term<'arena>], RenameError> {
        let initial_rigid_len = self.rigid_exprs;
        let mut telescope = telescope.clone();
        let mut terms = SliceVec::new(self.scope, telescope.len());

        while let Some((value, next_telescope)) = self.elim_context().split_telescope(telescope) {
            match self.rename(flexible_var, &value) {
                Ok(term) => {
                    terms.push(term);
                    let var = Arc::new(Value::rigid_var(self.rigid_exprs.next_global()));
                    telescope = next_telescope(var);
                    self.rigid_exprs.push();
                }
                Err(error) => {
                    self.rigid_exprs.truncate(initial_rigid_len);
                    return Err(error);
                }
            }
        }

        self.rigid_exprs.truncate(initial_rigid_len);
        Ok(terms.into())
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

    fn next_rigid_var<'arena>(&self) -> ArcValue<'arena> {
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
