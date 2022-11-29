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
    self, ArcValue, Branches, Closure, Elim, Head, SplitBranches, Telescope, Value,
};
use crate::core::{Prim, Term};
use crate::env::{EnvLen, Index, Level, SharedEnv, SliceEnv, UniqueEnv};
use crate::source::{Spanned, StringId};

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

/// An error that was found in the spine of a unification problem.
#[derive(Debug, Clone)]
pub enum SpineError {
    /// A local variable appeared multiple times in the spine of a unification
    /// problem.
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
    /// well-typed! In contrast, if the problem spine only has distinct local
    /// variables, even if the return type is dependent, local variables block
    /// all computation in the return type, and the pattern solution is
    /// guaranteed to be well-typed.
    NonLinearSpine(Level),
    /// A metavariable was found in the problem spine.
    NonLocalFunApp,
    /// A record projection was found in the problem spine.
    RecordProj(StringId),
    /// A constant match was found in the problem spine.
    ConstMatch,
}

/// An error that occurred when renaming the solution.
#[derive(Debug, Clone)]
pub enum RenameError {
    /// A free local variable in the compared value does not occur in the
    /// problem spine.
    ///
    /// For example, where `z : U` is a local variable:
    ///
    /// ```text
    /// ?α x y =? z -> z
    /// ```
    ///
    /// There is no solution for this metavariable because `?α` is the
    /// topmost-level scope, so it can only abstract over `x` and `y`, but
    /// these don't occur in `z -> z`.
    EscapingLocalVar(Level),
    /// The metavariable occurs in the value being compared against.
    /// This is sometimes referred to as an 'occurs check' failure.
    ///
    /// For example:
    ///
    /// ```text
    /// ?α =? ?α -> ?α
    /// ```
    ///
    /// Here `?α` occurs in the right hand side, so in order to solve this
    /// metavariable we would end up going into an infinite loop,
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
    /// A renaming that is used when solving metavariables using pattern
    /// unification. We store it in the parent context, re-initialising it on
    /// each call to [`Context::solve`] in order to reuse previous allocations.
    renaming: &'env mut PartialRenaming,
    /// Item expressions.
    item_exprs: &'env SliceEnv<ArcValue<'arena>>,
    /// The length of the local environment.
    local_exprs: EnvLen,
    /// Solutions for metavariables.
    meta_exprs: &'env mut SliceEnv<Option<ArcValue<'arena>>>,
}

impl<'arena, 'env> Context<'arena, 'env> {
    pub fn new(
        scope: &'arena Scope<'arena>,
        renaming: &'env mut PartialRenaming,
        item_exprs: &'env SliceEnv<ArcValue<'arena>>,
        local_exprs: EnvLen,
        meta_exprs: &'env mut SliceEnv<Option<ArcValue<'arena>>>,
    ) -> Context<'arena, 'env> {
        Context {
            scope,
            renaming,
            item_exprs,
            local_exprs,
            meta_exprs,
        }
    }

    fn elim_env(&self) -> semantics::ElimEnv<'arena, '_> {
        semantics::ElimEnv::new(self.item_exprs, self.meta_exprs)
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

        let value0 = self.elim_env().force(value0);
        let value1 = self.elim_env().force(value1);

        match (value0.as_ref(), value1.as_ref()) {
            // `ReportedError`s result from errors that have already been
            // reported, so we prevent them from triggering more errors.
            (Value::Stuck(Head::Prim(Prim::ReportedError), _), _)
            | (_, Value::Stuck(Head::Prim(Prim::ReportedError), _)) => Ok(()),

            // Local-local and meta-meta cases
            //
            // Both values have head variables in common, so all we need to do
            // is unify the elimination spines.
            (Value::Stuck(Head::Prim(prim0), spine0), Value::Stuck(Head::Prim(prim1), spine1))
                if prim0 == prim1 =>
            {
                self.unify_spines(spine0, spine1)
            }
            (
                Value::Stuck(Head::LocalVar(var0), spine0),
                Value::Stuck(Head::LocalVar(var1), spine1),
            ) if var0 == var1 => self.unify_spines(spine0, spine1),
            (
                Value::Stuck(Head::MetaVar(var0), spine0),
                Value::Stuck(Head::MetaVar(var1), spine1),
            ) if var0 == var1 => self.unify_spines(spine0, spine1),

            (Value::Universe, Value::Universe) => Ok(()),

            (
                Value::FunType(_, param_type0, body_type0),
                Value::FunType(_, param_type1, body_type1),
            ) => {
                self.unify(param_type0, param_type1)?;
                self.unify_closures(body_type0, body_type1)
            }
            (Value::FunLit(_, body_expr0), Value::FunLit(_, body_expr1)) => {
                self.unify_closures(body_expr0, body_expr1)
            }
            (Value::FunLit(_, body_expr), _) => self.unify_fun_lit(body_expr, &value1),
            (_, Value::FunLit(_, body_expr)) => self.unify_fun_lit(body_expr, &value0),

            (Value::RecordType(labels0, types0), Value::RecordType(labels1, types1)) => {
                if labels0 != labels1 {
                    return Err(Error::Mismatch);
                }
                self.unify_telescopes(types0, types1)
            }
            (Value::RecordLit(labels0, exprs0), Value::RecordLit(labels1, exprs1)) => {
                if labels0 != labels1 {
                    return Err(Error::Mismatch);
                }
                for (expr0, expr1) in Iterator::zip(exprs0.iter(), exprs1.iter()) {
                    self.unify(expr0, expr1)?;
                }
                Ok(())
            }
            (Value::RecordLit(labels, exprs), _) => self.unify_record_lit(labels, exprs, &value1),
            (_, Value::RecordLit(labels, exprs)) => self.unify_record_lit(labels, exprs, &value0),

            (Value::ArrayLit(elem_exprs0), Value::ArrayLit(elem_exprs1)) => {
                for (elem_expr0, elem_expr1) in
                    Iterator::zip(elem_exprs0.iter(), elem_exprs1.iter())
                {
                    self.unify(elem_expr0, elem_expr1)?;
                }
                Ok(())
            }

            (Value::FormatRecord(labels0, formats0), Value::FormatRecord(labels1, formats1)) => {
                if labels0 != labels1 {
                    return Err(Error::Mismatch);
                }
                self.unify_telescopes(formats0, formats1)
            }

            (
                Value::FormatCond(label0, format0, cond0),
                Value::FormatCond(label1, format1, cond1),
            ) => {
                if label0 != label1 {
                    return Err(Error::Mismatch);
                }
                self.unify(format0, format1)?;
                self.unify_closures(cond0, cond1)
            }

            (Value::ConstLit(const0), Value::ConstLit(const1)) if const0 == const1 => Ok(()),

            // Meta-local cases
            //
            // One of the values has a metavariable at its head, so we
            // attempt to solve it using pattern unification.
            (Value::Stuck(Head::MetaVar(var0), spine0), _) => self.solve(*var0, spine0, &value1),
            (_, Value::Stuck(Head::MetaVar(var1), spine1)) => self.solve(*var1, spine1, &value0),

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
                (Elim::FunApp(arg_expr0), Elim::FunApp(arg_expr1)) => {
                    self.unify(arg_expr0, arg_expr1)?;
                }
                (Elim::RecordProj(label0), Elim::RecordProj(label1)) if label0 == label1 => {}
                (Elim::ConstMatch(branches0), Elim::ConstMatch(branches1)) => {
                    self.unify_branches(branches0, branches1)?;
                }
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
        let var = Spanned::empty(Arc::new(Value::local_var(self.local_exprs.next_level())));
        let value0 = self.elim_env().apply_closure(closure0, var.clone());
        let value1 = self.elim_env().apply_closure(closure1, var);

        self.local_exprs.push();
        let result = self.unify(&value0, &value1);
        self.local_exprs.pop();

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

        let initial_local_len = self.local_exprs;
        let mut telescope0 = telescope0.clone();
        let mut telescope1 = telescope1.clone();

        while let Some(((value0, next_telescope0), (value1, next_telescope1))) = Option::zip(
            self.elim_env().split_telescope(telescope0),
            self.elim_env().split_telescope(telescope1),
        ) {
            if let Err(error) = self.unify(&value0, &value1) {
                self.local_exprs.truncate(initial_local_len);
                return Err(error);
            }

            let var = Spanned::empty(Arc::new(Value::local_var(self.local_exprs.next_level())));
            telescope0 = next_telescope0(var.clone());
            telescope1 = next_telescope1(var);
            self.local_exprs.push();
        }

        self.local_exprs.truncate(initial_local_len);
        Ok(())
    }

    /// Unify two [constant branches][Branches].
    fn unify_branches<P: PartialEq + Copy>(
        &mut self,
        branches0: &Branches<'arena, P>,
        branches1: &Branches<'arena, P>,
    ) -> Result<(), Error> {
        use SplitBranches::*;

        let mut branches0 = branches0.clone();
        let mut branches1 = branches1.clone();

        loop {
            match (
                self.elim_env().split_branches(branches0),
                self.elim_env().split_branches(branches1),
            ) {
                (
                    Branch((const0, body_expr0), next_branches0),
                    Branch((const1, body_expr1), next_branches1),
                ) if const0 == const1 => match self.unify(&body_expr0, &body_expr1) {
                    Err(err) => return Err(err),
                    Ok(()) => {
                        branches0 = next_branches0;
                        branches1 = next_branches1;
                    }
                },
                (Default(_, default_expr0), Default(_, default_expr1)) => {
                    return self.unify_closures(&default_expr0, &default_expr1);
                }
                (None, None) => return Ok(()),
                (_, _) => return Err(Error::Mismatch),
            }
        }
    }

    /// Unify a function literal with a value, using eta-conversion.
    ///
    /// ```fathom
    /// (fun x => f x) = f
    /// ```
    fn unify_fun_lit(
        &mut self,
        body_expr: &Closure<'arena>,
        value: &ArcValue<'arena>,
    ) -> Result<(), Error> {
        let var = Spanned::empty(Arc::new(Value::local_var(self.local_exprs.next_level())));
        let value = self.elim_env().fun_app(value.clone(), var.clone());
        let body_expr = self.elim_env().apply_closure(body_expr, var);

        self.local_exprs.push();
        let result = self.unify(&body_expr, &value);
        self.local_exprs.pop();

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
            let field_value = self.elim_env().record_proj(value.clone(), *label);
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
    /// If successful, the metavariable environment will be updated with a
    /// solution that looks something like:
    ///
    /// ```text
    /// ?α := fun spine => value
    /// ```
    fn solve(
        &mut self,
        meta_var: Level,
        spine: &[Elim<'arena>],
        value: &ArcValue<'arena>,
    ) -> Result<(), Error> {
        self.init_renaming(spine)?;
        let term = self.rename(meta_var, value)?;
        let fun_term = self.fun_intros(spine, term);
        let mut local_exprs = SharedEnv::new();
        let solution = self.elim_env().eval_env(&mut local_exprs).eval(&fun_term);

        self.meta_exprs.set_level(meta_var, Some(solution));

        Ok(())
    }

    /// Re-initialise the [`Context::renaming`] by mapping the local variables
    /// in the spine to the local variables in the solution. This can fail if
    /// the spine does not contain distinct local variables.
    fn init_renaming(&mut self, spine: &[Elim<'arena>]) -> Result<(), SpineError> {
        self.renaming.init(self.local_exprs);

        for elim in spine {
            match elim {
                Elim::FunApp(arg_expr) => match self.elim_env().force(arg_expr).as_ref() {
                    Value::Stuck(Head::LocalVar(source_var), spine)
                        if spine.is_empty() && self.renaming.set_local(*source_var) => {}
                    Value::Stuck(Head::LocalVar(source_var), _) => {
                        return Err(SpineError::NonLinearSpine(*source_var))
                    }
                    _ => return Err(SpineError::NonLocalFunApp),
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
            Elim::FunApp(_) => Term::FunLit(term.span(), None, self.scope.to_scope(term)),
            Elim::RecordProj(_) | Elim::ConstMatch(_) => {
                unreachable!("should have been caught by `init_renaming`")
            }
        })
    }

    /// Rename `value` to a [`Term`], while at the same time using the current
    /// renaming to update variable indices, failing if the partial renaming is
    /// not defined (resulting in an [scope error][Error::ScopeError]), and also
    /// checking for occurrences of the `meta_var` (resulting in an [occurs
    /// check error][Error::InfiniteSolution]).
    ///
    /// This allows us to subsequently wrap the returned term in function
    /// literals, using [`Context::function_intros`].
    fn rename(
        &mut self,
        meta_var: Level,
        value: &ArcValue<'arena>,
    ) -> Result<Term<'arena>, RenameError> {
        let val = self.elim_env().force(value);
        let span = val.span();
        match val.as_ref() {
            Value::Stuck(head, spine) => {
                let head_expr = match head {
                    Head::Prim(prim) => Term::Prim(span, *prim),
                    Head::LocalVar(source_var) => match self.renaming.get_as_index(*source_var) {
                        None => return Err(RenameError::EscapingLocalVar(*source_var)),
                        Some(target_var) => Term::LocalVar(span, target_var),
                    },
                    Head::MetaVar(var) => match *var {
                        var if meta_var == var => return Err(RenameError::InfiniteSolution),
                        var => Term::MetaVar(span, var),
                    },
                };

                spine.iter().fold(Ok(head_expr), |head_expr, elim| {
                    Ok(match elim {
                        Elim::FunApp(arg_expr) => Term::FunApp(
                            span,
                            self.scope.to_scope(head_expr?),
                            self.scope.to_scope(self.rename(meta_var, arg_expr)?),
                        ),
                        Elim::RecordProj(label) => {
                            Term::RecordProj(span, self.scope.to_scope(head_expr?), *label)
                        }
                        Elim::ConstMatch(branches) => {
                            let mut branches = branches.clone();
                            let mut pattern_branches =
                                SliceVec::new(self.scope, branches.num_patterns());

                            let default_branch = loop {
                                match self.elim_env().split_branches(branches) {
                                    SplitBranches::Branch((r#const, body_expr), next_branch) => {
                                        pattern_branches
                                            .push((r#const, self.rename(meta_var, &body_expr)?));
                                        branches = next_branch;
                                    }
                                    SplitBranches::Default(default_name, default_expr) => {
                                        break Some((
                                            default_name,
                                            self.rename_closure(meta_var, &default_expr)?,
                                        ))
                                    }
                                    SplitBranches::None => break None,
                                }
                            };

                            Term::ConstMatch(
                                span,
                                self.scope.to_scope(head_expr?),
                                pattern_branches.into(),
                                default_branch
                                    .map(|(name, expr)| (name, self.scope.to_scope(expr) as &_)),
                            )
                        }
                    })
                })
            }

            Value::Universe => Ok(Term::Universe(span)),

            Value::FunType(param_name, param_type, body_type) => {
                let param_type = self.rename(meta_var, param_type)?;
                let body_type = self.rename_closure(meta_var, body_type)?;

                Ok(Term::FunType(
                    span,
                    *param_name,
                    self.scope.to_scope(param_type),
                    self.scope.to_scope(body_type),
                ))
            }
            Value::FunLit(param_name, body_expr) => {
                let body_expr = self.rename_closure(meta_var, body_expr)?;

                Ok(Term::FunLit(
                    span,
                    *param_name,
                    self.scope.to_scope(body_expr),
                ))
            }

            Value::RecordType(labels, types) => {
                let labels = self.scope.to_scope(labels); // FIXME: avoid copy if this is the same arena?
                let types = self.rename_telescope(meta_var, types)?;

                Ok(Term::RecordType(span, labels, types))
            }
            Value::RecordLit(labels, exprs) => {
                let labels = self.scope.to_scope(labels); // FIXME: avoid copy if this is the same arena?
                let mut new_exprs = SliceVec::new(self.scope, exprs.len());
                for expr in exprs {
                    new_exprs.push(self.rename(meta_var, expr)?);
                }

                Ok(Term::RecordLit(span, labels, new_exprs.into()))
            }

            Value::ArrayLit(elem_exprs) => {
                let mut new_elem_exprs = SliceVec::new(self.scope, elem_exprs.len());
                for elem_expr in elem_exprs {
                    new_elem_exprs.push(self.rename(meta_var, elem_expr)?);
                }

                Ok(Term::ArrayLit(span, new_elem_exprs.into()))
            }

            Value::FormatRecord(labels, formats) => {
                let labels = self.scope.to_scope(labels); // FIXME: avoid copy if this is the same arena?
                let formats = self.rename_telescope(meta_var, formats)?;

                Ok(Term::FormatRecord(span, labels, formats))
            }
            Value::FormatCond(label, format, cond) => {
                let format = self.rename(meta_var, format)?;
                let cond = self.rename_closure(meta_var, cond)?;
                Ok(Term::FormatCond(
                    span,
                    *label,
                    self.scope.to_scope(format),
                    self.scope.to_scope(cond),
                ))
            }
            Value::FormatOverlap(labels, formats) => {
                let labels = self.scope.to_scope(labels); // FIXME: avoid copy if this is the same arena?
                let formats = self.rename_telescope(meta_var, formats)?;

                Ok(Term::FormatOverlap(span, labels, formats))
            }

            Value::ConstLit(constant) => Ok(Term::ConstLit(span, *constant)),
        }
    }

    /// Rename a closure back into a [`Term`].
    fn rename_closure(
        &mut self,
        meta_var: Level,
        closure: &Closure<'arena>,
    ) -> Result<Term<'arena>, RenameError> {
        let source_var = self.renaming.next_local_var();
        let value = self.elim_env().apply_closure(closure, source_var);

        self.renaming.push_local();
        let term = self.rename(meta_var, &value);
        self.renaming.pop_local();

        term
    }

    /// Rename a telescope back into a [`Term`].
    fn rename_telescope(
        &mut self,
        meta_var: Level,
        telescope: &Telescope<'arena>,
    ) -> Result<&'arena [Term<'arena>], RenameError> {
        let initial_renaming_len = self.renaming.len();
        let mut telescope = telescope.clone();
        let mut terms = SliceVec::new(self.scope, telescope.len());

        while let Some((value, next_telescope)) = self.elim_env().split_telescope(telescope) {
            match self.rename(meta_var, &value) {
                Ok(term) => {
                    terms.push(term);
                    let source_var = self.renaming.next_local_var();
                    telescope = next_telescope(source_var);
                    self.renaming.push_local();
                }
                Err(error) => {
                    self.renaming.truncate(initial_renaming_len);
                    return Err(error);
                }
            }
        }

        self.renaming.truncate(initial_renaming_len);
        Ok(terms.into())
    }
}

/// A partial renaming from a source environment to a target environment.
pub struct PartialRenaming {
    /// Mapping from local variables in the source environment to local
    /// variables in the target environment.
    source: UniqueEnv<Option<Level>>,
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

    fn next_local_var<'arena>(&self) -> ArcValue<'arena> {
        Spanned::empty(Arc::new(Value::local_var(self.source.len().next_level())))
    }

    /// Set a local source variable to local target variable mapping, ensuring
    /// that the variable appears uniquely.
    ///
    /// # Returns
    ///
    /// - `true` if the local binding was set successfully.
    /// - `false` if the local binding was already set.
    fn set_local(&mut self, source_var: Level) -> bool {
        let is_unique = self.get_as_level(source_var).is_none();

        if is_unique {
            let target_var = Some(self.target.next_level());
            self.source.set_level(source_var, target_var);
            self.target.push();
        }

        is_unique
    }

    /// Push an extra local binding onto the renaming.
    fn push_local(&mut self) {
        let target_var = self.target.next_level();
        self.source.push(Some(target_var));
        self.target.push();
    }

    /// Pop a local binding off the renaming.
    fn pop_local(&mut self) {
        self.source.pop();
        self.target.pop();
    }

    /// Get the local variable in the target environment that will be used in
    /// place of the `source_var`.
    fn get_as_level(&self, source_var: Level) -> Option<Level> {
        self.source.get_level(source_var).copied().flatten()
    }

    /// Rename a local variable in the source environment to a local variable in
    /// the target environment.
    fn get_as_index(&self, source_var: Level) -> Option<Index> {
        let target_var = self.get_as_level(source_var)?;
        Some(self.target.level_to_index(target_var).unwrap())
    }

    fn len(&self) -> (EnvLen, EnvLen) {
        (self.source.len(), self.target)
    }

    fn truncate(&mut self, len: (EnvLen, EnvLen)) {
        self.source.truncate(len.0);
        self.target.truncate(len.1);
    }
}
