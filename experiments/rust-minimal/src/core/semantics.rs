//! The operational semantics of the core language, implemented using
//! [normalisation by evaluation](https://en.wikipedia.org/wiki/Normalisation_by_evaluation).

use scoped_arena::Scope;
use std::panic::panic_any;
use std::sync::Arc;

use crate::core::{EntryInfo, Term};
use crate::env::{EnvLen, GlobalVar, SharedEnv, SliceEnv};
use crate::{SliceBuilder, StringId};

/// Atomically reference counted values. We use reference counting to increase
/// the amount of sharing we can achieve during evaluation.
pub type ArcValue<'arena> = Arc<Value<'arena>>;

/// Values in weak-head-normal form, with bindings converted to closures.
#[derive(Debug, Clone)]
pub enum Value<'arena> {
    /// A value whose computation has been blocked as a result of trying to
    /// [evaluate][EvalContext::eval] an open [term][Term], along with a spine
    /// of eliminations. Subsequent eliminations applied to this value are
    /// accumulated in the spine.
    Stuck(Head, Vec<Elim<'arena>>),
    /// Universes.
    Universe,
    /// Dependent function types.
    FunType(Option<StringId>, ArcValue<'arena>, Closure<'arena>),
    /// Function introductions.
    FunIntro(Option<StringId>, Closure<'arena>),
    /// Record types.
    RecordType(&'arena [StringId], Telescope<'arena>),
    /// Record introductions.
    RecordIntro(&'arena [StringId], Vec<ArcValue<'arena>>),
}

impl<'arena> Value<'arena> {
    pub fn rigid_var(global: GlobalVar) -> Value<'arena> {
        Value::Stuck(Head::RigidVar(global), Vec::new())
    }

    pub fn flexible_var(global: GlobalVar) -> Value<'arena> {
        Value::Stuck(Head::FlexibleVar(global), Vec::new())
    }

    pub fn reported_error() -> Value<'arena> {
        Value::Stuck(Head::ReportedError, Vec::new())
    }
}

/// The head of a [stuck value][Value::Stuck].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Head {
    /// Variables that refer to rigid binders.
    RigidVar(GlobalVar),
    /// Variables that refer to unsolved flexible problems.
    FlexibleVar(GlobalVar), // TODO: Use a RefCell here?
    /// Error sentinel.
    ReportedError,
}

/// A pending elimination to be reduced if the [head][Head] of a [stuck
/// value][Value::Stuck] becomes known.
#[derive(Debug, Clone)]
pub enum Elim<'arena> {
    /// Function eliminations.
    Fun(ArcValue<'arena>),
    /// Record eliminations.
    Record(StringId),
}

/// A closure is a term that can later be instantiated with a value.
#[derive(Debug, Clone)]
pub struct Closure<'arena> {
    /// Rigid environment where the closed [term] is bound. A new entry will
    /// need to be pushed to this environment before evaluating the term.
    rigid_exprs: SharedEnv<ArcValue<'arena>>,
    /// The term that is closed over.
    term: &'arena Term<'arena>,
}

impl<'arena> Closure<'arena> {
    pub fn new(
        rigid_exprs: SharedEnv<ArcValue<'arena>>,
        term: &'arena Term<'arena>,
    ) -> Closure<'arena> {
        Closure { rigid_exprs, term }
    }
}

/// A series of terms where each term might depend on previous terms.
#[derive(Debug, Clone)]
pub struct Telescope<'arena> {
    /// Rigid environment where the telescope's [terms] are bound.
    rigid_exprs: SharedEnv<ArcValue<'arena>>,
    /// The terms in the telescope.
    terms: &'arena [Term<'arena>],
}

impl<'arena> Telescope<'arena> {
    pub fn new(
        rigid_exprs: SharedEnv<ArcValue<'arena>>,
        terms: &'arena [Term<'arena>],
    ) -> Telescope<'arena> {
        Telescope { rigid_exprs, terms }
    }

    /// The number of terms in the telescope.
    pub fn len(&self) -> usize {
        self.terms.len()
    }
}

#[derive(Debug, Clone)]
pub struct NextTelescope<'arena> {
    /// Rigid environment  where the telescope's [terms] are bound. A new entry
    /// will need to be pushed to this environment before evaluating the next
    /// term in the telescope.
    rigid_exprs: SharedEnv<ArcValue<'arena>>,
    /// The terms in the telescope.
    terms: &'arena [Term<'arena>],
}

impl<'arena> NextTelescope<'arena> {
    /// Resume the telescope with a value that corresponds to the previous entry
    /// in the telescope.
    pub fn resume(mut self, previous_value: ArcValue<'arena>) -> Telescope<'arena> {
        self.rigid_exprs.push(previous_value);
        Telescope::new(self.rigid_exprs, self.terms)
    }
}

/// Errors encountered while interpreting terms.
// TODO: include stack trace(??)
#[derive(Clone, Debug)]
pub enum Error {
    InvalidRigidVar,
    InvalidFlexibleVar,
    InvalidFunctionElim,
    InvalidRecordElim,
}

impl Error {
    pub fn description(&self) -> &str {
        match &self {
            Error::InvalidRigidVar => "invalid rigid variable",
            Error::InvalidFlexibleVar => "invalid flexible variable",
            Error::InvalidFunctionElim => "invalid function elim",
            Error::InvalidRecordElim => "invalid record elim",
        }
    }
}

/// Evaluation context.
///
/// Like the [`ElimContext`], this allows for the running of computations, but
/// also maintains a rigid environment, allowing for evaluation.
pub struct EvalContext<'arena, 'env> {
    rigid_exprs: &'env mut SharedEnv<ArcValue<'arena>>,
    flexible_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>,
}

impl<'arena, 'env> EvalContext<'arena, 'env> {
    pub fn new(
        rigid_exprs: &'env mut SharedEnv<ArcValue<'arena>>,
        flexible_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>,
    ) -> EvalContext<'arena, 'env> {
        EvalContext {
            rigid_exprs,
            flexible_exprs,
        }
    }

    fn elim_context(&self) -> ElimContext<'arena, 'env> {
        ElimContext::new(self.flexible_exprs)
    }

    fn close_term(&self, term: &'arena Term<'arena>) -> Closure<'arena> {
        Closure::new(self.rigid_exprs.clone(), term)
    }

    /// Fully normalise a term by first [evaluating][EvalContext::eval] it into
    /// a [value][Value], then [quoting it back][QuoteContext::quote] into a
    /// [term][Term].
    pub fn normalise<'out_arena>(
        &mut self,
        scope: &'out_arena Scope<'out_arena>,
        term: &Term<'arena>,
    ) -> Term<'out_arena> {
        QuoteContext::new(scope, self.rigid_exprs.len(), self.flexible_exprs)
            .quote(&self.eval(term))
    }

    /// Evaluate a [term][Term] into a [value][Value].
    ///
    /// This could be loosely thought of as a just-in-time implementation of
    /// closure conversion + partial evaluation (for more discussion see [this
    /// twitter thread](https://twitter.com/brendanzab/status/1423536653658771457)).
    pub fn eval(&mut self, term: &Term<'arena>) -> ArcValue<'arena> {
        match term {
            Term::RigidVar(var) => match self.rigid_exprs.get_local(*var) {
                Some(value) => value.clone(),
                None => panic_any(Error::InvalidRigidVar),
            },
            Term::FlexibleVar(var) => match self.flexible_exprs.get_global(*var) {
                Some(Some(value)) => value.clone(),
                Some(None) => Arc::new(Value::flexible_var(*var)),
                None => panic_any(Error::InvalidFlexibleVar),
            },
            Term::FlexibleInsertion(var, rigid_infos) => {
                let mut head_expr = self.eval(&Term::FlexibleVar(*var));
                for (info, expr) in Iterator::zip(rigid_infos.iter(), self.rigid_exprs.iter()) {
                    head_expr = match info {
                        EntryInfo::Concrete => head_expr,
                        EntryInfo::Abstract => {
                            self.elim_context().apply_fun(head_expr, expr.clone())
                        }
                    };
                }
                head_expr
            }
            Term::Ann(expr, _) => self.eval(expr),
            Term::Let(_, def_expr, output_expr) => {
                let def_expr = self.eval(def_expr);
                self.rigid_exprs.push(def_expr);
                let output_expr = self.eval(output_expr);
                self.rigid_exprs.pop();
                output_expr
            }
            Term::Universe => Arc::new(Value::Universe),
            Term::FunType(input_name, input_type, output_type) => Arc::new(Value::FunType(
                *input_name,
                self.eval(input_type),
                self.close_term(output_type),
            )),
            Term::FunIntro(input_name, output_expr) => {
                Arc::new(Value::FunIntro(*input_name, self.close_term(output_expr)))
            }
            Term::FunElim(head_expr, input_expr) => {
                let head_expr = self.eval(head_expr);
                let input_expr = self.eval(input_expr);
                self.elim_context().apply_fun(head_expr, input_expr)
            }
            Term::RecordType(labels, types) => {
                let types = Telescope::new(self.rigid_exprs.clone(), types);

                Arc::new(Value::RecordType(labels, types))
            }
            Term::RecordIntro(labels, exprs) => {
                let exprs = exprs.iter().map(|expr| self.eval(expr)).collect();

                Arc::new(Value::RecordIntro(labels, exprs))
            }
            Term::RecordElim(head_expr, label) => {
                let head_expr = self.eval(head_expr);
                self.elim_context().apply_record(head_expr, *label)
            }
            Term::ReportedError => Arc::new(Value::reported_error()),
        }
    }
}

/// Elimination context.
///
/// Contains enough state to run computations, but does not contain a rigid
/// environment that would be needed for full evaluation.
pub struct ElimContext<'arena, 'env> {
    flexible_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>,
}

impl<'arena, 'env> ElimContext<'arena, 'env> {
    pub fn new(
        flexible_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>,
    ) -> ElimContext<'arena, 'env> {
        ElimContext { flexible_exprs }
    }

    /// Bring a value up-to-date with any new unification solutions that
    /// might now be present at the head of in the given value.
    pub fn force(&self, value: &ArcValue<'arena>) -> ArcValue<'arena> {
        let mut forced_value = value.clone();
        // Attempt to force flexible values until we don't see any more.
        while let Value::Stuck(Head::FlexibleVar(var), spine) = forced_value.as_ref() {
            match self.flexible_exprs.get_global(*var) {
                // Apply the spine to the solution. This might uncover another
                // flexible value so we'll continue looping.
                Some(Some(expr)) => forced_value = self.apply_spine(expr.clone(), spine),
                // There's no solution for this flexible variable yet, meaning
                // that we've forced the value as much as possible for now
                Some(None) => break,
                None => panic_any(Error::InvalidFlexibleVar),
            }
        }
        forced_value
    }

    /// Apply a closure to a value.
    pub fn apply_closure(
        &self,
        closure: &Closure<'arena>,
        value: ArcValue<'arena>,
    ) -> ArcValue<'arena> {
        let mut rigid_exprs = closure.rigid_exprs.clone();
        rigid_exprs.push(value);
        EvalContext::new(&mut rigid_exprs, self.flexible_exprs).eval(closure.term)
    }

    /// Split a telescope into the first value, and the next telescope
    /// containing the rest of the values.
    pub fn split_telescope(
        &self,
        telescope: Telescope<'arena>,
    ) -> Option<(ArcValue<'arena>, NextTelescope<'arena>)> {
        let (term, terms) = telescope.terms.split_first()?;
        let mut rigid_exprs = telescope.rigid_exprs;
        let value = EvalContext::new(&mut rigid_exprs, self.flexible_exprs).eval(term);
        Some((value, NextTelescope { rigid_exprs, terms }))
    }

    /// Apply a function elimination to an expression, performing
    /// [beta-reduction] if possible.
    ///
    /// [beta-reduction]: https://ncatlab.org/nlab/show/beta-reduction
    pub fn apply_fun(
        &self,
        mut head_expr: ArcValue<'arena>,
        input_expr: ArcValue<'arena>,
    ) -> ArcValue<'arena> {
        match Arc::make_mut(&mut head_expr) {
            // Beta-reduction
            Value::FunIntro(_, output_expr) => self.apply_closure(output_expr, input_expr),
            // The computation is stuck, preventing further reduction
            Value::Stuck(_, spine) => {
                spine.push(Elim::Fun(input_expr));
                head_expr
            }
            _ => panic_any(Error::InvalidFunctionElim),
        }
    }

    /// Apply a record elimination to an expression, performing
    /// [beta-reduction] if possible.
    ///
    /// [beta-reduction]: https://ncatlab.org/nlab/show/beta-reduction
    pub fn apply_record(
        &self,
        mut head_expr: ArcValue<'arena>,
        label: StringId,
    ) -> ArcValue<'arena> {
        match Arc::make_mut(&mut head_expr) {
            // Beta-reduction
            Value::RecordIntro(labels, exprs) => (labels.iter())
                .position(|current_label| *current_label == label)
                .and_then(|expr_index| exprs.get(expr_index).cloned())
                .unwrap_or_else(|| panic_any(Error::InvalidRecordElim)),
            // The computation is stuck, preventing further reduction
            Value::Stuck(_, spine) => {
                spine.push(Elim::Record(label));
                head_expr
            }
            _ => panic_any(Error::InvalidRecordElim),
        }
    }

    /// Apply an expression to an elimination spine.
    pub fn apply_spine(
        &self,
        mut head_expr: ArcValue<'arena>,
        spine: &[Elim<'arena>],
    ) -> ArcValue<'arena> {
        for elim in spine {
            head_expr = match elim {
                Elim::Fun(input_expr) => self.apply_fun(head_expr, input_expr.clone()),
                Elim::Record(label) => self.apply_record(head_expr, *label),
            };
        }
        head_expr
    }
}

/// Quotation context.
///
/// This context keeps track of the length of the environment, allowing for
/// quotation.
#[derive(Clone)]
pub struct QuoteContext<'in_arena, 'out_arena, 'env> {
    scope: &'out_arena Scope<'out_arena>,
    rigid_exprs: EnvLen,
    flexible_exprs: &'env SliceEnv<Option<ArcValue<'in_arena>>>,
}

impl<'in_arena, 'out_arena, 'env> QuoteContext<'in_arena, 'out_arena, 'env> {
    pub fn new(
        scope: &'out_arena Scope<'out_arena>,
        rigid_exprs: EnvLen,
        flexible_exprs: &'env SliceEnv<Option<ArcValue<'in_arena>>>,
    ) -> QuoteContext<'in_arena, 'out_arena, 'env> {
        QuoteContext {
            scope,
            rigid_exprs,
            flexible_exprs,
        }
    }

    fn elim_context<'this>(&'this self) -> ElimContext<'in_arena, 'env> {
        ElimContext::new(self.flexible_exprs)
    }

    fn push_rigid(&mut self) {
        self.rigid_exprs.push();
    }

    fn pop_rigid(&mut self) {
        self.rigid_exprs.pop();
    }

    /// Quote a [value][Value] back into a [term][Term].
    pub fn quote(&mut self, value: &ArcValue<'in_arena>) -> Term<'out_arena> {
        let value = self.elim_context().force(value);
        match value.as_ref() {
            Value::Stuck(head, spine) => {
                let mut head_expr = match head {
                    Head::RigidVar(var) => {
                        // FIXME: Unwrap
                        Term::RigidVar(self.rigid_exprs.global_to_local(*var).unwrap())
                    }
                    Head::FlexibleVar(var) => Term::FlexibleVar(*var),
                    Head::ReportedError => Term::ReportedError,
                };

                for elim in spine {
                    head_expr = match elim {
                        Elim::Fun(input_expr) => {
                            let input_expr = self.quote(input_expr);
                            Term::FunElim(
                                self.scope.to_scope(head_expr),
                                self.scope.to_scope(input_expr),
                            )
                        }
                        Elim::Record(label) => {
                            Term::RecordElim(self.scope.to_scope(head_expr), *label)
                        }
                    };
                }

                head_expr
            }
            Value::Universe => Term::Universe,
            Value::FunType(input_name, input_type, output_type) => {
                let input_type = self.quote(input_type);
                let output_type = self.quote_closure(output_type);

                Term::FunType(
                    *input_name,
                    self.scope.to_scope(input_type),
                    self.scope.to_scope(output_type),
                )
            }
            Value::FunIntro(input_name, output_expr) => {
                let output_expr = self.quote_closure(output_expr);

                Term::FunIntro(*input_name, self.scope.to_scope(output_expr))
            }
            Value::RecordType(labels, types) => {
                let labels = self.scope.to_scope_from_iter(labels.iter().copied()); // FIXME: avoid copy if this is the same arena?
                let types = self.quote_telescope(types);

                Term::RecordType(labels, types)
            }
            Value::RecordIntro(labels, exprs) => {
                let labels = self.scope.to_scope_from_iter(labels.iter().copied()); // FIXME: avoid copy if this is the same arena?
                let exprs =
                    (self.scope).to_scope_from_iter(exprs.iter().map(|expr| self.quote(expr)));

                Term::RecordIntro(labels, exprs)
            }
        }
    }

    /// Quote a [closure][Closure] back into a [term][Term].
    fn quote_closure(&mut self, closure: &Closure<'in_arena>) -> Term<'out_arena> {
        let var = Arc::new(Value::rigid_var(self.rigid_exprs.next_global()));
        let value = self.elim_context().apply_closure(closure, var);

        self.push_rigid();
        let term = self.quote(&value);
        self.pop_rigid();

        term
    }

    /// Quote a [telescope][Telescope] back into a slice of [terms][Term].
    fn quote_telescope(
        &mut self,
        telescope: &Telescope<'in_arena>,
    ) -> &'out_arena mut [Term<'out_arena>] {
        let initial_rigid_len = self.rigid_exprs;
        let mut telescope = telescope.clone();
        let mut terms = SliceBuilder::new(self.scope, telescope.len(), Term::ReportedError);

        while let Some((value, next_telescope)) = self.elim_context().split_telescope(telescope) {
            let var = Arc::new(Value::rigid_var(self.rigid_exprs.next_global()));
            telescope = next_telescope.resume(var);
            terms.push(self.quote(&value));
            self.rigid_exprs.push();
        }

        self.rigid_exprs.truncate(initial_rigid_len);
        terms.into()
    }
}

/// Conversion context.
///
/// This context keeps track of the length of the environment, for use in
/// conversion checking.
pub struct ConversionContext<'arena, 'env> {
    rigid_exprs: EnvLen,
    flexible_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>,
}

impl<'arena, 'env> ConversionContext<'arena, 'env> {
    pub fn new(
        rigid_exprs: EnvLen,
        flexible_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>,
    ) -> ConversionContext<'arena, 'env> {
        ConversionContext {
            rigid_exprs,
            flexible_exprs,
        }
    }

    fn elim_context(&self) -> ElimContext<'arena, 'env> {
        ElimContext::new(self.flexible_exprs)
    }

    fn push_rigid(&mut self) {
        self.rigid_exprs.push();
    }

    fn pop_rigid(&mut self) {
        self.rigid_exprs.pop();
    }

    /// Check that one value is [computationally equal] to another value.
    ///
    /// This is sometimes referred to as 'conversion checking', or checking
    /// for 'definitional equality'.
    ///
    /// We perform [eta-conversion] here, if possible.
    ///
    /// [computationally equal]: https://ncatlab.org/nlab/show/equality#computational_equality
    /// [eta-conversion]: https://ncatlab.org/nlab/show/eta-conversion
    pub fn is_equal(&mut self, value0: &ArcValue<'_>, value1: &ArcValue<'_>) -> bool {
        let value0 = self.elim_context().force(value0);
        let value1 = self.elim_context().force(value1);

        match (value0.as_ref(), value1.as_ref()) {
            // `ReportedError`s result from errors that have already been
            // reported, so we prevent them from triggering more errors.
            (Value::Stuck(Head::ReportedError, _), _)
            | (_, Value::Stuck(Head::ReportedError, _)) => true,

            (Value::Stuck(head0, spine0), Value::Stuck(head1, spine1)) => {
                if head0 != head1 || spine0.len() != spine1.len() {
                    return false;
                }
                for (elim0, elim1) in Iterator::zip(spine0.iter(), spine1.iter()) {
                    match (elim0, elim1) {
                        (Elim::Fun(input_expr0), Elim::Fun(input_expr1))
                            if self.is_equal(input_expr0, input_expr1) => {}
                        (_, _) => return false,
                    }
                }
                true
            }
            (Value::Universe, Value::Universe) => true,

            (
                Value::FunType(_, input_type0, output_type0),
                Value::FunType(_, input_type1, output_type1),
            ) => {
                self.is_equal(input_type0, input_type1)
                    && self.is_equal_closures(output_type0, output_type1)
            }

            (Value::FunIntro(_, output_expr0), Value::FunIntro(_, output_expr1)) => {
                self.is_equal_closures(output_expr0, output_expr1)
            }
            // Eta-conversion
            (Value::FunIntro(_, output_expr), _) => {
                self.is_equal_fun_intro_elim(output_expr, &value1)
            }
            (_, Value::FunIntro(_, output_expr)) => {
                self.is_equal_fun_intro_elim(output_expr, &value0)
            }

            (Value::RecordType(labels0, types0), Value::RecordType(labels1, types1)) => {
                if labels0 != labels1 {
                    return false;
                }
                self.is_equal_telescopes(types0, types1)
            }

            (Value::RecordIntro(labels0, exprs0), Value::RecordIntro(labels1, exprs1)) => {
                if labels0 != labels1 {
                    return false;
                }
                for (expr0, expr1) in Iterator::zip(exprs0.iter(), exprs1.iter()) {
                    if !self.is_equal(&expr0, &expr1) {
                        return false;
                    }
                }
                true
            }
            // Eta-conversion
            (Value::RecordIntro(labels, exprs), _) => {
                self.is_equal_record_intro_elim(labels, exprs, &value1)
            }
            (_, Value::RecordIntro(labels, exprs)) => {
                self.is_equal_record_intro_elim(labels, exprs, &value0)
            }

            (_, _) => false,
        }
    }

    /// Check that two [closures][Closure] are equal.
    pub fn is_equal_closures(&mut self, closure0: &Closure<'_>, closure1: &Closure<'_>) -> bool {
        let var = Arc::new(Value::rigid_var(self.rigid_exprs.next_global()));
        let value0 = self.elim_context().apply_closure(closure0, var.clone());
        let value1 = self.elim_context().apply_closure(closure1, var);

        self.push_rigid();
        let result = self.is_equal(&value0, &value1);
        self.pop_rigid();

        result
    }

    /// Check that two [telescopes][Telescope] are equal.
    pub fn is_equal_telescopes(
        &mut self,
        telescope0: &Telescope<'_>,
        telescope1: &Telescope<'_>,
    ) -> bool {
        if telescope0.len() != telescope1.len() {
            return false;
        }

        let initial_rigid_len = self.rigid_exprs;
        let mut telescope0 = telescope0.clone();
        let mut telescope1 = telescope1.clone();

        while let Some(((value0, next_telescope0), (value1, next_telescope1))) = Option::zip(
            self.elim_context().split_telescope(telescope0),
            self.elim_context().split_telescope(telescope1),
        ) {
            if !self.is_equal(&value0, &value1) {
                self.rigid_exprs.truncate(initial_rigid_len);
                return false;
            }

            let var = Arc::new(Value::rigid_var(self.rigid_exprs.next_global()));
            telescope0 = next_telescope0.resume(var.clone());
            telescope1 = next_telescope1.resume(var);
            self.rigid_exprs.push();
        }

        self.rigid_exprs.truncate(initial_rigid_len);
        true
    }

    /// Check that a function is equal to a value, using eta-conversion.
    fn is_equal_fun_intro_elim(&mut self, output_expr: &Closure<'_>, value: &ArcValue<'_>) -> bool {
        let var = Arc::new(Value::rigid_var(self.rigid_exprs.next_global()));
        let value = self.elim_context().apply_fun(value.clone(), var.clone());
        let output_expr = self.elim_context().apply_closure(output_expr, var);

        self.push_rigid();
        let result = self.is_equal(&output_expr, &value);
        self.pop_rigid();

        result
    }

    /// Check that a record is equal to a value, using eta-conversion.
    fn is_equal_record_intro_elim(
        &mut self,
        labels: &[StringId],
        exprs: &[ArcValue<'_>],
        value: &ArcValue<'_>,
    ) -> bool {
        for (label, expr) in Iterator::zip(labels.iter(), exprs.iter()) {
            let field_value = self.elim_context().apply_record(value.clone(), *label);
            if !self.is_equal(expr, &field_value) {
                return false;
            }
        }
        true
    }
}
