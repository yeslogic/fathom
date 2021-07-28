//! The operational semantics of the core language, implemented using
//! [normalisation by evaluation](https://en.wikipedia.org/wiki/Normalisation_by_evaluation).

use std::sync::Arc;

use crate::core::{Arena, BindingMode, Term};
use crate::env::{EnvLen, GlobalVar, SharedEnv, SliceEnv};
use crate::StringId;

/// Values in weak-head-normal form.
#[derive(Debug, Clone)]
pub enum Value<'arena> {
    /// A value whose computation has stopped as a result of trying to
    /// [evaluate][`EvalContext::eval`] an open [term][`Term`]. Any eliminations
    /// that are subsequently applied to the value are accumulated in a spine.
    Stuck(Head, Vec<Elim<'arena>>),
    /// Universes.
    Universe,
    /// Dependent function types.
    FunType(Option<StringId>, Arc<Value<'arena>>, Closure<'arena>),
    /// Function introductions.
    FunIntro(Option<StringId>, Closure<'arena>),
    // RecordType(&'arena [StringId], Telescope<'arena>),
    // RecordIntro(&'arena [StringId], Vec<Arc<Value<'arena>>>),
}

impl<'arena> Value<'arena> {
    pub fn bound_var(global: GlobalVar) -> Value<'arena> {
        Value::Stuck(Head::BoundVar(global), Vec::new())
    }

    pub fn problem_var(global: GlobalVar) -> Value<'arena> {
        Value::Stuck(Head::ProblemVar(global), Vec::new())
    }

    pub fn reported_error() -> Value<'arena> {
        Value::Stuck(Head::ReportedError, Vec::new())
    }
}

/// The head of a [stuck value][`Value::Stuck`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Head {
    /// Variables that refer to binders.
    BoundVar(GlobalVar),
    /// Variables that refer to unification problems.
    ProblemVar(GlobalVar),
    /// Error sentinel.
    ReportedError,
}

/// A pending elimination to be reduced if the [head][`Head`] of a
/// [stuck value][`Value::Stuck`] becomes known.
#[derive(Debug, Clone)]
pub enum Elim<'arena> {
    /// Function eliminations.
    Fun(Arc<Value<'arena>>),
    // Record(StringId),
}

/// A closure is a term that can later be instantiated with a value.
#[derive(Debug, Clone)]
pub struct Closure<'arena> {
    bindings: SharedEnv<Arc<Value<'arena>>>,
    term: &'arena Term<'arena>,
}

impl<'arena> Closure<'arena> {
    pub fn new(
        bindings: SharedEnv<Arc<Value<'arena>>>,
        term: &'arena Term<'arena>,
    ) -> Closure<'arena> {
        Closure { bindings, term }
    }
}

/// Errors encountered while interpreting terms.
// TODO: include stack trace(??)
#[derive(Clone, Debug)]
pub enum Error {
    InvalidBoundVar,
    InvalidProblemVar,
    InvalidFunctionElimHead,
}

pub type Result<T> = std::result::Result<T, Error>;

/// Evaluation context.
pub struct EvalContext<'arena, 'env> {
    bindings: &'env mut SharedEnv<Arc<Value<'arena>>>,
    problems: &'env SliceEnv<Option<Arc<Value<'arena>>>>,
}

impl<'arena, 'env> EvalContext<'arena, 'env> {
    pub fn new(
        bindings: &'env mut SharedEnv<Arc<Value<'arena>>>,
        problems: &'env SliceEnv<Option<Arc<Value<'arena>>>>,
    ) -> EvalContext<'arena, 'env> {
        EvalContext { bindings, problems }
    }

    fn elim_context(&self) -> ElimContext<'arena, 'env> {
        ElimContext::new(self.problems)
    }

    fn close_term(&self, term: &'arena Term<'arena>) -> Closure<'arena> {
        Closure::new(self.bindings.clone(), term)
    }

    /// Fully normalise a term by first [evaluating][`EvalContext::eval] it into
    /// a [value][`Value`], then [reading it back][`ReadbackContext::redback`]
    /// into a [term][`Term`].
    pub fn normalise<'out_arena>(
        &mut self,
        arena: &'out_arena Arena<'out_arena>,
        term: &Term<'arena>,
    ) -> Result<Term<'out_arena>> {
        ReadbackContext::new(arena, self.bindings.len(), self.problems).readback(&self.eval(term)?)
    }

    /// Evaluate a [term][`Term`] into a [value][`Value`].
    pub fn eval(&mut self, term: &Term<'arena>) -> Result<Arc<Value<'arena>>> {
        match term {
            Term::BoundVar(var) => match self.bindings.get_local(*var) {
                Some(value) => Ok(value.clone()),
                None => Err(Error::InvalidBoundVar),
            },
            Term::ProblemVar(var) => match self.problems.get_global(*var) {
                Some(Some(value)) => Ok(value.clone()),
                Some(None) => Ok(Arc::new(Value::problem_var(*var))),
                None => Err(Error::InvalidProblemVar),
            },
            Term::InsertedProblem(var, bindings) => {
                let head_expr = self.eval(&Term::ProblemVar(*var))?;
                self.apply_assumptions(head_expr, bindings)
            }
            Term::Ann(expr, _) => self.eval(expr),
            Term::Let(_, def_expr, output_expr) => {
                let def_expr = self.eval(def_expr)?;
                self.bindings.push(def_expr);
                let output_expr = self.eval(output_expr);
                self.bindings.pop();
                output_expr
            }
            Term::Universe => Ok(Arc::new(Value::Universe)),
            Term::FunType(input_name, input_type, output_type) => Ok(Arc::new(Value::FunType(
                *input_name,
                self.eval(input_type)?,
                self.close_term(output_type),
            ))),
            Term::FunIntro(input_name, output_expr) => Ok(Arc::new(Value::FunIntro(
                *input_name,
                self.close_term(output_expr),
            ))),
            Term::FunElim(head_expr, input_expr) => {
                let head_expr = self.eval(head_expr)?;
                let input_expr = self.eval(input_expr)?;
                self.elim_context().fun_elim(head_expr, input_expr)
            }
            Term::ReportedError => Ok(Arc::new(Value::reported_error())),
        }
    }

    /// Apply the currently bound assumptions to a value, using an environment
    /// of binding modes from an [inserted problem][`Term::InsertedProblem`].
    fn apply_assumptions(
        &self,
        mut head_expr: Arc<Value<'arena>>,
        bindings: &SliceEnv<BindingMode>,
    ) -> Result<Arc<Value<'arena>>> {
        for (mode, expr) in Iterator::zip(bindings.iter(), self.bindings.iter()) {
            head_expr = match mode {
                BindingMode::Defined => head_expr,
                BindingMode::Assumed => self.elim_context().fun_elim(head_expr, expr.clone())?,
            };
        }
        Ok(head_expr)
    }
}

/// Elimination context.
///
/// This only requires a reference to the unification solutions,
/// as the bound expressions will be supplied by any closures that are
/// encountered during evaluation.
pub struct ElimContext<'arena, 'env> {
    problems: &'env SliceEnv<Option<Arc<Value<'arena>>>>,
}

impl<'arena, 'env> ElimContext<'arena, 'env> {
    pub fn new(problems: &'env SliceEnv<Option<Arc<Value<'arena>>>>) -> ElimContext<'arena, 'env> {
        ElimContext { problems }
    }

    /// Bring a value up-to-date with any new unification solutions that
    /// might now be present at the head of in the given value.
    pub fn force(&self, value: &Arc<Value<'arena>>) -> Result<Arc<Value<'arena>>> {
        match value.as_ref() {
            Value::Stuck(Head::ProblemVar(var), spine) => {
                // Check to see if a solution for this unification
                // variable was found since we last checked.
                match self.problems.get_global(*var) {
                    Some(Some(value)) => {
                        // Apply the spine to the updated head value.
                        let value = self.spine_elim(value.clone(), spine)?;
                        // The result of the spine application might also have a
                        // solved unification problem at its head, so force that
                        // too while we're at it!
                        self.force(&value)
                    }
                    Some(None) => Ok(value.clone()),
                    None => Err(Error::InvalidProblemVar),
                }
            }
            _ => Ok(value.clone()),
        }
    }

    /// Apply a closure to a value.
    pub fn closure_elim(
        &self,
        closure: &Closure<'arena>,
        value: Arc<Value<'arena>>,
    ) -> Result<Arc<Value<'arena>>> {
        EvalContext::new(&mut closure.bindings.push_clone(value), self.problems).eval(closure.term)
    }

    /// Apply a function elimination to an expression, performing
    /// [beta-reduction] if possible.
    ///
    /// [beta-reduction]: https://ncatlab.org/nlab/show/beta-reduction
    pub fn fun_elim(
        &self,
        mut head_expr: Arc<Value<'arena>>,
        input_expr: Arc<Value<'arena>>,
    ) -> Result<Arc<Value<'arena>>> {
        match Arc::make_mut(&mut head_expr) {
            // Beta-reduction
            Value::FunIntro(_, output_expr) => self.closure_elim(output_expr, input_expr),
            // The computation is stuck, preventing further reduction
            Value::Stuck(_, spine) => {
                spine.push(Elim::Fun(input_expr));
                Ok(head_expr)
            }
            _ => Err(Error::InvalidFunctionElimHead),
        }
    }

    /// Apply an expression to an elimination spine.
    pub fn spine_elim(
        &self,
        mut head_expr: Arc<Value<'arena>>,
        spine: &[Elim<'arena>],
    ) -> Result<Arc<Value<'arena>>> {
        for elim in spine {
            head_expr = match elim {
                Elim::Fun(input_expr) => self.fun_elim(head_expr, input_expr.clone())?,
            };
        }
        Ok(head_expr)
    }
}

/// Readback context.
pub struct ReadbackContext<'in_arena, 'out_arena, 'env> {
    arena: &'out_arena Arena<'out_arena>,
    bindings: EnvLen,
    problems: &'env SliceEnv<Option<Arc<Value<'in_arena>>>>,
}

impl<'in_arena, 'out_arena, 'env> ReadbackContext<'in_arena, 'out_arena, 'env> {
    pub fn new(
        arena: &'out_arena Arena<'out_arena>,
        bindings: EnvLen,
        problems: &'env SliceEnv<Option<Arc<Value<'in_arena>>>>,
    ) -> ReadbackContext<'in_arena, 'out_arena, 'env> {
        ReadbackContext {
            arena,
            bindings,
            problems,
        }
    }

    fn elim_context(&self) -> ElimContext<'in_arena, 'env> {
        ElimContext::new(self.problems)
    }

    fn push_binding(&mut self) {
        self.bindings.push();
    }

    fn pop_binding(&mut self) {
        self.bindings.pop();
    }

    /// Read a [value][`Value`] back into a [term][`Term`].
    pub fn readback(&mut self, value: &Arc<Value<'in_arena>>) -> Result<Term<'out_arena>> {
        match self.elim_context().force(value)?.as_ref() {
            Value::Stuck(head, spine) => {
                let mut head_expr = match head {
                    Head::BoundVar(var) => {
                        // FIXME: Unwrap
                        Term::BoundVar(self.bindings.global_to_local(*var).unwrap())
                    }
                    Head::ProblemVar(var) => Term::ProblemVar(*var),
                    Head::ReportedError => Term::ReportedError,
                };

                for elim in spine {
                    head_expr = match elim {
                        Elim::Fun(input_expr) => {
                            let input_expr = self.readback(input_expr)?;
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
                let input_type = self.readback(input_type)?;
                let output_type = self.readback_closure(output_type)?;

                Ok(Term::FunType(
                    *input_name,
                    self.arena.alloc_term(input_type),
                    self.arena.alloc_term(output_type),
                ))
            }
            Value::FunIntro(input_name, output_expr) => {
                let output_expr = self.readback_closure(output_expr)?;

                Ok(Term::FunIntro(
                    *input_name,
                    self.arena.alloc_term(output_expr),
                ))
            }
        }
    }

    /// Read a [closure][`Closure`] back into a [term][`Term`].
    fn readback_closure(&mut self, closure: &Closure<'in_arena>) -> Result<Term<'out_arena>> {
        let var = Arc::new(Value::bound_var(self.bindings.next_global()));
        let value = self.elim_context().closure_elim(closure, var)?;

        self.push_binding();
        let term = self.readback(&value);
        self.pop_binding();

        term
    }
}

/// Conversion context.
pub struct ConversionContext<'arena, 'env> {
    bindings: EnvLen,
    problems: &'env SliceEnv<Option<Arc<Value<'arena>>>>,
}

impl<'arena, 'env> ConversionContext<'arena, 'env> {
    pub fn new(
        bindings: EnvLen,
        problems: &'env SliceEnv<Option<Arc<Value<'arena>>>>,
    ) -> ConversionContext<'arena, 'env> {
        ConversionContext { bindings, problems }
    }

    fn elim_context(&self) -> ElimContext<'arena, 'env> {
        ElimContext::new(self.problems)
    }

    fn push_binding(&mut self) {
        self.bindings.push();
    }

    fn pop_binding(&mut self) {
        self.bindings.pop();
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
    pub fn is_equal(&mut self, value0: &Arc<Value<'_>>, value1: &Arc<Value<'_>>) -> Result<bool> {
        let value0 = self.elim_context().force(value0)?;
        let value1 = self.elim_context().force(value1)?;

        match (value0.as_ref(), value1.as_ref()) {
            // `ReportedError`s result from errors that have already been
            // reported, so we say that they are equal to any other value to
            // prevent them from triggering more errors.
            (Value::Stuck(Head::ReportedError, _), _)
            | (_, Value::Stuck(Head::ReportedError, _)) => Ok(true),

            (Value::Stuck(head0, spine0), Value::Stuck(head1, spine1)) => {
                if head0 != head1 || spine0.len() != spine1.len() {
                    return Ok(false);
                }
                for (elim0, elim1) in Iterator::zip(spine0.iter(), spine1.iter()) {
                    match (elim0, elim1) {
                        (Elim::Fun(input_expr0), Elim::Fun(input_expr1))
                            if self.is_equal(input_expr0, input_expr1)? => {}
                        (_, _) => return Ok(false),
                    }
                }
                Ok(true)
            }
            (Value::Universe, Value::Universe) => Ok(true),

            (
                Value::FunType(_, input_type0, output_type0),
                Value::FunType(_, input_type1, output_type1),
            ) => Ok(self.is_equal(input_type0, input_type1)?
                && self.is_equal_closures(output_type0, output_type1)?),

            (Value::FunIntro(_, output_expr0), Value::FunIntro(_, output_expr1)) => {
                self.is_equal_closures(output_expr0, output_expr1)
            }
            (Value::FunIntro(_, output_expr), _) => self.is_equal_fun_intro(output_expr, &value1),
            (_, Value::FunIntro(_, output_expr)) => self.is_equal_fun_intro(output_expr, &value0),

            (_, _) => Ok(false),
        }
    }

    /// Check that two [closures][`Closure`] are equal.
    pub fn is_equal_closures(
        &mut self,
        closure0: &Closure<'_>,
        closure1: &Closure<'_>,
    ) -> Result<bool> {
        let var = Arc::new(Value::bound_var(self.bindings.next_global()));
        let value0 = self.elim_context().closure_elim(closure0, var.clone())?;
        let value1 = self.elim_context().closure_elim(closure1, var)?;

        self.push_binding();
        let result = self.is_equal(&value0, &value1);
        self.pop_binding();

        result
    }

    /// Check that a closure is equal to a value, using function eta-conversion.
    fn is_equal_fun_intro(
        &mut self,
        output_expr: &Closure<'_>,
        value: &Arc<Value<'_>>,
    ) -> Result<bool> {
        let var = Arc::new(Value::bound_var(self.bindings.next_global()));
        let output_expr = self.elim_context().closure_elim(output_expr, var.clone())?;
        let value = self.elim_context().fun_elim(value.clone(), var)?;

        self.push_binding();
        let result = self.is_equal(&output_expr, &value);
        self.pop_binding();

        result
    }
}
