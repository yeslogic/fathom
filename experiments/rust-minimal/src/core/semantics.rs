//! The operational semantics of the core language, implemented using
//! [normalisation by evaluation](https://en.wikipedia.org/wiki/Normalisation_by_evaluation).

use std::sync::Arc;

use crate::core::{Arena, EntryInfo, Term};
use crate::env::{EnvLen, GlobalVar, SharedEnv, SliceEnv};
use crate::StringId;

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
    FunType(Option<StringId>, Arc<Value<'arena>>, Closure<'arena>),
    /// Function introductions.
    FunIntro(Option<StringId>, Closure<'arena>),
    // RecordType(&'arena [StringId], Telescope<'arena>),
    // RecordIntro(&'arena [StringId], Vec<Arc<Value<'arena>>>),
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
    /// Variables that refer to binders.
    RigidVar(GlobalVar),
    /// Variables that refer to unification flexible_exprs.
    FlexibleVar(GlobalVar),
    /// Error sentinel.
    ReportedError,
}

/// A pending elimination to be reduced if the [head][Head] of a [stuck
/// value][Value::Stuck] becomes known.
#[derive(Debug, Clone)]
pub enum Elim<'arena> {
    /// Function eliminations.
    Fun(Arc<Value<'arena>>),
    // Record(StringId),
}

/// A closure is a term that can later be instantiated with a value.
#[derive(Debug, Clone)]
pub struct Closure<'arena> {
    rigid_exprs: SharedEnv<Arc<Value<'arena>>>,
    term: &'arena Term<'arena>,
}

impl<'arena> Closure<'arena> {
    pub fn new(
        rigid_exprs: SharedEnv<Arc<Value<'arena>>>,
        term: &'arena Term<'arena>,
    ) -> Closure<'arena> {
        Closure { rigid_exprs, term }
    }
}

/// Errors encountered while interpreting terms.
// TODO: include stack trace(??)
#[derive(Clone, Debug)]
pub enum Error {
    InvalidRigidVar,
    InvalidFlexibleVar,
    InvalidFunctionElimHead,
}

pub type Result<T> = std::result::Result<T, Error>;

/// Evaluation context.
pub struct EvalContext<'arena, 'env> {
    rigid_exprs: &'env mut SharedEnv<Arc<Value<'arena>>>,
    flexible_exprs: &'env SliceEnv<Option<Arc<Value<'arena>>>>,
}

impl<'arena, 'env> EvalContext<'arena, 'env> {
    pub fn new(
        rigid_exprs: &'env mut SharedEnv<Arc<Value<'arena>>>,
        flexible_exprs: &'env SliceEnv<Option<Arc<Value<'arena>>>>,
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
        arena: &'out_arena Arena<'out_arena>,
        term: &Term<'arena>,
    ) -> Result<Term<'out_arena>> {
        QuoteContext::new(arena, self.rigid_exprs.len(), self.flexible_exprs)
            .quote(&self.eval(term)?)
    }

    /// Evaluate a [term][Term] into a [value][Value].
    ///
    /// This could be loosely thought of as a just-in-time implementation of
    /// closure conversion + partial evaluation (for more discussion see [this
    /// twitter thread](https://twitter.com/brendanzab/status/1423536653658771457)).
    pub fn eval(&mut self, term: &Term<'arena>) -> Result<Arc<Value<'arena>>> {
        match term {
            Term::RigidVar(var) => match self.rigid_exprs.get_local(*var) {
                Some(value) => Ok(value.clone()),
                None => Err(Error::InvalidRigidVar),
            },
            Term::FlexibleVar(var) => match self.flexible_exprs.get_global(*var) {
                Some(Some(value)) => Ok(value.clone()),
                Some(None) => Ok(Arc::new(Value::flexible_var(*var))),
                None => Err(Error::InvalidFlexibleVar),
            },
            Term::FlexibleInsertion(var, rigid_infos) => {
                let mut head_expr = self.eval(&Term::FlexibleVar(*var))?;
                for (infor, expr) in Iterator::zip(rigid_infos.iter(), self.rigid_exprs.iter()) {
                    head_expr = match infor {
                        EntryInfo::Concrete => head_expr,
                        EntryInfo::Abstract => {
                            self.elim_context().apply_fun(head_expr, expr.clone())?
                        }
                    };
                }
                Ok(head_expr)
            }
            Term::Ann(expr, _) => self.eval(expr),
            Term::Let(_, def_expr, output_expr) => {
                let def_expr = self.eval(def_expr)?;
                self.rigid_exprs.push(def_expr);
                let output_expr = self.eval(output_expr);
                self.rigid_exprs.pop();
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
                self.elim_context().apply_fun(head_expr, input_expr)
            }
            Term::ReportedError => Ok(Arc::new(Value::reported_error())),
        }
    }
}

/// Elimination context.
pub struct ElimContext<'arena, 'env> {
    flexible_exprs: &'env SliceEnv<Option<Arc<Value<'arena>>>>,
}

impl<'arena, 'env> ElimContext<'arena, 'env> {
    pub fn new(
        flexible_exprs: &'env SliceEnv<Option<Arc<Value<'arena>>>>,
    ) -> ElimContext<'arena, 'env> {
        ElimContext { flexible_exprs }
    }

    /// Bring a value up-to-date with any new unification solutions that
    /// might now be present at the head of in the given value.
    pub fn force(&self, value: &Arc<Value<'arena>>) -> Result<Arc<Value<'arena>>> {
        let mut forced_value = value.clone();
        // Attempt to force flexible values until we don't see any more.
        while let Value::Stuck(Head::FlexibleVar(var), spine) = forced_value.as_ref() {
            match (self.flexible_exprs.get_global(*var)).ok_or(Error::InvalidFlexibleVar)? {
                // Apply the spine to the solution. This might uncover another
                // flexible value so we'll continue looping.
                Some(expr) => forced_value = self.apply_spine(expr.clone(), spine)?,
                // There's no solution for this flexible variable yet, meaning
                // that we've forced the value as much as possible for now
                None => break,
            }
        }
        Ok(forced_value)
    }

    /// Apply a closure to a value.
    pub fn apply_closure(
        &self,
        closure: &Closure<'arena>,
        value: Arc<Value<'arena>>,
    ) -> Result<Arc<Value<'arena>>> {
        let mut rigid_exprs = closure.rigid_exprs.clone();
        rigid_exprs.push(value);
        EvalContext::new(&mut rigid_exprs, self.flexible_exprs).eval(closure.term)
    }

    /// Apply a function elimination to an expression, performing
    /// [beta-reduction] if possible.
    ///
    /// [beta-reduction]: https://ncatlab.org/nlab/show/beta-reduction
    pub fn apply_fun(
        &self,
        mut head_expr: Arc<Value<'arena>>,
        input_expr: Arc<Value<'arena>>,
    ) -> Result<Arc<Value<'arena>>> {
        match Arc::make_mut(&mut head_expr) {
            // Beta-reduction
            Value::FunIntro(_, output_expr) => self.apply_closure(output_expr, input_expr),
            // The computation is stuck, preventing further reduction
            Value::Stuck(_, spine) => {
                spine.push(Elim::Fun(input_expr));
                Ok(head_expr)
            }
            _ => Err(Error::InvalidFunctionElimHead),
        }
    }

    /// Apply an expression to an elimination spine.
    pub fn apply_spine(
        &self,
        mut head_expr: Arc<Value<'arena>>,
        spine: &[Elim<'arena>],
    ) -> Result<Arc<Value<'arena>>> {
        for elim in spine {
            head_expr = match elim {
                Elim::Fun(input_expr) => self.apply_fun(head_expr, input_expr.clone())?,
            };
        }
        Ok(head_expr)
    }
}

/// Quotation context.
pub struct QuoteContext<'in_arena, 'out_arena, 'env> {
    arena: &'out_arena Arena<'out_arena>,
    rigid_exprs: EnvLen,
    flexible_exprs: &'env SliceEnv<Option<Arc<Value<'in_arena>>>>,
}

impl<'in_arena, 'out_arena, 'env> QuoteContext<'in_arena, 'out_arena, 'env> {
    pub fn new(
        arena: &'out_arena Arena<'out_arena>,
        rigid_exprs: EnvLen,
        flexible_exprs: &'env SliceEnv<Option<Arc<Value<'in_arena>>>>,
    ) -> QuoteContext<'in_arena, 'out_arena, 'env> {
        QuoteContext {
            arena,
            rigid_exprs,
            flexible_exprs,
        }
    }

    fn elim_context(&self) -> ElimContext<'in_arena, 'env> {
        ElimContext::new(self.flexible_exprs)
    }

    fn push_rigid(&mut self) {
        self.rigid_exprs.push();
    }

    fn pop_rigid(&mut self) {
        self.rigid_exprs.pop();
    }

    /// Quote a [value][Value] back into a [term][Term].
    pub fn quote(&mut self, value: &Arc<Value<'in_arena>>) -> Result<Term<'out_arena>> {
        match self.elim_context().force(value)?.as_ref() {
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
                            let input_expr = self.quote(input_expr)?;
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
                let input_type = self.quote(input_type)?;
                let output_type = self.quote_closure(output_type)?;

                Ok(Term::FunType(
                    *input_name,
                    self.arena.alloc_term(input_type),
                    self.arena.alloc_term(output_type),
                ))
            }
            Value::FunIntro(input_name, output_expr) => {
                let output_expr = self.quote_closure(output_expr)?;

                Ok(Term::FunIntro(
                    *input_name,
                    self.arena.alloc_term(output_expr),
                ))
            }
        }
    }

    /// Quote a [closure][Closure] back into a [term][Term].
    fn quote_closure(&mut self, closure: &Closure<'in_arena>) -> Result<Term<'out_arena>> {
        let var = Arc::new(Value::rigid_var(self.rigid_exprs.next_global()));
        let value = self.elim_context().apply_closure(closure, var)?;

        self.push_rigid();
        let term = self.quote(&value);
        self.pop_rigid();

        term
    }
}

/// Conversion context.
pub struct ConversionContext<'arena, 'env> {
    rigid_exprs: EnvLen,
    flexible_exprs: &'env SliceEnv<Option<Arc<Value<'arena>>>>,
}

impl<'arena, 'env> ConversionContext<'arena, 'env> {
    pub fn new(
        rigid_exprs: EnvLen,
        flexible_exprs: &'env SliceEnv<Option<Arc<Value<'arena>>>>,
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
    pub fn is_equal(&mut self, value0: &Arc<Value<'_>>, value1: &Arc<Value<'_>>) -> Result<bool> {
        let value0 = self.elim_context().force(value0)?;
        let value1 = self.elim_context().force(value1)?;

        match (value0.as_ref(), value1.as_ref()) {
            // `ReportedError`s result from errors that have already been
            // reported, so we prevent them from triggering more errors.
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
            (Value::FunIntro(_, output_expr), _) => {
                self.is_equal_fun_intro_elim(output_expr, &value1)
            }
            (_, Value::FunIntro(_, output_expr)) => {
                self.is_equal_fun_intro_elim(output_expr, &value0)
            }

            (_, _) => Ok(false),
        }
    }

    /// Check that two [closures][Closure] are equal.
    pub fn is_equal_closures(
        &mut self,
        closure0: &Closure<'_>,
        closure1: &Closure<'_>,
    ) -> Result<bool> {
        let var = Arc::new(Value::rigid_var(self.rigid_exprs.next_global()));
        let value0 = self.elim_context().apply_closure(closure0, var.clone())?;
        let value1 = self.elim_context().apply_closure(closure1, var)?;

        self.push_rigid();
        let result = self.is_equal(&value0, &value1);
        self.pop_rigid();

        result
    }

    /// Check that a function is equal to a value, using eta-conversion.
    fn is_equal_fun_intro_elim(
        &mut self,
        output_expr: &Closure<'_>,
        value: &Arc<Value<'_>>,
    ) -> Result<bool> {
        let var = Arc::new(Value::rigid_var(self.rigid_exprs.next_global()));
        let value = self.elim_context().apply_fun(value.clone(), var.clone())?;
        let output_expr = self.elim_context().apply_closure(output_expr, var)?;

        self.push_rigid();
        let result = self.is_equal(&output_expr, &value);
        self.pop_rigid();

        result
    }
}
