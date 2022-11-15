//! The semantics of the core language, implemented using [normalisation by
//! evaluation](https://en.wikipedia.org/wiki/Normalisation_by_evaluation).

use scoped_arena::Scope;
use std::panic::panic_any;
use std::sync::Arc;

use crate::alloc::SliceVec;
use crate::core::{prim, Const, LocalInfo, Prim, Term};
use crate::env::{EnvLen, Index, Level, SharedEnv, SliceEnv};
use crate::source::{Span, Spanned};
use crate::StringId;

/// Atomically reference counted values. We use reference counting to increase
/// the amount of sharing we can achieve during evaluation.
pub type ArcValue<'arena> = Spanned<Arc<Value<'arena>>>;

/// Values in weak-head-normal form, with bindings converted to closures.
#[derive(Debug, Clone)]
pub enum Value<'arena> {
    /// A value whose computation has been blocked as a result of trying to
    /// [evaluate][EvalEnv::eval] an open [term][Term], along with a spine
    /// of eliminations. Subsequent eliminations applied to this value are
    /// accumulated in the spine.
    Stuck(Head, Vec<Elim<'arena>>),

    /// Universes.
    Universe,

    /// Dependent function types.
    FunType(Option<StringId>, ArcValue<'arena>, Closure<'arena>),
    /// Function literals.
    FunLit(Option<StringId>, Closure<'arena>),

    /// Record types.
    RecordType(&'arena [StringId], Telescope<'arena>),
    /// Record literals.
    RecordLit(&'arena [StringId], Vec<ArcValue<'arena>>),

    /// Array literals.
    ArrayLit(Vec<ArcValue<'arena>>),

    /// Record formats, consisting of a list of dependent formats.
    FormatRecord(&'arena [StringId], Telescope<'arena>),
    /// Conditional format, consisting of a format and predicate.
    FormatCond(StringId, ArcValue<'arena>, Closure<'arena>),
    /// Overlap formats, consisting of a list of dependent formats, overlapping
    /// in memory.
    FormatOverlap(&'arena [StringId], Telescope<'arena>),

    /// Constant literals.
    ConstLit(Const),
}

impl<'arena> Value<'arena> {
    pub fn prim(prim: Prim, params: impl IntoIterator<Item = ArcValue<'arena>>) -> Value<'arena> {
        let params = params.into_iter().map(Elim::FunApp).collect();
        Value::Stuck(Head::Prim(prim), params)
    }

    pub fn local_var(level: Level) -> Value<'arena> {
        Value::Stuck(Head::LocalVar(level), Vec::new())
    }

    pub fn meta_var(level: Level) -> Value<'arena> {
        Value::Stuck(Head::MetaVar(level), Vec::new())
    }

    pub fn match_prim_spine(&self) -> Option<(Prim, &[Elim<'arena>])> {
        match self {
            Value::Stuck(Head::Prim(prim), spine) => Some((*prim, spine)),
            _ => None,
        }
    }
}

/// The head of a [stuck value][Value::Stuck].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Head {
    /// Primitives that have not yet been reduced.
    Prim(Prim),
    /// Variables that refer to local binders.
    LocalVar(Level),
    /// Variables that refer to unsolved unification problems.
    MetaVar(Level), // TODO: Use a RefCell here?
}

/// A pending elimination to be reduced if the [head][Head] of a [stuck
/// value][Value::Stuck] becomes known.
#[derive(Debug, Clone)]
pub enum Elim<'arena> {
    /// Function applications.
    FunApp(ArcValue<'arena>),
    /// Record projections.
    RecordProj(StringId),
    /// Match on a constant.
    ConstMatch(Branches<'arena, Const>),
}

/// A closure is a term that can later be instantiated with a value.
#[derive(Debug, Clone)]
pub struct Closure<'arena> {
    /// Local environment where the closed [term][Self.term] is bound. A new
    /// entry will need to be pushed to this environment before evaluating the
    /// term.
    local_exprs: SharedEnv<ArcValue<'arena>>,
    /// The term that is closed over.
    term: &'arena Term<'arena>,
}

impl<'arena> Closure<'arena> {
    /// Construct a closure.
    pub fn new(
        local_exprs: SharedEnv<ArcValue<'arena>>,
        term: &'arena Term<'arena>,
    ) -> Closure<'arena> {
        Closure { local_exprs, term }
    }

    pub fn span(&self) -> Span {
        self.term.span()
    }
}

/// A series of terms where each term might depend on previous terms.
///
/// The term ‘telescope’ was [coined by de Bruijn] to allude to how each
/// variable scopes over subsequent variables in a nested fashion, like how the
/// segments of an “old-fashioned” expandable telescope slide into each other.
///
/// [coined by de Bruijn]: https://doi.org/10.1016/0890-5401(91)90066-B
#[derive(Debug, Clone)]
pub struct Telescope<'arena> {
    /// Local environment where the telescope's [terms][Self.terms] are bound.
    local_exprs: SharedEnv<ArcValue<'arena>>,
    /// `Repr` should be applied to each term in the telescope.
    apply_repr: bool,
    /// The terms in the telescope.
    terms: &'arena [Term<'arena>],
}

impl<'arena> Telescope<'arena> {
    /// Construct a telescope.
    pub fn new(
        local_exprs: SharedEnv<ArcValue<'arena>>,
        terms: &'arena [Term<'arena>],
    ) -> Telescope<'arena> {
        Telescope {
            local_exprs,
            apply_repr: false,
            terms,
        }
    }

    fn apply_repr(self) -> Telescope<'arena> {
        debug_assert_eq!(self.apply_repr, false);
        Telescope {
            apply_repr: true,
            ..self
        }
    }

    /// The number of terms in the telescope.
    pub fn len(&self) -> usize {
        self.terms.len()
    }
}

/// The branches of a single-level pattern match.
#[derive(Debug, Clone)]
pub struct Branches<'arena, P> {
    local_exprs: SharedEnv<ArcValue<'arena>>,
    pattern_branches: &'arena [(P, Term<'arena>)],
    default_expr: Option<&'arena Term<'arena>>,
}

impl<'arena, P> Branches<'arena, P> {
    /// Construct a single-level pattern match.
    pub fn new(
        local_exprs: SharedEnv<ArcValue<'arena>>,
        pattern_branches: &'arena [(P, Term<'arena>)],
        default_expr: Option<&'arena Term<'arena>>,
    ) -> Branches<'arena, P> {
        Branches {
            local_exprs,
            pattern_branches,
            default_expr,
        }
    }

    /// The number of pattern branches.
    pub fn num_patterns(&self) -> usize {
        self.pattern_branches.len()
    }
}

pub type PatternBranch<'arena, P> = (P, ArcValue<'arena>);

#[derive(Clone, Debug)]
pub enum SplitBranches<'arena, P> {
    Branch(PatternBranch<'arena, P>, Branches<'arena, P>),
    Default(Closure<'arena>),
    None,
}

/// Errors encountered while interpreting terms.
// TODO: include stack trace(??)
#[derive(Clone, Debug)]
pub enum Error {
    UnboundItemVar,
    UnboundLocalVar,
    UnboundMetaVar,
    InvalidFunctionApp,
    InvalidRecordProj,
    InvalidConstMatch,
    InvalidFormatRepr,
    MissingConstDefault,
}

impl Error {
    pub fn description(&self) -> &str {
        match &self {
            Error::UnboundItemVar => "unbound item variable",
            Error::UnboundLocalVar => "unbound local variable",
            Error::UnboundMetaVar => "unbound metavariable",
            Error::InvalidFunctionApp => "invalid function application",
            Error::InvalidRecordProj => "invalid record projection",
            Error::InvalidConstMatch => "invalid constant match",
            Error::InvalidFormatRepr => "invalid format repr",
            Error::MissingConstDefault => "missing default expression",
        }
    }
}

/// Evaluation environment.
///
/// Like the [`ElimEnv`], this allows for the running of computations, but
/// also maintains a local environment, allowing for evaluation.
pub struct EvalEnv<'arena, 'env> {
    elim_env: ElimEnv<'arena, 'env>,
    local_exprs: &'env mut SharedEnv<ArcValue<'arena>>,
}

impl<'arena, 'env> EvalEnv<'arena, 'env> {
    pub fn new(
        elim_env: ElimEnv<'arena, 'env>,
        local_exprs: &'env mut SharedEnv<ArcValue<'arena>>,
    ) -> EvalEnv<'arena, 'env> {
        EvalEnv {
            elim_env,
            local_exprs,
        }
    }

    fn quote_env(&self) -> QuoteEnv<'arena, 'env> {
        QuoteEnv::new(self.elim_env, self.local_exprs.len())
    }

    fn get_local_expr<'this: 'env>(&'this self, var: Index) -> &'env ArcValue<'arena> {
        let value = self.local_exprs.get_index(var);
        value.unwrap_or_else(|| panic_any(Error::UnboundLocalVar))
    }

    /// Fully normalise a term by first [evaluating][EvalEnv::eval] it into
    /// a [value][Value], then [quoting it back][QuoteEnv::quote] into a
    /// [term][Term].
    pub fn normalise<'out_arena>(
        &mut self,
        scope: &'out_arena Scope<'out_arena>,
        term: &Term<'arena>,
    ) -> Term<'out_arena> {
        self.quote_env().quote(scope, &self.eval(term))
    }

    /// Evaluate a [term][Term] into a [value][Value].
    ///
    /// This could be loosely thought of as a just-in-time implementation of
    /// closure conversion + partial evaluation (for more discussion see [this
    /// twitter thread](https://twitter.com/brendanzab/status/1423536653658771457)).
    pub fn eval(&mut self, term: &Term<'arena>) -> ArcValue<'arena> {
        match term {
            Term::ItemVar(span, var) => {
                Spanned::new(*span, Arc::clone(self.elim_env.get_item_expr(*var)))
            }
            Term::MetaVar(span, var) => match self.elim_env.get_meta_expr(*var) {
                Some(value) => Spanned::new(*span, Arc::clone(value)),
                None => Spanned::new(*span, Arc::new(Value::meta_var(*var))),
            },
            Term::LocalVar(span, var) => Spanned::new(*span, Arc::clone(self.get_local_expr(*var))),
            Term::InsertedMeta(span, var, local_infos) => {
                let head_expr = self.eval(&Term::MetaVar(*span, *var));
                self.apply_local_infos(head_expr, local_infos)
            }
            Term::Ann(span, expr, _) => Spanned::merge(*span, self.eval(expr)),
            Term::Let(span, _, _, def_expr, body_expr) => {
                let def_expr = self.eval(def_expr);
                self.local_exprs.push(def_expr);
                let body_expr = self.eval(body_expr);
                self.local_exprs.pop();
                Spanned::merge(*span, body_expr)
            }

            Term::Universe(span) => Spanned::new(*span, Arc::new(Value::Universe)),

            Term::FunType(span, param_name, param_type, body_type) => Spanned::new(
                *span,
                Arc::new(Value::FunType(
                    *param_name,
                    self.eval(param_type),
                    Closure::new(self.local_exprs.clone(), body_type),
                )),
            ),
            Term::FunLit(span, param_name, body_expr) => Spanned::new(
                *span,
                Arc::new(Value::FunLit(
                    *param_name,
                    Closure::new(self.local_exprs.clone(), body_expr),
                )),
            ),
            Term::FunApp(span, head_expr, arg_expr) => {
                let head_expr = self.eval(head_expr);
                let arg_expr = self.eval(arg_expr);
                Spanned::merge(*span, self.elim_env.fun_app(head_expr, arg_expr))
            }

            Term::RecordType(span, labels, types) => {
                let types = Telescope::new(self.local_exprs.clone(), types);
                Spanned::new(*span, Arc::new(Value::RecordType(labels, types)))
            }
            Term::RecordLit(span, labels, exprs) => {
                let exprs = exprs.iter().map(|expr| self.eval(expr)).collect();
                Spanned::new(*span, Arc::new(Value::RecordLit(labels, exprs)))
            }
            Term::RecordProj(span, head_expr, label) => {
                let head_expr = self.eval(head_expr);
                Spanned::merge(*span, self.elim_env.record_proj(head_expr, *label))
            }

            Term::ArrayLit(span, exprs) => {
                let exprs = exprs.iter().map(|expr| self.eval(expr)).collect();
                Spanned::new(*span, Arc::new(Value::ArrayLit(exprs)))
            }

            Term::FormatRecord(span, labels, formats) => {
                let formats = Telescope::new(self.local_exprs.clone(), formats);
                Spanned::new(*span, Arc::new(Value::FormatRecord(labels, formats)))
            }
            Term::FormatCond(span, name, format, cond) => {
                let format = self.eval(format);
                let cond_expr = Closure::new(self.local_exprs.clone(), cond);
                Spanned::new(*span, Arc::new(Value::FormatCond(*name, format, cond_expr)))
            }
            Term::FormatOverlap(span, labels, formats) => {
                let formats = Telescope::new(self.local_exprs.clone(), formats);
                Spanned::new(*span, Arc::new(Value::FormatOverlap(labels, formats)))
            }

            Term::Prim(span, prim) => Spanned::new(*span, Arc::new(Value::prim(*prim, []))),

            Term::ConstLit(span, r#const) => {
                Spanned::new(*span, Arc::new(Value::ConstLit(*r#const)))
            }
            Term::ConstMatch(span, head_expr, branches, default_expr) => {
                let head_expr = self.eval(head_expr);
                let branches = Branches::new(self.local_exprs.clone(), branches, *default_expr);
                Spanned::merge(*span, self.elim_env.const_match(head_expr, branches))
            }
        }
    }

    fn apply_local_infos(
        &mut self,
        mut head_expr: ArcValue<'arena>,
        infos: &[LocalInfo],
    ) -> ArcValue<'arena> {
        for (info, expr) in Iterator::zip(infos.iter(), self.local_exprs.iter()) {
            head_expr = match info {
                LocalInfo::Def => head_expr,
                LocalInfo::Param => self.elim_env.fun_app(head_expr, expr.clone()),
            };
        }
        head_expr
    }
}

/// Elimination environment.
///
/// Contains enough state to run computations, but does not contain a local
/// environment that would be needed for full evaluation.
#[derive(Copy, Clone)]
pub struct ElimEnv<'arena, 'env> {
    item_exprs: &'env SliceEnv<ArcValue<'arena>>,
    meta_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>,
}

impl<'arena, 'env> ElimEnv<'arena, 'env> {
    pub fn new(
        item_exprs: &'env SliceEnv<ArcValue<'arena>>,
        meta_exprs: &'env SliceEnv<Option<ArcValue<'arena>>>,
    ) -> ElimEnv<'arena, 'env> {
        ElimEnv {
            item_exprs,
            meta_exprs,
        }
    }

    pub fn eval_env(
        &self,
        local_exprs: &'env mut SharedEnv<ArcValue<'arena>>,
    ) -> EvalEnv<'arena, 'env> {
        EvalEnv::new(*self, local_exprs)
    }

    pub fn conversion_env(&self, local_exprs: EnvLen) -> ConversionEnv<'arena, 'env> {
        ConversionEnv::new(*self, local_exprs)
    }

    fn get_item_expr(&self, var: Level) -> &'env ArcValue<'arena> {
        let value = self.item_exprs.get_level(var);
        value.unwrap_or_else(|| panic_any(Error::UnboundItemVar))
    }

    fn get_meta_expr(&self, var: Level) -> &'env Option<ArcValue<'arena>> {
        let value = self.meta_exprs.get_level(var);
        value.unwrap_or_else(|| panic_any(Error::UnboundMetaVar))
    }

    /// Bring a value up-to-date with any new unification solutions that
    /// might now be present at the head of in the given value.
    pub fn force(&self, value: &ArcValue<'arena>) -> ArcValue<'arena> {
        let mut forced_value = value.clone();
        // Attempt to force metavariables until we don't see any more.
        while let Value::Stuck(Head::MetaVar(var), spine) = forced_value.as_ref() {
            match self.get_meta_expr(*var) {
                // Apply the spine to the solution. This might uncover another
                // metavariable so we'll continue looping.
                Some(expr) => forced_value = self.apply_spine(expr.clone(), spine),
                // There's no solution for this metavariable yet, meaning
                // that we've forced the value as much as possible for now
                None => break,
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
        let mut local_exprs = closure.local_exprs.clone();
        local_exprs.push(value);
        self.eval_env(&mut local_exprs).eval(closure.term)
    }

    /// Split a telescope into the first value, and a continuation that returns
    /// a telescope containing the rest of the values.
    pub fn split_telescope(
        &self,
        mut telescope: Telescope<'arena>,
    ) -> Option<(
        ArcValue<'arena>,
        impl FnOnce(ArcValue<'arena>) -> Telescope<'arena>,
    )> {
        let (term, terms) = telescope.terms.split_first()?;
        let mut env = self.eval_env(&mut telescope.local_exprs);
        let value = match telescope.apply_repr {
            true => self.format_repr(&env.eval(term)),
            false => env.eval(term),
        };

        Some((value, move |previous_value| {
            telescope.local_exprs.push(previous_value);
            telescope.terms = terms;
            telescope
        }))
    }

    pub fn split_branches<P: Copy>(
        &self,
        mut branches: Branches<'arena, P>,
    ) -> SplitBranches<'arena, P> {
        match branches.pattern_branches.split_first() {
            Some(((pattern, body_expr), pattern_branches)) => {
                branches.pattern_branches = pattern_branches;
                let mut context = self.eval_env(&mut branches.local_exprs);
                SplitBranches::Branch((*pattern, context.eval(body_expr)), branches)
            }
            None => match branches.default_expr {
                Some(default_expr) => {
                    SplitBranches::Default(Closure::new(branches.local_exprs, default_expr))
                }
                None => SplitBranches::None,
            },
        }
    }

    /// Apply a function application to an expression, performing
    /// [beta-reduction] if possible.
    ///
    /// [beta-reduction]: https://ncatlab.org/nlab/show/beta-reduction
    pub fn fun_app(
        &self,
        mut head_expr: ArcValue<'arena>,
        arg_expr: ArcValue<'arena>,
    ) -> ArcValue<'arena> {
        match Arc::make_mut(&mut head_expr) {
            // Beta-reduction
            Value::FunLit(_, body_expr) => self.apply_closure(body_expr, arg_expr), // FIXME: use span from head/arg exprs?
            // The computation is stuck, preventing further reduction
            Value::Stuck(head, spine) => {
                spine.push(Elim::FunApp(arg_expr));

                match head {
                    Head::Prim(prim) => prim::step(*prim)(self, spine).unwrap_or(head_expr),
                    _ => head_expr,
                }
            }
            _ => panic_any(Error::InvalidFunctionApp),
        }
    }

    /// Apply a record projection to an expression, performing
    /// [beta-reduction] if possible.
    ///
    /// [beta-reduction]: https://ncatlab.org/nlab/show/beta-reduction
    pub fn record_proj(
        &self,
        mut head_expr: ArcValue<'arena>,
        label: StringId,
    ) -> ArcValue<'arena> {
        match Arc::make_mut(&mut head_expr) {
            // Beta-reduction
            Value::RecordLit(labels, exprs) => (labels.iter())
                .position(|current_label| *current_label == label)
                .and_then(|expr_index| exprs.get(expr_index).cloned())
                .unwrap_or_else(|| panic_any(Error::InvalidRecordProj)),
            // The computation is stuck, preventing further reduction
            Value::Stuck(_, spine) => {
                spine.push(Elim::RecordProj(label));
                head_expr
            }
            _ => panic_any(Error::InvalidRecordProj),
        }
    }

    /// Apply a constant match to an expression, performing [beta-reduction] if
    /// possible.
    ///
    /// [beta-reduction]: https://ncatlab.org/nlab/show/beta-reduction
    fn const_match(
        &self,
        mut head_expr: ArcValue<'arena>,
        mut branches: Branches<'arena, Const>,
    ) -> ArcValue<'arena> {
        match Arc::make_mut(&mut head_expr) {
            Value::ConstLit(r#const) => {
                // Try each branch
                for (branch_const, body_expr) in branches.pattern_branches {
                    if r#const == branch_const {
                        return self.eval_env(&mut branches.local_exprs).eval(body_expr);
                    }
                }
                // Otherwise call default with `head_expr`
                let mut local_exprs = branches.local_exprs.clone();
                local_exprs.push(head_expr);
                match branches.default_expr {
                    Some(default_expr) => self.eval_env(&mut local_exprs).eval(default_expr),
                    None => panic_any(Error::MissingConstDefault),
                }
            }
            // The computation is stuck, preventing further reduction
            Value::Stuck(_, spine) => {
                spine.push(Elim::ConstMatch(branches));
                head_expr
            }
            _ => panic_any(Error::InvalidConstMatch),
        }
    }

    /// Apply an expression to an elimination spine.
    fn apply_spine(&self, head_expr: ArcValue<'arena>, spine: &[Elim<'arena>]) -> ArcValue<'arena> {
        spine.iter().fold(head_expr, |head_expr, elim| match elim {
            Elim::FunApp(arg_expr) => self.fun_app(head_expr, arg_expr.clone()),
            Elim::RecordProj(label) => self.record_proj(head_expr, *label),
            Elim::ConstMatch(split) => self.const_match(head_expr, split.clone()),
        })
    }

    /// Find the representation type of a format description.
    pub fn format_repr(&self, format: &ArcValue<'arena>) -> ArcValue<'arena> {
        let value = match format.as_ref() {
            Value::FormatRecord(labels, formats) | Value::FormatOverlap(labels, formats) => {
                Value::RecordType(labels, formats.clone().apply_repr())
            }
            Value::FormatCond(_, format, _) => return self.format_repr(format),
            Value::Stuck(Head::Prim(prim), spine) => match prim::repr(*prim)(self, spine) {
                Some(r#type) => return r#type,
                None => Value::prim(Prim::FormatRepr, [format.clone()]),
            },
            Value::Stuck(_, _) => Value::prim(Prim::FormatRepr, [format.clone()]),
            _ => panic_any(Error::InvalidFormatRepr),
        };

        Spanned::new(format.span(), Arc::new(value))
    }
}

/// Quotation environment.
///
/// This environment keeps track of the length of the local environment,
/// and the values of metavariable expressions, allowing for quotation.
pub struct QuoteEnv<'in_arena, 'env> {
    elim_env: ElimEnv<'in_arena, 'env>,
    local_exprs: EnvLen,
    unfold_metas: bool,
}

impl<'in_arena, 'env> QuoteEnv<'in_arena, 'env> {
    pub fn new(
        elim_env: ElimEnv<'in_arena, 'env>,
        local_exprs: EnvLen,
    ) -> QuoteEnv<'in_arena, 'env> {
        QuoteEnv {
            elim_env,
            local_exprs,
            unfold_metas: false,
        }
    }

    pub fn unfolding_metas(mut self) -> QuoteEnv<'in_arena, 'env> {
        self.unfold_metas = true;
        self
    }

    fn push_local(&mut self) {
        self.local_exprs.push();
    }

    fn pop_local(&mut self) {
        self.local_exprs.pop();
    }

    /// Quote a [value][Value] back into a [term][Term].
    pub fn quote<'out_arena>(
        &mut self,
        scope: &'out_arena Scope<'out_arena>,
        value: &ArcValue<'in_arena>,
    ) -> Term<'out_arena> {
        // NOTE: this copies more than is necessary when `'in_arena == 'out_arena`:
        // for example when copying label slices.

        let value = self.elim_env.force(value);
        let span = value.span();
        match value.as_ref() {
            Value::Stuck(head, spine) => spine.iter().fold(
                self.quote_head(scope, span, head),
                |head_expr, elim| match elim {
                    Elim::FunApp(arg_expr) => Term::FunApp(
                        span,
                        scope.to_scope(head_expr),
                        scope.to_scope(self.quote(scope, arg_expr)),
                    ),
                    Elim::RecordProj(label) => {
                        Term::RecordProj(span, scope.to_scope(head_expr), *label)
                    }
                    Elim::ConstMatch(branches) => {
                        let mut branches = branches.clone();
                        let mut pattern_branches = SliceVec::new(scope, branches.num_patterns());

                        let default_expr = loop {
                            match self.elim_env.split_branches(branches) {
                                SplitBranches::Branch((r#const, body_expr), next_branches) => {
                                    pattern_branches.push((r#const, self.quote(scope, &body_expr)));
                                    branches = next_branches;
                                }
                                SplitBranches::Default(default_expr) => break Some(default_expr),
                                SplitBranches::None => break None,
                            }
                        };

                        Term::ConstMatch(
                            span,
                            scope.to_scope(head_expr),
                            pattern_branches.into(),
                            default_expr.map(|expr| self.quote_closure(scope, &expr)),
                        )
                    }
                },
            ),

            Value::Universe => Term::Universe(span),

            Value::FunType(param_name, param_type, body_type) => Term::FunType(
                span,
                *param_name,
                scope.to_scope(self.quote(scope, param_type)),
                self.quote_closure(scope, body_type),
            ),
            Value::FunLit(param_name, body_expr) => {
                Term::FunLit(span, *param_name, self.quote_closure(scope, body_expr))
            }

            Value::RecordType(labels, types) => Term::RecordType(
                span,
                scope.to_scope_from_iter(labels.iter().copied()),
                self.quote_telescope(scope, types),
            ),
            Value::RecordLit(labels, exprs) => Term::RecordLit(
                span,
                scope.to_scope_from_iter(labels.iter().copied()),
                scope.to_scope_from_iter(exprs.iter().map(|expr| self.quote(scope, expr))),
            ),
            Value::ArrayLit(exprs) => Term::ArrayLit(
                span,
                scope.to_scope_from_iter(exprs.iter().map(|expr| self.quote(scope, expr))),
            ),

            Value::FormatRecord(labels, formats) => Term::FormatRecord(
                span,
                scope.to_scope_from_iter(labels.iter().copied()),
                self.quote_telescope(scope, formats),
            ),
            Value::FormatCond(label, format, cond) => Term::FormatCond(
                span,
                *label,
                scope.to_scope(self.quote(scope, format)),
                self.quote_closure(scope, cond),
            ),
            Value::FormatOverlap(labels, formats) => Term::FormatOverlap(
                span,
                scope.to_scope_from_iter(labels.iter().copied()),
                self.quote_telescope(scope, formats),
            ),

            Value::ConstLit(r#const) => Term::ConstLit(span, *r#const),
        }
    }

    /// Quote an [elimination head][Head] back into a [term][Term].
    fn quote_head<'out_arena>(
        &mut self,
        scope: &'out_arena Scope<'out_arena>,
        span: Span,
        head: &Head,
    ) -> Term<'out_arena> {
        match head {
            Head::Prim(prim) => Term::Prim(span, *prim),
            Head::LocalVar(var) => match self.local_exprs.level_to_index(*var) {
                Some(var) => Term::LocalVar(span, var),
                None => panic_any(Error::UnboundLocalVar),
            },
            Head::MetaVar(var) if self.unfold_metas => {
                match self.elim_env.get_meta_expr(*var) {
                    // The metavariable has a solution, so unfold it.
                    Some(value) => self.quote(scope, value),
                    // NOTE: We might want to replace this with `ReportedError`.
                    None => Term::MetaVar(span, *var),
                }
            }
            Head::MetaVar(var) => Term::MetaVar(span, *var),
        }
    }

    /// Quote a [closure][Closure] back into a [term][Term].
    fn quote_closure<'out_arena>(
        &mut self,
        scope: &'out_arena Scope<'out_arena>,
        closure: &Closure<'in_arena>,
    ) -> &'out_arena Term<'out_arena> {
        let var = Arc::new(Value::local_var(self.local_exprs.next_level()));
        let value = self.elim_env.apply_closure(closure, Spanned::empty(var));

        self.push_local();
        let term = self.quote(scope, &value);
        self.pop_local();

        scope.to_scope(term)
    }

    /// Quote a [telescope][Telescope] back into a slice of [terms][Term].
    fn quote_telescope<'out_arena>(
        &mut self,
        scope: &'out_arena Scope<'out_arena>,
        telescope: &Telescope<'in_arena>,
    ) -> &'out_arena [Term<'out_arena>] {
        let initial_local_len = self.local_exprs;
        let mut telescope = telescope.clone();
        let mut terms = SliceVec::new(scope, telescope.len());

        while let Some((value, next_telescope)) = self.elim_env.split_telescope(telescope) {
            let var = Arc::new(Value::local_var(self.local_exprs.next_level()));
            telescope = next_telescope(Spanned::empty(var));
            terms.push(self.quote(scope, &value));
            self.local_exprs.push();
        }

        self.local_exprs.truncate(initial_local_len);
        terms.into()
    }
}

pub enum TermOrValue<'in_arena, 'out_arena> {
    Value(ArcValue<'in_arena>),
    Term(Term<'out_arena>),
}

impl<'arena, 'env> EvalEnv<'arena, 'env> {
    /// Unfold all solved metavariable solutions and meta-headed eliminations.
    ///
    /// If all the metas have been solved, this will result in a term that no
    /// longer depends on the environment of metavariable solutions.
    ///
    /// This is sometimes known as _zonking_.
    ///
    /// # References
    ///
    /// - [The GHC Commentary: Type Variables and Zonking](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/type-checker#types-variables-and-zonking)
    /// - [What does the GHC source mean by "zonk"?](https://stackoverflow.com/questions/31889048/what-does-the-ghc-source-mean-by-zonk)
    /// - [Email by Simon Peyton-Jones](https://mail.haskell.org/pipermail/glasgow-haskell-users/2013-August/024209.html)
    pub fn unfold_metas<'out_arena>(
        &mut self,
        scope: &'out_arena Scope<'out_arena>,
        term: &Term<'arena>,
    ) -> Term<'out_arena> {
        match term {
            Term::ItemVar(span, var) => Term::ItemVar(*span, *var),
            Term::LocalVar(span, var) => Term::LocalVar(*span, *var),

            // These terms might be elimination spines with metavariables at
            // their head that need to be unfolded.
            Term::MetaVar(..) | Term::FunApp(..) | Term::RecordProj(..) | Term::ConstMatch(..) => {
                match self.unfold_meta_var_spines(scope, term) {
                    TermOrValue::Term(term) => term,
                    TermOrValue::Value(value) => self.quote_env().quote(scope, &value),
                }
            }

            Term::InsertedMeta(span, var, infos) => match self.elim_env.get_meta_expr(*var) {
                Some(value) => {
                    let value = self.apply_local_infos(value.clone(), infos);
                    self.quote_env().quote(scope, &value)
                }
                None => {
                    let infos = scope.to_scope_from_iter(infos.iter().copied());
                    Term::InsertedMeta(*span, *var, infos)
                }
            },
            Term::Ann(span, expr, r#type) => Term::Ann(
                *span,
                scope.to_scope(self.unfold_metas(scope, expr)),
                scope.to_scope(self.unfold_metas(scope, r#type)),
            ),
            Term::Let(span, def_name, def_type, def_expr, body_expr) => Term::Let(
                *span,
                *def_name,
                scope.to_scope(self.unfold_metas(scope, def_type)),
                scope.to_scope(self.unfold_metas(scope, def_expr)),
                self.unfold_bound_metas(scope, body_expr),
            ),

            Term::Universe(span) => Term::Universe(*span),

            Term::FunType(span, param_name, param_type, body_type) => Term::FunType(
                *span,
                *param_name,
                scope.to_scope(self.unfold_metas(scope, param_type)),
                self.unfold_bound_metas(scope, body_type),
            ),
            Term::FunLit(span, param_name, body_expr) => Term::FunLit(
                *span,
                *param_name,
                self.unfold_bound_metas(scope, body_expr),
            ),

            Term::RecordType(span, labels, types) => Term::RecordType(
                *span,
                scope.to_scope_from_iter(labels.iter().copied()),
                self.unfold_telescope_metas(scope, types),
            ),
            Term::RecordLit(span, labels, exprs) => Term::RecordLit(
                *span,
                scope.to_scope_from_iter(labels.iter().copied()),
                scope.to_scope_from_iter(exprs.iter().map(|expr| self.unfold_metas(scope, expr))),
            ),

            Term::ArrayLit(span, exprs) => Term::ArrayLit(
                *span,
                scope.to_scope_from_iter(exprs.iter().map(|expr| self.unfold_metas(scope, expr))),
            ),

            Term::FormatRecord(span, labels, formats) => Term::FormatRecord(
                *span,
                scope.to_scope_from_iter(labels.iter().copied()),
                self.unfold_telescope_metas(scope, formats),
            ),
            Term::FormatCond(span, name, format, pred) => Term::FormatCond(
                *span,
                *name,
                scope.to_scope(self.unfold_metas(scope, format)),
                self.unfold_bound_metas(scope, pred),
            ),
            Term::FormatOverlap(span, labels, formats) => Term::FormatOverlap(
                *span,
                scope.to_scope_from_iter(labels.iter().copied()),
                self.unfold_telescope_metas(scope, formats),
            ),

            Term::Prim(span, prim) => Term::Prim(*span, *prim),

            Term::ConstLit(span, r#const) => Term::ConstLit(*span, *r#const),
        }
    }

    /// Unfold elimination spines with solved metavariables at their head.
    fn unfold_meta_var_spines<'out_arena>(
        &mut self,
        scope: &'out_arena Scope<'out_arena>,
        term: &Term<'arena>,
    ) -> TermOrValue<'arena, 'out_arena> {
        // Recurse to find the head of an elimination, checking if it's a
        // metavariable. If so, check if it has a solution, and then apply
        // eliminations to the solution in turn on our way back out.
        match term {
            Term::MetaVar(span, var) => match self.elim_env.get_meta_expr(*var) {
                // The metavariable has a solution, so unfold it.
                Some(value) => TermOrValue::Value(value.clone()),
                // No solution was found for the metavariable.
                // NOTE: We might want to replace this with `ReportedError`.
                None => TermOrValue::Term(Term::MetaVar(*span, *var)),
            },
            Term::InsertedMeta(span, var, infos) => {
                match self.elim_env.get_meta_expr(*var) {
                    // The metavariable has a solution, so unfold it.
                    Some(value) => TermOrValue::Value(self.apply_local_infos(value.clone(), infos)),
                    // No solution was found for the metavariable.
                    // NOTE: We might want to replace this with `ReportedError`.
                    None => {
                        let infos = scope.to_scope_from_iter(infos.iter().copied());
                        TermOrValue::Term(Term::InsertedMeta(*span, *var, infos))
                    }
                }
            }

            Term::FunApp(span, head_expr, arg_expr) => {
                match self.unfold_meta_var_spines(scope, head_expr) {
                    TermOrValue::Term(head_expr) => TermOrValue::Term(Term::FunApp(
                        *span,
                        scope.to_scope(head_expr),
                        scope.to_scope(self.unfold_metas(scope, arg_expr)),
                    )),
                    TermOrValue::Value(head_expr) => {
                        let arg_expr = self.eval(arg_expr);
                        TermOrValue::Value(self.elim_env.fun_app(head_expr, arg_expr))
                    }
                }
            }
            Term::RecordProj(span, head_expr, label) => {
                match self.unfold_meta_var_spines(scope, head_expr) {
                    TermOrValue::Term(head_expr) => TermOrValue::Term(Term::RecordProj(
                        *span,
                        scope.to_scope(head_expr),
                        *label,
                    )),
                    TermOrValue::Value(head_expr) => {
                        TermOrValue::Value(self.elim_env.record_proj(head_expr, *label))
                    }
                }
            }
            Term::ConstMatch(span, head_expr, branches, default) => {
                match self.unfold_meta_var_spines(scope, head_expr) {
                    TermOrValue::Term(head_expr) => TermOrValue::Term(Term::ConstMatch(
                        *span,
                        scope.to_scope(head_expr),
                        scope.to_scope_from_iter(
                            (branches.iter())
                                .map(|(r#const, expr)| (*r#const, self.unfold_metas(scope, expr))),
                        ),
                        default.map(|expr| self.unfold_bound_metas(scope, expr)),
                    )),
                    TermOrValue::Value(head_expr) => {
                        let branches = Branches::new(self.local_exprs.clone(), branches, *default);
                        TermOrValue::Value(self.elim_env.const_match(head_expr, branches))
                    }
                }
            }

            term => TermOrValue::Term(self.unfold_metas(scope, term)),
        }
    }

    fn unfold_bound_metas<'out_arena>(
        &mut self,
        scope: &'out_arena Scope<'out_arena>,
        term: &Term<'arena>,
    ) -> &'out_arena Term<'out_arena> {
        let var = Arc::new(Value::local_var(self.local_exprs.len().next_level()));

        self.local_exprs.push(Spanned::empty(var));
        let term = self.unfold_metas(scope, term);
        self.local_exprs.pop();

        scope.to_scope(term)
    }

    fn unfold_telescope_metas<'out_arena>(
        &mut self,
        scope: &'out_arena Scope<'out_arena>,
        terms: &[Term<'arena>],
    ) -> &'out_arena [Term<'out_arena>] {
        let initial_locals = self.local_exprs.len();

        let terms = scope.to_scope_from_iter(terms.iter().map(|term| {
            let term = self.unfold_metas(scope, term);
            let var = Arc::new(Value::local_var(self.local_exprs.len().next_level()));
            self.local_exprs.push(Spanned::empty(var));
            term
        }));

        self.local_exprs.truncate(initial_locals);

        terms
    }
}

/// Conversion environment.
///
/// This environment keeps track of the length of the local environment,
/// and the values of metavariable expressions, allowing for conversion.
pub struct ConversionEnv<'arena, 'env> {
    elim_env: ElimEnv<'arena, 'env>,
    local_exprs: EnvLen,
}

impl<'arena, 'env> ConversionEnv<'arena, 'env> {
    pub fn new(
        elim_env: ElimEnv<'arena, 'env>,
        local_exprs: EnvLen,
    ) -> ConversionEnv<'arena, 'env> {
        ConversionEnv {
            elim_env,
            local_exprs,
        }
    }

    fn push_local(&mut self) {
        self.local_exprs.push();
    }

    fn pop_local(&mut self) {
        self.local_exprs.pop();
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
        let value0 = self.elim_env.force(value0);
        let value1 = self.elim_env.force(value1);

        match (value0.as_ref(), value1.as_ref()) {
            // `ReportedError`s result from errors that have already been
            // reported, so we prevent them from triggering more errors.
            (Value::Stuck(Head::Prim(Prim::ReportedError), _), _)
            | (_, Value::Stuck(Head::Prim(Prim::ReportedError), _)) => true,

            (Value::Stuck(head0, spine0), Value::Stuck(head1, spine1)) => {
                head0 == head1 && self.is_equal_spines(spine0, spine1)
            }
            (Value::Universe, Value::Universe) => true,

            (
                Value::FunType(_, param_type0, body_type0),
                Value::FunType(_, param_type1, body_type1),
            ) => {
                self.is_equal(param_type0, param_type1)
                    && self.is_equal_closures(body_type0, body_type1)
            }
            (Value::FunLit(_, body_expr0), Value::FunLit(_, body_expr1)) => {
                self.is_equal_closures(body_expr0, body_expr1)
            }
            (Value::FunLit(_, body_expr), _) => self.is_equal_fun_lit(body_expr, &value1),
            (_, Value::FunLit(_, body_expr)) => self.is_equal_fun_lit(body_expr, &value0),

            (Value::RecordType(labels0, types0), Value::RecordType(labels1, types1)) => {
                labels0 == labels1 && self.is_equal_telescopes(types0, types1)
            }
            (Value::RecordLit(labels0, exprs0), Value::RecordLit(labels1, exprs1)) => {
                labels0 == labels1
                    && Iterator::zip(exprs0.iter(), exprs1.iter())
                        .all(|(expr0, expr1)| self.is_equal(expr0, expr1))
            }
            (Value::RecordLit(labels, exprs), _) => {
                self.is_equal_record_lit(labels, exprs, &value1)
            }
            (_, Value::RecordLit(labels, exprs)) => {
                self.is_equal_record_lit(labels, exprs, &value0)
            }

            (Value::ArrayLit(exprs0), Value::ArrayLit(exprs1)) => {
                Iterator::zip(exprs0.iter(), exprs1.iter())
                    .all(|(expr0, expr1)| self.is_equal(expr0, expr1))
            }

            (Value::FormatRecord(labels0, formats0), Value::FormatRecord(labels1, formats1))
            | (Value::FormatOverlap(labels0, formats0), Value::FormatOverlap(labels1, formats1)) => {
                labels0 == labels1 && self.is_equal_telescopes(formats0, formats1)
            }

            (
                Value::FormatCond(label0, format0, cond0),
                Value::FormatCond(label1, format1, cond1),
            ) => {
                label0 == label1
                    && self.is_equal(format0, format1)
                    && self.is_equal_closures(cond0, cond1)
            }

            (Value::ConstLit(const0), Value::ConstLit(const1)) => const0 == const1,

            (_, _) => false,
        }
    }

    /// Check that two elimination spines are equal.
    pub fn is_equal_spines(&mut self, spine0: &[Elim<'_>], spine1: &[Elim<'_>]) -> bool {
        spine0.len() == spine1.len()
            && Iterator::zip(spine0.iter(), spine1.iter()).all(|(elim0, elim1)| {
                match (elim0, elim1) {
                    (Elim::FunApp(expr0), Elim::FunApp(expr1)) => self.is_equal(expr0, expr1),
                    (Elim::RecordProj(label0), Elim::RecordProj(label1)) => label0 == label1,
                    (Elim::ConstMatch(branches0), Elim::ConstMatch(branches1)) => {
                        self.is_equal_branches(branches0, branches1)
                    }
                    (_, _) => false,
                }
            })
    }

    /// Check that two [closures][Closure] are equal.
    pub fn is_equal_closures(&mut self, closure0: &Closure<'_>, closure1: &Closure<'_>) -> bool {
        let var = Spanned::empty(Arc::new(Value::local_var(self.local_exprs.next_level())));
        let value0 = self.elim_env.apply_closure(closure0, var.clone());
        let value1 = self.elim_env.apply_closure(closure1, var);

        self.push_local();
        let result = self.is_equal(&value0, &value1);
        self.pop_local();

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

        let initial_local_len = self.local_exprs;
        let mut telescope0 = telescope0.clone();
        let mut telescope1 = telescope1.clone();

        while let Some(((value0, next_telescope0), (value1, next_telescope1))) = Option::zip(
            self.elim_env.split_telescope(telescope0),
            self.elim_env.split_telescope(telescope1),
        ) {
            if !self.is_equal(&value0, &value1) {
                self.local_exprs.truncate(initial_local_len);
                return false;
            }

            let var = Spanned::empty(Arc::new(Value::local_var(self.local_exprs.next_level())));
            telescope0 = next_telescope0(var.clone());
            telescope1 = next_telescope1(var);
            self.local_exprs.push();
        }

        self.local_exprs.truncate(initial_local_len);
        true
    }

    /// Check that two [constant branches][Branches] are equal.
    fn is_equal_branches<P: PartialEq + Copy>(
        &mut self,
        branches0: &Branches<'_, P>,
        branches1: &Branches<'_, P>,
    ) -> bool {
        use SplitBranches::*;

        let mut branches0 = branches0.clone();
        let mut branches1 = branches1.clone();

        loop {
            match (
                self.elim_env.split_branches(branches0),
                self.elim_env.split_branches(branches1),
            ) {
                (
                    Branch((const0, body_expr0), next_branches0),
                    Branch((const1, body_expr1), next_branches1),
                ) if const0 == const1 && self.is_equal(&body_expr0, &body_expr1) => {
                    branches0 = next_branches0;
                    branches1 = next_branches1;
                }
                (Default(default_expr0), Default(default_expr1)) => {
                    return self.is_equal_closures(&default_expr0, &default_expr1);
                }
                (None, None) => return true,
                (_, _) => return false,
            }
        }
    }

    /// Check that a function literal is equal to a value, using eta-conversion.
    ///
    /// ```fathom
    /// (fun x => f x) = f
    /// ```
    fn is_equal_fun_lit(&mut self, body_expr: &Closure<'_>, value: &ArcValue<'_>) -> bool {
        let var = Spanned::empty(Arc::new(Value::local_var(self.local_exprs.next_level())));
        let value = self.elim_env.fun_app(value.clone(), var.clone());
        let body_expr = self.elim_env.apply_closure(body_expr, var);

        self.push_local();
        let result = self.is_equal(&body_expr, &value);
        self.pop_local();

        result
    }

    /// Check that a record literal is equal to a value, using eta-conversion.
    ///
    /// ```fathom
    /// { x = r.x, y = r.y, .. } = r
    /// ```
    fn is_equal_record_lit(
        &mut self,
        labels: &[StringId],
        exprs: &[ArcValue<'_>],
        value: &ArcValue<'_>,
    ) -> bool {
        Iterator::zip(labels.iter(), exprs.iter()).all(|(label, expr)| {
            let field_value = self.elim_env.record_proj(value.clone(), *label);
            self.is_equal(expr, &field_value)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[allow(dead_code)]
    fn value_has_unify_and_is_equal_impls(value: Value<'_>) {
        // The following match will fail to be exhaustive after new variants
        // are added to `Value`. When this happens, it’s a prompt to make sure
        // that the variants are handled in:
        //
        // - surface::elaboration::Context::unify
        // - core::semantics::is_equal
        //
        // NOTE: Only update the match below when you've updated the above functions.
        match value {
            Value::Stuck(..) => {}
            Value::Universe => {}
            Value::FunType(..) => {}
            Value::FunLit(..) => {}
            Value::RecordType(..) => {}
            Value::RecordLit(..) => {}
            Value::ArrayLit(..) => {}
            Value::FormatRecord(..) => {}
            Value::FormatCond(..) => {}
            Value::FormatOverlap(..) => {}
            Value::ConstLit(..) => {}
        }
    }

    #[allow(dead_code)]
    fn elim_has_unify_and_is_equal_impls(elim: Elim<'_>) {
        // The following match will fail to be exhaustive after new variants
        // are added to `Elim`. When this happens, it’s a prompt to make sure
        // that the variants are handled in:
        //
        // - surface::elaboration::Context::unify
        // - core::semantics::is_equal
        //
        // NOTE: Only update the match below when you've updated the above functions.
        match elim {
            Elim::FunApp(..) => {}
            Elim::RecordProj(..) => {}
            Elim::ConstMatch(..) => {}
        }
    }
}
