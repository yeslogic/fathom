use super::*;
use crate::surface::elaboration;

mod compile;
mod coverage;

pub use self::compile::compile_match;
pub use self::coverage::check_coverage;

/// Simple patterns that have had some initial elaboration performed on them
#[derive(Debug, Clone)]
pub enum CheckedPattern<'arena> {
    /// Error sentinel
    ReportedError(ByteRange),
    /// Placeholder patterns that match everything
    Placeholder(ByteRange),
    /// Pattern that binds local variable
    Binder(ByteRange, Symbol),
    /// Constant literals
    ConstLit(ByteRange, Const),
    /// Record literals
    RecordLit(ByteRange, &'arena [Symbol], &'arena [Self]),
}

impl<'arena> CheckedPattern<'arena> {
    pub fn range(&self) -> ByteRange {
        match self {
            CheckedPattern::ReportedError(range)
            | CheckedPattern::Placeholder(range)
            | CheckedPattern::Binder(range, _)
            | CheckedPattern::ConstLit(range, _)
            | CheckedPattern::RecordLit(range, _, _) => *range,
        }
    }

    pub fn name(&self) -> Option<Symbol> {
        match self {
            CheckedPattern::Binder(_, name) => Some(*name),
            _ => None,
        }
    }

    /// Returns `true` if `self` is a "wildcard" pattern - ie always matches its
    /// scrutinee
    pub fn is_wildcard(&self) -> bool {
        match self {
            CheckedPattern::ReportedError(_)
            | CheckedPattern::Placeholder(_)
            | CheckedPattern::Binder(_, _) => true,
            CheckedPattern::ConstLit(_, _) | CheckedPattern::RecordLit(_, _, _) => false,
        }
    }

    /// Returns `true` if `self` evaluates its scrutinee exactly once.
    /// Used as a heuristic to prevent increase in runtime when expanding
    /// pattern matches
    pub fn is_atomic(&self) -> bool {
        match self {
            CheckedPattern::ReportedError(_)
            | CheckedPattern::Placeholder(_)
            | CheckedPattern::Binder(_, _)
            | CheckedPattern::ConstLit(_, _) => true,
            CheckedPattern::RecordLit(_, _, _) => false,
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(self, CheckedPattern::ReportedError(_))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PatternMode {
    Fun,
    Let,
    Match,
}

impl<'arena> Context<'arena> {
    /// Check that a pattern matches an expected type.
    pub fn check_pattern(
        &mut self,
        pattern: &Pattern<'_, ByteRange>,
        expected_type: &ArcValue<'arena>,
    ) -> CheckedPattern<'arena> {
        let expected_type = self.elim_env().force(expected_type);

        match (pattern, expected_type.as_ref()) {
            (Pattern::Name(range, name), _) => CheckedPattern::Binder(*range, *name),
            (Pattern::Placeholder(range), _) => CheckedPattern::Placeholder(*range),
            (Pattern::StringLiteral(range, lit), _) => {
                let constant = match expected_type.match_prim_spine() {
                    Some((Prim::U8Type, [])) => self.parse_ascii(*range, *lit, Const::U8),
                    Some((Prim::U16Type, [])) => self.parse_ascii(*range, *lit, Const::U16),
                    Some((Prim::U32Type, [])) => self.parse_ascii(*range, *lit, Const::U32),
                    Some((Prim::U64Type, [])) => self.parse_ascii(*range, *lit, Const::U64),
                    // Some((Prim::Array8Type, [len, _])) => todo!(),
                    // Some((Prim::Array16Type, [len, _])) => todo!(),
                    // Some((Prim::Array32Type, [len, _])) => todo!(),
                    // Some((Prim::Array64Type, [len, _])) => todo!(),
                    Some((Prim::ReportedError, _)) => None,
                    _ => {
                        let expected_type = self.pretty_value(&expected_type);
                        self.push_message(Message::StringLiteralNotSupported {
                            range: self.file_range(*range),
                            expected_type,
                        });
                        None
                    }
                };

                match constant {
                    Some(constant) => CheckedPattern::ConstLit(*range, constant),
                    None => CheckedPattern::ReportedError(*range),
                }
            }
            (Pattern::NumberLiteral(range, lit), _) => {
                let constant = match expected_type.match_prim_spine() {
                    Some((Prim::U8Type, [])) => self.parse_number_radix(*range, *lit, Const::U8),
                    Some((Prim::U16Type, [])) => self.parse_number_radix(*range, *lit, Const::U16),
                    Some((Prim::U32Type, [])) => self.parse_number_radix(*range, *lit, Const::U32),
                    Some((Prim::U64Type, [])) => self.parse_number_radix(*range, *lit, Const::U64),
                    Some((Prim::S8Type, [])) => self.parse_number(*range, *lit, Const::S8),
                    Some((Prim::S16Type, [])) => self.parse_number(*range, *lit, Const::S16),
                    Some((Prim::S32Type, [])) => self.parse_number(*range, *lit, Const::S32),
                    Some((Prim::S64Type, [])) => self.parse_number(*range, *lit, Const::S64),
                    Some((Prim::F32Type, [])) => self.parse_number(*range, *lit, Const::F32),
                    Some((Prim::F64Type, [])) => self.parse_number(*range, *lit, Const::F64),
                    Some((Prim::ReportedError, _)) => None,
                    _ => {
                        let expected_type = self.pretty_value(&expected_type);
                        self.push_message(Message::NumericLiteralNotSupported {
                            range: self.file_range(*range),
                            expected_type,
                        });
                        None
                    }
                };

                match constant {
                    Some(constant) => CheckedPattern::ConstLit(*range, constant),
                    None => CheckedPattern::ReportedError(*range),
                }
            }
            (Pattern::BooleanLiteral(range, boolean), _) => {
                let constant = match expected_type.match_prim_spine() {
                    Some((Prim::BoolType, [])) => match *boolean {
                        true => Some(Const::Bool(true)),
                        false => Some(Const::Bool(false)),
                    },
                    _ => {
                        self.push_message(Message::BooleanLiteralNotSupported {
                            range: self.file_range(*range),
                        });
                        None
                    }
                };

                match constant {
                    Some(constant) => CheckedPattern::ConstLit(*range, constant),
                    None => CheckedPattern::ReportedError(*range),
                }
            }
            (
                Pattern::RecordLiteral(range, pattern_fields),
                Value::RecordType(labels, telescope),
            ) => {
                if self
                    .check_record_fields(*range, pattern_fields, |field| field.label, labels)
                    .is_err()
                {
                    return CheckedPattern::ReportedError(*range);
                }

                let mut telescope = telescope.clone();
                let mut pattern_fields = pattern_fields.iter();
                let mut patterns = SliceVec::new(self.scope, labels.len());

                while let Some((field, (r#type, next_telescope))) = Option::zip(
                    pattern_fields.next(),
                    self.elim_env().split_telescope(telescope.clone()),
                ) {
                    let pattern = self.check_pattern(&field.pattern, &r#type);
                    telescope = next_telescope(Spanned::empty(Arc::new(Value::local_var(
                        self.local_env.len().next_level(),
                    ))));
                    patterns.push(pattern);
                }
                CheckedPattern::RecordLit(*range, labels, patterns.into())
            }
            (Pattern::TupleLiteral(range, elem_patterns), Value::RecordType(labels, telescope)) => {
                if self
                    .check_tuple_fields(*range, elem_patterns, |pattern| pattern.range(), labels)
                    .is_err()
                {
                    return CheckedPattern::ReportedError(*range);
                }

                let mut telescope = telescope.clone();
                let mut elem_patterns = elem_patterns.iter();
                let mut patterns = SliceVec::new(self.scope, elem_patterns.len());

                while let Some((pattern, (r#type, next_telescope))) = Option::zip(
                    elem_patterns.next(),
                    self.elim_env().split_telescope(telescope.clone()),
                ) {
                    let pattern = self.check_pattern(pattern, &r#type);
                    telescope = next_telescope(Spanned::empty(Arc::new(Value::local_var(
                        self.local_env.len().next_level(),
                    ))));
                    patterns.push(pattern);
                }

                CheckedPattern::RecordLit(*range, labels, patterns.into())
            }
            _ => {
                let range = pattern.range();
                let (pattern, r#type) = self.synth_pattern(pattern);
                match self.unification_context().unify(&r#type, &expected_type) {
                    Ok(()) => pattern,
                    Err(error) => {
                        let found = self.pretty_value(&r#type);
                        let expected = self.pretty_value(&expected_type);
                        self.push_message(Message::FailedToUnify {
                            range: self.file_range(range),
                            found,
                            expected,
                            error,
                        });
                        CheckedPattern::ReportedError(range)
                    }
                }
            }
        }
    }

    /// Synthesize the type of a pattern.
    pub fn synth_pattern(
        &mut self,
        pattern: &Pattern<ByteRange>,
    ) -> (CheckedPattern<'arena>, ArcValue<'arena>) {
        let file_range = self.file_range(pattern.range());
        match pattern {
            Pattern::Name(range, name) => {
                let source = MetaSource::NamedPatternType(file_range, *name);
                let r#type = self.push_unsolved_type(source);
                (CheckedPattern::Binder(*range, *name), r#type)
            }
            Pattern::Placeholder(range) => {
                let source = MetaSource::PlaceholderPatternType(file_range);
                let r#type = self.push_unsolved_type(source);
                (CheckedPattern::Placeholder(*range), r#type)
            }
            Pattern::StringLiteral(range, _) => {
                self.push_message(Message::AmbiguousStringLiteral { range: file_range });
                let source = MetaSource::ReportedErrorType(file_range);
                let r#type = self.push_unsolved_type(source);
                (CheckedPattern::ReportedError(*range), r#type)
            }
            Pattern::NumberLiteral(range, _) => {
                self.push_message(Message::AmbiguousNumericLiteral { range: file_range });
                let source = MetaSource::ReportedErrorType(file_range);
                let r#type = self.push_unsolved_type(source);
                (CheckedPattern::ReportedError(*range), r#type)
            }
            Pattern::BooleanLiteral(range, val) => {
                let r#const = Const::Bool(*val);
                let r#type = self.bool_type.clone();
                (CheckedPattern::ConstLit(*range, r#const), r#type)
            }
            Pattern::RecordLiteral(range, pattern_fields) => {
                let (labels, pattern_fields) =
                    self.report_duplicate_labels(*range, pattern_fields, |f| f.label);

                let mut patterns = SliceVec::new(self.scope, labels.len());
                let mut types = SliceVec::new(self.scope, labels.len());

                for field in pattern_fields {
                    let (pattern, r#type) = self.synth_pattern(&field.pattern);
                    patterns.push(pattern);
                    types.push(self.quote_env().quote(self.scope, &r#type));
                }

                let telescope = Telescope::new(self.local_env.exprs.clone(), types.into());
                (
                    CheckedPattern::RecordLit(*range, labels, patterns.into()),
                    Spanned::new(
                        file_range.into(),
                        Arc::new(Value::RecordType(labels, telescope)),
                    ),
                )
            }
            Pattern::TupleLiteral(range, elem_patterns) => {
                let labels = Symbol::get_tuple_labels(0..elem_patterns.len());
                let labels = self.scope.to_scope_from_iter(labels.iter().copied());

                let mut patterns = SliceVec::new(self.scope, labels.len());
                let mut types = SliceVec::new(self.scope, labels.len());

                for pattern in elem_patterns.iter() {
                    let (pattern, r#type) = self.synth_pattern(pattern);
                    patterns.push(pattern);
                    types.push(self.quote_env().quote(self.scope, &r#type));
                }

                let telescope = Telescope::new(self.local_env.exprs.clone(), types.into());
                (
                    CheckedPattern::RecordLit(*range, labels, patterns.into()),
                    Spanned::new(
                        file_range.into(),
                        Arc::new(Value::RecordType(labels, telescope)),
                    ),
                )
            }
        }
    }

    /// Check that the type of an annotated pattern matches an expected type.
    pub fn check_ann_pattern(
        &mut self,
        pattern: &Pattern<ByteRange>,
        r#type: Option<&Term<'_, ByteRange>>,
        expected_type: &ArcValue<'arena>,
    ) -> CheckedPattern<'arena> {
        match r#type {
            None => self.check_pattern(pattern, expected_type),
            Some(r#type) => {
                let range = r#type.range();
                let r#type = self.check(r#type, &self.universe.clone());
                let r#type = self.eval_env().eval(&r#type);

                match self.unification_context().unify(&r#type, expected_type) {
                    Ok(()) => self.check_pattern(pattern, &r#type),
                    Err(error) => {
                        let found = self.pretty_value(&r#type);
                        let expected = self.pretty_value(expected_type);
                        self.push_message(Message::FailedToUnify {
                            range: self.file_range(range),
                            found,
                            expected,
                            error,
                        });
                        CheckedPattern::ReportedError(range)
                    }
                }
            }
        }
    }

    /// Synthesize the type of an annotated pattern.
    pub fn synth_ann_pattern(
        &mut self,
        pattern: &Pattern<ByteRange>,
        r#type: Option<&Term<'_, ByteRange>>,
    ) -> (CheckedPattern<'arena>, core::Term<'arena>, ArcValue<'arena>) {
        match r#type {
            None => {
                let (pattern, type_value) = self.synth_pattern(pattern);
                let r#type = self.quote_env().quote(self.scope, &type_value);
                (pattern, r#type, type_value)
            }
            Some(r#type) => {
                let r#type = self.check(r#type, &self.universe.clone());
                let type_value = self.eval_env().eval(&r#type);
                (self.check_pattern(pattern, &type_value), r#type, type_value)
            }
        }
    }

    /// Push a pattern onto the local context
    pub fn push_pattern(
        &mut self,
        pattern: &CheckedPattern<'arena>,
        scrut: Scrutinee<'arena>,
        value: ArcValue<'arena>,
        mode: PatternMode,
        toplevel: bool,
    ) -> Vec<(Option<Symbol>, Scrutinee<'arena>)> {
        let mut defs = Vec::new();
        self.push_pattern_inner(pattern, scrut, value, mode, toplevel, &mut defs);
        defs
    }

    fn push_pattern_inner(
        &mut self,
        pattern: &CheckedPattern<'arena>,
        scrut: Scrutinee<'arena>,
        value: ArcValue<'arena>,
        mode: PatternMode,
        toplevel: bool,
        defs: &mut Vec<(Option<Symbol>, Scrutinee<'arena>)>,
    ) {
        let r#type = scrut.r#type.clone();
        match pattern {
            CheckedPattern::ReportedError(_) | CheckedPattern::Placeholder(_) => {
                match (mode, toplevel) {
                    (PatternMode::Fun, true) => self.local_env.push_param(None, r#type),
                    (PatternMode::Let, true) => {
                        defs.push((None, scrut));
                        self.local_env.push_def(None, value, r#type)
                    }
                    (PatternMode::Match, true) => self.local_env.push_def(None, value, r#type),
                    _ => {}
                }
            }
            CheckedPattern::Binder(_, name) => match (mode, toplevel) {
                (PatternMode::Fun, true) => self.local_env.push_param(Some(*name), r#type),
                (PatternMode::Fun, _) | (PatternMode::Let, _) | (PatternMode::Match, _) => {
                    defs.push((Some(*name), scrut));
                    self.local_env.push_def(Some(*name), value, r#type)
                }
            },
            CheckedPattern::ConstLit(_, _) => match (mode, toplevel) {
                (PatternMode::Fun, true) => self.local_env.push_param(None, r#type),
                (PatternMode::Let, true) => {
                    defs.push((None, scrut));
                    self.local_env.push_def(None, value, r#type)
                }
                _ => {}
            },
            CheckedPattern::RecordLit(_, labels, patterns) => {
                if let (PatternMode::Fun, true) = (mode, toplevel) {
                    self.local_env.push_param(None, r#type.clone())
                }

                let range = scrut.range;
                let mut iter = Iterator::zip(labels.iter(), patterns.iter());
                let mut telescope = self
                    .elim_env()
                    .force(&r#type)
                    .match_record_type()
                    .unwrap()
                    .clone();
                while let Some(((label, pattern), (r#type, next_telescope))) = Option::zip(
                    iter.next(),
                    self.elim_env().split_telescope(telescope.clone()),
                ) {
                    telescope = next_telescope(self.local_env.next_var());
                    let value = self.elim_env().record_proj(value.clone(), *label);
                    let expr =
                        core::Term::RecordProj(self.file_range(range).into(), scrut.expr, *label);
                    let scrut = Scrutinee {
                        range,
                        expr: self.scope.to_scope(expr),
                        r#type,
                    };
                    self.push_pattern_inner(pattern, scrut, value, mode, false, defs)
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Constructor<'arena> {
    Const(Const),
    Record(&'arena [Symbol]),
}

impl<'arena> Constructor<'arena> {
    /// Return number of fields `self` carries
    pub fn arity(&self) -> usize {
        match self {
            Constructor::Const(_) => 0,
            Constructor::Record(labels) => labels.len(),
        }
    }

    pub fn is_exhaustive(ctors: &[Constructor]) -> bool {
        match ctors.first() {
            None => false,
            Some(ctor) => match ctor.num_inhabitants() {
                None => false,
                Some(n) => ctors.len() as u128 >= n,
            },
        }
    }

    /// Return the number of inhabitants of `self`.
    /// `None` represents infinity
    pub fn num_inhabitants(&self) -> Option<u128> {
        match self {
            Constructor::Const(r#const) => r#const.num_inhabitants(),
            Constructor::Record(_) => Some(1),
        }
    }

    pub fn as_const(&self) -> Option<&Const> {
        match self {
            Constructor::Const(r#const) => Some(r#const),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PatMatrix<'arena> {
    rows: Vec<PatRow<'arena>>,
    indices: Vec<usize>,
}

#[derive(Debug, Clone)]
/// The right hand side of a match clause
pub struct Body<'arena> {
    /// The expression to be evaluated
    expr: core::Term<'arena>,
    /// The variables to be let-bound before `expr` is evaluated
    defs: Vec<(Option<Symbol>, Scrutinee<'arena>)>,
}

impl<'arena> Body<'arena> {
    pub fn new(expr: core::Term<'arena>, defs: Vec<(Option<Symbol>, Scrutinee<'arena>)>) -> Self {
        Self { expr, defs }
    }
}

impl<'arena> PatMatrix<'arena> {
    pub fn new(rows: Vec<PatRow<'arena>>) -> Self {
        if let Some((first, rows)) = rows.split_first() {
            for row in rows {
                debug_assert_eq!(
                    first.entries.len(),
                    row.entries.len(),
                    "All rows must be same length"
                );
            }
        }
        let indices = (0..rows.len()).collect();
        Self { rows, indices }
    }

    pub fn singleton(scrut: Scrutinee<'arena>, pat: CheckedPattern<'arena>) -> Self {
        Self::new(vec![PatRow::singleton((pat, scrut))])
    }

    pub fn num_rows(&self) -> usize {
        self.rows.len()
    }

    pub fn num_columns(&self) -> Option<usize> {
        self.rows.first().map(|row| row.len())
    }

    /// Return true if `self` is the null matrix, `∅` - ie `self` has zero rows
    pub fn is_null(&self) -> bool {
        self.num_rows() == 0
    }

    /// Return true if `self` is the unit matrix, `()` - ie `self` has zero
    /// columns and at least one row
    pub fn is_unit(&self) -> bool {
        self.num_columns() == Some(0)
    }

    /// Iterate over all the pairs in the `index`th column
    pub fn column(&self, index: usize) -> impl ExactSizeIterator<Item = &RowEntry<'arena>> + '_ {
        self.rows.iter().map(move |row| &row.entries[index])
    }

    pub fn row(&self, index: usize) -> &PatRow<'arena> {
        &self.rows[index]
    }

    pub fn row_index(&self, index: usize) -> usize {
        self.indices[index]
    }

    /// Collect all the `Constructor`s in the `index`th column
    pub fn column_constructors(&self, index: usize) -> Vec<Constructor<'arena>> {
        let mut ctors = Vec::with_capacity(self.num_rows());
        for (pat, _) in self.column(index) {
            match pat {
                CheckedPattern::ReportedError(_)
                | CheckedPattern::Placeholder(_)
                | CheckedPattern::Binder(_, _) => continue,
                CheckedPattern::ConstLit(_, r#const) => ctors.push(Constructor::Const(*r#const)),
                CheckedPattern::RecordLit(_, labels, _) => ctors.push(Constructor::Record(labels)),
            }
        }
        ctors.sort_unstable();
        ctors.dedup();
        ctors
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = (&PatRow<'arena>, usize)> {
        self.rows.iter().zip(self.indices.iter().copied())
    }
}

/// An element in a `PatRow`: `<scrut.expr> is <pat>`.
/// This notation is taken from [How to compile pattern matching]
pub type RowEntry<'arena> = (CheckedPattern<'arena>, Scrutinee<'arena>);

#[derive(Debug, Clone)]
pub struct PatRow<'arena> {
    entries: Vec<RowEntry<'arena>>,
}

impl<'arena> PatRow<'arena> {
    pub fn new(entries: Vec<RowEntry<'arena>>) -> Self {
        Self { entries }
    }

    pub fn singleton(entry: RowEntry<'arena>) -> Self {
        Self::new(vec![entry])
    }

    pub fn tail(&self) -> Self {
        assert!(!self.is_empty(), "Cannot take tail of empty `PatRow`");
        Self::new(self.entries[1..].to_vec())
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn first(&self) -> Option<&RowEntry<'arena>> {
        self.entries.first()
    }

    pub fn all_wildcards(&self) -> bool {
        self.entries.iter().all(|(pat, _)| pat.is_wildcard())
    }

    pub fn split_first(&self) -> Option<(&RowEntry<'arena>, Self)> {
        let (first, rest) = self.entries.split_first()?;
        Some((first, Self::new(rest.to_vec())))
    }

    pub fn append(&mut self, mut other: Self) {
        self.entries.append(&mut other.entries);
    }

    pub fn patterns(&self) -> impl ExactSizeIterator<Item = &CheckedPattern<'arena>> {
        self.entries.iter().map(|(pattern, _)| pattern)
    }
}

impl<'arena> CheckedPattern<'arena> {
    /// Specialise `self` with respect to the constructor `ctor`.
    pub fn specialize(
        &self,
        ctx: &elaboration::Context<'arena>,
        ctor: &Constructor,
        scrut: &Scrutinee<'arena>,
    ) -> Option<PatRow<'arena>> {
        match self {
            CheckedPattern::ReportedError(range)
            | CheckedPattern::Placeholder(range)
            | CheckedPattern::Binder(range, _) => {
                let columns =
                    vec![(CheckedPattern::Placeholder(*range), scrut.clone()); ctor.arity()];
                Some(PatRow::new(columns))
            }
            CheckedPattern::ConstLit(_, r#const) if *ctor == Constructor::Const(*r#const) => {
                Some(PatRow::new(vec![]))
            }
            CheckedPattern::RecordLit(_, labels, patterns)
                if *ctor == Constructor::Record(labels) =>
            {
                let mut columns = Vec::with_capacity(labels.len());
                let mut iter = Iterator::zip(labels.iter(), patterns.iter());
                let mut telescope = ctx
                    .elim_env()
                    .force(&scrut.r#type)
                    .match_record_type()
                    .unwrap()
                    .clone();

                while let Some(((label, pattern), (r#type, next_telescope))) = Option::zip(
                    iter.next(),
                    ctx.elim_env().split_telescope(telescope.clone()),
                ) {
                    telescope = next_telescope(ctx.local_env.next_var());
                    let scrut_expr = core::Term::RecordProj(Span::Empty, scrut.expr, *label);
                    let scrut_expr = ctx.scope.to_scope(scrut_expr);
                    let scrut = Scrutinee {
                        range: scrut.range,
                        expr: scrut_expr,
                        r#type,
                    };
                    columns.push((pattern.clone(), scrut));
                }
                Some(PatRow::new(columns))
            }
            CheckedPattern::ConstLit(_, _) | CheckedPattern::RecordLit(_, _, _) => None,
        }
    }
}

impl<'arena> PatRow<'arena> {
    /// Specialise `self` with respect to the constructor `ctor`.
    pub fn specialize(
        &self,
        ctx: &mut elaboration::Context<'arena>,
        ctor: &Constructor,
    ) -> Option<PatRow<'arena>> {
        assert!(!self.entries.is_empty(), "Cannot specialize empty `PatRow`");
        let ((pat, scrut), rest) = self.split_first().unwrap();
        let mut row = pat.specialize(ctx, ctor, scrut)?;
        row.append(rest);
        Some(row)
    }
}

impl<'arena> PatMatrix<'arena> {
    /// Specialise `self` with respect to the constructor `ctor`.
    /// This is the `S` function in *Compiling pattern matching to good decision
    /// trees*
    pub fn specialize(&self, ctx: &mut elaboration::Context<'arena>, ctor: &Constructor) -> Self {
        let (rows, indices) = self
            .iter()
            .flat_map(|(row, body)| {
                let row = row.specialize(ctx, ctor)?;
                Some((row, body))
            })
            .unzip();
        Self { rows, indices }
    }

    /// Discard the first column, and all rows starting with a constructed
    /// pattern. This is the `D` function in *Compiling pattern matching to
    /// good decision trees*
    pub fn default(&self) -> Self {
        assert!(
            !self.is_unit(),
            "Cannot default `PatMatrix` with no columns"
        );
        let (rows, indices) = self
            .iter()
            .flat_map(|(row, body)| match row.first().unwrap().0 {
                CheckedPattern::ReportedError(_)
                | CheckedPattern::Placeholder(_)
                | CheckedPattern::Binder(_, _) => Some((row.tail(), body)),
                CheckedPattern::ConstLit(_, _) | CheckedPattern::RecordLit(_, _, _) => None,
            })
            .unzip();
        Self { rows, indices }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg(target_pointer_width = "64")]
    fn checked_pattern_size() {
        assert_eq!(std::mem::size_of::<CheckedPattern>(), 48);
    }
}
