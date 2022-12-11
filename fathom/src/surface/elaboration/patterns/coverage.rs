//! # Bibliography
//! - [Warnings for pattern matching](http://moscova.inria.fr/~maranget/papers/warn/index.html)
//! - [rustc usefulness check](https://github.com/rust-lang/rust/blob/8a09420ac48658cad726e0a6997687ceac4151e3/compiler/rustc_mir_build/src/thir/pattern/usefulness.rs)

// TODO: Currently we only report that the match is non-exhaustive, but we do
// not report which patterns are missing. The algorithm for calculating the set
// of missing patterns is described in part two of *Warnings for pattern
// matching*

use super::*;
use crate::surface::elaboration;

pub fn check_coverage<'arena>(
    ctx: &mut elaboration::Context<'arena>,
    matrix: &PatMatrix<'arena>,
    match_range: ByteRange,
    scrut_range: ByteRange,
) {
    let dummy_scrut = Scrutinee {
        range: scrut_range,
        expr: ctx.scope.to_scope(core::Term::error(Span::Empty)),
        r#type: ArcValue::new(ctx.file_range(scrut_range).into(), Arc::new(Value::ERROR)),
    };
    let row = PatRow::singleton((CheckedPattern::Placeholder(match_range), dummy_scrut));

    // A matrix is exhaustive iff the the wildcard pattern `_` is not useful
    if is_useful(ctx, matrix, &row) {
        ctx.push_message(Message::NonExhaustiveMatchExpr {
            match_expr_range: ctx.file_range(match_range),
            scrutinee_expr_range: ctx.file_range(scrut_range),
        });
    }

    // A matrix row is reachable iff it is useful relative to the rows in the matrix
    // above it
    let mut rows = Vec::with_capacity(matrix.num_rows());
    for (row, _) in matrix.iter() {
        let matrix = PatMatrix::new(rows.clone());
        rows.push(row.clone());

        // Don't check reachability for patterns with errors
        if row.patterns().any(|pattern| pattern.is_error()) {
            continue;
        }

        if !is_useful(ctx, &matrix, row) {
            let range = row.first().unwrap().0.range();
            ctx.push_message(Message::UnreachablePattern {
                range: ctx.file_range(range),
            });
        }
    }
}

/// A row of patterns, *q*, is useful relative to a matrix *m* iff there is a
/// value matched by `q` and not matched by *m*. This is the `U` function in
/// *Warnings for pattern matching*
fn is_useful<'arena>(
    ctx: &mut elaboration::Context<'arena>,
    matrix: &PatMatrix<'arena>,
    row: &PatRow<'arena>,
) -> bool {
    if let Some(n) = matrix.num_columns() {
        debug_assert_eq!(
            n,
            row.len(),
            "`row` must have a pattern for each column of `matrix`"
        )
    }

    // Base case 1:
    // If the matrix has no columns, but at least one row, the test row is not
    // useful
    if matrix.is_unit() {
        return false;
    }

    // Base case 2:
    // If the matrix has no columns and no rows, the test row is useful
    if matrix.is_null() {
        return true;
    }

    let (pat, _) = row.first().unwrap();
    match pat {
        // Inductive case 1:
        // If the first pattern is a constructed pattern, specialise the matrix and test row and
        // recurse
        CheckedPattern::ConstLit(_, r#const) => {
            is_useful_ctor(ctx, matrix, row, &Constructor::Const(*r#const))
        }
        CheckedPattern::RecordLit(_, labels, _) => {
            is_useful_ctor(ctx, matrix, row, &Constructor::Record(labels))
        }

        // Inductive case 2:
        // If the first pattern is a wildcard pattern, collect all the constructors in the first
        // column of matrix and test for exhaustiveness
        CheckedPattern::ReportedError(_)
        | CheckedPattern::Placeholder(_)
        | CheckedPattern::Binder(_, _) => {
            let ctors = matrix.column_constructors(0);
            match Constructor::is_exhaustive(&ctors) {
                // Inductive case 2a:
                // If the constructors are exhaustive, specialise the matrix and test row against
                // each constructor and recurse
                true => ctors
                    .into_iter()
                    .any(|ctor| is_useful_ctor(ctx, matrix, row, &ctor)),
                // Inductive case 2b:
                // If the constructors are not exhaustive, recurse on the defaulted matrix
                false => {
                    let matrix = matrix.default();
                    is_useful(ctx, &matrix, &row.tail())
                }
            }
        }
    }
}

fn is_useful_ctor<'arena>(
    ctx: &mut elaboration::Context<'arena>,
    matrix: &PatMatrix<'arena>,
    row: &PatRow<'arena>,
    ctor: &Constructor<'arena>,
) -> bool {
    let matrix = matrix.specialize(ctx, ctor);
    let row = row.specialize(ctx, ctor);
    match row {
        None => false,
        Some(row) => is_useful(ctx, &matrix, &row),
    }
}
