//! # Resources
//! - The compilation algorithm is taken from [Compiling pattern matching to
//!   good decision trees].
//! - You may also find [The Case for Pattern Matching] helpful, it describes
//!   the above algorithm in less formal language and compares it to an older
//!   algorithm that produces backtracking trees.
//! - [How to compile pattern matching] describes an algorithm very similar to
//!   [Compiling pattern matching to good decision trees]. It provided the
//!   useful insight that each entry in the pattern matrix must describe not
//!   just the pattern, but the expression being matched against (which changes
//!   over the course of the algorithm). This point was not made explicit in
//!   [Compiling pattern matching to good decision trees]
//!
//! [Compiling pattern matching to good decision trees]: https://dl.acm.org/doi/10.1145/1411304.1411311
//! [The Case for Pattern Matching]: https://alan-j-hu.github.io/writing/pattern-matching.html
//! [How to compile pattern matching]: https://julesjacobs.com/notes/patternmatching/patternmatching.pdf

// TODO: Use join points to prevent code size explosion. See [Compiling without continuations](https://www.microsoft.com/en-us/research/publication/compiling-without-continuations)

use super::*;
use crate::surface::elaboration;

/// Compilation of pattern matrices to decision trees.
/// This is the `CC` function in *Compiling pattern matching to good decision
/// trees*.
pub fn compile_match<'arena>(
    ctx: &mut elaboration::Context<'arena>,
    matrix: &mut PatMatrix<'arena>,
    bodies: &[Body<'arena>],
    mut shift_amount: EnvLen,
) -> core::Term<'arena> {
    // Base case 1:
    // If the matrix is empty, matching always fails.
    if matrix.is_null() {
        return core::Term::error(Span::Empty); // TODO
    }

    // Base case 2:
    // If the first row is all wildcards, matching always suceeds.
    if matrix.row(0).all_wildcards() {
        let index = matrix.row_index(0);
        let Body { expr, defs } = &bodies[index];

        let initial_len = ctx.local_env.len();
        #[allow(clippy::needless_collect)]
        let defs: Vec<_> = defs
            .iter()
            .map(|(def_name, scrut)| {
                let def_name = *def_name;
                let def_range = scrut.range;
                let def_type = ctx.quote_env().quote(ctx.scope, &scrut.r#type);
                let def_expr = scrut.expr.shift(ctx.scope, shift_amount);
                let def_value = ctx.eval_env().eval(&def_expr);
                ctx.local_env
                    .push_def(def_name, def_value, scrut.r#type.clone());
                shift_amount.push();
                (def_range, def_name, def_type, def_expr)
            })
            .collect();
        ctx.local_env.truncate(initial_len);

        return defs.into_iter().rev().fold(
            expr.clone(),
            |body, (def_range, def_name, def_type, def_expr)| {
                core::Term::Let(
                    ctx.file_range(def_range).into(),
                    def_name,
                    ctx.scope.to_scope(def_type),
                    ctx.scope.to_scope(def_expr),
                    ctx.scope.to_scope(body),
                )
            },
        );
    }

    // Inductive case:
    // The matrix must have at least one column with at least one non-wildcard
    // pattern. Select such a column, and for each constructor in the column,
    // generate a decision subtree. If the column is non-exhaustive, generate a
    // default branch as well.
    let column = matrix.column_to_split_on().unwrap();
    matrix.swap_columns(0, column);
    for (pat, scrut) in matrix.column(0) {
        match pat {
            CheckedPattern::ConstLit(_, _) => {
                let scrut_expr = scrut.expr.shift(ctx.scope, shift_amount);
                let ctors = matrix.column_constructors(0);

                let mut branches = SliceVec::new(ctx.scope, ctors.len());
                for ctor in ctors.iter() {
                    let r#const = ctor.as_const().unwrap();
                    let mut matrix = matrix.specialize(ctx, ctor);
                    let expr = compile_match(ctx, &mut matrix, bodies, shift_amount);
                    branches.push((*r#const, expr))
                }

                let default_branch = match Constructor::is_exhaustive(&ctors) {
                    true => None,
                    false => {
                        let name = None; // TODO: recover default branch name?
                        let mut matrix = matrix.default();

                        let value = ctx.eval_env().eval(&scrut_expr);
                        ctx.local_env.push_def(name, value, scrut.r#type.clone());
                        shift_amount.push();
                        let expr = compile_match(ctx, &mut matrix, bodies, shift_amount);
                        ctx.local_env.pop();

                        Some((name, ctx.scope.to_scope(expr) as &_))
                    }
                };

                return core::Term::ConstMatch(
                    Span::Empty,
                    ctx.scope.to_scope(scrut_expr),
                    branches.into(),
                    default_branch,
                );
            }

            // There is only one constructor for each record type,
            // so we only need to generate a single subtree (ie no branching needed)
            CheckedPattern::RecordLit(_, labels, _) => {
                let mut matrix = matrix.specialize(ctx, &Constructor::Record(labels));
                return compile_match(ctx, &mut matrix, bodies, shift_amount);
            }

            // Skip over non-constructor patterns
            CheckedPattern::ReportedError(_)
            | CheckedPattern::Placeholder(_)
            | CheckedPattern::Binder(_, _) => continue,
        }
    }

    unreachable!()
}

impl<'arena> PatMatrix<'arena> {
    /// Return the index of any column in the matrix with at least one
    /// non-wildcard pattern. At the moment, we simply selec the leftmost
    /// column, but more advanced splitting heuristcs can be used to
    /// minimize the size of the decision tree and potentially skip some tests
    /// altogether (see section 8 of *Compiling pattern matching to good
    /// decision trees*)
    pub fn column_to_split_on(&self) -> Option<usize> {
        assert!(!self.is_null(), "Cannot split null `PatternMatrix`");

        (0..self.num_columns().unwrap()).find(|&column| {
            self.column(column).any(|(pat, _)| match pat {
                CheckedPattern::ConstLit(_, _) | CheckedPattern::RecordLit(_, _, _) => true,
                CheckedPattern::ReportedError(_)
                | CheckedPattern::Placeholder(_)
                | CheckedPattern::Binder(_, _) => false,
            })
        })
    }

    pub fn swap_columns(&mut self, column1: usize, column2: usize) {
        assert!(
            column1 < self.num_columns().unwrap_or(0),
            "column1 is out of bounds (num_columns = {:?})",
            self.num_columns()
        );
        assert!(
            column2 < self.num_columns().unwrap_or(0),
            "column2 is out of bounds (num_columns = {:?})",
            self.num_columns()
        );

        for row in self.rows.iter_mut() {
            row.entries.swap(column1, column2);
        }
    }
}
