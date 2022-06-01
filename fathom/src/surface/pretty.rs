use pretty::{Doc, DocAllocator, DocBuilder, DocPtr, RefDoc};
use scoped_arena::Scope;
use std::cell::RefCell;

use crate::surface::{Pattern, Term};
use crate::{StringId, StringInterner};

/// Term precedences
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Prec {
    Top = 0,
    Let,
    Fun,
    App,
    Atomic,
}

const INDENT: isize = 4;

pub struct Context<'interner, 'arena> {
    interner: &'interner RefCell<StringInterner>,
    scope: &'arena Scope<'arena>,
}

impl<'interner, 'arena> Context<'interner, 'arena> {
    pub fn new(
        interner: &'interner RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
    ) -> Context<'interner, 'arena> {
        Context { interner, scope }
    }

    fn string_id(&'arena self, name: StringId) -> DocBuilder<'arena, Self> {
        match self.interner.borrow().resolve(name) {
            Some(name) => self.text(name.to_owned()),
            None => self.text("#error"),
        }
    }

    fn pattern<Range>(&'arena self, pattern: &Pattern<Range>) -> DocBuilder<'arena, Self> {
        match pattern {
            Pattern::Placeholder(_) => self.text("_"),
            Pattern::Name(_, name) => self.string_id(*name),
            Pattern::StringLiteral(_, number) => self.string_id(*number),
            Pattern::NumberLiteral(_, number) => self.string_id(*number),
            Pattern::BooleanLiteral(_, boolean) => match *boolean {
                true => self.text("true"),
                false => self.text("false"),
            },
        }
    }

    fn ann_pattern<Range>(
        &'arena self,
        prec: Prec,
        pattern: &Pattern<Range>,
        r#type: Option<&Term<'_, Range>>,
    ) -> DocBuilder<'arena, Self> {
        match r#type {
            None => self.pattern(pattern),
            Some(r#type) => self.paren(
                prec > Prec::Top,
                self.concat([
                    self.concat([self.pattern(&pattern), self.space(), self.text(":")])
                        .group(),
                    self.softline(),
                    self.term_prec(Prec::Top, &r#type),
                ]),
            ),
        }
    }

    pub fn term<Range>(&'arena self, term: &Term<'_, Range>) -> DocBuilder<'arena, Self> {
        self.term_prec(Prec::Top, term)
    }

    pub fn term_prec<Range>(
        &'arena self,
        prec: Prec,
        term: &Term<'_, Range>,
    ) -> DocBuilder<'arena, Self> {
        // FIXME: indentation and grouping

        match term {
            Term::Name(_, name) => self.string_id(*name),
            Term::Hole(_, name) => self.concat([self.text("?"), self.string_id(*name)]),
            Term::Placeholder(_) => self.text("_"),
            Term::Ann(_, expr, r#type) => self.paren(
                prec > Prec::Top,
                self.concat([
                    self.concat([
                        self.term_prec(Prec::Let, &expr),
                        self.space(),
                        self.text(":"),
                    ])
                    .group(),
                    self.softline(),
                    self.term_prec(Prec::Top, &r#type),
                ]),
            ),
            Term::Let(_, def_pattern, def_type, def_expr, output_expr) => self.paren(
                prec > Prec::Let,
                self.concat([
                    self.concat([
                        self.text("let"),
                        self.space(),
                        self.ann_pattern(Prec::Top, def_pattern, *def_type),
                        self.space(),
                        self.text("="),
                        self.softline(),
                        self.term_prec(Prec::Let, def_expr),
                        self.text(";"),
                    ])
                    .group(),
                    self.line(),
                    self.term_prec(Prec::Let, output_expr),
                ]),
            ),
            Term::Match(_, scrutinee, equations) => self.sequence(
                self.concat([
                    self.text("match"),
                    self.space(),
                    self.term_prec(Prec::Atomic, scrutinee),
                    self.space(),
                    self.text("{"),
                ]),
                equations.iter().map(|(pattern, output_expr)| {
                    self.concat([
                        self.pattern(pattern),
                        self.space(),
                        self.text("=>"),
                        self.space(),
                        self.term_prec(Prec::Top, r#output_expr),
                    ])
                }),
                self.text(","),
                self.text("}"),
            ),
            Term::Universe(_) => self.text("Type"),
            Term::FunType(_, input_pattern, input_type, output_type) => self.paren(
                prec > Prec::Fun,
                self.concat([
                    self.concat([
                        self.text("fun"),
                        self.space(),
                        self.ann_pattern(Prec::Atomic, input_pattern, *input_type),
                        self.space(),
                        self.text("->"),
                    ])
                    .group(),
                    self.softline(),
                    self.term_prec(Prec::Fun, output_type),
                ]),
            ),
            Term::Arrow(_, input_type, output_type) => self.paren(
                prec > Prec::Fun,
                self.concat([
                    self.term_prec(Prec::App, input_type),
                    self.softline(),
                    self.text("->"),
                    self.softline(),
                    self.term_prec(Prec::Fun, output_type),
                ]),
            ),
            Term::FunLiteral(_, input_pattern, input_type, output_expr) => self.paren(
                prec > Prec::Fun,
                self.concat([
                    self.concat([
                        self.text("fun"),
                        self.space(),
                        self.ann_pattern(Prec::Atomic, input_pattern, *input_type),
                        self.space(),
                        self.text("=>"),
                    ])
                    .group(),
                    self.space(),
                    self.term_prec(Prec::Let, output_expr),
                ]),
            ),
            Term::App(_, head_expr, input_expr) => self.paren(
                prec > Prec::App,
                self.concat([
                    self.term_prec(Prec::App, head_expr),
                    self.space(),
                    self.term_prec(Prec::Atomic, input_expr),
                ]),
            ),
            Term::RecordType(_, type_fields) => self.sequence(
                self.text("{"),
                type_fields.iter().map(|((_, label), r#type)| {
                    self.concat([
                        self.string_id(*label),
                        self.space(),
                        self.text(":"),
                        self.space(),
                        self.term_prec(Prec::Top, r#type),
                    ])
                }),
                self.text(","),
                self.text("}"),
            ),
            Term::RecordLiteral(_, expr_fields) => self.sequence(
                self.text("{"),
                expr_fields.iter().map(|((_, label), r#expr)| {
                    self.concat([
                        self.string_id(*label),
                        self.space(),
                        self.text("="),
                        self.space(),
                        self.term_prec(Prec::Top, r#expr),
                    ])
                }),
                self.text(","),
                self.text("}"),
            ),
            Term::UnitLiteral(_) => self.text("{}"),
            Term::Proj(_, head_expr, (_, label)) => self.concat([
                self.term_prec(Prec::Atomic, head_expr),
                self.text("."),
                self.string_id(*label),
            ]),
            Term::ArrayLiteral(_, exprs) => self.sequence(
                self.text("["),
                exprs.iter().map(|expr| self.term_prec(Prec::Top, expr)),
                self.text(","),
                self.text("]"),
            ),
            Term::StringLiteral(_, number) => {
                self.concat([self.text("\""), self.string_id(*number), self.text("\"")])
            }
            Term::NumberLiteral(_, number) => self.string_id(*number),
            Term::BooleanLiteral(_, boolean) => match *boolean {
                true => self.text("true"),
                false => self.text("false"),
            },
            Term::FormatRecord(_, format_fields) => self.sequence(
                self.text("{"),
                format_fields.iter().map(|((_, label), format)| {
                    self.concat([
                        self.string_id(*label),
                        self.space(),
                        self.text("<-"),
                        self.space(),
                        self.term_prec(Prec::Top, format),
                    ])
                }),
                self.text(","),
                self.text("}"),
            ),
            Term::FormatCond(_, (_, label), format, cond) => self.concat([
                self.text("{"),
                self.space(),
                self.string_id(*label),
                self.space(),
                self.text("<-"),
                self.space(),
                self.term_prec(Prec::Top, format),
                self.space(),
                self.text("|"),
                self.space(),
                self.term_prec(Prec::Top, cond),
                self.space(),
                self.text("}"),
            ]),
            Term::FormatOverlap(_, format_fields) => self.sequence(
                self.concat([self.text("overlap"), self.space(), self.text("{")]),
                format_fields.iter().map(|((_, label), format)| {
                    self.concat([
                        self.string_id(*label),
                        self.space(),
                        self.text("<-"),
                        self.space(),
                        self.term_prec(Prec::Top, format),
                    ])
                }),
                self.text(","),
                self.text("}"),
            ),
            Term::ReportedError(_) => self.text("#error"),
        }
    }

    /// Wrap a document in parens.
    fn paren(&'arena self, wrap: bool, doc: DocBuilder<'arena, Self>) -> DocBuilder<'arena, Self> {
        if wrap {
            self.concat([self.text("("), doc, self.text(")")])
        } else {
            doc
        }
    }

    /// Pretty prints a delimited sequence of documents with a trailing
    /// separator if it is formatted over multiple lines.
    pub fn sequence(
        &'arena self,
        start_delim: DocBuilder<'arena, Self>,
        docs: impl ExactSizeIterator<Item = DocBuilder<'arena, Self>> + Clone,
        separator: DocBuilder<'arena, Self>,
        end_delim: DocBuilder<'arena, Self>,
    ) -> DocBuilder<'arena, Self> {
        if docs.len() == 0 {
            self.concat([start_delim, end_delim])
        } else {
            DocBuilder::flat_alt(
                self.concat([
                    start_delim.clone(),
                    self.concat(
                        docs.clone()
                            .map(|doc| self.concat([self.hardline(), doc, separator.clone()])),
                    )
                    .nest(INDENT),
                    self.hardline(),
                    end_delim.clone(),
                ]),
                self.concat([
                    start_delim,
                    self.space(),
                    self.intersperse(docs, self.concat([separator, self.space()])),
                    self.space(),
                    end_delim,
                ]),
            )
            .group()
        }
    }
}

impl<'interner, 'arena, A: 'arena> DocAllocator<'arena, A> for Context<'interner, 'arena> {
    type Doc = RefDoc<'arena, A>;

    #[inline]
    fn alloc(&'arena self, doc: Doc<'arena, Self::Doc, A>) -> Self::Doc {
        // Based on the `DocAllocator` implementation for `pretty::Arena`
        RefDoc(match doc {
            // Return 'static references for common variants to avoid some allocations
            Doc::Nil => &Doc::Nil,
            Doc::Hardline => &Doc::Hardline,
            Doc::Fail => &Doc::Fail,
            // space()
            Doc::BorrowedText(" ") => &Doc::BorrowedText(" "),
            // line()
            Doc::FlatAlt(RefDoc(Doc::Hardline), RefDoc(Doc::BorrowedText(" "))) => {
                &Doc::FlatAlt(RefDoc(&Doc::Hardline), RefDoc(&Doc::BorrowedText(" ")))
            }
            // line_()
            Doc::FlatAlt(RefDoc(Doc::Hardline), RefDoc(Doc::Nil)) => {
                &Doc::FlatAlt(RefDoc(&Doc::Hardline), RefDoc(&Doc::Nil))
            }
            // softline()
            Doc::Group(RefDoc(Doc::FlatAlt(
                RefDoc(Doc::Hardline),
                RefDoc(Doc::BorrowedText(" ")),
            ))) => &Doc::Group(RefDoc(&Doc::FlatAlt(
                RefDoc(&Doc::Hardline),
                RefDoc(&Doc::BorrowedText(" ")),
            ))),
            // softline_()
            Doc::Group(RefDoc(Doc::FlatAlt(RefDoc(Doc::Hardline), RefDoc(Doc::Nil)))) => {
                &Doc::Group(RefDoc(&Doc::FlatAlt(
                    RefDoc(&Doc::Hardline),
                    RefDoc(&Doc::Nil),
                )))
            }

            // Language tokens
            Doc::BorrowedText("fun") => &Doc::BorrowedText("fun"),
            Doc::BorrowedText("let") => &Doc::BorrowedText("let"),
            Doc::BorrowedText("overlap") => &Doc::BorrowedText("overlap"),
            Doc::BorrowedText("Type") => &Doc::BorrowedText("Type"),
            Doc::BorrowedText(":") => &Doc::BorrowedText(":"),
            Doc::BorrowedText(",") => &Doc::BorrowedText(","),
            Doc::BorrowedText("=") => &Doc::BorrowedText("="),
            Doc::BorrowedText("=>") => &Doc::BorrowedText("=>"),
            Doc::BorrowedText(".") => &Doc::BorrowedText("."),
            Doc::BorrowedText("->") => &Doc::BorrowedText("->"),
            Doc::BorrowedText("<-") => &Doc::BorrowedText("<-"),
            Doc::BorrowedText(";") => &Doc::BorrowedText(";"),
            Doc::BorrowedText("_") => &Doc::BorrowedText("_"),
            Doc::BorrowedText("{") => &Doc::BorrowedText("{"),
            Doc::BorrowedText("}") => &Doc::BorrowedText("}"),
            Doc::BorrowedText("[") => &Doc::BorrowedText("["),
            Doc::BorrowedText("]") => &Doc::BorrowedText("]"),
            Doc::BorrowedText("(") => &Doc::BorrowedText("("),
            Doc::BorrowedText(")") => &Doc::BorrowedText(")"),

            _ => self.scope.to_scope(doc),
        })
    }

    fn alloc_column_fn(
        &'arena self,
        f: impl 'arena + Fn(usize) -> Self::Doc,
    ) -> <Self::Doc as DocPtr<'arena, A>>::ColumnFn {
        self.scope.to_scope(f)
    }

    fn alloc_width_fn(
        &'arena self,
        f: impl 'arena + Fn(isize) -> Self::Doc,
    ) -> <Self::Doc as DocPtr<'arena, A>>::WidthFn {
        self.scope.to_scope(f)
    }
}
