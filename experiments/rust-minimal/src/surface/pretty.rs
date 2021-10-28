use pretty::{Doc, DocAllocator, DocBuilder, DocPtr, RefDoc};
use scoped_arena::Scope;

use crate::surface::Term;
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

pub struct Context<'doc> {
    interner: &'doc StringInterner,
    scope: &'doc Scope<'doc>,
}

impl<'doc> Context<'doc> {
    pub fn new(interner: &'doc StringInterner, scope: &'doc Scope<'doc>) -> Context<'doc> {
        Context { interner, scope }
    }

    pub fn string_id(&'doc self, name: StringId) -> DocBuilder<'doc, Self> {
        self.text(self.interner.resolve(name).unwrap_or("<ERROR>"))
    }

    pub fn ann<Range>(
        &'doc self,
        expr: &Term<'_, Range>,
        r#type: &Term<'_, Range>,
    ) -> DocBuilder<'doc, Self> {
        self.concat([
            self.concat([
                self.term_prec(Prec::Let, &expr),
                self.space(),
                self.text(":"),
            ])
            .group(),
            self.softline(),
            self.term_prec(Prec::Top, &r#type),
        ])
    }

    pub fn paren(&'doc self, wrap: bool, doc: DocBuilder<'doc, Self>) -> DocBuilder<'doc, Self> {
        if wrap {
            self.concat([self.text("("), doc, self.text(")")])
        } else {
            doc
        }
    }

    pub fn term<Range>(&'doc self, term: &Term<'_, Range>) -> DocBuilder<'doc, Self> {
        self.term_prec(Prec::Top, term)
    }

    pub fn term_prec<Range>(
        &'doc self,
        prec: Prec,
        term: &Term<'_, Range>,
    ) -> DocBuilder<'doc, Self> {
        // FIXME: indentation and grouping

        match term {
            Term::Name(_, name) => self.string_id(*name),
            Term::Hole(_, None) => self.text("_"),
            Term::Hole(_, Some(name)) => self.concat([self.text("_"), self.string_id(*name)]),
            Term::Ann(_, expr, r#type) => self.ann(expr, r#type),
            Term::Let(_, (_, def_name), def_type, def_expr, output_expr) => self.paren(
                prec > Prec::Let,
                self.concat([
                    self.concat([
                        self.text("let"),
                        self.space(),
                        self.string_id(*def_name),
                        self.space(),
                        match def_type {
                            None => self.nil(),
                            Some(def_type) => self.concat([
                                self.text(":"),
                                self.softline(),
                                self.term_prec(Prec::Fun, def_type),
                                self.space(),
                            ]),
                        },
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
            Term::Universe(_) => self.text("Type"),
            Term::FunType(_, (_, input_name), input_type, output_type) => self.paren(
                prec > Prec::Fun,
                self.concat([
                    self.concat([
                        self.text("fun"),
                        self.space(),
                        self.text("("),
                        self.string_id(*input_name),
                        self.space(),
                        self.text(":"),
                        self.softline(),
                        self.term_prec(Prec::Top, input_type),
                        self.text(")"),
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
            Term::FunLiteral(_, (_, input_name), output_expr) => self.paren(
                prec > Prec::Fun,
                self.concat([
                    self.concat([
                        self.text("fun"),
                        self.space(),
                        self.string_id(*input_name),
                        self.space(),
                        self.text("=>"),
                    ])
                    .group(),
                    self.space(),
                    self.term_prec(Prec::Fun, output_expr),
                ]),
            ),
            Term::FunElim(_, head_expr, input_expr) => self.paren(
                prec > Prec::App,
                self.concat([
                    self.term_prec(Prec::App, head_expr),
                    self.space(),
                    self.term_prec(Prec::Atomic, input_expr),
                ]),
            ),
            Term::RecordType(_, type_fields) => self.concat([
                self.text("{"),
                self.space(),
                self.intersperse(
                    type_fields.iter().map(|((_, label), r#type)| {
                        self.concat([
                            self.string_id(*label),
                            self.space(),
                            self.text(":"),
                            self.space(),
                            self.term_prec(Prec::Top, r#type),
                        ])
                    }),
                    self.concat([self.text(","), self.space()]),
                ),
                self.space(),
                self.text("}"),
            ]),
            Term::RecordLiteral(_, expr_fields) => self.concat([
                self.text("{"),
                self.space(),
                self.intersperse(
                    expr_fields.iter().map(|((_, label), r#expr)| {
                        self.concat([
                            self.string_id(*label),
                            self.space(),
                            self.text("="),
                            self.space(),
                            self.term_prec(Prec::Top, r#expr),
                        ])
                    }),
                    self.concat([self.text(","), self.space()]),
                ),
                self.space(),
                self.text("}"),
            ]),
            Term::UnitLiteral(_) => self.text("{}"),
            Term::RecordElim(_, head_expr, (_, label)) => self.concat([
                self.term_prec(Prec::Atomic, head_expr),
                self.text("."),
                self.string_id(*label),
            ]),
            Term::ArrayLiteral(_, exprs) => self.concat([
                self.text("["),
                self.space(),
                self.intersperse(
                    exprs.iter().map(|expr| self.term_prec(Prec::Top, expr)),
                    self.concat([self.text(","), self.space()]),
                ),
                self.space(),
                self.text("]"),
            ]),
            Term::NumberLiteral(_, number) => self.string_id(*number),
            Term::FormatRecord(_, format_fields) => self.concat([
                self.text("{"),
                self.space(),
                self.intersperse(
                    format_fields.iter().map(|((_, label), format)| {
                        self.concat([
                            self.string_id(*label),
                            self.space(),
                            self.text("<-"),
                            self.space(),
                            self.term_prec(Prec::Top, format),
                        ])
                    }),
                    self.concat([self.text(","), self.space()]),
                ),
                self.space(),
                self.text("}"),
            ]),
            Term::ReportedError(_) => self.text("reported_error"),
        }
    }
}

// NOTE: based on the `DocAllocator` implementation for `pretty::Arena`
impl<'doc, A: 'doc> DocAllocator<'doc, A> for Context<'doc> {
    type Doc = RefDoc<'doc, A>;

    #[inline]
    fn alloc(&'doc self, doc: Doc<'doc, Self::Doc, A>) -> Self::Doc {
        RefDoc(match doc {
            // Return 'static references for common variants to avoid some allocations
            Doc::Nil => &Doc::Nil,
            Doc::Line => &Doc::Line,
            Doc::Fail => &Doc::Fail,
            // space()
            Doc::BorrowedText(" ") => &Doc::BorrowedText(" "),
            // line()
            Doc::FlatAlt(RefDoc(Doc::Line), RefDoc(Doc::BorrowedText(" "))) => {
                &Doc::FlatAlt(RefDoc(&Doc::Line), RefDoc(&Doc::BorrowedText(" ")))
            }
            // line_()
            Doc::FlatAlt(RefDoc(Doc::Line), RefDoc(Doc::Nil)) => {
                &Doc::FlatAlt(RefDoc(&Doc::Line), RefDoc(&Doc::Nil))
            }
            // softline()
            Doc::Group(RefDoc(Doc::FlatAlt(RefDoc(Doc::Line), RefDoc(Doc::BorrowedText(" "))))) => {
                &Doc::Group(RefDoc(&Doc::FlatAlt(
                    RefDoc(&Doc::Line),
                    RefDoc(&Doc::BorrowedText(" ")),
                )))
            }
            // softline_()
            Doc::Group(RefDoc(Doc::FlatAlt(RefDoc(Doc::Line), RefDoc(Doc::Nil)))) => {
                &Doc::Group(RefDoc(&Doc::FlatAlt(RefDoc(&Doc::Line), RefDoc(&Doc::Nil))))
            }
            _ => self.scope.to_scope(doc),
        })
    }

    fn alloc_column_fn(
        &'doc self,
        f: impl 'doc + Fn(usize) -> Self::Doc,
    ) -> <Self::Doc as DocPtr<'doc, A>>::ColumnFn {
        self.scope.to_scope(f)
    }

    fn alloc_width_fn(
        &'doc self,
        f: impl 'doc + Fn(isize) -> Self::Doc,
    ) -> <Self::Doc as DocPtr<'doc, A>>::WidthFn {
        self.scope.to_scope(f)
    }
}
