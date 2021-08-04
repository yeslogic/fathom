use pretty::{DocAllocator, DocBuilder};

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

pub struct Context<'doc, D> {
    interner: &'doc StringInterner,
    alloc: &'doc D,
}

impl<'doc, D> std::ops::Deref for Context<'doc, D> {
    type Target = &'doc D;

    // FIXME: `&&'doc D` is a bit ick... egh
    fn deref(&self) -> &&'doc D {
        &self.alloc
    }
}

impl<'doc, D> Context<'doc, D>
where
    D: DocAllocator<'doc>,
{
    pub fn new(interner: &'doc StringInterner, alloc: &'doc D) -> Context<'doc, D> {
        Context { interner, alloc }
    }

    pub fn name(&self, name: StringId) -> DocBuilder<'doc, D> {
        self.text(self.interner.resolve(name).unwrap_or("<ERROR>"))
    }

    pub fn ann(&self, expr: &Term<'_>, r#type: &Term<'_>) -> DocBuilder<'doc, D> {
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

    pub fn paren(&self, wrap: bool, doc: DocBuilder<'doc, D>) -> DocBuilder<'doc, D> {
        if wrap {
            self.concat([self.text("("), doc, self.text(")")])
        } else {
            doc
        }
    }

    pub fn term(&self, term: &Term<'_>) -> DocBuilder<'doc, D> {
        self.term_prec(Prec::Top, term)
    }

    pub fn term_prec(&self, prec: Prec, term: &Term<'_>) -> DocBuilder<'doc, D> {
        // FIXME: indentation and grouping

        match term {
            Term::Name(_, name) => self.name(*name),
            Term::Hole(_, None) => self.text("_"),
            Term::Hole(_, Some(name)) => self.concat([self.text("_"), self.name(*name)]),
            Term::Ann(expr, r#type) => self.ann(expr, r#type),
            Term::Let(_, (_, def_name), def_type, def_expr, output_expr) => self.paren(
                prec > Prec::Let,
                self.concat([
                    self.concat([
                        self.text("let"),
                        self.space(),
                        self.name(*def_name),
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
                        self.name(*input_name),
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
            Term::FunArrow(input_type, output_type) => self.paren(
                prec > Prec::Fun,
                self.concat([
                    self.term_prec(Prec::App, input_type),
                    self.softline(),
                    self.text("->"),
                    self.softline(),
                    self.term_prec(Prec::Fun, output_type),
                ]),
            ),
            Term::FunIntro(_, (_, input_name), output_expr) => self.paren(
                prec > Prec::Fun,
                self.concat([
                    self.concat([
                        self.text("fun"),
                        self.space(),
                        self.name(*input_name),
                        self.space(),
                        self.text("=>"),
                    ])
                    .group(),
                    self.space(),
                    self.term_prec(Prec::Fun, output_expr),
                ]),
            ),
            Term::FunElim(head_expr, input_expr) => self.paren(
                prec > Prec::App,
                self.concat([
                    self.term_prec(Prec::App, head_expr),
                    self.space(),
                    self.term_prec(Prec::Atomic, input_expr),
                ]),
            ),
            Term::ReportedError(_) => self.text("_"),
        }
    }
}
