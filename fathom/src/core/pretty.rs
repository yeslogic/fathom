//! A pretty printer for the core language
//!
//! This is mainly intended for debugging.
//!
//! Example:
//!
//! ```
//! use codespan_reporting::term::termcolor::{BufferedStandardStream, ColorChoice};
//! use fathom::core::pretty::Context;
//! use fathom::core::Module;
//! use fathom::StringInterner;
//! use std::cell::RefCell;
//! use std::io::Write;
//!
//! // These are created for demonstration
//! let mut scope = scoped_arena::Scope::new();
//! let interner = RefCell::new(StringInterner::new());
//! let module = Module { items: &[] };
//!
//! let pp = Context::new(&interner, &mut scope);
//! let doc = pp.module(&module).into_doc();
//! let mut stream = BufferedStandardStream::stdout(ColorChoice::Auto);
//! let emit_width = 100;
//! writeln!(stream, "{}", doc.pretty(emit_width)).unwrap();
//! stream.flush().unwrap();
//! ```

use pretty::{Doc, DocAllocator, DocBuilder, DocPtr, RefDoc};
use scoped_arena::Scope;
use std::cell::RefCell;

use crate::core::{Item, Module, Term};
use crate::{StringId, StringInterner};

/// Term precedences
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Prec {
    Top = 0,
    Let,
    Group,
    Fun,
    App,
    Proj,
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

    pub fn module(&'arena self, module: &Module<'arena>) -> DocBuilder<'arena, Self> {
        self.intersperse(
            module.items.iter().map(|item| self.item(item)),
            self.hardline(),
        )
    }

    fn item(&'arena self, item: &Item<'arena>) -> DocBuilder<'arena, Self> {
        match item {
            Item::Def {
                label,
                r#type,
                expr,
            } => self
                .concat([
                    self.text("def"),
                    self.space(),
                    self.ann_pattern(Prec::Top, Some(*label), r#type),
                    self.space(),
                    self.text("="),
                    self.softline(),
                    self.term_prec(Prec::Group, expr),
                    self.text(";"),
                    self.hardline(),
                ])
                .group(),
        }
    }

    fn pattern(&'arena self, pattern: Option<StringId>) -> DocBuilder<'arena, Self> {
        match pattern {
            Some(name) => self.string_id(name),
            None => self.text("_"),
        }
    }

    fn ann_pattern(
        &'arena self,
        prec: Prec,
        pattern: Option<StringId>,
        r#type: &Term<'arena>,
    ) -> DocBuilder<'arena, Self> {
        self.paren(
            prec > Prec::Top,
            self.concat([
                self.concat([self.pattern(pattern), self.space(), self.text(":")])
                    .group(),
                self.softline(),
                self.term_prec(Prec::Top, r#type),
            ]),
        )
    }

    pub fn term(&'arena self, term: &Term<'arena>) -> DocBuilder<'arena, Self> {
        self.term_prec(Prec::Top, term)
    }

    fn term_prec(&'arena self, prec: Prec, term: &Term<'arena>) -> DocBuilder<'arena, Self> {
        // FIXME: indentation and grouping

        match term {
            Term::ItemVar(_, level) => self.text(format!("ItemVar({:?})", level)),
            Term::LocalVar(_, index) => self.text(format!("LocalVar({:?})", index)),
            Term::MetaVar(_, index) => self.text(format!("MetaVar({:?})", index)),
            Term::InsertedMeta(_, level, info) => {
                self.text(format!("InsertedMeta({:?}, {:?})", level, info))
            }
            Term::Ann(_, expr, r#type) => self.paren(
                prec > Prec::Top,
                self.concat([
                    self.concat([
                        self.term_prec(Prec::Let, expr),
                        self.space(),
                        self.text(":"),
                    ])
                    .group(),
                    self.softline(),
                    self.term_prec(Prec::Top, r#type),
                ]),
            ),
            Term::Let(_, def_pattern, def_type, def_expr, body_expr) => self.paren(
                prec > Prec::Let,
                self.concat([
                    self.concat([
                        self.text("let"),
                        self.space(),
                        self.ann_pattern(Prec::Top, *def_pattern, *def_type),
                        self.space(),
                        self.text("="),
                        self.softline(),
                        self.term_prec(Prec::Let, def_expr),
                        self.text(";"),
                    ])
                    .group(),
                    self.line(),
                    self.term_prec(Prec::Let, body_expr),
                ]),
            ),
            Term::Universe(_) => self.text("Type"),
            Term::FunType(_, param_name, param_type, body_type) => self.paren(
                prec > Prec::Fun,
                self.concat([
                    self.concat([
                        self.text("fun"),
                        // TODO: Share with Term::Ann
                        self.paren(
                            prec > Prec::Top,
                            self.concat([
                                self.concat([
                                    if let Some(name) = param_name {
                                        self.string_id(*name)
                                    } else {
                                        self.nil()
                                    },
                                    self.space(),
                                    self.text(":"),
                                ])
                                .group(),
                                self.softline(),
                                self.term_prec(Prec::Top, param_type),
                            ]),
                        ),
                        self.space(),
                        self.text("->"),
                    ])
                    .group(),
                    self.softline(),
                    self.term_prec(Prec::Fun, body_type),
                ]),
            ),
            Term::FunLit(_, param_name, body_expr) => self.paren(
                prec > Prec::Fun,
                self.concat([
                    self.concat([
                        self.text("fun"),
                        if let Some(name) = param_name {
                            self.string_id(*name)
                        } else {
                            self.nil()
                        },
                        self.space(),
                        self.text("=>"),
                    ])
                    .group(),
                    self.space(),
                    self.term_prec(Prec::Let, body_expr),
                ]),
            ),
            Term::FunApp(_, head_expr, arg_expr) => self.paren(
                prec > Prec::App,
                self.concat([
                    self.term_prec(Prec::Proj, head_expr),
                    self.space(),
                    self.term_prec(Prec::Proj, arg_expr),
                ]),
            ),
            Term::RecordType(_, labels, types) => self.sequence(
                self.text("{"),
                labels.iter().zip(types.iter()).map(|(&label, type_)| {
                    self.concat([
                        self.string_id(label),
                        self.space(),
                        self.text(":"),
                        self.space(),
                        self.term_prec(Prec::Top, type_),
                    ])
                }),
                self.text(","),
                self.text("}"),
            ),
            Term::RecordLit(_, labels, exprs) => self.sequence(
                self.text("{"),
                labels.iter().zip(exprs.iter()).map(|(&label, expr)| {
                    self.concat([
                        self.string_id(label),
                        self.space(),
                        self.text("="),
                        self.space(),
                        self.term_prec(Prec::Top, expr),
                    ])
                }),
                self.text(","),
                self.text("}"),
            ),
            // Term::UnitLiteral(_) => self.text("{}"),
            Term::RecordProj(_, head_expr, label) => self.concat([
                self.term_prec(Prec::Atomic, head_expr),
                self.text(".").append(self.string_id(*label)),
            ]),
            Term::ArrayLit(_, exprs) => self.sequence(
                self.text("["),
                exprs.iter().map(|expr| self.term_prec(Prec::Top, expr)),
                self.text(","),
                self.text("]"),
            ),
            Term::ConstLit(_, const_) => self.text(format!("{:?}", const_)),
            Term::FormatRecord(_, labels, formats) => self.sequence(
                self.text("{"),
                labels
                    .iter()
                    .zip(formats.iter())
                    .map(|(&label, format)| self.format_field(label, format)),
                self.text(","),
                self.text("}"),
            ),
            Term::FormatCond(_, label, format, cond) => self.concat([
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
            Term::FormatOverlap(_, labels, formats) => self.sequence(
                self.concat([self.text("overlap"), self.space(), self.text("{")]),
                labels
                    .iter()
                    .zip(formats.iter())
                    .map(|(&label, format)| self.format_field(label, format)),
                self.text(","),
                self.text("}"),
            ),
            Term::Prim(_, prim) => self.text(format!("{:?}", prim)),
            Term::ConstMatch(_, scrutinee, branches, default_expr) => self.sequence(
                self.concat([
                    self.text("match"),
                    self.space(),
                    self.term_prec(Prec::Proj, scrutinee),
                    self.space(),
                    self.text("{"),
                ]),
                branches
                    .iter()
                    .map(|(pattern, body_expr)| {
                        self.concat([
                            self.text(format!("{:?}", pattern)),
                            self.space(),
                            self.text("=>"),
                            self.space(),
                            self.term_prec(Prec::Top, body_expr),
                        ])
                    })
                    .chain(default_expr.iter().map(|default| {
                        self.concat([
                            self.text("_"),
                            self.space(),
                            self.text("=>"),
                            self.space(),
                            self.term_prec(Prec::Top, default),
                        ])
                    }))
                    .collect::<Vec<_>>()
                    .into_iter(),
                self.text(","),
                self.text("}"),
            ),
        }
    }

    fn format_field(
        &'arena self,
        label: StringId,
        format: &Term<'arena>,
    ) -> DocBuilder<'arena, Self> {
        self.concat([
            self.string_id(label),
            self.space(),
            self.text("<-"),
            self.space(),
            self.term_prec(Prec::Top, format),
        ])
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
            Doc::BorrowedText("where") => &Doc::BorrowedText("where"),
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
