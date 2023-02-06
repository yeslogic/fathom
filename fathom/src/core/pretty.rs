//! A pretty printer for the core language
//!
//! This is mainly intended for debugging.
//!
//! Example:
//!
//! ```
//! use std::cell::RefCell;
//! use std::io::Write;
//!
//! use codespan_reporting::term::termcolor::{BufferedStandardStream, ColorChoice};
//! use fathom::core::pretty::Context;
//! use fathom::core::Module;
//! use fathom::files::FileId;
//!
//! // These are created for demonstration
//! let module = Module { items: &[] };
//!
//! let pp = Context::new();
//! let doc = pp.module(&module);
//! let mut stream = BufferedStandardStream::stdout(ColorChoice::Auto);
//! let emit_width = 100;
//! writeln!(stream, "{}", doc.pretty(emit_width)).unwrap();
//! stream.flush().unwrap();
//! ```

use pretty::RcDoc;

use crate::core::{Item, Module, Plicity, Term};
use crate::surface::lexer::is_keyword;
use crate::symbol::Symbol;

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

pub struct Context {}

impl<'arena> Context {
    pub fn new() -> Context {
        Context {}
    }

    fn ident(&'arena self, name: Symbol) -> RcDoc {
        match name.resolve() {
            name if is_keyword(name) => RcDoc::text("r#").append(RcDoc::text(name.to_owned())),
            name => RcDoc::text(name.to_owned()),
        }
    }

    pub fn module(&'arena self, module: &Module<'arena>) -> RcDoc {
        RcDoc::intersperse(
            module.items.iter().map(|item| self.item(item)),
            RcDoc::hardline(),
        )
    }

    fn item(&'arena self, item: &Item<'arena>) -> RcDoc {
        match item {
            Item::Def {
                label,
                r#type,
                expr,
            } => RcDoc::concat([
                RcDoc::text("def"),
                RcDoc::space(),
                self.ann_pattern(Prec::Top, Some(*label), r#type),
                RcDoc::space(),
                RcDoc::text("="),
                RcDoc::softline(),
                self.term_prec(Prec::Group, expr),
                RcDoc::text(";"),
                RcDoc::hardline(),
            ])
            .group(),
        }
    }

    fn pattern(&'arena self, pattern: Option<Symbol>) -> RcDoc {
        match pattern {
            Some(name) => self.ident(name),
            None => RcDoc::text("_"),
        }
    }

    fn ann_pattern(
        &'arena self,
        prec: Prec,
        pattern: Option<Symbol>,
        r#type: &Term<'arena>,
    ) -> RcDoc {
        self.paren(
            prec > Prec::Top,
            RcDoc::concat([
                RcDoc::concat([self.pattern(pattern), RcDoc::space(), RcDoc::text(":")]).group(),
                RcDoc::softline(),
                self.term_prec(Prec::Top, r#type),
            ]),
        )
    }

    fn plicity(&'arena self, plicity: Plicity) -> RcDoc {
        match plicity {
            Plicity::Explicit => RcDoc::nil(),
            Plicity::Implicit => RcDoc::text("@"),
        }
    }

    pub fn term(&'arena self, term: &Term<'arena>) -> RcDoc {
        self.term_prec(Prec::Top, term)
    }

    fn term_prec(&'arena self, prec: Prec, term: &Term<'arena>) -> RcDoc {
        // FIXME: indentation and grouping

        match term {
            Term::ItemVar(_, level) => RcDoc::text(format!("Item({level})")),
            Term::LocalVar(_, index) => RcDoc::text(format!("Local({index})")),
            Term::MetaVar(_, index) => RcDoc::text(format!("Meta({index})")),
            Term::InsertedMeta(_, level, info) => {
                RcDoc::text(format!("InsertedMeta({level:?}, {info:?})"))
            }
            Term::Ann(_, expr, r#type) => self.paren(
                prec > Prec::Top,
                RcDoc::concat([
                    RcDoc::concat([
                        self.term_prec(Prec::Let, expr),
                        RcDoc::space(),
                        RcDoc::text(":"),
                    ])
                    .group(),
                    RcDoc::softline(),
                    self.term_prec(Prec::Top, r#type),
                ]),
            ),
            Term::Let(_, def, body_expr) => self.paren(
                prec > Prec::Let,
                RcDoc::concat([
                    RcDoc::concat([
                        RcDoc::text("let"),
                        RcDoc::space(),
                        self.ann_pattern(Prec::Top, def.name, &def.r#type),
                        RcDoc::space(),
                        RcDoc::text("="),
                        RcDoc::softline(),
                        self.term_prec(Prec::Let, &def.expr),
                        RcDoc::text(";"),
                    ])
                    .group(),
                    RcDoc::line(),
                    self.term_prec(Prec::Let, body_expr),
                ]),
            ),
            Term::Universe(_) => RcDoc::text("Type"),
            Term::FunType(_, plicity, param_name, param_type, body_type) => self.paren(
                prec > Prec::Fun,
                RcDoc::concat([
                    RcDoc::concat([
                        RcDoc::text("fun"),
                        RcDoc::space(),
                        // TODO: Share with Term::Ann
                        self.paren(
                            prec > Prec::Top,
                            RcDoc::concat([
                                RcDoc::concat([
                                    self.plicity(*plicity),
                                    match param_name {
                                        Some(name) => self.ident(*name),
                                        None => RcDoc::text("_"),
                                    },
                                    RcDoc::space(),
                                    RcDoc::text(":"),
                                ])
                                .group(),
                                RcDoc::softline(),
                                self.term_prec(Prec::Top, param_type),
                            ]),
                        ),
                        RcDoc::space(),
                        RcDoc::text("->"),
                    ])
                    .group(),
                    RcDoc::softline(),
                    self.term_prec(Prec::Fun, body_type),
                ]),
            ),
            Term::FunLit(_, plicity, param_name, body_expr) => self.paren(
                prec > Prec::Fun,
                RcDoc::concat([
                    RcDoc::concat([
                        RcDoc::text("fun"),
                        RcDoc::space(),
                        self.plicity(*plicity),
                        match param_name {
                            Some(name) => self.ident(*name),
                            None => RcDoc::text("_"),
                        },
                        RcDoc::space(),
                        RcDoc::text("=>"),
                    ])
                    .group(),
                    RcDoc::space(),
                    self.term_prec(Prec::Let, body_expr),
                ]),
            ),
            Term::FunApp(_, plicity, head_expr, arg_expr) => self.paren(
                prec > Prec::App,
                RcDoc::concat([
                    self.plicity(*plicity),
                    self.term_prec(Prec::Proj, head_expr),
                    RcDoc::space(),
                    self.term_prec(Prec::Proj, arg_expr),
                ]),
            ),
            Term::RecordType(_, labels, types) => self.sequence(
                RcDoc::text("{"),
                labels.iter().zip(types.iter()).map(|(&label, type_)| {
                    RcDoc::concat([
                        self.ident(label),
                        RcDoc::space(),
                        RcDoc::text(":"),
                        RcDoc::space(),
                        self.term_prec(Prec::Top, type_),
                    ])
                }),
                RcDoc::text(","),
                RcDoc::text("}"),
            ),
            Term::RecordLit(_, labels, exprs) => self.sequence(
                RcDoc::text("{"),
                labels.iter().zip(exprs.iter()).map(|(&label, expr)| {
                    RcDoc::concat([
                        self.ident(label),
                        RcDoc::space(),
                        RcDoc::text("="),
                        RcDoc::space(),
                        self.term_prec(Prec::Top, expr),
                    ])
                }),
                RcDoc::text(","),
                RcDoc::text("}"),
            ),
            Term::RecordProj(_, head_expr, label) => RcDoc::concat([
                self.term_prec(Prec::Atomic, head_expr),
                RcDoc::text(".").append(self.ident(*label)),
            ]),
            Term::ArrayLit(_, exprs) => self.sequence(
                RcDoc::text("["),
                exprs.iter().map(|expr| self.term_prec(Prec::Top, expr)),
                RcDoc::text(","),
                RcDoc::text("]"),
            ),
            Term::ConstLit(_, const_) => RcDoc::text(format!("{const_:?}")),
            Term::FormatRecord(_, labels, formats) => self.sequence(
                RcDoc::text("{"),
                labels
                    .iter()
                    .zip(formats.iter())
                    .map(|(&label, format)| self.format_field(label, format)),
                RcDoc::text(","),
                RcDoc::text("}"),
            ),
            Term::FormatCond(_, label, format, cond) => RcDoc::concat([
                RcDoc::text("{"),
                RcDoc::space(),
                self.ident(*label),
                RcDoc::space(),
                RcDoc::text("<-"),
                RcDoc::space(),
                self.term_prec(Prec::Top, format),
                RcDoc::space(),
                RcDoc::text("|"),
                RcDoc::space(),
                self.term_prec(Prec::Top, cond),
                RcDoc::space(),
                RcDoc::text("}"),
            ]),
            Term::FormatOverlap(_, labels, formats) => self.sequence(
                RcDoc::concat([RcDoc::text("overlap"), RcDoc::space(), RcDoc::text("{")]),
                labels
                    .iter()
                    .zip(formats.iter())
                    .map(|(&label, format)| self.format_field(label, format)),
                RcDoc::text(","),
                RcDoc::text("}"),
            ),
            Term::Prim(_, prim) => RcDoc::text(format!("{prim:?}")),
            Term::ConstMatch(_, scrutinee, branches, default_expr) => self.sequence(
                RcDoc::concat([
                    RcDoc::text("match"),
                    RcDoc::space(),
                    self.term_prec(Prec::Proj, scrutinee),
                    RcDoc::space(),
                    RcDoc::text("{"),
                ]),
                branches
                    .iter()
                    .map(|(pattern, body_expr)| {
                        RcDoc::concat([
                            RcDoc::text(format!("{pattern:?}")),
                            RcDoc::space(),
                            RcDoc::text("=>"),
                            RcDoc::space(),
                            self.term_prec(Prec::Top, body_expr),
                        ])
                    })
                    .chain(default_expr.iter().map(|&(name, default)| {
                        RcDoc::concat([
                            match name {
                                Some(name) => self.ident(name),
                                None => RcDoc::text("_"),
                            },
                            RcDoc::space(),
                            RcDoc::text("=>"),
                            RcDoc::space(),
                            self.term_prec(Prec::Top, default),
                        ])
                    }))
                    .collect::<Vec<_>>()
                    .into_iter(),
                RcDoc::text(","),
                RcDoc::text("}"),
            ),
        }
    }

    fn format_field(&'arena self, label: Symbol, format: &Term<'arena>) -> RcDoc {
        RcDoc::concat([
            self.ident(label),
            RcDoc::space(),
            RcDoc::text("<-"),
            RcDoc::space(),
            self.term_prec(Prec::Top, format),
        ])
    }

    /// Wrap a document in parens.
    fn paren(&'arena self, wrap: bool, doc: RcDoc<'arena>) -> RcDoc {
        if wrap {
            RcDoc::concat([RcDoc::text("("), doc, RcDoc::text(")")])
        } else {
            doc
        }
    }

    /// Pretty prints a delimited sequence of documents with a trailing
    /// separator if it is formatted over multiple lines.
    pub fn sequence(
        &'arena self,
        start_delim: RcDoc<'arena>,
        docs: impl ExactSizeIterator<Item = RcDoc<'arena, ()>> + Clone,
        separator: RcDoc<'arena>,
        end_delim: RcDoc<'arena>,
    ) -> RcDoc {
        if docs.len() == 0 {
            return RcDoc::concat([start_delim, end_delim]);
        }

        let docs = RcDoc::intersperse(docs, RcDoc::concat([separator.clone(), RcDoc::line()]));

        RcDoc::concat([
            start_delim,
            RcDoc::concat([
                RcDoc::line(),
                docs,
                RcDoc::flat_alt(separator, RcDoc::nil()),
            ])
            .nest(INDENT),
            RcDoc::line(),
            end_delim,
        ])
        .group()
    }
}
