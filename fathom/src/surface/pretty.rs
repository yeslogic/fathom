use std::cell::RefCell;

use pretty::{Doc, DocAllocator, DocPtr, RefDoc};
use scoped_arena::Scope;

use crate::source::{StringId, StringInterner};
use crate::surface::lexer::is_keyword;
use crate::surface::{Arg, FormatField, Item, LetDef, Module, Param, Pattern, Plicity, Term};

const INDENT: isize = 4;

type DocBuilder<'interner, 'arena> = pretty::DocBuilder<'arena, Context<'interner, 'arena>>;

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

    fn string_id(&'arena self, name: StringId) -> DocBuilder<'interner, 'arena> {
        match self.interner.borrow().resolve(name) {
            Some(name) => self.text(name.to_owned()),
            None => self.text("#error"),
        }
    }

    fn ident(&'arena self, name: StringId) -> DocBuilder<'interner, 'arena> {
        match self.interner.borrow().resolve(name) {
            Some(name) if is_keyword(name) => self.text(format!("r#{name}")),
            Some(name) => self.text(name.to_owned()),
            None => self.text("#error"),
        }
    }

    pub fn module<Range>(
        &'arena self,
        module: &Module<'_, Range>,
    ) -> DocBuilder<'interner, 'arena> {
        self.intersperse(
            module.items.iter().map(|item| self.item(item)),
            self.hardline(),
        )
    }

    fn item<Range>(&'arena self, item: &Item<'_, Range>) -> DocBuilder<'interner, 'arena> {
        match item {
            Item::Def(item) => self
                .concat([
                    self.text("def"),
                    self.space(),
                    match item.r#type {
                        None => self.concat([
                            self.ident(item.label.1),
                            self.params(item.params),
                            self.space(),
                        ]),
                        Some(r#type) => self.concat([
                            self.concat([
                                self.ident(item.label.1),
                                self.params(item.params),
                                self.space(),
                                self.text(":"),
                            ])
                            .group(),
                            self.softline(),
                            self.term(r#type),
                        ]),
                    },
                    self.space(),
                    self.text("="),
                    self.softline(),
                    self.term(item.expr),
                    self.text(";"),
                ])
                .group(),
            Item::ReportedError(_) => self.text("#error"),
        }
    }

    fn pattern<Range>(&'arena self, pattern: &Pattern<Range>) -> DocBuilder<'interner, 'arena> {
        match pattern {
            Pattern::Placeholder(_) => self.text("_"),
            Pattern::Name(_, name) => self.ident(*name),
            Pattern::StringLiteral(_, number) => self.string_id(*number),
            Pattern::NumberLiteral(_, number) => self.string_id(*number),
            Pattern::BooleanLiteral(_, boolean) => match *boolean {
                true => self.text("true"),
                false => self.text("false"),
            },
        }
    }

    fn plicity(&'arena self, plicity: Plicity) -> DocBuilder<'interner, 'arena> {
        match plicity {
            Plicity::Explicit => self.nil(),
            Plicity::Implicit => self.text("@"),
        }
    }

    fn ann_pattern<Range>(
        &'arena self,
        pattern: &Pattern<Range>,
        r#type: Option<&Term<'_, Range>>,
    ) -> DocBuilder<'interner, 'arena> {
        match r#type {
            None => self.pattern(pattern),
            Some(r#type) => self.concat([
                self.concat([self.pattern(pattern), self.space(), self.text(":")])
                    .group(),
                self.softline(),
                self.term(r#type),
            ]),
        }
    }

    fn param<Range>(&'arena self, param: &Param<'_, Range>) -> DocBuilder<'interner, 'arena> {
        match &param.r#type {
            None => self.concat([self.plicity(param.plicity), self.pattern(&param.pattern)]),
            Some(r#type) => self.concat([
                self.text("("),
                self.concat([
                    self.plicity(param.plicity),
                    self.pattern(&param.pattern),
                    self.space(),
                    self.text(":"),
                ])
                .group(),
                self.softline(),
                self.term(r#type),
                self.text(")"),
            ]),
        }
    }

    fn params<Range>(&'arena self, params: &[Param<'_, Range>]) -> DocBuilder<'interner, 'arena> {
        self.concat((params.iter()).map(|param| self.concat([self.space(), self.param(param)])))
    }

    fn arg<Range>(&'arena self, arg: &Arg<'_, Range>) -> DocBuilder<'interner, 'arena> {
        self.concat([self.plicity(arg.plicity), self.term(&arg.term)])
    }

    pub fn term<Range>(&'arena self, term: &Term<'_, Range>) -> DocBuilder<'interner, 'arena> {
        // FIXME: indentation and grouping

        match term {
            Term::Paren(_, term) => self.paren(self.term(term)),
            Term::Name(_, name) => self.ident(*name),
            Term::Hole(_, name) => self.concat([self.text("?"), self.ident(*name)]),
            Term::Placeholder(_) => self.text("_"),
            Term::Ann(_, expr, r#type) => self.concat([
                self.concat([self.term(expr), self.space(), self.text(":")])
                    .group(),
                self.softline(),
                self.term(r#type),
            ]),
            Term::Let(_, def, body_expr) => self.concat([
                self.concat([
                    self.text("let"),
                    self.space(),
                    self.let_def(def),
                    self.text(";"),
                ])
                .group(),
                self.line(),
                self.term(body_expr),
            ]),
            Term::Letrec(_, defs, body_expr) => self.sequence(
                false,
                self.text("letrec "),
                defs.iter().map(|def| self.let_def(def)),
                self.text(","),
                self.concat([self.text(";"), self.line(), self.term(body_expr)]),
            ),
            Term::If(_, cond_expr, then_expr, mut else_expr) => {
                let mut branches = Vec::new();

                while let Term::If(_, cond_expr, then_expr, next_else) = else_expr {
                    branches.push((*cond_expr, *then_expr));
                    else_expr = next_else;
                }

                self.text("if ")
                    .append(self.term(cond_expr))
                    .append(
                        self.line()
                            .append(self.text("then "))
                            .append(self.term(then_expr))
                            .nest(INDENT),
                    )
                    .append(self.concat(branches.iter().map(|(cond_expr, then_expr)| {
                        self.line()
                            .append("else if ")
                            .append(self.term(cond_expr))
                            .append(" then ")
                            .append(self.term(then_expr))
                            .nest(INDENT)
                    })))
                    .append(
                        self.line()
                            .append(self.text("else "))
                            .append(self.term(else_expr))
                            .nest(INDENT),
                    )
                    .group()
            }
            Term::Match(_, scrut, equations) => {
                let equations = equations.iter().map(|(pattern, term)| {
                    self.pattern(pattern)
                        .append(self.text(" => "))
                        .append(self.term(term))
                });
                self.sequence(
                    true,
                    self.text("match ").append(self.term(scrut)).append(" {"),
                    equations,
                    self.text(","),
                    self.text("}"),
                )
            }
            Term::Universe(_) => self.text("Type"),
            Term::FunType(_, patterns, body_type) => self.concat([
                self.concat([
                    self.text("fun"),
                    self.params(patterns),
                    self.space(),
                    self.text("->"),
                ])
                .group(),
                self.softline(),
                self.term(body_type),
            ]),
            Term::Arrow(_, plicity, param_type, body_type) => self.concat([
                self.plicity(*plicity),
                self.term(param_type),
                self.softline(),
                self.text("->"),
                self.softline(),
                self.term(body_type),
            ]),
            Term::FunLiteral(_, patterns, body_expr) => self.concat([
                self.concat([
                    self.text("fun"),
                    self.params(patterns),
                    self.space(),
                    self.text("=>"),
                ])
                .group(),
                self.space(),
                self.term(body_expr),
            ]),
            Term::App(_, head_expr, args) => self.concat([
                self.term(head_expr),
                self.space(),
                self.intersperse((args.iter()).map(|arg| self.arg(arg)), self.space()),
            ]),
            Term::RecordType(_, fields) => {
                let fields = fields.iter().map(|field| {
                    self.ident(field.label.1)
                        .append(" : ")
                        .append(self.term(&field.r#type))
                });
                self.sequence(true, self.text("{"), fields, self.text(","), self.text("}"))
            }
            Term::RecordLiteral(_, fields) => {
                let fields = fields.iter().map(|field| {
                    self.ident(field.label.1)
                        .append(" = ")
                        .append(self.term(&field.expr))
                });
                self.sequence(true, self.text("{"), fields, self.text(","), self.text("}"))
            }
            Term::Tuple(_, terms) if terms.len() == 1 => {
                self.text("(").append(self.term(&terms[0]).append(",)"))
            }
            Term::Tuple(_, terms) => {
                let terms = terms.iter().map(|term| self.term(term));
                self.sequence(false, self.text("("), terms, self.text(","), self.text(")"))
            }
            Term::Proj(_, head_expr, labels) => self.concat([
                self.term(head_expr),
                self.concat(
                    (labels.iter()).map(|(_, label)| self.text(".").append(self.ident(*label))),
                ),
            ]),
            Term::ArrayLiteral(_, terms) => {
                let terms = terms.iter().map(|term| self.term(term));
                self.sequence(false, self.text("["), terms, self.text(","), self.text("]"))
            }
            Term::StringLiteral(_, number) => {
                self.concat([self.text("\""), self.string_id(*number), self.text("\"")])
            }
            Term::NumberLiteral(_, number) => self.string_id(*number),
            Term::BooleanLiteral(_, boolean) => match *boolean {
                true => self.text("true"),
                false => self.text("false"),
            },
            Term::FormatRecord(_, fields) => {
                let fields = fields.iter().map(|field| self.format_field(field));
                self.sequence(true, self.text("{"), fields, self.text(","), self.text("}"))
            }
            Term::FormatCond(_, (_, label), format, cond) => self.concat([
                self.text("{"),
                self.space(),
                self.ident(*label),
                self.space(),
                self.text("<-"),
                self.space(),
                self.term(format),
                self.space(),
                self.text("|"),
                self.space(),
                self.term(cond),
                self.space(),
                self.text("}"),
            ]),
            Term::FormatOverlap(_, fields) => {
                let fields = fields.iter().map(|field| self.format_field(field));
                self.sequence(
                    true,
                    self.text("overlap {"),
                    fields,
                    self.text(","),
                    self.text("}"),
                )
            }
            Term::BinOp(_, lhs, op, rhs) => self.concat([
                self.term(lhs),
                self.space(),
                self.text(op.as_str()),
                self.space(),
                self.term(rhs),
            ]),
            Term::ReportedError(_) => self.text("#error"),
        }
    }

    fn let_def<Range>(&'arena self, def: &LetDef<'_, Range>) -> DocBuilder<'interner, 'arena> {
        self.concat([
            self.ann_pattern(&def.pattern, def.r#type.as_ref()),
            self.space(),
            self.text("="),
            self.softline(),
            self.term(&def.expr),
        ])
    }

    fn format_field<Range>(
        &'arena self,
        format_field: &FormatField<'_, Range>,
    ) -> DocBuilder<'interner, 'arena> {
        match format_field {
            FormatField::Format {
                label,
                format,
                pred,
            } => self.concat([
                self.ident(label.1),
                self.space(),
                self.text("<-"),
                self.space(),
                self.term(format),
                match pred {
                    Some(pred) => self.concat([
                        self.space(),
                        self.text("where"),
                        self.space(),
                        self.term(pred),
                    ]),
                    None => self.nil(),
                },
            ]),
            FormatField::Computed {
                label,
                r#type,
                expr,
            } => self.concat([
                self.text("let"),
                self.space(),
                self.ident(label.1),
                match r#type {
                    Some(r#type) => self.concat([
                        self.space(),
                        self.text(":"),
                        self.space(),
                        self.term(r#type),
                    ]),
                    None => self.nil(),
                },
                self.space(),
                self.text("="),
                self.space(),
                self.term(expr),
            ]),
        }
    }

    /// Wrap a document in parens.
    fn paren(&'arena self, doc: DocBuilder<'interner, 'arena>) -> DocBuilder<'interner, 'arena> {
        self.concat([self.text("("), doc, self.text(")")])
    }

    /// Pretty prints a delimited sequence of documents with a trailing
    /// separator if it is formatted over multiple lines.
    /// If `space` is true, extra spaces are added before and after the
    /// delimiters
    pub fn sequence(
        &'arena self,
        space: bool,
        start_delim: DocBuilder<'interner, 'arena>,
        docs: impl ExactSizeIterator<Item = DocBuilder<'interner, 'arena>> + Clone,
        separator: DocBuilder<'interner, 'arena>,
        end_delim: DocBuilder<'interner, 'arena>,
    ) -> DocBuilder<'interner, 'arena> {
        if docs.len() == 0 {
            return self.concat([start_delim, end_delim]);
        }

        let docs = self.intersperse(docs, self.concat([separator.clone(), self.line()]));
        self.concat([
            start_delim,
            self.concat([
                if space { self.line() } else { self.line_() },
                docs,
                DocBuilder::flat_alt(separator, self.nil()),
            ])
            .nest(INDENT),
            if space { self.line() } else { self.line_() },
            end_delim,
        ])
        .group()
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
