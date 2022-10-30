use pretty::{docs, Doc, DocAllocator, DocBuilder, DocPtr, RefDoc};
use scoped_arena::Scope;
use std::cell::RefCell;

use crate::surface::{BinOp, FormatField, Item, Module, Pattern, Term};
use crate::{StringId, StringInterner};

/// Term precedences
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Prec {
    Top = 0,
    Let,
    Fun,
    Eq,
    Cmp,
    Mul,
    Add,
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

    pub fn module<Range>(&'arena self, module: &Module<'_, Range>) -> DocBuilder<'arena, Self> {
        self.intersperse(
            module.items.iter().map(|item| self.item(item)),
            self.hardline(),
        )
    }

    fn item<Range>(&'arena self, item: &Item<'_, Range>) -> DocBuilder<'arena, Self> {
        match item {
            Item::Def(item) => self
                .concat([
                    self.text("def"),
                    self.space(),
                    match item.type_ {
                        None => self.concat([
                            self.string_id(item.label.1),
                            self.ann_patterns(item.patterns),
                            self.space(),
                        ]),
                        Some(r#type) => self.concat([
                            self.concat([
                                self.string_id(item.label.1),
                                self.ann_patterns(item.patterns),
                                self.space(),
                                self.text(":"),
                            ])
                            .group(),
                            self.softline(),
                            self.term_prec(Prec::Top, r#type),
                        ]),
                    },
                    self.space(),
                    self.text("="),
                    self.softline(),
                    self.term_prec(Prec::Let, item.expr),
                    self.text(";"),
                ])
                .group(),
            Item::ReportedError(_) => self.text("#error"),
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
                    self.concat([self.pattern(pattern), self.space(), self.text(":")])
                        .group(),
                    self.softline(),
                    self.term_prec(Prec::Top, r#type),
                ]),
            ),
        }
    }

    fn ann_patterns<Range>(
        &'arena self,
        patterns: &[(Pattern<Range>, Option<&Term<'_, Range>>)],
    ) -> DocBuilder<'arena, Self> {
        self.concat(patterns.iter().map(|(pattern, r#type)| {
            self.concat([
                self.space(),
                self.ann_pattern(Prec::Atomic, pattern, *r#type),
            ])
        }))
    }

    pub fn term<Range>(&'arena self, term: &Term<'_, Range>) -> DocBuilder<'arena, Self> {
        self.term_prec(Prec::Top, term)
    }

    fn term_prec<Range>(
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
                        self.ann_pattern(Prec::Top, def_pattern, *def_type),
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
            Term::If(_, cond_expr, then_expr, else_expr) => {
                let cond = docs![
                    self,
                    "if",
                    self.space(),
                    self.term_prec(Prec::Let, cond_expr),
                ];
                let then = docs![
                    self,
                    "then",
                    self.space(),
                    self.term_prec(Prec::Let, then_expr),
                ];
                let r#else = docs![
                    self,
                    "else",
                    self.space(),
                    self.term_prec(Prec::Let, else_expr),
                ];
                self.paren(
                    prec > Prec::Let,
                    DocBuilder::flat_alt(
                        docs![
                            self,
                            cond.clone(),
                            docs![self, self.hardline(), then.clone()].nest(INDENT),
                            docs![self, self.hardline(), r#else.clone()].nest(INDENT),
                        ],
                        docs![self, cond, self.space(), then, self.space(), r#else],
                    )
                    .group(),
                )
            }
            Term::Match(_, scrutinee, equations) => self.sequence(
                self.concat([
                    self.text("match"),
                    self.space(),
                    self.term_prec(Prec::Proj, scrutinee),
                    self.space(),
                    self.text("{"),
                ]),
                equations.iter().map(|(pattern, body_expr)| {
                    self.concat([
                        self.pattern(pattern),
                        self.space(),
                        self.text("=>"),
                        self.space(),
                        self.term_prec(Prec::Top, r#body_expr),
                    ])
                }),
                self.text(","),
                self.text("}"),
            ),
            Term::Universe(_) => self.text("Type"),
            Term::FunType(_, patterns, body_type) => self.paren(
                prec > Prec::Fun,
                self.concat([
                    self.concat([
                        self.text("fun"),
                        self.ann_patterns(patterns),
                        self.space(),
                        self.text("->"),
                    ])
                    .group(),
                    self.softline(),
                    self.term_prec(Prec::Fun, body_type),
                ]),
            ),
            Term::Arrow(_, param_type, body_type) => self.paren(
                prec > Prec::Fun,
                self.concat([
                    self.term_prec(Prec::App, param_type),
                    self.softline(),
                    self.text("->"),
                    self.softline(),
                    self.term_prec(Prec::Fun, body_type),
                ]),
            ),
            Term::FunLiteral(_, patterns, body_expr) => self.paren(
                prec > Prec::Fun,
                self.concat([
                    self.concat([
                        self.text("fun"),
                        self.ann_patterns(patterns),
                        self.space(),
                        self.text("=>"),
                    ])
                    .group(),
                    self.space(),
                    self.term_prec(Prec::Let, body_expr),
                ]),
            ),
            Term::App(_, head_expr, arg_exprs) => self.paren(
                prec > Prec::App,
                self.concat([
                    self.term_prec(Prec::Proj, head_expr),
                    self.space(),
                    self.intersperse(
                        (arg_exprs.iter()).map(|arg_expr| self.term_prec(Prec::Proj, arg_expr)),
                        self.space(),
                    ),
                ]),
            ),
            Term::RecordType(_, type_fields) => self.sequence(
                self.text("{"),
                type_fields.iter().map(|field| {
                    self.concat([
                        self.string_id(field.label.1),
                        self.space(),
                        self.text(":"),
                        self.space(),
                        self.term_prec(Prec::Top, &field.type_),
                    ])
                }),
                self.text(","),
                self.text("}"),
            ),
            Term::RecordLiteral(_, expr_fields) => self.sequence(
                self.text("{"),
                expr_fields.iter().map(|field| {
                    self.concat([
                        self.string_id(field.label.1),
                        self.space(),
                        self.text("="),
                        self.space(),
                        self.term_prec(Prec::Top, &field.expr),
                    ])
                }),
                self.text(","),
                self.text("}"),
            ),
            Term::UnitLiteral(_) => self.text("{}"),
            Term::Proj(_, head_expr, labels) => self.concat([
                self.term_prec(Prec::Atomic, head_expr),
                self.concat(
                    (labels.iter()).map(|(_, label)| self.text(".").append(self.string_id(*label))),
                ),
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
                format_fields.iter().map(|field| self.format_field(field)),
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
                format_fields.iter().map(|field| self.format_field(field)),
                self.text(","),
                self.text("}"),
            ),
            Term::BinOp(_, lhs, op, rhs) => self.paren(
                prec > op.precedence(),
                self.concat([
                    self.term_prec(op.lhs_prec(), lhs),
                    self.space(),
                    self.text(op.as_str()),
                    self.space(),
                    self.term_prec(op.rhs_prec(), rhs),
                ]),
            ),
            Term::ReportedError(_) => self.text("#error"),
        }
    }

    fn format_field<Range>(
        &'arena self,
        format_field: &FormatField<'_, Range>,
    ) -> DocBuilder<'arena, Self> {
        match format_field {
            FormatField::Format {
                label,
                format,
                pred,
            } => self.concat([
                self.string_id(label.1),
                self.space(),
                self.text("<-"),
                self.space(),
                self.term_prec(Prec::Top, format),
                match pred {
                    Some(pred) => self.concat([
                        self.space(),
                        self.text("where"),
                        self.space(),
                        self.term_prec(Prec::Top, pred),
                    ]),
                    None => self.nil(),
                },
            ]),
            FormatField::Computed { label, type_, expr } => self.concat([
                self.text("let"),
                self.space(),
                self.string_id(label.1),
                match type_ {
                    Some(r#type) => self.concat([
                        self.space(),
                        self.text(":"),
                        self.space(),
                        self.term_prec(Prec::Top, r#type),
                    ]),
                    None => self.nil(),
                },
                self.space(),
                self.text("="),
                self.space(),
                self.term_prec(Prec::Top, expr),
            ]),
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

macro_rules! borrowed_text {
    (match $doc:expr, {$($text:literal),*}, _ => $default:expr) => {
        match $doc {
            $(
            Doc::BorrowedText($text) => &Doc::BorrowedText($text),
            )*
            _ => $default,
        }
    };
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
            _ => {
                borrowed_text!(
                    match doc, {
                        "def", "else", "false", "fun", "if", "let", "match", "overlap", "then",
                        "true", "Type", "where", ":", ",", "=", "!=", "==", "=>", ">=", ">", "<=",
                        "<", ".", "/", "->", "<-", "-", "|", "+", ";", "*", "_", "{", "}", "[",
                        "]", "(", ")"
                    },
                    _ => self.scope.to_scope(doc)
                )
            }
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

impl<Range> BinOp<Range> {
    fn precedence(&self) -> Prec {
        self.precedence_impl().1
    }

    fn lhs_prec(&self) -> Prec {
        self.precedence_impl().0
    }

    fn rhs_prec(&self) -> Prec {
        self.precedence_impl().2
    }

    /// Returns the precedence of this operator and its operands
    ///
    /// (lhs, op, rhs)
    fn precedence_impl(&self) -> (Prec, Prec, Prec) {
        match self {
            BinOp::Eq(_) | BinOp::Neq(_) => (Prec::Cmp, Prec::Eq, Prec::Eq),
            BinOp::Lt(_) | BinOp::Lte(_) | BinOp::Gt(_) | BinOp::Gte(_) => {
                (Prec::Add, Prec::Cmp, Prec::Cmp)
            }
            BinOp::Add(_) | BinOp::Sub(_) => (Prec::Mul, Prec::Add, Prec::Add),
            BinOp::Mul(_) | BinOp::Div(_) => (Prec::App, Prec::Mul, Prec::Mul),
        }
    }
}
