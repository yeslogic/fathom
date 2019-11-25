//! The surface syntax for the data description language.

use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;
use pretty::{DocAllocator, DocBuilder};
use std::sync::Arc;

use crate::diagnostics;
use crate::lexer::SpannedToken;
use crate::literal;

pub mod compile;
pub mod delaborate;
pub mod elaborate;

mod grammar {
    include!(concat!(env!("OUT_DIR"), "/surface/grammar.rs"));
}

/// A module of items.
#[derive(Debug, Clone)]
pub struct Module {
    /// The file in which this module was defined.
    pub file_id: FileId,
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// The items in this module.
    pub items: Vec<Item>,
}

impl Module {
    pub fn parse(
        file_id: FileId,
        tokens: impl IntoIterator<Item = Result<SpannedToken, Diagnostic>>,
        report: &mut dyn FnMut(Diagnostic),
    ) -> Module {
        grammar::ModuleParser::new()
            .parse(file_id, report, tokens)
            .unwrap_or_else(|error| {
                report(diagnostics::error::parse(file_id, error));
                Module {
                    file_id,
                    doc: Arc::new([]),
                    items: Vec::new(),
                }
            })
    }

    pub fn doc<'core, D>(&'core self, alloc: &'core D) -> DocBuilder<'core, D>
    where
        D: DocAllocator<'core>,
        D::Doc: Clone,
    {
        let docs = match self.doc.as_ref() {
            [] => None,
            doc => Some(alloc.intersperse(
                doc.iter().map(|line| format!("//!{}", line)),
                alloc.newline(),
            )),
        };
        let items = self.items.iter().map(|item| item.doc(alloc));

        (alloc.nil())
            .append(alloc.intersperse(
                docs.into_iter().chain(items),
                alloc.newline().append(alloc.newline()),
            ))
            .append(alloc.newline())
    }
}

/// Items in a module.
#[derive(Debug, Clone)]
pub enum Item {
    /// Alias definitions.
    ///
    /// ```text
    /// alias <name> = <term>;
    /// ```
    Alias(Alias),
    /// Struct definitions.
    ///
    /// ```text
    /// struct <name> {}
    /// ```
    Struct(StructType),
}

impl Item {
    pub fn doc<'core, D>(&'core self, alloc: &'core D) -> DocBuilder<'core, D>
    where
        D: DocAllocator<'core>,
        D::Doc: Clone,
    {
        match self {
            Item::Alias(alias) => alias.doc(alloc),
            Item::Struct(struct_ty) => struct_ty.doc(alloc),
        }
    }
}

/// Alias definition.
#[derive(Debug, Clone)]
pub struct Alias {
    /// The full span of this definition.
    pub span: Span,
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// Name of this definition.
    pub name: (Span, String),
    /// Optional type annotation
    pub ty: Option<Term>,
    /// Fields in the struct.
    pub term: Term,
}

impl Alias {
    pub fn doc<'core, D>(&'core self, alloc: &'core D) -> DocBuilder<'core, D>
    where
        D: DocAllocator<'core>,
        D::Doc: Clone,
    {
        let docs = alloc.concat(self.doc.iter().map(|line| {
            (alloc.nil())
                .append(format!("///{}", line))
                .append(alloc.newline())
        }));

        (alloc.nil())
            .append(docs)
            .append(&self.name.1)
            .append(alloc.space())
            .append("=")
            .group()
            .append(match &self.ty {
                None => alloc.nil(),
                Some(ty) => (alloc.nil())
                    .append(alloc.space())
                    .append(ty.doc(alloc))
                    .group()
                    .nest(4),
            })
            .append(
                (alloc.nil())
                    .append(alloc.space())
                    .append(self.term.doc(alloc))
                    .group()
                    .append(";")
                    .nest(4),
            )
    }
}

/// A struct type definition.
#[derive(Debug, Clone)]
pub struct StructType {
    /// The full span of this definition.
    pub span: Span,
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// Name of this definition.
    pub name: (Span, String),
    /// Fields in the struct.
    pub fields: Vec<TypeField>,
}

impl StructType {
    pub fn doc<'core, D>(&'core self, alloc: &'core D) -> DocBuilder<'core, D>
    where
        D: DocAllocator<'core>,
        D::Doc: Clone,
    {
        let docs = alloc.concat(self.doc.iter().map(|line| {
            (alloc.nil())
                .append(format!("///{}", line))
                .append(alloc.newline())
        }));

        let struct_prefix = (alloc.nil())
            .append("struct")
            .append(alloc.space())
            .append(&self.name.1)
            .append(alloc.space());

        let struct_ty = if self.fields.is_empty() {
            (alloc.nil()).append(struct_prefix).append("{}").group()
        } else {
            (alloc.nil())
                .append(struct_prefix)
                .append("{")
                .group()
                .append(alloc.concat(self.fields.iter().map(|field| {
                    (alloc.nil())
                        .append(alloc.newline())
                        .append(field.doc(alloc))
                        .nest(4)
                        .group()
                })))
                .append(alloc.newline())
                .append("}")
        };

        (alloc.nil()).append(docs).append(struct_ty)
    }
}

/// A field in a struct type definition.
#[derive(Debug, Clone)]
pub struct TypeField {
    pub doc: Arc<[String]>,
    pub name: (Span, String),
    pub term: Term,
}

impl TypeField {
    pub fn doc<'core, D>(&'core self, alloc: &'core D) -> DocBuilder<'core, D>
    where
        D: DocAllocator<'core>,
        D::Doc: Clone,
    {
        let docs = alloc.concat(self.doc.iter().map(|line| {
            (alloc.nil())
                .append(format!("///{}", line))
                .append(alloc.newline())
        }));

        (alloc.nil())
            .append(docs)
            .append(
                (alloc.nil())
                    .append(&self.name.1)
                    .append(alloc.space())
                    .append(":")
                    .group(),
            )
            .append(
                (alloc.nil())
                    .append(alloc.space())
                    .append(self.term.doc(alloc))
                    .append(","),
            )
    }
}

/// Patterns.
#[derive(Debug, Clone)]
pub enum Pattern {
    /// Named patterns.
    Name(Span, String),
    /// Numeric literals.
    NumberLiteral(Span, literal::Number),
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Name(span, _) | Pattern::NumberLiteral(span, _) => *span,
        }
    }

    pub fn doc<'core, D>(&'core self, alloc: &'core D) -> DocBuilder<'core, D>
    where
        D: DocAllocator<'core>,
        D::Doc: Clone,
    {
        match self {
            Pattern::Name(_, name) => alloc.text(name),
            Pattern::NumberLiteral(_, literal) => alloc.as_string(literal),
        }
    }
}

/// Terms.
#[derive(Debug, Clone)]
pub enum Term {
    /// Parenthesised expressions.
    Paren(Span, Box<Term>),
    /// Annotated terms.
    Ann(Box<Term>, Box<Term>),
    /// Names.
    Name(Span, String),
    /// Numeric literals.
    NumberLiteral(Span, literal::Number),

    /// If-else expressions.
    If(Span, Box<Term>, Box<Term>, Box<Term>),
    /// Match expressions.
    Match(Span, Box<Term>, Vec<(Pattern, Term)>),

    /// Error sentinel terms.
    Error(Span),
}

impl Term {
    pub fn span(&self) -> Span {
        match self {
            Term::Ann(term, ty) => Span::merge(term.span(), ty.span()),
            Term::Paren(span, _)
            | Term::Name(span, _)
            | Term::NumberLiteral(span, _)
            | Term::If(span, _, _, _)
            | Term::Match(span, _, _)
            | Term::Error(span) => *span,
        }
    }

    pub fn doc<'core, D>(&'core self, alloc: &'core D) -> DocBuilder<'core, D>
    where
        D: DocAllocator<'core>,
        D::Doc: Clone,
    {
        match self {
            Term::Paren(_, term) => alloc.text("(").append(term.doc(alloc)).append(")"),
            Term::Ann(term, ty) => (alloc.nil())
                .append(term.doc(alloc))
                .append(alloc.space())
                .append(":")
                .group()
                .append((alloc.space()).append(ty.doc(alloc)).group().nest(4)),
            Term::Name(_, name) => alloc.text(name),
            Term::NumberLiteral(_, literal) => alloc.as_string(literal),
            Term::If(_, head, if_true, if_false) => (alloc.nil())
                .append("if")
                .append(alloc.space())
                .append(head.doc(alloc))
                .append(alloc.space())
                .append("{")
                .group()
                .append(alloc.space().append(if_true.doc(alloc)).group().nest(4))
                .append(alloc.space())
                .append(
                    (alloc.nil())
                        .append("}")
                        .append(alloc.space())
                        .append("else")
                        .append(alloc.space())
                        .append("{")
                        .nest(4),
                )
                .append(alloc.space().append(if_false.doc(alloc)).group().nest(4))
                .append(alloc.space())
                .append("}"),
            Term::Match(_, head, branches) => (alloc.nil())
                .append("match")
                .append(alloc.space())
                .append(head.doc(alloc))
                .append(alloc.space())
                .append("{")
                .append(alloc.concat(branches.iter().map(|(pattern, term)| {
                    (alloc.nil())
                        .append(alloc.newline())
                        .append(
                            (alloc.nil())
                                .append(pattern.doc(alloc))
                                .append(alloc.space())
                                .append("=>")
                                .group(),
                        )
                        .append(
                            (alloc.nil())
                                .append(alloc.space())
                                .append(term.doc(alloc))
                                .append(","),
                        )
                        .nest(4)
                        .group()
                })))
                .append(alloc.newline())
                .append("}"),
            Term::Error(_) => alloc.text("!"),
        }
    }
}
