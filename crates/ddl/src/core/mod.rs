//! The core type theory of the data description language.

use codespan::{ByteIndex, FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;
use num_bigint::BigInt;
use pretty::{DocAllocator, DocBuilder};
use std::borrow::Borrow;
use std::fmt;
use std::sync::Arc;

use crate::lexer::SpannedToken;
use crate::{diagnostics, ieee754};

mod grammar {
    include!(concat!(env!("OUT_DIR"), "/core/grammar.rs"));
}

pub mod compile;
pub mod semantics;
pub mod validate;

/// A label.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Label(pub String);

impl Label {
    pub fn doc<'core, D>(&'core self, alloc: &'core D) -> DocBuilder<'core, D>
    where
        D: DocAllocator<'core>,
        D::Doc: Clone,
    {
        alloc.text(&self.0)
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Borrow<str> for Label {
    fn borrow(&self) -> &str {
        &self.0
    }
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

impl PartialEq for Module {
    fn eq(&self, other: &Module) -> bool {
        self.items == other.items
    }
}

/// Items in a module.
#[derive(Debug, Clone)]
pub enum Item {
    /// External definitions
    Extern(Extern),
    /// Alias definitions
    Alias(Alias),
    /// Struct definitions.
    Struct(StructType),
}

impl Item {
    pub fn span(&self) -> Span {
        match self {
            Item::Extern(r#extern) => r#extern.span,
            Item::Struct(struct_ty) => struct_ty.span,
            Item::Alias(alias) => alias.span,
        }
    }

    pub fn doc<'core, D>(&'core self, alloc: &'core D) -> DocBuilder<'core, D>
    where
        D: DocAllocator<'core>,
        D::Doc: Clone,
    {
        match self {
            Item::Extern(r#extern) => r#extern.doc(alloc),
            Item::Alias(alias) => alias.doc(alloc),
            Item::Struct(struct_ty) => struct_ty.doc(alloc),
        }
    }
}

impl PartialEq for Item {
    fn eq(&self, other: &Item) -> bool {
        match (self, other) {
            (Item::Extern(extern0), Item::Extern(extern1)) => *extern0 == *extern1,
            (Item::Alias(alias0), Item::Alias(alias1)) => *alias0 == *alias1,
            (Item::Struct(struct_ty0), Item::Struct(struct_ty1)) => *struct_ty0 == *struct_ty1,
            (_, _) => false,
        }
    }
}

/// An external definition.
#[derive(Debug, Clone)]
pub struct Extern {
    /// The full span of this definition.
    pub span: Span,
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// Name of this definition.
    pub name: Label,
    /// The type of this definition.
    pub ty: Term,
}

impl Extern {
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
            .append("extern")
            .append(alloc.space())
            .append(self.name.doc(alloc))
            .append(alloc.space())
            .append(":")
            .group()
            .append(
                (alloc.nil())
                    .append(alloc.space())
                    .append(self.ty.doc(alloc))
                    .group()
                    .append(";")
                    .nest(4),
            )
    }
}

impl PartialEq for Extern {
    fn eq(&self, other: &Extern) -> bool {
        self.name == other.name && self.ty == other.ty
    }
}

/// An alias definition.
#[derive(Debug, Clone)]
pub struct Alias {
    /// The full span of this definition.
    pub span: Span,
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// Name of this definition.
    pub name: Label,
    /// The term that is aliased.
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
            .append(self.name.doc(alloc))
            .append(alloc.space())
            .append("=")
            .group()
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

impl PartialEq for Alias {
    fn eq(&self, other: &Alias) -> bool {
        self.name == other.name && self.term == other.term
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
    pub name: Label,
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
            .append(self.name.doc(alloc))
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

impl PartialEq for StructType {
    fn eq(&self, other: &StructType) -> bool {
        self.name == other.name && self.fields == other.fields
    }
}

/// A field in a struct type definition.
#[derive(Debug, Clone)]
pub struct TypeField {
    pub doc: Arc<[String]>,
    pub start: ByteIndex,
    pub name: Label,
    pub term: Term,
}

impl TypeField {
    pub fn span(&self) -> Span {
        Span::new(self.start, self.term.span().end())
    }

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
                    .append(self.name.doc(alloc))
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

impl PartialEq for TypeField {
    fn eq(&self, other: &TypeField) -> bool {
        self.name == other.name && self.term == other.term
    }
}

/// Universes.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Universe {
    Type,
    Format,
    Kind,
}

impl Universe {
    pub fn doc<'core, D>(&'core self, alloc: &'core D) -> DocBuilder<'core, D>
    where
        D: DocAllocator<'core>,
        D::Doc: Clone,
    {
        match self {
            Universe::Type => alloc.text("Type"),
            Universe::Format => alloc.text("Format"),
            Universe::Kind => alloc.text("Kind"),
        }
    }
}

/// Terms.
#[derive(Debug, Clone)]
pub enum Term {
    /// Item references
    Item(Span, Label),

    /// Terms annotated with types.
    Ann(Arc<Term>, Arc<Term>),

    /// Universes.
    Universe(Span, Universe),

    /// Host integer constants.
    IntConst(Span, BigInt),
    /// Host IEEE-754 single-precision floating point constants.
    F32Const(Span, f32),
    /// Host IEEE-754 double-precision floating point constants.
    F64Const(Span, f64),

    /// A boolean elimination.
    BoolElim(Span, Arc<Term>, Arc<Term>, Arc<Term>),

    /// Error sentinel.
    Error(Span),
}

impl Term {
    pub fn span(&self) -> Span {
        match self {
            Term::Item(span, _)
            | Term::Universe(span, _)
            | Term::IntConst(span, _)
            | Term::F32Const(span, _)
            | Term::F64Const(span, _)
            | Term::BoolElim(span, _, _, _)
            | Term::Error(span) => *span,
            Term::Ann(term, ty) => Span::merge(term.span(), ty.span()),
        }
    }

    pub fn doc<'core, D>(&'core self, alloc: &'core D) -> DocBuilder<'core, D>
    where
        D: DocAllocator<'core>,
        D::Doc: Clone,
    {
        self.doc_prec(alloc, 0)
    }

    pub fn doc_prec<'core, D>(&'core self, alloc: &'core D, prec: u8) -> DocBuilder<'core, D>
    where
        D: DocAllocator<'core>,
        D::Doc: Clone,
    {
        use num_traits::Float;
        use std::borrow::Cow;

        let show_paren = |cond, doc| match cond {
            true => alloc.text("(").append(doc).append(")"),
            false => doc,
        };

        // Workaround -0.0 ridiculousness
        fn format_float<T: Float + From<u8> + fmt::Display>(value: T) -> Cow<'static, str> {
            if value == <T as From<u8>>::from(0) && value.is_sign_negative() {
                "-0".into()
            } else {
                value.to_string().into()
            }
        }

        match self {
            Term::Item(_, label) => (alloc.nil())
                .append("item")
                .append(alloc.space())
                .append(alloc.as_string(label)),
            Term::Ann(term, ty) => show_paren(
                prec > 0,
                (alloc.nil())
                    .append(term.doc_prec(alloc, prec + 1))
                    .append(alloc.space())
                    .append(":")
                    .group()
                    .append(
                        (alloc.space())
                            .append(ty.doc_prec(alloc, prec + 1))
                            .group()
                            .nest(4),
                    ),
            ),
            Term::Universe(_, universe) => universe.doc(alloc),
            Term::IntConst(_, value) => (alloc.nil())
                .append("int")
                .append(alloc.space())
                .append(alloc.as_string(value)),
            Term::F32Const(_, value) => (alloc.nil())
                .append("f32")
                .append(alloc.space())
                .append(format_float(*value)),
            Term::F64Const(_, value) => (alloc.nil())
                .append("f64")
                .append(alloc.space())
                .append(format_float(*value)),
            Term::BoolElim(_, term, if_true, if_false) => (alloc.nil())
                .append("bool_elim")
                .append(alloc.space())
                .append(term.doc(alloc))
                .append(alloc.space())
                .append("{")
                .append(alloc.space())
                .append(if_true.doc(alloc))
                .append(",")
                .append(alloc.space())
                .append(if_false.doc(alloc))
                .append(alloc.space())
                .append("}"),
            Term::Error(_) => alloc.text("!"),
        }
    }
}

impl PartialEq for Term {
    fn eq(&self, other: &Term) -> bool {
        match (self, other) {
            (Term::Item(_, label0), Term::Item(_, label1)) => label0 == label1,
            (Term::Ann(term0, ty0), Term::Ann(term1, ty1)) => term0 == term1 && ty0 == ty1,
            (Term::IntConst(_, val0), Term::IntConst(_, val1)) => val0 == val1,
            (Term::F32Const(_, val0), Term::F32Const(_, val1)) => ieee754::logical_eq(*val0, *val1),
            (Term::F64Const(_, val0), Term::F64Const(_, val1)) => ieee754::logical_eq(*val0, *val1),
            (Term::Universe(_, universe0), Term::Universe(_, universe1)) => universe0 == universe1,
            (
                Term::BoolElim(_, head0, if_true0, if_false0),
                Term::BoolElim(_, head1, if_true1, if_false1),
            ) => head0 == head1 && if_true0 == if_true1 && if_false0 == if_false1,
            (Term::Error(_), Term::Error(_)) => true,
            (_, _) => false,
        }
    }
}

/// The head of a neutral term.
#[derive(Debug, Clone, PartialEq)]
pub enum Head {
    /// Item references.
    Item(Label),
    /// Errors.
    Error,
}

/// An eliminator that is 'stuck' on some head.
#[derive(Debug, Clone, PartialEq)]
pub enum Elim {
    // FIXME: environment?
    Bool(Arc<Term>, Arc<Term>),
}

/// Values.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// Neutral terms
    Neutral(Head, Vec<Elim>),

    /// Universes.
    Universe(Universe),

    /// Host integer constants.
    IntConst(BigInt),
    /// Host IEEE-754 single-precision floating point constants.
    F32Const(f32),
    /// Host IEEE-754 double-precision floating point constants.
    F64Const(f64),

    /// Error sentinel.
    Error,
}
