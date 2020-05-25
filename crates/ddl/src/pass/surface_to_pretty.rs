//! Pretty prints the surface language to a textual form.

use pretty::{DocAllocator, DocBuilder};

use crate::ast::surface::{Alias, Item, Module, Pattern, StructType, Term, TypeField};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Prec {
    Term = 0,
    Arrow,
    App,
    Atomic,
}

pub fn from_module<'a, D>(alloc: &'a D, module: &'a Module) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    let docs = match module.doc.as_ref() {
        [] => None,
        doc => Some(alloc.intersperse(
            doc.iter().map(|line| alloc.text("//!").append(line)),
            alloc.hardline(),
        )),
    };
    let items = module.items.iter().map(|item| from_item(alloc, item));

    (alloc.nil())
        .append(alloc.intersperse(
            docs.into_iter().chain(items),
            alloc.hardline().append(alloc.hardline()),
        ))
        .append(alloc.hardline())
}

pub fn from_item<'a, D>(alloc: &'a D, item: &'a Item) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    match item {
        Item::Alias(alias) => from_alias(alloc, alias),
        Item::Struct(struct_ty) => from_struct_ty(alloc, struct_ty),
    }
}

pub fn from_alias<'a, D>(alloc: &'a D, alias: &'a Alias) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    let docs = alloc.concat(alias.doc.iter().map(|line| {
        (alloc.nil())
            .append(format!("///{}", line))
            .append(alloc.hardline())
    }));

    (alloc.nil())
        .append(docs)
        .append(&alias.name.1)
        .append(alloc.space())
        .append("=")
        .group()
        .append(match &alias.ty {
            None => alloc.nil(),
            Some(ty) => (alloc.nil())
                .append(alloc.space())
                .append(from_term_prec(alloc, ty, Prec::Term))
                .group()
                .nest(4),
        })
        .append(
            (alloc.nil())
                .append(alloc.space())
                .append(from_term_prec(alloc, &alias.term, Prec::Term))
                .group()
                .append(";")
                .nest(4),
        )
}

pub fn from_struct_ty<'a, D>(alloc: &'a D, struct_ty: &'a StructType) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    let docs = alloc.concat(struct_ty.doc.iter().map(|line| {
        (alloc.nil())
            .append(format!("///{}", line))
            .append(alloc.hardline())
    }));

    let struct_prefix = (alloc.nil())
        .append("struct")
        .append(alloc.space())
        .append(&struct_ty.name.1)
        .append(alloc.space());

    let struct_ty = if struct_ty.fields.is_empty() {
        (alloc.nil()).append(struct_prefix).append("{}").group()
    } else {
        (alloc.nil())
            .append(struct_prefix)
            .append("{")
            .group()
            .append(alloc.concat(struct_ty.fields.iter().map(|field| {
                (alloc.nil())
                    .append(alloc.hardline())
                    .append(from_ty_field(alloc, field))
                    .nest(4)
                    .group()
            })))
            .append(alloc.hardline())
            .append("}")
    };

    (alloc.nil()).append(docs).append(struct_ty)
}

pub fn from_ty_field<'a, D>(alloc: &'a D, ty_field: &'a TypeField) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    let docs = alloc.concat(ty_field.doc.iter().map(|line| {
        (alloc.nil())
            .append(format!("///{}", line))
            .append(alloc.hardline())
    }));

    (alloc.nil())
        .append(docs)
        .append(
            (alloc.nil())
                .append(&ty_field.name.1)
                .append(alloc.space())
                .append(":")
                .group(),
        )
        .append(
            (alloc.nil())
                .append(alloc.space())
                .append(from_term_prec(alloc, &ty_field.term, Prec::Term))
                .append(","),
        )
}

pub fn from_pattern<'a, D>(alloc: &'a D, pattern: &'a Pattern) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    match pattern {
        Pattern::Name(_, name) => alloc.text(name),
        Pattern::NumberLiteral(_, literal) => alloc.as_string(literal),
    }
}

pub fn from_term<'a, D>(alloc: &'a D, term: &'a Term) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    from_term_prec(alloc, term, Prec::Term)
}

pub fn from_term_prec<'a, D>(alloc: &'a D, term: &'a Term, prec: Prec) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    match term {
        Term::Ann(term, ty) => paren(
            alloc,
            prec > Prec::Term,
            (alloc.nil())
                .append(from_term_prec(alloc, term, Prec::Arrow))
                .append(alloc.space())
                .append(":")
                .group()
                .append(
                    (alloc.space())
                        .append(from_term_prec(alloc, ty, Prec::Term))
                        .group()
                        .nest(4),
                ),
        ),
        Term::Name(_, name) => alloc.text(name),
        Term::TypeType(_) => alloc.text("Type"),
        Term::FunctionType(param_type, body_type) => paren(
            alloc,
            prec > Prec::App,
            (alloc.nil())
                .append(from_term_prec(alloc, param_type, Prec::Atomic))
                .append(alloc.space())
                .append("->")
                .append(alloc.space())
                .append(from_term_prec(alloc, body_type, Prec::Arrow)),
        ),
        Term::FunctionElim(head, arguments) => paren(
            alloc,
            prec > Prec::App,
            from_term_prec(alloc, head, Prec::Atomic).append(
                (alloc.nil())
                    .append(alloc.concat(arguments.iter().map(|argument| {
                        (alloc.space()).append(from_term_prec(alloc, argument, Prec::Atomic))
                    })))
                    .group()
                    .nest(4),
            ),
        ),
        Term::NumberLiteral(_, literal) => alloc.as_string(literal),
        Term::If(_, head, if_true, if_false) => (alloc.nil())
            .append("if")
            .append(alloc.space())
            .append(from_term_prec(alloc, head, Prec::Term))
            .append(alloc.space())
            .append("{")
            .group()
            .append(
                alloc
                    .space()
                    .append(from_term_prec(alloc, if_true, Prec::Term))
                    .group()
                    .nest(4),
            )
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
            .append(
                alloc
                    .space()
                    .append(from_term_prec(alloc, if_false, Prec::Term))
                    .group()
                    .nest(4),
            )
            .append(alloc.space())
            .append("}"),
        Term::Match(_, head, branches) => (alloc.nil())
            .append("match")
            .append(alloc.space())
            .append(from_term_prec(alloc, head, Prec::Term))
            .append(alloc.space())
            .append("{")
            .append(alloc.concat(branches.iter().map(|(pattern, term)| {
                (alloc.nil())
                    .append(alloc.hardline())
                    .append(
                        (alloc.nil())
                            .append(from_pattern(alloc, pattern))
                            .append(alloc.space())
                            .append("=>")
                            .group(),
                    )
                    .append(
                        (alloc.nil())
                            .append(alloc.space())
                            .append(from_term_prec(alloc, term, Prec::Term))
                            .append(","),
                    )
                    .nest(4)
                    .group()
            })))
            .append(alloc.hardline())
            .append("}"),
        Term::FormatType(_) => alloc.text("Format"),
        Term::Error(_) => alloc.text("!"),
    }
}

fn paren<'a, D>(alloc: &'a D, b: bool, doc: DocBuilder<'a, D>) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    if b {
        alloc.text("(").append(doc).append(")")
    } else {
        doc
    }
}
