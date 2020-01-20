//! Pretty prints the surface language to a textual form.

use pretty::{DocAllocator, DocBuilder};

use crate::surface::{Alias, Item, Module, Pattern, StructType, Term, TypeField};

pub fn pretty_module<'a, D>(alloc: &'a D, module: &'a Module) -> DocBuilder<'a, D>
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
    let items = module.items.iter().map(|item| pretty_item(alloc, item));

    (alloc.nil())
        .append(alloc.intersperse(
            docs.into_iter().chain(items),
            alloc.hardline().append(alloc.hardline()),
        ))
        .append(alloc.hardline())
}

pub fn pretty_item<'a, D>(alloc: &'a D, item: &'a Item) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    match item {
        Item::Alias(alias) => pretty_alias(alloc, alias),
        Item::Struct(struct_ty) => pretty_struct_ty(alloc, struct_ty),
    }
}

pub fn pretty_alias<'a, D>(alloc: &'a D, alias: &'a Alias) -> DocBuilder<'a, D>
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
                .append(pretty_term(alloc, ty))
                .group()
                .nest(4),
        })
        .append(
            (alloc.nil())
                .append(alloc.space())
                .append(pretty_term(alloc, &alias.term))
                .group()
                .append(";")
                .nest(4),
        )
}

pub fn pretty_struct_ty<'a, D>(alloc: &'a D, struct_ty: &'a StructType) -> DocBuilder<'a, D>
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
                    .append(pretty_ty_field(alloc, field))
                    .nest(4)
                    .group()
            })))
            .append(alloc.hardline())
            .append("}")
    };

    (alloc.nil()).append(docs).append(struct_ty)
}

pub fn pretty_ty_field<'a, D>(alloc: &'a D, ty_field: &'a TypeField) -> DocBuilder<'a, D>
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
                .append(pretty_term(alloc, &ty_field.term))
                .append(","),
        )
}

pub fn pretty_pattern<'a, D>(alloc: &'a D, pattern: &'a Pattern) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    match pattern {
        Pattern::Name(_, name) => alloc.text(name),
        Pattern::NumberLiteral(_, literal) => alloc.as_string(literal),
    }
}

pub fn pretty_term<'a, D>(alloc: &'a D, term: &'a Term) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    match term {
        Term::Paren(_, term) => alloc.text("(").append(pretty_term(alloc, term)).append(")"),
        Term::Ann(term, ty) => (alloc.nil())
            .append(pretty_term(alloc, term))
            .append(alloc.space())
            .append(":")
            .group()
            .append(
                (alloc.space())
                    .append(pretty_term(alloc, ty))
                    .group()
                    .nest(4),
            ),
        Term::Name(_, name) => alloc.text(name),
        Term::Format(_) => alloc.text("Format"),
        Term::Host(_) => alloc.text("Host"),
        Term::Kind(_) => alloc.text("Kind"),
        Term::NumberLiteral(_, literal) => alloc.as_string(literal),
        Term::If(_, head, if_true, if_false) => (alloc.nil())
            .append("if")
            .append(alloc.space())
            .append(pretty_term(alloc, head))
            .append(alloc.space())
            .append("{")
            .group()
            .append(
                alloc
                    .space()
                    .append(pretty_term(alloc, if_true))
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
                    .append(pretty_term(alloc, if_false))
                    .group()
                    .nest(4),
            )
            .append(alloc.space())
            .append("}"),
        Term::Match(_, head, branches) => (alloc.nil())
            .append("match")
            .append(alloc.space())
            .append(pretty_term(alloc, head))
            .append(alloc.space())
            .append("{")
            .append(alloc.concat(branches.iter().map(|(pattern, term)| {
                (alloc.nil())
                    .append(alloc.hardline())
                    .append(
                        (alloc.nil())
                            .append(pretty_pattern(alloc, pattern))
                            .append(alloc.space())
                            .append("=>")
                            .group(),
                    )
                    .append(
                        (alloc.nil())
                            .append(alloc.space())
                            .append(pretty_term(alloc, term))
                            .append(","),
                    )
                    .nest(4)
                    .group()
            })))
            .append(alloc.hardline())
            .append("}"),
        Term::Error(_) => alloc.text("!"),
    }
}
