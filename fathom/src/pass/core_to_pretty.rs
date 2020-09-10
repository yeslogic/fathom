use crate::lang::core::{Alias, Constant, Item, Module, StructType, Term, TypeField};
use pretty::{DocAllocator, DocBuilder};

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
        Item::Struct(struct_type) => from_struct_type(alloc, struct_type),
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
        .append(alloc.as_string(&alias.name))
        .append(alloc.space())
        .append("=")
        .group()
        .append(
            (alloc.nil())
                .append(alloc.space())
                .append(from_term_prec(alloc, &alias.term, Prec::Term))
                .group()
                .append(";")
                .nest(4),
        )
}

pub fn from_struct_type<'a, D>(alloc: &'a D, struct_type: &'a StructType) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    let docs = alloc.concat(struct_type.doc.iter().map(|line| {
        (alloc.nil())
            .append(format!("///{}", line))
            .append(alloc.hardline())
    }));

    let struct_prefix = (alloc.nil())
        .append("struct")
        .append(alloc.space())
        .append(alloc.as_string(&struct_type.name))
        .append(alloc.space());

    let struct_type = if struct_type.fields.is_empty() {
        (alloc.nil()).append(struct_prefix).append("{}").group()
    } else {
        (alloc.nil())
            .append(struct_prefix)
            .append("{")
            .group()
            .append(alloc.concat(struct_type.fields.iter().map(|field| {
                (alloc.nil())
                    .append(alloc.hardline())
                    .append(from_ty_field(alloc, field))
                    .nest(4)
                    .group()
            })))
            .append(alloc.hardline())
            .append("}")
    };

    (alloc.nil()).append(docs).append(struct_type)
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
                .append(alloc.as_string(&ty_field.name))
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

pub fn from_constant<'a, D>(alloc: &'a D, constant: &'a Constant) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    use num_traits::Float;
    use std::borrow::Cow;

    // Workaround -0.0 ridiculousness
    fn format_float<T: Float + From<u8> + std::fmt::Display>(value: T) -> Cow<'static, str> {
        if value == <T as From<u8>>::from(0) && value.is_sign_negative() {
            "-0".into()
        } else {
            value.to_string().into()
        }
    }

    match constant {
        Constant::Int(value) => (alloc.nil())
            .append("int")
            .append(alloc.space())
            .append(alloc.as_string(value)),
        Constant::F32(value) => (alloc.nil())
            .append("f32")
            .append(alloc.space())
            .append(format_float(*value)),
        Constant::F64(value) => (alloc.nil())
            .append("f64")
            .append(alloc.space())
            .append(format_float(*value)),
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
        Term::Global(_, name) => (alloc.nil())
            .append("global")
            .append(alloc.space())
            .append(alloc.as_string(name)),
        Term::Item(_, name) => (alloc.nil())
            .append("item")
            .append(alloc.space())
            .append(alloc.as_string(name)),
        Term::Ann(term, r#type) => paren(
            alloc,
            prec > Prec::Term,
            (alloc.nil())
                .append(from_term_prec(alloc, term, Prec::Arrow))
                .append(alloc.space())
                .append(":")
                .group()
                .append(
                    (alloc.space())
                        .append(from_term_prec(alloc, r#type, Prec::Term))
                        .group()
                        .nest(4),
                ),
        ),
        Term::TypeType(_) => alloc.text("Type"),
        Term::FunctionType(param_type, body_type) => paren(
            alloc,
            prec > Prec::Arrow,
            (alloc.nil())
                .append(from_term_prec(alloc, param_type, Prec::App))
                .append(alloc.space())
                .append("->")
                .append(alloc.space())
                .append(from_term_prec(alloc, body_type, Prec::Arrow)),
        ),
        Term::FunctionElim(head, argument) => paren(
            alloc,
            prec > Prec::App,
            (alloc.nil())
                .append(from_term_prec(alloc, head, Prec::Atomic))
                .append(
                    (alloc.space())
                        .append(from_term_prec(alloc, argument, Prec::Atomic))
                        .group()
                        .nest(4),
                ),
        ),
        Term::Constant(_, constant) => from_constant(alloc, constant),
        Term::BoolElim(_, head, if_true, if_false) => (alloc.nil())
            .append("bool_elim")
            .append(alloc.space())
            .append(from_term_prec(alloc, head, Prec::Term))
            .append(alloc.space())
            .append("{")
            .append(alloc.space())
            .append(from_term_prec(alloc, if_true, Prec::Term))
            .append(",")
            .append(alloc.space())
            .append(from_term_prec(alloc, if_false, Prec::Term))
            .append(alloc.space())
            .append("}"),
        Term::IntElim(_, head, branches, default) => (alloc.nil())
            .append("int_elim")
            .append(alloc.space())
            .append(from_term_prec(alloc, head, Prec::Term))
            .append(alloc.space())
            .append("{")
            .append(alloc.concat(branches.iter().map(|(value, term)| {
                (alloc.nil())
                    .append(alloc.space())
                    .append(alloc.as_string(value))
                    .append(alloc.space())
                    .append("=>")
                    .append(alloc.space())
                    .append(from_term_prec(alloc, term, Prec::Term))
                    .append(",")
            })))
            .append(alloc.space())
            .append(from_term_prec(alloc, default, Prec::Term))
            .append(alloc.space())
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
