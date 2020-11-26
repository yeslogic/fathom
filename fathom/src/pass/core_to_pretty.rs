use crate::lang::core::{
    Constant, FieldDeclaration, FieldDefinition, Item, ItemData, Module, Primitive, Sort,
    StructFormat, StructType, Term, TermData,
};
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
    match &item.data {
        ItemData::Constant(constant) => from_constant(alloc, constant),
        ItemData::StructType(struct_type) => from_struct_type(alloc, struct_type),
        ItemData::StructFormat(struct_format) => from_struct_format(alloc, struct_format),
    }
}

pub fn from_constant<'a, D>(alloc: &'a D, constant: &'a Constant) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    let docs = alloc.concat(constant.doc.iter().map(|line| {
        (alloc.nil())
            .append(format!("///{}", line))
            .append(alloc.hardline())
    }));

    (alloc.nil())
        .append(docs)
        .append("const")
        .append(alloc.space())
        .append(alloc.as_string(&constant.name))
        .append(alloc.space())
        .append("=")
        .group()
        .append(
            (alloc.nil())
                .append(alloc.space())
                .append(from_term_prec(alloc, &constant.term, Prec::Term))
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
        .append(alloc.space())
        .append(":")
        .append(alloc.space())
        .append("Type")
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
                    .append(from_field_declaration(alloc, field))
                    .nest(4)
                    .group()
            })))
            .append(alloc.hardline())
            .append("}")
    };

    (alloc.nil()).append(docs).append(struct_type)
}

pub fn from_struct_format<'a, D>(alloc: &'a D, struct_format: &'a StructFormat) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    let docs = alloc.concat(struct_format.doc.iter().map(|line| {
        (alloc.nil())
            .append(format!("///{}", line))
            .append(alloc.hardline())
    }));

    let struct_prefix = (alloc.nil())
        .append("struct")
        .append(alloc.space())
        .append(alloc.as_string(&struct_format.name))
        .append(alloc.space())
        .append(":")
        .append(alloc.space())
        .append("Format")
        .append(alloc.space());

    let struct_format = if struct_format.fields.is_empty() {
        (alloc.nil()).append(struct_prefix).append("{}").group()
    } else {
        (alloc.nil())
            .append(struct_prefix)
            .append("{")
            .group()
            .append(alloc.concat(struct_format.fields.iter().map(|field| {
                (alloc.nil())
                    .append(alloc.hardline())
                    .append(from_field_declaration(alloc, field))
                    .nest(4)
                    .group()
            })))
            .append(alloc.hardline())
            .append("}")
    };

    (alloc.nil()).append(docs).append(struct_format)
}

pub fn from_struct_term<'a, D>(
    alloc: &'a D,
    field_definitions: &'a [FieldDefinition],
) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    let struct_prefix = (alloc.nil()).append("struct").append(alloc.space());

    if field_definitions.is_empty() {
        (alloc.nil()).append(struct_prefix).append("{}").group()
    } else {
        (alloc.nil())
            .append(struct_prefix)
            .append("{")
            .group()
            .append(
                alloc.concat(field_definitions.iter().map(|field_definition| {
                    (alloc.nil())
                        .append(alloc.hardline())
                        .append(from_field_definition(alloc, field_definition))
                        .nest(4)
                        .group()
                })),
            )
            .append(alloc.hardline())
            .append("}")
    }
}

pub fn from_field_declaration<'a, D>(
    alloc: &'a D,
    field_declaration: &'a FieldDeclaration,
) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    let docs = alloc.concat(field_declaration.doc.iter().map(|line| {
        (alloc.nil())
            .append(format!("///{}", line))
            .append(alloc.hardline())
    }));

    (alloc.nil())
        .append(docs)
        .append(
            (alloc.nil())
                .append(alloc.as_string(&field_declaration.label.data))
                .append(alloc.space())
                .append(":")
                .group(),
        )
        .append(
            (alloc.nil())
                .append(alloc.space())
                .append(from_term_prec(alloc, &field_declaration.type_, Prec::Term))
                .append(","),
        )
}

pub fn from_field_definition<'a, D>(
    alloc: &'a D,
    field_definition: &'a FieldDefinition,
) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    (alloc.nil())
        .append(
            (alloc.nil())
                .append(alloc.as_string(&field_definition.label.data))
                .append(alloc.space())
                .append("=")
                .group(),
        )
        .append(
            (alloc.nil())
                .append(alloc.space())
                .append(from_term_prec(alloc, &field_definition.term, Prec::Term))
                .append(","),
        )
}

pub fn from_primitive<'a, D>(alloc: &'a D, primitive: &'a Primitive) -> DocBuilder<'a, D>
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

    match primitive {
        Primitive::Int(value) => (alloc.nil())
            .append("int")
            .append(alloc.space())
            .append(alloc.as_string(value)),
        Primitive::F32(value) => (alloc.nil())
            .append("f32")
            .append(alloc.space())
            .append(format_float(*value)),
        Primitive::F64(value) => (alloc.nil())
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
    match &term.data {
        TermData::Global(name) => (alloc.nil())
            .append("global")
            .append(alloc.space())
            .append(alloc.as_string(name)),
        TermData::Item(name) => (alloc.nil())
            .append("item")
            .append(alloc.space())
            .append(alloc.as_string(name)),
        TermData::Ann(term, r#type) => paren(
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

        TermData::Sort(Sort::Type) => alloc.text("Type"),
        TermData::Sort(Sort::Kind) => alloc.text("Kind"),

        TermData::FunctionType(param_type, body_type) => paren(
            alloc,
            prec > Prec::Arrow,
            (alloc.nil())
                .append(from_term_prec(alloc, param_type, Prec::App))
                .append(alloc.space())
                .append("->")
                .append(alloc.space())
                .append(from_term_prec(alloc, body_type, Prec::Arrow)),
        ),
        TermData::FunctionElim(head, argument) => paren(
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

        TermData::StructTerm(field_definitions) => from_struct_term(alloc, field_definitions),
        TermData::StructElim(head, label) => (alloc.nil())
            .append(paren(alloc, true, from_term(alloc, head)))
            .append(".")
            .append(alloc.as_string(label)),

        TermData::ArrayTerm(elem_terms) => (alloc.nil())
            .append("array")
            .append(alloc.space())
            .append("[")
            .append(
                alloc.intersperse(
                    elem_terms
                        .iter()
                        .map(|elem_term| from_term(alloc, elem_term)),
                    alloc.text(",").append(alloc.space()),
                ),
            )
            .append("]"),

        TermData::Primitive(primitive) => from_primitive(alloc, primitive),
        TermData::BoolElim(head, if_true, if_false) => (alloc.nil())
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
        TermData::IntElim(head, branches, default) => (alloc.nil())
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

        TermData::FormatType => alloc.text("Format"),

        TermData::Repr => alloc.text("repr"),

        TermData::Error => alloc.text("!"),
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
