//! Pretty prints the surface language to a textual form.

use pretty::{DocAllocator, DocBuilder};

use crate::lang::surface::{
    Constant, FieldDeclaration, FieldDefinition, Item, ItemData, Module, Pattern, PatternData,
    StructType, Term, TermData,
};

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
        .append(&constant.name.data)
        .append(alloc.space())
        .append("=")
        .group()
        .append(match &constant.type_ {
            None => alloc.nil(),
            Some(r#type) => (alloc.nil())
                .append(alloc.space())
                .append(from_term_prec(alloc, r#type, Prec::Term))
                .group()
                .nest(4),
        })
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

    let struct_prefix =
        (alloc.nil())
            .append("struct")
            .append(alloc.space())
            .append(&struct_type.name.data)
            .append(alloc.space())
            .append(alloc.concat(
                struct_type.params.iter().map(|(name, r#type)| {
                    from_param(alloc, &name.data, r#type).append(alloc.space())
                }),
            ))
            .append(":")
            .append(match &struct_type.type_ {
                None => alloc.nil(),
                Some(r#type) => (alloc.nil())
                    .append(alloc.space())
                    .append(from_term_prec(alloc, r#type, Prec::Term))
                    .group()
                    .nest(4),
            });

    let struct_type = if struct_type.fields.is_empty() {
        (alloc.nil())
            .append(alloc.space())
            .append(struct_prefix)
            .append("{}")
            .group()
    } else {
        (alloc.nil())
            .append(alloc.space())
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
                .append(&field_declaration.label.data)
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

pub fn from_pattern<'a, D>(alloc: &'a D, pattern: &'a Pattern) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    match &pattern.data {
        PatternData::Name(name) => alloc.text(name),
        PatternData::NumberLiteral(literal) => alloc.as_string(literal),
    }
}

pub fn from_param<'a, D>(alloc: &'a D, name: &'a str, r#type: &'a Term) -> DocBuilder<'a, D>
where
    D: DocAllocator<'a>,
    D::Doc: Clone,
{
    (alloc.nil())
        .append("(")
        .append(alloc.as_string(name))
        .append(alloc.space())
        .append(":")
        .group()
        .append(
            (alloc.space())
                .append(from_term(alloc, r#type))
                .group()
                .nest(4),
        )
        .append(")")
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
        TermData::Name(name) => alloc.text(name),

        TermData::KindType => alloc.text("Kind"),
        TermData::TypeType => alloc.text("Type"),

        TermData::FunctionType(param_type, body_type) => paren(
            alloc,
            prec > Prec::App,
            (alloc.nil())
                .append(from_term_prec(alloc, param_type, Prec::Atomic))
                .append(alloc.space())
                .append("->")
                .append(alloc.space())
                .append(from_term_prec(alloc, body_type, Prec::Arrow)),
        ),
        TermData::FunctionElim(head, arguments) => paren(
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

        TermData::StructTerm(field_definitions) => from_struct_term(alloc, field_definitions),
        TermData::StructElim(head, label) => (alloc.nil())
            .append(from_term_prec(alloc, head, Prec::Atomic))
            .append(".")
            .append(alloc.as_string(&label.data)),

        TermData::SequenceTerm(elem_terms) => (alloc.nil())
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

        TermData::NumberLiteral(literal) => alloc.as_string(literal),
        TermData::If(head, if_true, if_false) => (alloc.nil())
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
        TermData::Match(head, branches) => (alloc.nil())
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
