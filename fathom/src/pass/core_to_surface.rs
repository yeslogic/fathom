//! Converts the core syntax back into the surface syntax, reversing some (but
//! not all) parts of elaboration.
//!
//! The naming of this pass is not entirely standard, but was one of the better
//! ones to emerge from [this twitter discussion](https://twitter.com/brendanzab/status/1173798146356342784).

use crate::lang::core::{Item, ItemData, Module, Primitive, Sort, Term, TermData};
use crate::lang::{surface, Ranged};

// TODO: name/keyword avoidance!

pub fn from_module(module: &Module) -> surface::Module {
    surface::Module {
        file_id: module.file_id,
        doc: module.doc.clone(),
        items: module.items.iter().map(from_item).collect(),
    }
}

pub fn from_item(item: &Item) -> surface::Item {
    let item_data = match &item.data {
        ItemData::Constant(constant) => {
            let (term, r#type) = match &constant.term.data {
                TermData::Ann(term, r#type) => (from_term(term), Some(from_term(r#type))),
                _ => (from_term(&constant.term), None),
            };

            surface::ItemData::Constant(surface::Constant {
                doc: constant.doc.clone(),
                name: Ranged::from(constant.name.clone()),
                type_: r#type,
                term,
            })
        }
        ItemData::StructType(struct_type) => surface::ItemData::StructType(surface::StructType {
            doc: struct_type.doc.clone(),
            name: Ranged::from(struct_type.name.clone()),
            type_: Some(surface::Term::from(surface::TermData::TypeType)),
            fields: struct_type
                .fields
                .iter()
                .map(|field_declaration| surface::FieldDeclaration {
                    doc: field_declaration.doc.clone(),
                    label: Ranged::from(field_declaration.label.clone()),
                    type_: from_term(&field_declaration.type_),
                })
                .collect(),
        }),
        ItemData::StructFormat(struct_format) => {
            surface::ItemData::StructType(surface::StructType {
                doc: struct_format.doc.clone(),
                name: Ranged::from(struct_format.name.clone()),
                type_: Some(surface::Term::from(surface::TermData::FormatType)),
                fields: struct_format
                    .fields
                    .iter()
                    .map(|field_declaration| surface::FieldDeclaration {
                        doc: field_declaration.doc.clone(),
                        label: Ranged::from(field_declaration.label.clone()),
                        type_: from_term(&field_declaration.type_),
                    })
                    .collect(),
            })
        }
    };

    surface::Item::from(item_data)
}

pub fn from_term(term: &Term) -> surface::Term {
    let term_data = match &term.data {
        TermData::Global(name) => surface::TermData::Name(name.to_string()),
        TermData::Item(name) => surface::TermData::Name(name.to_string()),
        TermData::Ann(term, r#type) => {
            surface::TermData::Ann(Box::new(from_term(term)), Box::new(from_term(r#type)))
        }

        TermData::Sort(Sort::Kind) => surface::TermData::KindType,
        TermData::Sort(Sort::Type) => surface::TermData::TypeType,

        TermData::FunctionType(param_type, body_type) => surface::TermData::FunctionType(
            Box::new(from_term(param_type)),
            Box::new(from_term(body_type)),
        ),
        TermData::FunctionElim(head, argument) => surface::TermData::FunctionElim(
            Box::new(from_term(head)),
            vec![from_term(argument)], // TODO: flatten arguments
        ),

        TermData::StructTerm(field_definitions) => surface::TermData::StructTerm(
            field_definitions
                .iter()
                .map(|field_definition| surface::FieldDefinition {
                    label: Ranged::from(field_definition.label.clone()),
                    term: from_term(&field_definition.term),
                })
                .collect(),
        ),
        TermData::StructElim(head, field) => {
            surface::TermData::StructElim(Box::new(from_term(head)), Ranged::from(field.clone()))
        }

        TermData::ArrayTerm(elem_terms) => surface::TermData::SequenceTerm(
            elem_terms
                .iter()
                .map(|elem_term| from_term(elem_term))
                .collect(),
        ),

        TermData::Primitive(primitive) => match primitive {
            Primitive::Int(value) => surface::TermData::NumberLiteral(value.to_string()),
            Primitive::F32(value) => surface::TermData::NumberLiteral(value.to_string()),
            Primitive::F64(value) => surface::TermData::NumberLiteral(value.to_string()),
        },
        TermData::BoolElim(head, if_true, if_false) => surface::TermData::If(
            Box::new(from_term(head)),
            Box::new(from_term(if_true)),
            Box::new(from_term(if_false)),
        ),
        TermData::IntElim(head, branches, default) => surface::TermData::Match(
            Box::new(from_term(head)),
            branches
                .iter()
                .map(|(value, term)| {
                    let pattern_data = surface::PatternData::NumberLiteral(value.to_string());
                    (surface::Pattern::from(pattern_data), from_term(term))
                })
                .chain(std::iter::once((
                    surface::Pattern::from(surface::PatternData::Name("_".to_owned())),
                    from_term(default),
                )))
                .collect(),
        ),

        TermData::FormatType => surface::TermData::FormatType,

        TermData::Repr => surface::TermData::Repr,

        TermData::Error => surface::TermData::Error,
    };

    surface::Term::from(term_data)
}
