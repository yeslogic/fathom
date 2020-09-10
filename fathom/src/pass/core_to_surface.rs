//! Converts the core syntax back into the surface syntax, reversing some (but
//! not all) parts of elaboration.
//!
//! The naming of this pass is not entirely standard, but was one of the better
//! ones to emerge from [this twitter discussion](https://twitter.com/brendanzab/status/1173798146356342784).

use std::ops::Range;

use crate::lang::{core, surface};
use crate::literal;

// TODO: name/keyword avoidance!

pub fn from_module(module: &core::Module) -> surface::Module {
    surface::Module {
        file_id: module.file_id,
        doc: module.doc.clone(),
        items: module.items.iter().map(from_item).collect(),
    }
}

pub fn from_item(item: &core::Item) -> surface::Item {
    match item {
        core::Item::Alias(alias) => {
            let (term, r#type) = match alias.term.as_ref() {
                core::Term::Ann(term, r#type) => (from_term(term), Some(from_term(r#type))),
                term => (from_term(term), None),
            };

            surface::Item::Alias(surface::Alias {
                range: alias.range.clone(),
                doc: alias.doc.clone(),
                name: (0..0, alias.name.to_string()),
                type_: r#type,
                term,
            })
        }
        core::Item::Struct(struct_type) => surface::Item::Struct(surface::StructType {
            range: struct_type.range.clone(),
            doc: struct_type.doc.clone(),
            name: (0..0, struct_type.name.to_string()),
            fields: struct_type
                .fields
                .iter()
                .map(|type_field| {
                    surface::TypeField {
                        doc: type_field.doc.clone(),
                        // TODO: use `type_field.start`
                        name: (0..0, type_field.name.to_string()),
                        term: from_term(&type_field.term),
                    }
                })
                .collect(),
        }),
    }
}

pub fn from_term(term: &core::Term) -> surface::Term {
    match term {
        core::Term::Global(range, name) => surface::Term::Name(range.clone(), name.to_string()),
        core::Term::Item(range, name) => surface::Term::Name(range.clone(), name.to_string()),
        core::Term::Ann(term, r#type) => {
            surface::Term::Ann(Box::new(from_term(term)), Box::new(from_term(r#type)))
        }
        core::Term::TypeType(range) => surface::Term::TypeType(range.clone()),
        core::Term::FunctionType(param_type, body_type) => surface::Term::FunctionType(
            Box::new(from_term(param_type)),
            Box::new(from_term(body_type)),
        ),
        core::Term::FunctionElim(head, argument) => surface::Term::FunctionElim(
            Box::new(from_term(head)),
            vec![from_term(argument)], // TODO: flatten arguments
        ),
        core::Term::Constant(range, constant) => from_constant(range.clone(), constant),
        core::Term::BoolElim(range, head, if_true, if_false) => surface::Term::If(
            range.clone(),
            Box::new(from_term(head)),
            Box::new(from_term(if_true)),
            Box::new(from_term(if_false)),
        ),
        core::Term::IntElim(range, head, branches, default) => surface::Term::Match(
            range.clone(),
            Box::new(from_term(head)),
            branches
                .iter()
                .map(|(value, term)| {
                    let value = literal::Number::from_signed(0..0, value);
                    (
                        surface::Pattern::NumberLiteral(0..0, value),
                        from_term(term),
                    )
                })
                .chain(std::iter::once((
                    surface::Pattern::Name(0..0, "_".to_owned()),
                    from_term(default),
                )))
                .collect(),
        ),
        core::Term::FormatType(range) => surface::Term::FormatType(range.clone()),
        core::Term::Error(range) => surface::Term::Error(range.clone()),
    }
}

pub fn from_constant(range: Range<usize>, constant: &core::Constant) -> surface::Term {
    match constant {
        core::Constant::Int(value) => {
            surface::Term::NumberLiteral(range.clone(), literal::Number::from_signed(range, value))
        }
        core::Constant::F32(value) => {
            surface::Term::NumberLiteral(range.clone(), literal::Number::from_signed(range, value))
        }
        core::Constant::F64(value) => {
            surface::Term::NumberLiteral(range.clone(), literal::Number::from_signed(range, value))
        }
    }
}
