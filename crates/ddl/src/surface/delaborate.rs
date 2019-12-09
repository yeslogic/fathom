//! Converts the core syntax back into the surface syntax, reversing some (but
//! not all) parts of elaboration.
//!
//! The naming of this pass is not entirely standard, but was one of the better
//! ones to emerge from [this twitter discussion](https://twitter.com/brendanzab/status/1173798146356342784).

use codespan::Span;

use crate::{core, literal, surface};

// TODO: name/keyword avoidance!

pub fn delaborate_module(module: &core::Module) -> surface::Module {
    surface::Module {
        file_id: module.file_id,
        doc: module.doc.clone(),
        items: module.items.iter().map(delaborate_item).collect(),
    }
}

pub fn delaborate_item(item: &core::Item) -> surface::Item {
    match item {
        core::Item::Alias(alias) => {
            let (term, ty) = match &alias.term {
                core::Term::Ann(term, ty) => (delaborate_term(term), Some(delaborate_term(ty))),
                term => (delaborate_term(term), None),
            };

            surface::Item::Alias(surface::Alias {
                span: alias.span,
                doc: alias.doc.clone(),
                name: (Span::initial(), alias.name.to_string()),
                ty,
                term,
            })
        }
        core::Item::Struct(struct_ty) => surface::Item::Struct(surface::StructType {
            span: struct_ty.span,
            doc: struct_ty.doc.clone(),
            name: (Span::initial(), struct_ty.name.to_string()),
            fields: struct_ty
                .fields
                .iter()
                .map(|ty_field| {
                    surface::TypeField {
                        doc: ty_field.doc.clone(),
                        // TODO: use `ty_field.start`
                        name: (Span::initial(), ty_field.name.to_string()),
                        term: delaborate_term(&ty_field.term),
                    }
                })
                .collect(),
        }),
    }
}

pub fn delaborate_term(term: &core::Term) -> surface::Term {
    delaborate_term_prec(term, 0)
}

pub fn delaborate_term_prec(term: &core::Term, prec: u8) -> surface::Term {
    let delaborate_paren_prec = |cond, surface_term: surface::Term| match cond {
        true => surface::Term::Paren(surface_term.span(), Box::new(surface_term)),
        false => surface_term,
    };

    match term {
        core::Term::Item(span, label) => surface::Term::Name(*span, label.to_string()),
        core::Term::Ann(term, ty) => delaborate_paren_prec(
            prec > 0,
            surface::Term::Ann(
                Box::new(delaborate_term_prec(term, prec + 1)),
                Box::new(delaborate_term_prec(ty, prec + 1)),
            ),
        ),
        core::Term::Universe(span, universe) => match universe {
            core::Universe::Type => surface::Term::Name(*span, "Type".to_owned()),
            core::Universe::Format => surface::Term::Name(*span, "Format".to_owned()),
            core::Universe::Kind => surface::Term::Name(*span, "Kind".to_owned()),
        },
        core::Term::Constant(span, constant) => delaborate_constant(*span, constant),
        core::Term::BoolElim(span, head, if_true, if_false) => surface::Term::If(
            *span,
            Box::new(delaborate_term(head)),
            Box::new(delaborate_term(if_true)),
            Box::new(delaborate_term(if_false)),
        ),
        core::Term::IntElim(span, head, branches, default) => surface::Term::Match(
            *span,
            Box::new(delaborate_term(head)),
            branches
                .iter()
                .map(|(value, term)| {
                    let span = Span::default();
                    let value = literal::Number::from_signed(span, value);
                    (
                        surface::Pattern::NumberLiteral(span, value),
                        delaborate_term(term),
                    )
                })
                .chain(std::iter::once((
                    surface::Pattern::Name(Span::default(), "_".to_owned()),
                    delaborate_term(default),
                )))
                .collect(),
        ),
        core::Term::Error(span) => surface::Term::Error(*span),
    }
}

pub fn delaborate_constant(span: Span, constant: &core::Constant) -> surface::Term {
    match constant {
        core::Constant::Bool(true) => surface::Term::Name(span, "true".to_owned()),
        core::Constant::Bool(false) => surface::Term::Name(span, "false".to_owned()),
        core::Constant::Int(value) => {
            surface::Term::NumberLiteral(span, literal::Number::from_signed(span, value))
        }
        core::Constant::F32(value) => {
            surface::Term::NumberLiteral(span, literal::Number::from_signed(span, value))
        }
        core::Constant::F64(value) => {
            surface::Term::NumberLiteral(span, literal::Number::from_signed(span, value))
        }
    }
}
