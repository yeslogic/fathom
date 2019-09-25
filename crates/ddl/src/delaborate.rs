//! Converts the core syntax back into the surface syntax, reversing some (but
//! not all) parts of elaboration.
//!
//! The naming of this pass is not entirely standard, but was one of the better
//! ones to emerge from [this twitter discussion](https://twitter.com/brendanzab/status/1173798146356342784).

use codespan::Span;

use crate::{surface, core, literal};

// TODO: name/keyword avoidance!

pub fn delaborate_module(module: &core::Module) -> surface::Module {
    surface::Module {
        file_id: module.file_id,
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
        core::Term::Item(span, label) => surface::Term::Var(*span, label.to_string()),
        core::Term::Ann(term, ty) => delaborate_paren_prec(
            prec > 0,
            surface::Term::Ann(
                Box::new(delaborate_term_prec(term, prec + 1)),
                Box::new(delaborate_term_prec(ty, prec + 1)),
            ),
        ),
        core::Term::Kind(span) => surface::Term::Var(*span, "Kind".to_owned()),
        core::Term::Type(span) => surface::Term::Var(*span, "Type".to_owned()),
        core::Term::U8Type(span) => surface::Term::Var(*span, "U8".to_owned()),
        core::Term::U16LeType(span) => surface::Term::Var(*span, "U16Le".to_owned()),
        core::Term::U16BeType(span) => surface::Term::Var(*span, "U16Be".to_owned()),
        core::Term::U32LeType(span) => surface::Term::Var(*span, "U32Le".to_owned()),
        core::Term::U32BeType(span) => surface::Term::Var(*span, "U32Be".to_owned()),
        core::Term::U64LeType(span) => surface::Term::Var(*span, "U64Le".to_owned()),
        core::Term::U64BeType(span) => surface::Term::Var(*span, "U64Be".to_owned()),
        core::Term::S8Type(span) => surface::Term::Var(*span, "S8".to_owned()),
        core::Term::S16LeType(span) => surface::Term::Var(*span, "S16Le".to_owned()),
        core::Term::S16BeType(span) => surface::Term::Var(*span, "S16Be".to_owned()),
        core::Term::S32LeType(span) => surface::Term::Var(*span, "S32Le".to_owned()),
        core::Term::S32BeType(span) => surface::Term::Var(*span, "S32Be".to_owned()),
        core::Term::S64LeType(span) => surface::Term::Var(*span, "S64Le".to_owned()),
        core::Term::S64BeType(span) => surface::Term::Var(*span, "S64Be".to_owned()),
        core::Term::F32LeType(span) => surface::Term::Var(*span, "F32Le".to_owned()),
        core::Term::F32BeType(span) => surface::Term::Var(*span, "F32Be".to_owned()),
        core::Term::F64LeType(span) => surface::Term::Var(*span, "F64Le".to_owned()),
        core::Term::F64BeType(span) => surface::Term::Var(*span, "F64Be".to_owned()),
        core::Term::BoolType(span) => surface::Term::Var(*span, "Bool".to_owned()),
        core::Term::IntType(span) => surface::Term::Var(*span, "Int".to_owned()),
        core::Term::F32Type(span) => surface::Term::Var(*span, "F32".to_owned()),
        core::Term::F64Type(span) => surface::Term::Var(*span, "F64".to_owned()),
        core::Term::BoolConst(span, true) => surface::Term::Var(*span, "true".to_owned()),
        core::Term::BoolConst(span, false) => surface::Term::Var(*span, "false".to_owned()),
        core::Term::IntConst(span, value) => {
            surface::Term::NumberLiteral(*span, literal::Number::from_signed(*span, value))
        }
        core::Term::F32Const(span, value) => {
            surface::Term::NumberLiteral(*span, literal::Number::from_signed(*span, value))
        }
        core::Term::F64Const(span, value) => {
            surface::Term::NumberLiteral(*span, literal::Number::from_signed(*span, value))
        }
        core::Term::Error(span) => surface::Term::Error(*span),
    }
}
