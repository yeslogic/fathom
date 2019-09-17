//! Converts the core syntax back into the concrete syntax, reversing some (but
//! not all) parts of elaboration.
//!
//! The naming of this pass is not entirely standard, but was one of the better
//! ones to emerge from [this twitter discussion](https://twitter.com/brendanzab/status/1173798146356342784).

use codespan::Span;

use crate::{concrete, core};

// TODO: name/keyword avoidance!

pub fn delaborate_module(module: &core::Module) -> concrete::Module {
    concrete::Module {
        file_id: module.file_id,
        items: module.items.iter().map(delaborate_item).collect(),
    }
}

pub fn delaborate_item(item: &core::Item) -> concrete::Item {
    match item {
        core::Item::Alias(alias) => {
            let (term, ty) = match &alias.term {
                core::Term::Ann(term, ty) => (delaborate_term(term), Some(delaborate_term(ty))),
                term => (delaborate_term(term), None),
            };

            concrete::Item::Alias(concrete::Alias {
                span: alias.span,
                doc: alias.doc.clone(),
                name: (Span::initial(), alias.name.to_string()),
                ty,
                term,
            })
        }
        core::Item::Struct(struct_ty) => concrete::Item::Struct(concrete::StructType {
            span: struct_ty.span,
            doc: struct_ty.doc.clone(),
            name: (Span::initial(), struct_ty.name.to_string()),
            fields: struct_ty
                .fields
                .iter()
                .map(|ty_field| {
                    concrete::TypeField {
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

pub fn delaborate_term(term: &core::Term) -> concrete::Term {
    delaborate_term_prec(term, 0)
}

pub fn delaborate_term_prec(term: &core::Term, prec: u8) -> concrete::Term {
    let delaborate_paren_prec = |cond, concrete_term: concrete::Term| match cond {
        true => concrete::Term::Paren(concrete_term.span(), Box::new(concrete_term)),
        false => concrete_term,
    };

    match term {
        core::Term::Item(span, label) => concrete::Term::Var(*span, label.to_string()),
        core::Term::Ann(term, ty) => delaborate_paren_prec(
            prec > 0,
            concrete::Term::Ann(
                Box::new(delaborate_term_prec(term, prec + 1)),
                Box::new(delaborate_term_prec(ty, prec + 1)),
            ),
        ),
        core::Term::Kind(span) => concrete::Term::Var(*span, "Kind".to_owned()),
        core::Term::Type(span) => concrete::Term::Var(*span, "Type".to_owned()),
        core::Term::U8Type(span) => concrete::Term::Var(*span, "U8".to_owned()),
        core::Term::U16LeType(span) => concrete::Term::Var(*span, "U16Le".to_owned()),
        core::Term::U16BeType(span) => concrete::Term::Var(*span, "U16Be".to_owned()),
        core::Term::U32LeType(span) => concrete::Term::Var(*span, "U32Le".to_owned()),
        core::Term::U32BeType(span) => concrete::Term::Var(*span, "U32Be".to_owned()),
        core::Term::U64LeType(span) => concrete::Term::Var(*span, "U64Le".to_owned()),
        core::Term::U64BeType(span) => concrete::Term::Var(*span, "U64Be".to_owned()),
        core::Term::S8Type(span) => concrete::Term::Var(*span, "S8".to_owned()),
        core::Term::S16LeType(span) => concrete::Term::Var(*span, "S16Le".to_owned()),
        core::Term::S16BeType(span) => concrete::Term::Var(*span, "S16Be".to_owned()),
        core::Term::S32LeType(span) => concrete::Term::Var(*span, "S32Le".to_owned()),
        core::Term::S32BeType(span) => concrete::Term::Var(*span, "S32Be".to_owned()),
        core::Term::S64LeType(span) => concrete::Term::Var(*span, "S64Le".to_owned()),
        core::Term::S64BeType(span) => concrete::Term::Var(*span, "S64Be".to_owned()),
        core::Term::F32LeType(span) => concrete::Term::Var(*span, "F32Le".to_owned()),
        core::Term::F32BeType(span) => concrete::Term::Var(*span, "F32Be".to_owned()),
        core::Term::F64LeType(span) => concrete::Term::Var(*span, "F64Le".to_owned()),
        core::Term::F64BeType(span) => concrete::Term::Var(*span, "F64Be".to_owned()),
        core::Term::BoolType(span) => concrete::Term::Var(*span, "Bool".to_owned()),
        core::Term::IntType(span) => concrete::Term::Var(*span, "Int".to_owned()),
        core::Term::F32Type(span) => concrete::Term::Var(*span, "F32".to_owned()),
        core::Term::F64Type(span) => concrete::Term::Var(*span, "F64".to_owned()),
        core::Term::BoolConst(span, true) => concrete::Term::Var(*span, "true".to_owned()),
        core::Term::BoolConst(span, false) => concrete::Term::Var(*span, "false".to_owned()),
        core::Term::Error(span) => concrete::Term::Error(*span),
    }
}
