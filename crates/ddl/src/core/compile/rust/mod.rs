use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;
use inflector::Inflector;
use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use std::collections::HashMap;
use std::sync::Arc;

use crate::{core, rust};

mod diagnostics;

pub fn compile_module(module: &core::Module, report: &mut dyn FnMut(Diagnostic)) -> rust::Module {
    let mut context = ModuleContext {
        file_id: module.file_id,
        compiled_items: HashMap::new(),
        items: Vec::new(),
    };

    for core_item in &module.items {
        compile_item(&mut context, core_item, report);
    }

    rust::Module {
        doc: module.doc.clone(),
        items: context.items,
    }
}

#[derive(Debug, Clone)]
enum CompiledItem {
    Term {
        span: Span,
        name: String,
        ty: rust::Type,
        is_function: bool,
        is_const: bool,
    },
    Type {
        span: Span,
        name: String,
        is_copy: bool,
        host_ty: Option<rust::Type>,
    },
    Erased(Span),
    Error(Span),
}

impl CompiledItem {
    fn span(&self) -> Span {
        match self {
            CompiledItem::Term { span, .. }
            | CompiledItem::Type { span, .. }
            | CompiledItem::Erased(span)
            | CompiledItem::Error(span) => *span,
        }
    }
}

struct ModuleContext {
    file_id: FileId,
    compiled_items: HashMap<core::Label, CompiledItem>,
    items: Vec<rust::Item>,
}

fn compile_item(
    context: &mut ModuleContext,
    core_item: &core::Item,
    report: &mut dyn FnMut(Diagnostic),
) {
    match core_item {
        core::Item::Alias(core_alias) => compile_alias(context, core_alias, report),
        core::Item::Struct(core_struct_ty) => compile_struct_ty(context, core_struct_ty, report),
    }
}

fn compile_alias(
    context: &mut ModuleContext,
    core_alias: &core::Alias,
    report: &mut dyn FnMut(Diagnostic),
) {
    use std::collections::hash_map::Entry;

    let span = core_alias.span;

    let compiled_item = match compile_term(context, &core_alias.term, report) {
        CompiledTerm::Term { term, ty, is_const } => {
            let doc = core_alias.doc.clone();
            if is_const {
                let name = core_alias.name.0.to_screaming_snake_case(); // TODO: name avoidance
                context.items.push(rust::Item::Const(rust::Const {
                    doc,
                    name: name.clone(),
                    ty: ty.clone(),
                    term,
                }));
                CompiledItem::Term {
                    span,
                    name,
                    ty,
                    is_function: false,
                    is_const,
                }
            } else {
                let name = core_alias.name.0.to_snake_case(); // TODO: name avoidance
                context.items.push(rust::Item::Function(rust::Function {
                    doc,
                    name: name.clone(),
                    is_const,
                    ty: ty.clone(),
                    block: rust::Block::new(Vec::new(), term),
                }));
                CompiledItem::Term {
                    span,
                    name,
                    ty,
                    is_function: true,
                    is_const,
                }
            }
        }
        CompiledTerm::Erased => CompiledItem::Erased(span),
        CompiledTerm::Error => CompiledItem::Error(span),
        CompiledTerm::Type {
            ty,
            is_copy,
            host_ty,
            read,
        } => {
            let doc = core_alias.doc.clone();
            let name = core_alias.name.0.to_pascal_case(); // TODO: name avoidance
            let mut derives = Vec::new();
            if is_copy {
                derives.push("Copy".to_owned());
                derives.push("Clone".to_owned());
            }

            match read {
                Some(read) => match host_ty {
                    None => unreachable!("type level if for non-format type"),
                    Some(host_ty) => {
                        // Should we be using `ty` somewhere here?
                        context.items.push(rust::Item::Struct(rust::StructType {
                            derives,
                            doc,
                            name: name.clone(),
                            read: Some(rust::Block::new(
                                vec![rust::Statement::Let("inner".to_owned(), Box::new(read))],
                                rust::Term::Call(
                                    Box::new(rust::Term::name("Ok")),
                                    vec![rust::Term::Struct(
                                        name.clone(),
                                        vec![("inner".to_owned(), None)],
                                    )],
                                ),
                            )),
                            fields: vec![rust::TypeField {
                                doc: Arc::new([]),
                                name: "inner".to_owned(),
                                ty: host_ty,
                                by_ref: !is_copy,
                            }],
                        }));
                        CompiledItem::Type {
                            span,
                            name: name.clone(),
                            is_copy,
                            host_ty: Some(rust::Type::name(name.clone(), Vec::new())),
                        }
                    }
                },
                None => {
                    context.items.push(rust::Item::Alias(rust::Alias {
                        doc,
                        name: name.clone(),
                        ty,
                    }));
                    CompiledItem::Type {
                        span,
                        name,
                        is_copy,
                        host_ty,
                    }
                }
            }
        }
    };

    match context.compiled_items.entry(core_alias.name.clone()) {
        Entry::Occupied(entry) => {
            report(diagnostics::bug::item_name_reused(
                context.file_id,
                entry.key(),
                span,
                entry.get().span(),
            ));
        }
        Entry::Vacant(entry) => {
            entry.insert(compiled_item);
        }
    }
}

fn compile_struct_ty(
    context: &mut ModuleContext,
    core_struct_ty: &core::StructType,
    report: &mut dyn FnMut(Diagnostic),
) {
    use std::collections::hash_map::Entry;

    let invalid_type = || rust::Type::name("ddl_rt::InvalidDataDescription", Vec::new());

    let mut is_copy = true;
    let mut fields = Vec::with_capacity(core_struct_ty.fields.len());
    let mut read_statements = Vec::with_capacity(core_struct_ty.fields.len());

    for field in &core_struct_ty.fields {
        let name = field.name.0.to_snake_case();
        let (format_ty, host_ty, read, is_field_copy) =
            match compile_term(context, &field.term, report) {
                // TODO: error message!
                CompiledTerm::Term { .. } => (invalid_type(), invalid_type(), None, true),
                CompiledTerm::Type {
                    ty,
                    is_copy,
                    host_ty,
                    read,
                } => match &host_ty {
                    Some(host_ty) => (ty, host_ty.clone(), read, is_copy),
                    None => {
                        report(diagnostics::bug::host_type_found_in_field(
                            context.file_id,
                            core_struct_ty.span,
                            field.term.span(),
                        ));
                        (invalid_type(), invalid_type(), None, true)
                    }
                },
                CompiledTerm::Erased => {
                    report(diagnostics::bug::non_format_type_as_host_type(
                        context.file_id,
                        field.term.span(),
                    ));
                    (invalid_type(), invalid_type(), None, true)
                }
                CompiledTerm::Error => (invalid_type(), invalid_type(), None, true),
            };

        is_copy &= is_field_copy;
        fields.push(rust::TypeField {
            doc: field.doc.clone(),
            name,
            ty: host_ty,
            by_ref: !is_field_copy,
        });
        read_statements.push(rust::Statement::Let(
            field.name.0.clone(),
            Box::new(read.unwrap_or_else(|| rust::Term::Read(Box::new(format_ty)))),
        ));
    }

    let name = core_struct_ty.name.0.to_pascal_case(); // TODO: name avoidance
    let mut derives = Vec::new();
    if is_copy {
        derives.push("Copy".to_owned());
        derives.push("Clone".to_owned());
    }

    context.items.push(rust::Item::Struct(rust::StructType {
        derives,
        doc: core_struct_ty.doc.clone(),
        name: name.clone(),
        read: Some(rust::Block::new(
            read_statements,
            rust::Term::Call(
                Box::new(rust::Term::name("Ok")),
                vec![rust::Term::Struct(
                    name.clone(),
                    fields
                        .iter()
                        .map(|field| (field.name.clone(), None))
                        .collect(),
                )],
            ),
        )),
        fields,
    }));

    match context.compiled_items.entry(core_struct_ty.name.clone()) {
        Entry::Occupied(entry) => {
            report(diagnostics::bug::item_name_reused(
                context.file_id,
                entry.key(),
                core_struct_ty.span,
                entry.get().span(),
            ));
        }
        Entry::Vacant(entry) => {
            entry.insert(CompiledItem::Type {
                span: core_struct_ty.span,
                name: name.clone(),
                is_copy,
                host_ty: Some(rust::Type::name(name.clone(), Vec::new())),
            });
        }
    }
}

enum CompiledTerm {
    Term {
        term: rust::Term,
        ty: rust::Type,
        is_const: bool,
    },
    Type {
        ty: rust::Type,
        is_copy: bool,
        host_ty: Option<rust::Type>,
        read: Option<rust::Term>,
    },
    Erased,
    Error,
}

fn compile_term(
    context: &mut ModuleContext,
    core_term: &core::Term,
    report: &mut dyn FnMut(Diagnostic),
) -> CompiledTerm {
    let file_id = context.file_id;

    let ty_name = |name| rust::Type::name(name, Vec::new());
    let host_ty = |ty| CompiledTerm::Type {
        ty,
        is_copy: true,
        host_ty: None,
        read: None,
    };
    let format_ty = |ty, host_ty| CompiledTerm::Type {
        ty,
        is_copy: true,
        host_ty: Some(host_ty),
        read: None,
    };

    match core_term {
        core::Term::Item(span, label) => match context.compiled_items.get(label) {
            Some(CompiledItem::Term {
                name,
                ty,
                is_function,
                is_const,
                ..
            }) => CompiledTerm::Term {
                term: if *is_function {
                    rust::Term::Call(Box::new(rust::Term::Name(name.clone().into())), Vec::new())
                } else {
                    rust::Term::Name(name.clone().into())
                },
                ty: ty.clone(),
                is_const: *is_const,
            },
            Some(CompiledItem::Type {
                name,
                is_copy,
                host_ty,
                ..
            }) => CompiledTerm::Type {
                ty: rust::Type::Name(name.clone().into(), Vec::new()),
                is_copy: *is_copy,
                host_ty: host_ty.clone(),
                read: None,
            },
            Some(CompiledItem::Erased(_)) => CompiledTerm::Erased,
            Some(CompiledItem::Error(_)) => CompiledTerm::Error,
            None => {
                report(diagnostics::bug::unbound_item(file_id, label, *span));
                CompiledTerm::Error
            }
        },
        core::Term::Ann(term, _) => compile_term(context, term, report),
        core::Term::U8Type(_) => format_ty(ty_name("ddl_rt::U8"), ty_name("u8")),
        core::Term::U16LeType(_) => format_ty(ty_name("ddl_rt::U16Le"), ty_name("u16")),
        core::Term::U16BeType(_) => format_ty(ty_name("ddl_rt::U16Be"), ty_name("u16")),
        core::Term::U32LeType(_) => format_ty(ty_name("ddl_rt::U32Le"), ty_name("u32")),
        core::Term::U32BeType(_) => format_ty(ty_name("ddl_rt::U32Be"), ty_name("u32")),
        core::Term::U64LeType(_) => format_ty(ty_name("ddl_rt::U64Le"), ty_name("u64")),
        core::Term::U64BeType(_) => format_ty(ty_name("ddl_rt::U64Be"), ty_name("u64")),
        core::Term::S8Type(_) => format_ty(ty_name("ddl_rt::I8"), ty_name("i8")),
        core::Term::S16LeType(_) => format_ty(ty_name("ddl_rt::I16Le"), ty_name("i16")),
        core::Term::S16BeType(_) => format_ty(ty_name("ddl_rt::I16Be"), ty_name("i16")),
        core::Term::S32LeType(_) => format_ty(ty_name("ddl_rt::I32Le"), ty_name("i32")),
        core::Term::S32BeType(_) => format_ty(ty_name("ddl_rt::I32Be"), ty_name("i32")),
        core::Term::S64LeType(_) => format_ty(ty_name("ddl_rt::I64Le"), ty_name("i64")),
        core::Term::S64BeType(_) => format_ty(ty_name("ddl_rt::I64Be"), ty_name("i64")),
        core::Term::F32LeType(_) => format_ty(ty_name("ddl_rt::F32Le"), ty_name("f32")),
        core::Term::F32BeType(_) => format_ty(ty_name("ddl_rt::F32Be"), ty_name("f32")),
        core::Term::F64LeType(_) => format_ty(ty_name("ddl_rt::F64Le"), ty_name("f64")),
        core::Term::F64BeType(_) => format_ty(ty_name("ddl_rt::F64Be"), ty_name("f64")),
        core::Term::BoolType(_) => host_ty(ty_name("bool")),
        core::Term::IntType(span) => {
            report(diagnostics::error::unconstrained_int(file_id, *span));
            host_ty(ty_name("ddl_rt::InvalidDataDescription"))
        }
        core::Term::F32Type(_) => host_ty(ty_name("f32")),
        core::Term::F64Type(_) => host_ty(ty_name("f64")),
        core::Term::BoolConst(_, value) => CompiledTerm::Term {
            term: match value {
                true => rust::Term::name("true"),
                false => rust::Term::name("false"),
            },
            ty: ty_name("bool"),
            is_const: true,
        },
        core::Term::IntConst(span, value) => {
            match value.to_i64() {
                // TODO: don't default to I64.
                Some(value) => CompiledTerm::Term {
                    term: rust::Term::Constant(rust::Constant::I64(value)),
                    ty: ty_name("i64"),
                    is_const: true,
                },
                None => {
                    report(crate::diagnostics::bug::not_yet_implemented(
                        context.file_id,
                        *span,
                        "non-i64 types",
                    ));
                    CompiledTerm::Error
                }
            }
        }
        core::Term::F32Const(_, value) => CompiledTerm::Term {
            term: rust::Term::Constant(rust::Constant::F32(*value)),
            ty: ty_name("f32"),
            is_const: true,
        },
        core::Term::F64Const(_, value) => CompiledTerm::Term {
            term: rust::Term::Constant(rust::Constant::F64(*value)),
            ty: ty_name("f64"),
            is_const: true,
        },
        core::Term::BoolElim(span, head, if_true, if_false) => {
            let head = match compile_term(context, head, report) {
                CompiledTerm::Term { term: head, .. } => head,
                // TODO: Error
                _ => return CompiledTerm::Error,
            };

            match (
                compile_term(context, if_true, report),
                compile_term(context, if_false, report),
            ) {
                (
                    CompiledTerm::Term {
                        term: if_true,
                        ty: if_true_ty,
                        ..
                    },
                    CompiledTerm::Term { term: if_false, .. },
                ) => CompiledTerm::Term {
                    term: rust::Term::If(Box::new(head), Box::new(if_true), Box::new(if_false)),
                    ty: if_true_ty, // TODO: check if arms match
                    is_const: false,
                },
                (
                    CompiledTerm::Type {
                        ty: true_ty,
                        is_copy: true_is_copy,
                        host_ty: true_host_ty,
                        read: true_read,
                    },
                    CompiledTerm::Type {
                        ty: false_ty,
                        is_copy: false_is_copy,
                        host_ty: false_host_ty,
                        read: false_read,
                    },
                ) => match (true_host_ty, false_host_ty) {
                    (Some(true_host_ty), Some(false_host_ty)) => CompiledTerm::Type {
                        ty: ty_name("ddl_rt::InvalidDataDescription"),
                        is_copy: true_is_copy && false_is_copy,
                        host_ty: Some(rust::Type::name(
                            "ddl_rt::Either",
                            vec![true_host_ty, false_host_ty],
                        )),
                        read: Some(rust::Term::If(
                            Box::new(head),
                            Box::new(rust::Term::Call(
                                Box::new(rust::Term::name("ddl_rt::Either::Left")),
                                vec![true_read
                                    .unwrap_or_else(|| rust::Term::Read(Box::new(true_ty)))],
                            )),
                            Box::new(rust::Term::Call(
                                Box::new(rust::Term::name("ddl_rt::Either::Right")),
                                vec![false_read
                                    .unwrap_or_else(|| rust::Term::Read(Box::new(false_ty)))],
                            )),
                        )),
                    },
                    (_, _) => {
                        report(diagnostics::error::type_level_if_expression(file_id, *span));
                        CompiledTerm::Error
                    }
                },

                (CompiledTerm::Erased, CompiledTerm::Erased) => CompiledTerm::Erased,
                (CompiledTerm::Error, _) | (_, CompiledTerm::Error) => CompiledTerm::Error,

                // TODO: report bug: mismatched arms of if expression
                (_, _) => unimplemented!(),
            }
        }
        core::Term::IntElim(span, head, branches, default) => {
            let head = match compile_term(context, head, report) {
                CompiledTerm::Term { term: head, .. } => head,
                // TODO: Error
                _ => return CompiledTerm::Error,
            };

            match compile_term(context, default, report) {
                CompiledTerm::Term {
                    term: default,
                    ty: default_ty,
                    ..
                } => {
                    let branches = branches
                        .iter()
                        .filter_map(|(value, term)| match value.to_i64() {
                            Some(value) => Some((
                                rust::Pattern::Constant(rust::Constant::I64(value)),
                                match compile_term(context, term, report) {
                                    CompiledTerm::Term { term, .. } => term,
                                    // TODO: report bug: mismatched arms of match expression
                                    _ => rust::Term::Panic("error term".into()),
                                },
                            )),
                            None => {
                                report(crate::diagnostics::bug::not_yet_implemented(
                                    context.file_id,
                                    *span,
                                    "non-i64 patterns",
                                ));
                                None
                            }
                        })
                        .chain(std::iter::once((rust::Pattern::name("_"), default))) // TODO: Use pattern name
                        .collect();

                    CompiledTerm::Term {
                        term: rust::Term::Match(Box::new(head), branches),
                        ty: default_ty,
                        is_const: false,
                    }
                }
                CompiledTerm::Type {
                    ty: _default_ty,
                    is_copy: _default_is_copy,
                    host_ty: _default_host_ty,
                    ..
                } => {
                    report(crate::diagnostics::bug::not_yet_implemented(
                        context.file_id,
                        *span,
                        "type-level match expressions",
                    ));
                    CompiledTerm::Error
                }
                CompiledTerm::Erased => CompiledTerm::Erased,
                CompiledTerm::Error => CompiledTerm::Error,
            }
        }
        core::Term::Universe(_, _) => CompiledTerm::Erased,
        core::Term::Error(_) => CompiledTerm::Error,
    }
}

#[allow(dead_code)]
fn host_int(min: &BigInt, max: &BigInt) -> Option<rust::Type> {
    use std::{i16, i32, i64, i8, u16, u32, u64, u8};

    use crate::rust::Type;

    match () {
        () if *min >= u8::MIN.into() && *max <= u8::MAX.into() => Some(Type::name("u8", vec![])),
        () if *min >= u16::MIN.into() && *max <= u16::MAX.into() => Some(Type::name("u16", vec![])),
        () if *min >= u32::MIN.into() && *max <= u32::MAX.into() => Some(Type::name("u32", vec![])),
        () if *min >= u64::MIN.into() && *max <= u64::MAX.into() => Some(Type::name("u64", vec![])),
        () if *min >= i8::MIN.into() && *max <= i8::MAX.into() => Some(Type::name("i8", vec![])),
        () if *min >= i16::MIN.into() && *max <= i16::MAX.into() => Some(Type::name("i16", vec![])),
        () if *min >= i32::MIN.into() && *max <= i32::MAX.into() => Some(Type::name("i32", vec![])),
        () if *min >= i64::MIN.into() && *max <= i64::MAX.into() => Some(Type::name("i64", vec![])),
        () if min > max => None, // Impossible range
        _ => None,               // TODO: use bigint if outside bounds
    }
}
