use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Severity};
use inflector::Inflector;
use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;

use crate::{core, rust};

mod diagnostics;

// TODO: Make this path configurable
const RT_NAME: &str = "ddl_rt";

fn rt_ty_name(name: &str) -> rust::Type {
    rust::Type::name(format!("{}::{}", RT_NAME, name), Vec::new())
}

fn rt_invalid_ty() -> rust::Type {
    rt_ty_name("InvalidDataDescription")
}

fn derives(is_copy: bool) -> Vec<String> {
    let mut derives = Vec::new();
    if is_copy {
        derives.push("Copy".to_owned());
        derives.push("Clone".to_owned());
    }
    derives
}

pub fn compile_module(module: &core::Module, report: &mut dyn FnMut(Diagnostic)) -> rust::Module {
    let mut context = ModuleContext {
        file_id: module.file_id,
        compiled_items: HashMap::new(),
        enum_count: 0,
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
    enum_count: usize,
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
            match read {
                Some(read) => match host_ty {
                    None => unreachable!("type level if for non-format type"),
                    Some(host_ty) => {
                        // Should we be using `ty` somewhere here?
                        context.items.push(rust::Item::Struct(rust::StructType {
                            derives: derives(is_copy),
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

    let mut is_copy = true;
    let mut fields = Vec::with_capacity(core_struct_ty.fields.len());
    let mut read_statements = Vec::with_capacity(core_struct_ty.fields.len());

    for field in &core_struct_ty.fields {
        let name = field.name.0.to_snake_case();
        let (format_ty, host_ty, read, is_field_copy) =
            match compile_term(context, &field.term, report) {
                // TODO: error message!
                CompiledTerm::Term { .. } => (rt_invalid_ty(), rt_invalid_ty(), None, true),
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
                        (rt_invalid_ty(), rt_invalid_ty(), None, true)
                    }
                },
                CompiledTerm::Erased => {
                    report(diagnostics::non_format_type_as_host_type(
                        Severity::Bug,
                        context.file_id,
                        field.term.span(),
                    ));
                    (rt_invalid_ty(), rt_invalid_ty(), None, true)
                }
                CompiledTerm::Error => (rt_invalid_ty(), rt_invalid_ty(), None, true),
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
    context.items.push(rust::Item::Struct(rust::StructType {
        derives: derives(is_copy),
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
    let compiled_host_ty = |ty| CompiledTerm::Type {
        ty,
        is_copy: true,
        host_ty: None,
        read: None,
    };
    let compiled_format_ty = |ty, host_ty| CompiledTerm::Type {
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
        core::Term::U8Type(_) => compiled_format_ty(rt_ty_name("U8"), ty_name("u8")),
        core::Term::U16LeType(_) => compiled_format_ty(rt_ty_name("U16Le"), ty_name("u16")),
        core::Term::U16BeType(_) => compiled_format_ty(rt_ty_name("U16Be"), ty_name("u16")),
        core::Term::U32LeType(_) => compiled_format_ty(rt_ty_name("U32Le"), ty_name("u32")),
        core::Term::U32BeType(_) => compiled_format_ty(rt_ty_name("U32Be"), ty_name("u32")),
        core::Term::U64LeType(_) => compiled_format_ty(rt_ty_name("U64Le"), ty_name("u64")),
        core::Term::U64BeType(_) => compiled_format_ty(rt_ty_name("U64Be"), ty_name("u64")),
        core::Term::S8Type(_) => compiled_format_ty(rt_ty_name("I8"), ty_name("i8")),
        core::Term::S16LeType(_) => compiled_format_ty(rt_ty_name("I16Le"), ty_name("i16")),
        core::Term::S16BeType(_) => compiled_format_ty(rt_ty_name("I16Be"), ty_name("i16")),
        core::Term::S32LeType(_) => compiled_format_ty(rt_ty_name("I32Le"), ty_name("i32")),
        core::Term::S32BeType(_) => compiled_format_ty(rt_ty_name("I32Be"), ty_name("i32")),
        core::Term::S64LeType(_) => compiled_format_ty(rt_ty_name("I64Le"), ty_name("i64")),
        core::Term::S64BeType(_) => compiled_format_ty(rt_ty_name("I64Be"), ty_name("i64")),
        core::Term::F32LeType(_) => compiled_format_ty(rt_ty_name("F32Le"), ty_name("f32")),
        core::Term::F32BeType(_) => compiled_format_ty(rt_ty_name("F32Be"), ty_name("f32")),
        core::Term::F64LeType(_) => compiled_format_ty(rt_ty_name("F64Le"), ty_name("f64")),
        core::Term::F64BeType(_) => compiled_format_ty(rt_ty_name("F64Be"), ty_name("f64")),
        core::Term::BoolType(_) => compiled_host_ty(ty_name("bool")),
        core::Term::IntType(span) => {
            report(diagnostics::error::unconstrained_int(file_id, *span));
            compiled_host_ty(rt_invalid_ty())
        }
        core::Term::F32Type(_) => compiled_host_ty(ty_name("f32")),
        core::Term::F64Type(_) => compiled_host_ty(ty_name("f64")),
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
        core::Term::BoolElim(_, head, if_true, if_false) => {
            compile_bool_elim(context, head, if_true, if_false, report)
        }
        core::Term::IntElim(span, head, branches, default) => {
            compile_int_elim(context, *span, head, branches, default, report)
        }
        core::Term::Universe(_, _) => CompiledTerm::Erased,
        core::Term::Error(_) => CompiledTerm::Error,
    }
}

fn compile_bool_elim(
    context: &mut ModuleContext,
    head: &core::Term,
    if_true: &core::Term,
    if_false: &core::Term,
    report: &mut dyn FnMut(Diagnostic),
) -> CompiledTerm {
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
        ) => {
            let mut is_impossible = true;
            let (true_host_ty, true_read) = match true_host_ty {
                Some(host_ty) => {
                    is_impossible = false;
                    (
                        host_ty,
                        true_read.unwrap_or_else(|| rust::Term::Read(Box::new(true_ty))),
                    )
                }
                None => {
                    report(diagnostics::non_format_type_as_host_type(
                        Severity::Error,
                        context.file_id,
                        if_true.span(),
                    ));
                    (rt_invalid_ty(), rust::Term::Read(Box::new(rt_invalid_ty())))
                }
            };
            let (false_host_ty, false_read) = match false_host_ty {
                Some(host_ty) => {
                    is_impossible = false;
                    (
                        host_ty,
                        false_read.unwrap_or_else(|| rust::Term::Read(Box::new(false_ty))),
                    )
                }
                None => {
                    report(diagnostics::non_format_type_as_host_type(
                        Severity::Error,
                        context.file_id,
                        if_false.span(),
                    ));
                    (rt_invalid_ty(), rust::Term::Read(Box::new(rt_invalid_ty())))
                }
            };

            if is_impossible {
                return CompiledTerm::Error;
            }

            // TODO: name avoidance
            // TODO: improve naming
            let enum_name = format!("Enum{}", context.enum_count);
            context.enum_count += 1;

            let is_copy = true_is_copy && false_is_copy;

            let true_name = "True".to_owned();
            let true_ctor = rust::Term::name(format!("{}::{}", enum_name, true_name));
            let false_name = "False".to_owned();
            let false_ctor = rust::Term::name(format!("{}::{}", enum_name, false_name));
            context.items.push(rust::Item::Enum(rust::EnumType {
                derives: derives(is_copy),
                doc: Arc::new([]),
                name: enum_name.clone(),
                variants: vec![
                    rust::Variant {
                        doc: Arc::new([]),
                        name: true_name,
                        ty: true_host_ty,
                    },
                    rust::Variant {
                        doc: Arc::new([]),
                        name: false_name,
                        ty: false_host_ty,
                    },
                ],
            }));

            CompiledTerm::Type {
                ty: rt_invalid_ty(),
                is_copy,
                host_ty: Some(rust::Type::name(enum_name, Vec::new())),
                read: Some(rust::Term::If(
                    Box::new(head),
                    Box::new(rust::Term::Call(Box::new(true_ctor), vec![true_read])),
                    Box::new(rust::Term::Call(Box::new(false_ctor), vec![false_read])),
                )),
            }
        }

        (CompiledTerm::Erased, CompiledTerm::Erased) => CompiledTerm::Erased,
        (CompiledTerm::Error, _) | (_, CompiledTerm::Error) => CompiledTerm::Error,

        // TODO: report bug: mismatched arms of if expression
        (_, _) => unimplemented!(),
    }
}

fn compile_int_elim(
    context: &mut ModuleContext,
    span: Span,
    head: &core::Term,
    branches: &BTreeMap<BigInt, Arc<core::Term>>,
    default: &core::Term,
    report: &mut dyn FnMut(Diagnostic),
) -> CompiledTerm {
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
                            span,
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
            ty: default_ty,
            is_copy: default_is_copy,
            host_ty: default_host_ty,
            read: default_read,
        } => {
            // TODO: name avoidance
            // TODO: improve naming
            let enum_name = format!("Enum{}", context.enum_count);
            context.enum_count += 1;

            let mut is_copy = default_is_copy;
            let mut is_impossible = true;
            let mut variants = Vec::with_capacity(branches.len());
            let mut read_branches = Vec::with_capacity(branches.len());

            for (i, (value, term)) in branches.iter().enumerate() {
                // TODO: don't default to I64.
                let pattern = match value.to_i64() {
                    Some(value) => rust::Pattern::Constant(rust::Constant::I64(value)),
                    None => {
                        report(crate::diagnostics::bug::not_yet_implemented(
                            context.file_id,
                            span,
                            "non-i64 patterns",
                        ));
                        continue;
                    }
                };

                match compile_term(context, term, report) {
                    CompiledTerm::Type {
                        ty: branch_ty,
                        is_copy: branch_is_copy,
                        host_ty: branch_host_ty,
                        read: branch_read,
                    } => {
                        is_copy &= branch_is_copy;
                        let (branch_host_ty, branch_read) = match branch_host_ty {
                            Some(host_ty) => {
                                is_impossible = false;
                                (
                                    host_ty,
                                    branch_read
                                        .unwrap_or_else(|| rust::Term::Read(Box::new(branch_ty))),
                                )
                            }
                            None => {
                                report(diagnostics::non_format_type_as_host_type(
                                    Severity::Error,
                                    context.file_id,
                                    term.span(),
                                ));
                                (rt_invalid_ty(), rust::Term::Read(Box::new(rt_invalid_ty())))
                            }
                        };

                        // TODO: improve naming?
                        let branch_name = format!("Variant{}", i);
                        let branch_ctor =
                            rust::Term::name(format!("{}::{}", enum_name, branch_name));
                        variants.push(rust::Variant {
                            doc: Arc::new([]),
                            name: branch_name,
                            ty: branch_host_ty,
                        });
                        read_branches.push((
                            pattern,
                            rust::Term::Call(Box::new(branch_ctor), vec![branch_read]),
                        ));
                    }
                    // TODO: report bug: mismatched arms of match expression
                    _ => unimplemented!(),
                }
            }

            is_copy &= default_is_copy;
            let (default_host_ty, default_read) = match default_host_ty {
                Some(host_ty) => {
                    is_impossible = false;
                    (
                        host_ty,
                        default_read.unwrap_or_else(|| rust::Term::Read(Box::new(default_ty))),
                    )
                }
                None => {
                    report(diagnostics::non_format_type_as_host_type(
                        Severity::Error,
                        context.file_id,
                        default.span(),
                    ));
                    (rt_invalid_ty(), rust::Term::Read(Box::new(rt_invalid_ty())))
                }
            };

            if is_impossible {
                return CompiledTerm::Error;
            }

            // TODO: improve naming?
            let default_name = "Default".to_owned();
            let default_ctor = rust::Term::name(format!("{}::{}", enum_name, default_name));
            variants.push(rust::Variant {
                doc: Arc::new([]),
                name: default_name,
                ty: default_host_ty,
            });
            read_branches.push((
                rust::Pattern::Name("_".into()), // TODO: Use pattern name
                rust::Term::Call(Box::new(default_ctor), vec![default_read]),
            ));

            context.items.push(rust::Item::Enum(rust::EnumType {
                derives: derives(is_copy),
                doc: Arc::new([]),
                name: enum_name.clone(),
                variants,
            }));

            CompiledTerm::Type {
                ty: rt_invalid_ty(),
                is_copy,
                host_ty: Some(rust::Type::name(enum_name.clone(), Vec::new())),
                read: Some(rust::Term::Match(Box::new(head), read_branches)),
            }
        }
        CompiledTerm::Erased => CompiledTerm::Erased,
        CompiledTerm::Error => CompiledTerm::Error,
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
