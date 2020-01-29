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

pub fn compile_module(
    globals: &core::Globals,
    module: &core::Module,
    report: &mut dyn FnMut(Diagnostic),
) -> rust::Module {
    let mut context = Context {
        globals,
        file_id: module.file_id,
        enum_count: 0,
        compiled_items: HashMap::new(),
        core_items: HashMap::new(),
        rust_items: Vec::new(),
    };

    for core_item in &module.items {
        compile_item(&mut context, core_item, report);
    }

    rust::Module {
        doc: module.doc.clone(),
        items: context.rust_items,
    }
}

struct TermItem {
    span: Span,
    rust_name: String,
    rust_ty: rust::Type,
    is_function: bool,
    is_const: bool,
}

impl TermItem {
    fn to_term(&self) -> Term {
        Term {
            rust_term: if self.is_function {
                rust::Term::call(rust::Term::name(self.rust_name.clone()), Vec::new())
            } else {
                rust::Term::name(self.rust_name.clone())
            },
            rust_ty: self.rust_ty.clone(),
            is_const: self.is_const,
        }
    }
}

struct TypeItem {
    span: Span,
    rust_name: String,
    is_copy: bool,
    host_ty: Option<rust::Type>,
}

impl TypeItem {
    fn to_ty(&self) -> Type {
        Type {
            rust_ty: rust::Type::Name(self.rust_name.clone().into(), Vec::new()),
            is_copy: self.is_copy,
            host_ty: self.host_ty.clone(),
            read: None,
        }
    }
}

enum CompiledItem {
    Term(TermItem),
    Type(TypeItem),
    Erased(Span),
    Error(Span),
}

impl CompiledItem {
    fn span(&self) -> Span {
        match self {
            CompiledItem::Term(term_item) => term_item.span,
            CompiledItem::Type(ty_item) => ty_item.span,
            CompiledItem::Erased(span) | CompiledItem::Error(span) => *span,
        }
    }
}

struct Context<'me> {
    globals: &'me core::Globals,
    file_id: FileId,
    enum_count: usize,
    compiled_items: HashMap<&'me str, CompiledItem>,
    core_items: HashMap<&'me str, core::Item>,
    rust_items: Vec<rust::Item>,
}

fn compile_item<'item>(
    context: &mut Context<'item>,
    core_item: &'item core::Item,
    report: &mut dyn FnMut(Diagnostic),
) {
    match core_item {
        core::Item::Alias(core_alias) => compile_alias(context, core_alias, report),
        core::Item::Struct(core_struct_ty) => compile_struct_ty(context, core_struct_ty, report),
    }
}

fn compile_alias<'item>(
    context: &mut Context<'item>,
    core_alias: &'item core::Alias,
    report: &mut dyn FnMut(Diagnostic),
) {
    use std::collections::hash_map::Entry;

    let span = core_alias.span;

    let item = match compile_term(context, &core_alias.term, report) {
        CompiledTerm::Term(term) => {
            let doc = core_alias.doc.clone();
            if term.is_const {
                let rust_name = core_alias.name.to_screaming_snake_case(); // TODO: name avoidance
                context.rust_items.push(rust::Item::Const(rust::Const {
                    doc,
                    name: rust_name.clone(),
                    ty: term.rust_ty.clone(),
                    term: term.rust_term,
                }));
                CompiledItem::Term(TermItem {
                    span,
                    rust_name,
                    rust_ty: term.rust_ty,
                    is_function: false,
                    is_const: term.is_const,
                })
            } else {
                let rust_name = core_alias.name.to_snake_case(); // TODO: name avoidance
                let rust_item = rust::Item::Function(rust::Function {
                    doc,
                    name: rust_name.clone(),
                    is_const: term.is_const,
                    ty: term.rust_ty.clone(),
                    block: rust::Block::new(Vec::new(), term.rust_term),
                });
                context.rust_items.push(rust_item);
                CompiledItem::Term(TermItem {
                    span,
                    rust_name,
                    rust_ty: term.rust_ty,
                    is_function: true,
                    is_const: term.is_const,
                })
            }
        }
        CompiledTerm::Type(ty) => {
            let doc = core_alias.doc.clone();
            let rust_name = core_alias.name.to_pascal_case(); // TODO: name avoidance
            match ty.read {
                Some(read) => match ty.host_ty {
                    None => unreachable!("type level if for non-format type"),
                    Some(host_ty) => {
                        // Should we be using `ty` somewhere here?
                        // FIXME: Coercions between definitionally equal aliases?
                        let rust_item = rust::Item::Struct(rust::StructType {
                            derives: derives(ty.is_copy),
                            doc,
                            name: rust_name.clone(),
                            read: Some(rust::Block::new(
                                vec![rust::Statement::Let("inner".to_owned(), Box::new(read))],
                                rust::Term::call(
                                    rust::Term::name("Ok"),
                                    vec![rust::Term::Struct(
                                        rust_name.clone(),
                                        vec![("inner".to_owned(), None)],
                                    )],
                                ),
                            )),
                            fields: vec![rust::TypeField {
                                doc: Arc::new([]),
                                name: "inner".to_owned(),
                                ty: host_ty,
                                by_ref: !ty.is_copy,
                            }],
                        });
                        context.rust_items.push(rust_item);
                        CompiledItem::Type(TypeItem {
                            span,
                            rust_name: rust_name.clone(),
                            is_copy: ty.is_copy,
                            host_ty: Some(rust::Type::name(rust_name, Vec::new())),
                        })
                    }
                },
                None => {
                    context.rust_items.push(rust::Item::Alias(rust::Alias {
                        doc,
                        name: rust_name.clone(),
                        ty: ty.rust_ty,
                    }));
                    CompiledItem::Type(TypeItem {
                        span,
                        rust_name,
                        is_copy: ty.is_copy,
                        host_ty: ty.host_ty,
                    })
                }
            }
        }
        CompiledTerm::Erased => CompiledItem::Erased(span),
        CompiledTerm::Error => CompiledItem::Error(span),
    };

    match context.compiled_items.entry(&core_alias.name) {
        Entry::Occupied(entry) => {
            report(diagnostics::bug::item_name_reused(
                context.file_id,
                entry.key(),
                span,
                entry.get().span(),
            ));
        }
        Entry::Vacant(entry) => {
            entry.insert(item);
            let core_item = core::Item::Alias(core_alias.clone());
            context.core_items.insert(&core_alias.name, core_item);
        }
    }
}

fn compile_struct_ty<'item>(
    context: &mut Context<'item>,
    core_struct_ty: &'item core::StructType,
    report: &mut dyn FnMut(Diagnostic),
) {
    use std::collections::hash_map::Entry;

    let mut is_copy = true;
    let mut fields = Vec::with_capacity(core_struct_ty.fields.len());
    let mut read_statements = Vec::with_capacity(core_struct_ty.fields.len());

    for field in &core_struct_ty.fields {
        let rust_name = field.name.to_snake_case();
        let field_ty = compile_term_as_ty(context, &field.term, report).unwrap_or_else(|| Type {
            rust_ty: rt_invalid_ty(),
            host_ty: None,
            read: None,
            is_copy: true,
        });

        is_copy &= field_ty.is_copy;
        fields.push(rust::TypeField {
            doc: field.doc.clone(),
            name: rust_name,
            ty: field_ty.host_ty.unwrap_or_else(rt_invalid_ty),
            by_ref: !field_ty.is_copy,
        });
        let read = match field_ty.read {
            Some(read) => read,
            None => rust::Term::Read(Box::new(field_ty.rust_ty)),
        };
        read_statements.push(rust::Statement::Let(field.name.clone(), Box::new(read)));
    }

    let rust_name = core_struct_ty.name.to_pascal_case(); // TODO: name avoidance
    let rust_item = rust::Item::Struct(rust::StructType {
        derives: derives(is_copy),
        doc: core_struct_ty.doc.clone(),
        name: rust_name.clone(),
        read: Some(rust::Block::new(
            read_statements,
            rust::Term::call(
                rust::Term::name("Ok"),
                vec![rust::Term::Struct(
                    rust_name.clone(),
                    fields
                        .iter()
                        .map(|field| (field.name.clone(), None))
                        .collect(),
                )],
            ),
        )),
        fields,
    });
    context.rust_items.push(rust_item);

    match context.compiled_items.entry(&core_struct_ty.name) {
        Entry::Occupied(entry) => {
            report(diagnostics::bug::item_name_reused(
                context.file_id,
                entry.key(),
                core_struct_ty.span,
                entry.get().span(),
            ));
        }
        Entry::Vacant(entry) => {
            entry.insert(CompiledItem::Type(TypeItem {
                span: core_struct_ty.span,
                rust_name: rust_name.clone(),
                is_copy,
                host_ty: Some(rust::Type::name(rust_name, Vec::new())),
            }));
            let core_item = core::Item::Struct(core_struct_ty.clone());
            context.core_items.insert(&core_struct_ty.name, core_item);
        }
    }
}

struct Term {
    rust_term: rust::Term,
    rust_ty: rust::Type,
    is_const: bool,
}

struct Type {
    rust_ty: rust::Type,
    is_copy: bool,
    host_ty: Option<rust::Type>,
    read: Option<rust::Term>,
}

enum CompiledTerm {
    Term(Term),
    Type(Type),
    Erased,
    Error,
}

fn compile_term(
    context: &mut Context<'_>,
    core_term: &core::Term,
    report: &mut dyn FnMut(Diagnostic),
) -> CompiledTerm {
    let file_id = context.file_id;

    let host_ty = |rust_ty| {
        CompiledTerm::Type(Type {
            rust_ty,
            is_copy: true,
            host_ty: None,
            read: None,
        })
    };
    let format_ty = |rust_ty, host_ty| {
        CompiledTerm::Type(Type {
            rust_ty,
            is_copy: true,
            host_ty: Some(host_ty),
            read: None,
        })
    };

    match core::semantics::eval(context.globals, &context.core_items, core_term).as_ref() {
        core::Value::Neutral(core::Head::Global(span, name), elims) => {
            match (name.as_str(), elims.as_slice()) {
                // TODO: Put globals in an environment
                ("U8", []) => format_ty(rt_ty_name("U8"), rust::Type::name("u8", vec![])),
                ("U16Le", []) => format_ty(rt_ty_name("U16Le"), rust::Type::name("u16", vec![])),
                ("U16Be", []) => format_ty(rt_ty_name("U16Be"), rust::Type::name("u16", vec![])),
                ("U32Le", []) => format_ty(rt_ty_name("U32Le"), rust::Type::name("u32", vec![])),
                ("U32Be", []) => format_ty(rt_ty_name("U32Be"), rust::Type::name("u32", vec![])),
                ("U64Le", []) => format_ty(rt_ty_name("U64Le"), rust::Type::name("u64", vec![])),
                ("U64Be", []) => format_ty(rt_ty_name("U64Be"), rust::Type::name("u64", vec![])),
                ("S8", []) => format_ty(rt_ty_name("I8"), rust::Type::name("i8", vec![])),
                ("S16Le", []) => format_ty(rt_ty_name("I16Le"), rust::Type::name("i16", vec![])),
                ("S16Be", []) => format_ty(rt_ty_name("I16Be"), rust::Type::name("i16", vec![])),
                ("S32Le", []) => format_ty(rt_ty_name("I32Le"), rust::Type::name("i32", vec![])),
                ("S32Be", []) => format_ty(rt_ty_name("I32Be"), rust::Type::name("i32", vec![])),
                ("S64Le", []) => format_ty(rt_ty_name("I64Le"), rust::Type::name("i64", vec![])),
                ("S64Be", []) => format_ty(rt_ty_name("I64Be"), rust::Type::name("i64", vec![])),
                ("F32Le", []) => format_ty(rt_ty_name("F32Le"), rust::Type::name("f32", vec![])),
                ("F32Be", []) => format_ty(rt_ty_name("F32Be"), rust::Type::name("f32", vec![])),
                ("F64Le", []) => format_ty(rt_ty_name("F64Le"), rust::Type::name("f64", vec![])),
                ("F64Be", []) => format_ty(rt_ty_name("F64Be"), rust::Type::name("f64", vec![])),
                ("Bool", []) => host_ty(rust::Type::name("bool", vec![])),
                ("Int", []) => {
                    report(diagnostics::error::unconstrained_int(
                        file_id,
                        core_term.span(),
                    ));
                    host_ty(rt_invalid_ty())
                }
                ("F32", []) => host_ty(rust::Type::name("f32", vec![])),
                ("F64", []) => host_ty(rust::Type::name("f64", vec![])),
                ("true", []) => CompiledTerm::Term(Term {
                    rust_term: rust::Term::name("true"),
                    rust_ty: rust::Type::name("bool", vec![]),
                    is_const: true,
                }),
                ("false", []) => CompiledTerm::Term(Term {
                    rust_term: rust::Term::name("true"),
                    rust_ty: rust::Type::name("bool", vec![]),
                    is_const: true,
                }),
                (name, _) => {
                    report(crate::diagnostics::bug::global_name_not_found(
                        file_id, name, *span,
                    ));
                    host_ty(rt_invalid_ty())
                }
            }
        }
        core::Value::Neutral(core::Head::Item(span, name), elims) => {
            let head = match context.compiled_items.get(name.as_str()) {
                Some(CompiledItem::Term(term_item)) => CompiledTerm::Term(term_item.to_term()),
                Some(CompiledItem::Type(ty_item)) => CompiledTerm::Type(ty_item.to_ty()),
                Some(CompiledItem::Erased(_)) => CompiledTerm::Erased,
                Some(CompiledItem::Error(_)) => CompiledTerm::Error,
                None => {
                    report(diagnostics::bug::unbound_item(file_id, name, *span));
                    CompiledTerm::Error
                }
            };
            elims.iter().fold(head, |head, elim| match head {
                CompiledTerm::Term(head) => match elim {
                    core::Elim::Bool(_, if_true, if_false) => {
                        compile_bool_elim(context, head, if_true, if_false, report)
                    }
                    core::Elim::Int(span, branches, default) => {
                        compile_int_elim(context, *span, head, branches, default, report)
                    }
                },
                CompiledTerm::Type(head) => match elim {
                    // TODO: Build up type application?
                    elim => CompiledTerm::Error,
                },
                CompiledTerm::Error => CompiledTerm::Error,
                CompiledTerm::Erased => CompiledTerm::Erased,
            })
        }
        core::Value::Neutral(core::Head::Error(_), _) => CompiledTerm::Error,
        core::Value::Universe(_, _) => CompiledTerm::Erased,
        core::Value::Constant(span, constant) => compile_constant(context, *span, constant, report),
        core::Value::Error(_) => CompiledTerm::Error,
    }
}

fn compile_term_as_ty(
    context: &mut Context<'_>,
    core_term: &core::Term,
    report: &mut dyn FnMut(Diagnostic),
) -> Option<Type> {
    match compile_term(context, &core_term, report) {
        CompiledTerm::Term(_) => None, // TODO: report error
        CompiledTerm::Type(ty) => Some(ty),
        CompiledTerm::Erased => None, // TODO: report error
        CompiledTerm::Error => None,
    }
}

fn compile_constant(
    context: &mut Context<'_>,
    span: Span,
    constant: &core::Constant,
    report: &mut dyn FnMut(Diagnostic),
) -> CompiledTerm {
    match constant {
        core::Constant::Int(value) => match value.to_i64() {
            // TODO: don't default to I64.
            Some(value) => CompiledTerm::Term(Term {
                rust_term: rust::Term::Constant(rust::Constant::I64(value)),
                rust_ty: rust::Type::name("i64", Vec::new()),
                is_const: true,
            }),
            None => {
                report(crate::diagnostics::bug::not_yet_implemented(
                    context.file_id,
                    span,
                    "non-i64 types",
                ));
                CompiledTerm::Error
            }
        },
        core::Constant::F32(value) => CompiledTerm::Term(Term {
            rust_term: rust::Term::Constant(rust::Constant::F32(*value)),
            rust_ty: rust::Type::name("f32", Vec::new()),
            is_const: true,
        }),
        core::Constant::F64(value) => CompiledTerm::Term(Term {
            rust_term: rust::Term::Constant(rust::Constant::F64(*value)),
            rust_ty: rust::Type::name("f64", Vec::new()),
            is_const: true,
        }),
    }
}

fn compile_bool_elim(
    context: &mut Context<'_>,
    head: Term,
    if_true: &core::Term,
    if_false: &core::Term,
    report: &mut dyn FnMut(Diagnostic),
) -> CompiledTerm {
    match (
        compile_term(context, if_true, report),
        compile_term(context, if_false, report),
    ) {
        (CompiledTerm::Term(true_term), CompiledTerm::Term(false_term)) => {
            CompiledTerm::Term(Term {
                rust_term: rust::Term::If(
                    Box::new(head.rust_term),
                    Box::new(true_term.rust_term),
                    Box::new(false_term.rust_term),
                ),
                rust_ty: true_term.rust_ty, // TODO: check if arms match
                is_const: false,
            })
        }
        (CompiledTerm::Type(true_ty), CompiledTerm::Type(false_ty)) => {
            let mut is_impossible = true;
            let (true_host_ty, true_read) = match true_ty.host_ty {
                Some(host_ty) => {
                    is_impossible = false;
                    let read = match true_ty.read {
                        Some(read) => read,
                        None => rust::Term::Read(Box::new(true_ty.rust_ty)),
                    };
                    (host_ty, read)
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
            let (false_host_ty, false_read) = match false_ty.host_ty {
                Some(host_ty) => {
                    is_impossible = false;
                    let read = match false_ty.read {
                        Some(read) => read,
                        None => rust::Term::Read(Box::new(false_ty.rust_ty)),
                    };
                    (host_ty, read)
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
            let enum_rust_name = format!("Enum{}", context.enum_count);
            context.enum_count += 1;

            let is_copy = true_ty.is_copy && false_ty.is_copy;

            let true_rust_name = "True".to_owned();
            let true_ctor = rust::Term::name(format!("{}::{}", enum_rust_name, true_rust_name));
            let false_rust_name = "False".to_owned();
            let false_ctor = rust::Term::name(format!("{}::{}", enum_rust_name, false_rust_name));
            context.rust_items.push(rust::Item::Enum(rust::EnumType {
                derives: derives(is_copy),
                doc: Arc::new([]),
                name: enum_rust_name.clone(),
                variants: vec![
                    rust::Variant {
                        doc: Arc::new([]),
                        name: true_rust_name,
                        ty: true_host_ty,
                    },
                    rust::Variant {
                        doc: Arc::new([]),
                        name: false_rust_name,
                        ty: false_host_ty,
                    },
                ],
            }));

            CompiledTerm::Type(Type {
                rust_ty: rt_invalid_ty(),
                is_copy,
                host_ty: Some(rust::Type::name(enum_rust_name, Vec::new())),
                read: Some(rust::Term::If(
                    Box::new(head.rust_term),
                    Box::new(rust::Term::call(true_ctor, vec![true_read])),
                    Box::new(rust::Term::call(false_ctor, vec![false_read])),
                )),
            })
        }

        (CompiledTerm::Erased, CompiledTerm::Erased) => CompiledTerm::Erased,
        (CompiledTerm::Error, _) | (_, CompiledTerm::Error) => CompiledTerm::Error,

        // TODO: report bug: mismatched arms of if expression
        (_, _) => unimplemented!(),
    }
}

fn compile_int_elim(
    context: &mut Context<'_>,
    span: Span,
    head: Term,
    branches: &BTreeMap<BigInt, Arc<core::Term>>,
    default: &core::Term,
    report: &mut dyn FnMut(Diagnostic),
) -> CompiledTerm {
    match compile_term(context, default, report) {
        CompiledTerm::Term(default_term) => {
            let branches = branches
                .iter()
                .filter_map(|(value, term)| match value.to_i64() {
                    Some(value) => Some((
                        rust::Pattern::Constant(rust::Constant::I64(value)),
                        match compile_term(context, term, report) {
                            CompiledTerm::Term(term) => term.rust_term,
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
                .chain(std::iter::once((
                    rust::Pattern::name("_"),
                    default_term.rust_term,
                ))) // TODO: Use pattern name
                .collect();

            CompiledTerm::Term(Term {
                rust_term: rust::Term::Match(Box::new(head.rust_term), branches),
                rust_ty: default_term.rust_ty,
                is_const: false,
            })
        }
        CompiledTerm::Type(default_ty) => {
            // TODO: name avoidance
            // TODO: improve naming
            let enum_rust_name = format!("Enum{}", context.enum_count);
            context.enum_count += 1;

            let mut is_copy = default_ty.is_copy;
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
                    CompiledTerm::Type(branch_ty) => {
                        is_copy &= branch_ty.is_copy;
                        let (branch_host_ty, branch_read) = match branch_ty.host_ty {
                            Some(host_ty) => {
                                is_impossible = false;
                                let read = match branch_ty.read {
                                    Some(read) => read,
                                    None => rust::Term::Read(Box::new(branch_ty.rust_ty)),
                                };
                                (host_ty, read)
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
                        let branch_rust_name = format!("Variant{}", i);
                        let branch_ctor =
                            rust::Term::name(format!("{}::{}", enum_rust_name, branch_rust_name));
                        variants.push(rust::Variant {
                            doc: Arc::new([]),
                            name: branch_rust_name,
                            ty: branch_host_ty,
                        });
                        read_branches
                            .push((pattern, rust::Term::call(branch_ctor, vec![branch_read])));
                    }
                    // TODO: report bug: mismatched arms of match expression
                    _ => unimplemented!(),
                }
            }

            is_copy &= default_ty.is_copy;
            let (default_host_ty, default_read) = match default_ty.host_ty {
                Some(host_ty) => {
                    is_impossible = false;
                    let read = match default_ty.read {
                        Some(read) => read,
                        None => rust::Term::Read(Box::new(default_ty.rust_ty)),
                    };
                    (host_ty, read)
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
            let default_ctor = rust::Term::name(format!("{}::{}", enum_rust_name, default_name));
            variants.push(rust::Variant {
                doc: Arc::new([]),
                name: default_name,
                ty: default_host_ty,
            });
            read_branches.push((
                rust::Pattern::Name("_".into()), // TODO: Use pattern name
                rust::Term::call(default_ctor, vec![default_read]),
            ));

            context.rust_items.push(rust::Item::Enum(rust::EnumType {
                derives: derives(is_copy),
                doc: Arc::new([]),
                name: enum_rust_name.clone(),
                variants,
            }));

            CompiledTerm::Type(Type {
                rust_ty: rt_invalid_ty(),
                is_copy,
                host_ty: Some(rust::Type::name(enum_rust_name.clone(), Vec::new())),
                read: Some(rust::Term::Match(Box::new(head.rust_term), read_branches)),
            })
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
