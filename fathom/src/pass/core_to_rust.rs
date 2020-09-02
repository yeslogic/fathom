use codespan_reporting::diagnostic::{Diagnostic, Severity};
use inflector::Inflector;
use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use std::collections::{BTreeMap, HashMap};
use std::ops::Range;
use std::sync::Arc;

use crate::lang::core::semantics::{self, Elim, Head, Value};
use crate::lang::{core, rust};

mod diagnostics;

// TODO: Make this path configurable
const RT_NAME: &str = "fathom_rt";

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
    report: &mut dyn FnMut(Diagnostic<usize>),
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
        from_item(&mut context, core_item, report);
    }

    rust::Module {
        doc: module.doc.clone(),
        items: context.rust_items,
    }
}

struct TermItem {
    range: Range<usize>,
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
    range: Range<usize>,
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
    Erased(Range<usize>),
    Error(Range<usize>),
}

impl CompiledItem {
    fn range(&self) -> Range<usize> {
        match self {
            CompiledItem::Term(term_item) => term_item.range.clone(),
            CompiledItem::Type(ty_item) => ty_item.range.clone(),
            CompiledItem::Erased(range) | CompiledItem::Error(range) => range.clone(),
        }
    }
}

struct Context<'me> {
    globals: &'me core::Globals,
    file_id: usize,
    enum_count: usize,
    compiled_items: HashMap<&'me str, CompiledItem>,
    core_items: HashMap<&'me str, core::Item>,
    rust_items: Vec<rust::Item>,
}

fn from_item<'item>(
    context: &mut Context<'item>,
    core_item: &'item core::Item,
    report: &mut dyn FnMut(Diagnostic<usize>),
) {
    match core_item {
        core::Item::Alias(core_alias) => from_alias(context, core_alias, report),
        core::Item::Struct(core_struct_ty) => from_struct_ty(context, core_struct_ty, report),
    }
}

fn from_alias<'item>(
    context: &mut Context<'item>,
    core_alias: &'item core::Alias,
    report: &mut dyn FnMut(Diagnostic<usize>),
) {
    use std::collections::hash_map::Entry;

    let item = match from_term(context, &core_alias.term, report) {
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
                    range: core_alias.range.clone(),
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
                    range: core_alias.range.clone(),
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
                            range: core_alias.range.clone(),
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
                        range: core_alias.range.clone(),
                        rust_name,
                        is_copy: ty.is_copy,
                        host_ty: ty.host_ty,
                    })
                }
            }
        }
        CompiledTerm::Erased => CompiledItem::Erased(core_alias.range.clone()),
        CompiledTerm::Error => CompiledItem::Error(core_alias.range.clone()),
    };

    match context.compiled_items.entry(&core_alias.name) {
        Entry::Occupied(entry) => {
            report(diagnostics::bug::item_name_reused(
                context.file_id,
                entry.key(),
                core_alias.range.clone(),
                entry.get().range(),
            ));
        }
        Entry::Vacant(entry) => {
            entry.insert(item);
            let core_item = core::Item::Alias(core_alias.clone());
            context.core_items.insert(&core_alias.name, core_item);
        }
    }
}

fn from_struct_ty<'item>(
    context: &mut Context<'item>,
    core_struct_ty: &'item core::StructType,
    report: &mut dyn FnMut(Diagnostic<usize>),
) {
    use std::collections::hash_map::Entry;

    let mut is_copy = true;
    let mut fields = Vec::with_capacity(core_struct_ty.fields.len());
    let mut read_statements = Vec::with_capacity(core_struct_ty.fields.len());

    for field in &core_struct_ty.fields {
        let rust_name = field.name.to_snake_case();
        let field_ty = from_term_to_ty(context, &field.term, report).unwrap_or_else(|| Type {
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
                core_struct_ty.range.clone(),
                entry.get().range(),
            ));
        }
        Entry::Vacant(entry) => {
            entry.insert(CompiledItem::Type(TypeItem {
                range: core_struct_ty.range.clone(),
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

fn from_term(
    context: &mut Context<'_>,
    core_term: &core::Term,
    report: &mut dyn FnMut(Diagnostic<usize>),
) -> CompiledTerm {
    let value = semantics::eval(context.globals, &context.core_items, core_term);
    from_value(context, &value, report)
}

fn from_value(
    context: &mut Context<'_>,
    value: &Value,
    report: &mut dyn FnMut(Diagnostic<usize>),
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

    match value {
        Value::Neutral(Head::Global(range, name), elims) => {
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
                        range.clone(),
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
                ("FormatArray", [Elim::Function(len_range, len), Elim::Function(_, elem_ty)]) => {
                    let len = match len.as_ref() {
                        Value::Error(_) => return CompiledTerm::Error,
                        Value::Constant(_, core::Constant::Int(len)) => match len.to_usize() {
                            Some(len) => len,
                            None => {
                                report(diagnostics::bug::integer_out_of_bounds(
                                    context.file_id,
                                    len_range.clone(),
                                ));
                                return CompiledTerm::Error;
                            }
                        },
                        _ => {
                            report(diagnostics::bug::expected_integer(
                                context.file_id,
                                len_range.clone(),
                            ));
                            return CompiledTerm::Error;
                        }
                    };

                    let elem_ty =
                        from_value_to_ty(context, elem_ty, report).unwrap_or_else(|| Type {
                            rust_ty: rt_invalid_ty(),
                            host_ty: None,
                            read: None,
                            is_copy: true,
                        });

                    match elem_ty.host_ty {
                        Some(host_elem_ty) => CompiledTerm::Type(Type {
                            rust_ty: rt_invalid_ty(),
                            is_copy: false,
                            // TODO: read into fixed-size array (if possible)
                            host_ty: Some(rust::Type::Name("Vec".into(), vec![host_elem_ty])),
                            read: Some(rust::Term::ReadArray(
                                Box::new(rust::Term::Constant(rust::Constant::USize(len))),
                                Box::new(rust::Term::call(
                                    rust::Term::name("Ok"),
                                    vec![match elem_ty.read {
                                        Some(read) => read,
                                        None => rust::Term::Read(Box::new(elem_ty.rust_ty)),
                                    }],
                                )),
                            )),
                        }),
                        None => CompiledTerm::Type(Type {
                            rust_ty: rust::Type::Array(len, Box::new(elem_ty.rust_ty)),
                            is_copy: false,
                            host_ty: None,
                            read: None,
                        }),
                    }
                }
                ("List", [Elim::Function(_, elem_ty)]) => {
                    let elem_ty =
                        from_value_to_ty(context, elem_ty, report).unwrap_or_else(|| Type {
                            rust_ty: rt_invalid_ty(),
                            host_ty: None,
                            read: None,
                            is_copy: true,
                        });

                    CompiledTerm::Type(Type {
                        rust_ty: rust::Type::Name("Vec".into(), vec![elem_ty.rust_ty]),
                        is_copy: false,
                        host_ty: None,
                        read: None,
                    })
                }
                (name, elims) => {
                    let arity = match name {
                        "U8" | "U16Le" | "U16Be" | "U32Le" | "U32Be" | "U64Le" | "U64Be" | "S8"
                        | "S16Le" | "S16Be" | "S32Le" | "S32Be" | "S64Le" | "S64Be" | "F32Le"
                        | "F32Be" | "F64Le" | "F64Be" | "Bool" | "Int" | "F32" | "F64" | "true"
                        | "false" => Some(0),
                        "List" => Some(1),
                        "FormatArray" => Some(2),
                        _ => None,
                    };

                    match arity {
                        Some(arity) if elims.len() < arity => {
                            report(crate::diagnostics::bug::not_yet_implemented(
                                context.file_id,
                                value.range(),
                                "undersaturated type constructors",
                            ))
                        }
                        Some(_) => report(diagnostics::bug::oversaturated_fun_elim(
                            context.file_id,
                            value.range(),
                        )),
                        None => report(crate::diagnostics::bug::global_name_not_found(
                            file_id,
                            name,
                            range.clone(),
                        )),
                    }

                    CompiledTerm::Error
                }
            }
        }
        Value::Neutral(Head::Item(range, name), elims) => {
            let head = match context.compiled_items.get(name.as_str()) {
                Some(CompiledItem::Term(term_item)) => CompiledTerm::Term(term_item.to_term()),
                Some(CompiledItem::Type(ty_item)) => CompiledTerm::Type(ty_item.to_ty()),
                Some(CompiledItem::Erased(_)) => CompiledTerm::Erased,
                Some(CompiledItem::Error(_)) => CompiledTerm::Error,
                None => {
                    report(diagnostics::bug::unbound_item(file_id, name, range.clone()));
                    CompiledTerm::Error
                }
            };
            elims.iter().fold(head, |head, elim| match (&head, elim) {
                (_, Elim::Function(_, _)) => {
                    report(crate::diagnostics::bug::not_yet_implemented(
                        context.file_id,
                        value.range(),
                        "function eliminations on items",
                    ));
                    CompiledTerm::Error
                }
                (CompiledTerm::Term(head), Elim::Bool(_, if_true, if_false)) => {
                    let head = head.rust_term.clone();
                    from_bool_elim(context, head, if_true, if_false, report)
                }
                (CompiledTerm::Term(head), Elim::Int(range, branches, default)) => {
                    let head = head.rust_term.clone();
                    from_int_elim(context, range.clone(), head, branches, default, report)
                }
                (CompiledTerm::Error, _) => CompiledTerm::Error,
                (_, Elim::Bool(range, _, _)) | (_, Elim::Int(range, _, _)) => {
                    report(diagnostics::bug::unexpected_elim(file_id, range.clone()));
                    CompiledTerm::Error
                }
            })
        }
        Value::Neutral(Head::Error(_), _) => CompiledTerm::Error,
        Value::FormatType(_) | Value::TypeType(_) => CompiledTerm::Erased,
        Value::FunctionType(_, _) => {
            report(crate::diagnostics::bug::not_yet_implemented(
                context.file_id,
                value.range(),
                "function types",
            ));
            CompiledTerm::Error
        }
        Value::Constant(range, constant) => from_constant(context, range.clone(), constant, report),
        Value::Error(_) => CompiledTerm::Error,
    }
}

fn from_term_to_ty(
    context: &mut Context<'_>,
    core_term: &core::Term,
    report: &mut dyn FnMut(Diagnostic<usize>),
) -> Option<Type> {
    let value = semantics::eval(context.globals, &context.core_items, core_term);
    from_value_to_ty(context, &value, report)
}

fn from_value_to_ty(
    context: &mut Context<'_>,
    value: &Value,
    report: &mut dyn FnMut(Diagnostic<usize>),
) -> Option<Type> {
    match from_value(context, &value, report) {
        CompiledTerm::Type(ty) => Some(ty),
        CompiledTerm::Term(_) | CompiledTerm::Erased => {
            report(diagnostics::bug::expected_type(
                context.file_id,
                value.range(),
            ));
            None
        }
        CompiledTerm::Error => None,
    }
}

fn from_constant(
    context: &mut Context<'_>,
    range: Range<usize>,
    constant: &core::Constant,
    report: &mut dyn FnMut(Diagnostic<usize>),
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
                    range,
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

fn from_bool_elim(
    context: &mut Context<'_>,
    head: rust::Term,
    if_true: &core::Term,
    if_false: &core::Term,
    report: &mut dyn FnMut(Diagnostic<usize>),
) -> CompiledTerm {
    match (
        from_term(context, if_true, report),
        from_term(context, if_false, report),
    ) {
        (CompiledTerm::Term(true_term), CompiledTerm::Term(false_term)) => {
            CompiledTerm::Term(Term {
                rust_term: rust::Term::If(
                    Box::new(head),
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
                        if_true.range(),
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
                        if_false.range(),
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
                    Box::new(head),
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

fn from_int_elim(
    context: &mut Context<'_>,
    range: Range<usize>,
    head: rust::Term,
    branches: &BTreeMap<BigInt, Arc<core::Term>>,
    default: &core::Term,
    report: &mut dyn FnMut(Diagnostic<usize>),
) -> CompiledTerm {
    match from_term(context, default, report) {
        CompiledTerm::Term(default_term) => {
            let branches = branches
                .iter()
                .filter_map(|(value, term)| match value.to_i64() {
                    Some(value) => Some((
                        rust::Pattern::Constant(rust::Constant::I64(value)),
                        match from_term(context, term, report) {
                            CompiledTerm::Term(term) => term.rust_term,
                            // TODO: report bug: mismatched arms of match expression
                            _ => rust::Term::Panic("error term".into()),
                        },
                    )),
                    None => {
                        report(crate::diagnostics::bug::not_yet_implemented(
                            context.file_id,
                            range.clone(),
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
                rust_term: rust::Term::Match(Box::new(head), branches),
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
                            range.clone(),
                            "non-i64 patterns",
                        ));
                        continue;
                    }
                };

                match from_term(context, term, report) {
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
                                    term.range(),
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
                        default.range(),
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
                host_ty: Some(rust::Type::name(enum_rust_name, Vec::new())),
                read: Some(rust::Term::Match(Box::new(head), read_branches)),
            })
        }
        CompiledTerm::Erased => CompiledTerm::Erased,
        CompiledTerm::Error => CompiledTerm::Error,
    }
}

#[allow(dead_code)]
fn host_int(min: &BigInt, max: &BigInt) -> Option<rust::Type> {
    use std::{i16, i32, i64, i8, u16, u32, u64, u8};

    use crate::lang::rust::Type;

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
