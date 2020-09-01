//! Elaboration from the surface syntax into the core syntax.
//!
//! Performs the following:
//!
//! - name resolution
//! - desugaring
//! - pattern compilation (TODO)
//! - bidirectional type checking (TODO)
//! - unification (TODO)

use codespan_reporting::diagnostic::{Diagnostic, Severity};
use num_bigint::BigInt;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::ops::Range;
use std::sync::Arc;

use crate::ast::core::semantics::{self, Head, Value};
use crate::ast::{core, surface};
use crate::diagnostics;

/// Contextual information to be used during elaboration.
pub struct Context<'me> {
    /// The global environment.
    globals: &'me core::Globals,
    /// The file where these items are defined (for diagnostic reporting).
    file_id: usize,
    /// Labels that have previously been used for items, along with the source
    /// range where they were introduced (for diagnostic reporting).
    items: HashMap<&'me str, core::Item>,
    /// List of types currently bound in this context. These could either
    /// refer to items or local bindings.
    tys: Vec<(&'me str, Arc<Value>)>,
}

impl<'me> Context<'me> {
    /// Create a new context.
    pub fn new(globals: &'me core::Globals, file_id: usize) -> Context<'me> {
        Context {
            globals,
            file_id,
            items: HashMap::new(),
            tys: Vec::new(),
        }
    }

    /// Lookup the type of a binding corresponding to `name` in the context,
    /// returning `None` if `name` was not yet bound.
    pub fn lookup_ty(&self, name: &str) -> Option<&Arc<Value>> {
        Some(&self.tys.iter().rev().find(|(n, _)| *n == name)?.1)
    }
}

/// Translate a surface module into a core module, while validating that it is
/// well-formed.
pub fn from_module(
    globals: &core::Globals,
    surface_module: &surface::Module,
    report: &mut dyn FnMut(Diagnostic<usize>),
) -> core::Module {
    let item_context = Context::new(globals, surface_module.file_id);
    core::Module {
        file_id: surface_module.file_id,
        doc: surface_module.doc.clone(),
        items: from_items(item_context, &surface_module.items, report),
    }
}

/// Translate surface items into core items, while validating that they are
/// well-formed.
pub fn from_items<'items>(
    mut context: Context<'items>,
    surface_items: &'items [surface::Item],
    report: &mut dyn FnMut(Diagnostic<usize>),
) -> Vec<core::Item> {
    let mut core_items = Vec::new();

    for item in surface_items.iter() {
        use std::collections::hash_map::Entry;

        match item {
            surface::Item::Alias(alias) => {
                let (core_term, ty) = match &alias.ty {
                    Some(surface_ty) => {
                        let core_ty = from_term_to_ty(&context, surface_ty, report);
                        let ty = semantics::eval(context.globals, &context.items, &core_ty);
                        let core_term = from_term_check_ty(&context, &alias.term, &ty, report);
                        (core::Term::Ann(Arc::new(core_term), Arc::new(core_ty)), ty)
                    }
                    None => from_term_synth_ty(&context, &alias.term, report),
                };

                // FIXME: Avoid shadowing builtin definitions
                match context.items.entry(&alias.name.1) {
                    Entry::Vacant(entry) => {
                        let item = core::Alias {
                            range: alias.range.clone(),
                            doc: alias.doc.clone(),
                            name: entry.key().to_string(),
                            term: Arc::new(core_term),
                        };

                        let core_item = core::Item::Alias(item);
                        core_items.push(core_item.clone());
                        context.tys.push((*entry.key(), ty));
                        entry.insert(core_item);
                    }
                    Entry::Occupied(entry) => report(diagnostics::item_redefinition(
                        Severity::Error,
                        context.file_id,
                        entry.key(),
                        alias.range.clone(),
                        entry.get().range(),
                    )),
                }
            }
            surface::Item::Struct(struct_ty) => {
                let core_fields = from_struct_ty_fields(&context, &struct_ty.fields, report);

                // FIXME: Avoid shadowing builtin definitions
                match context.items.entry(&struct_ty.name.1) {
                    Entry::Vacant(entry) => {
                        let item = core::StructType {
                            range: struct_ty.range.clone(),
                            doc: struct_ty.doc.clone(),
                            name: entry.key().to_string(),
                            fields: core_fields,
                        };

                        let core_item = core::Item::Struct(item);
                        core_items.push(core_item.clone());
                        let ty = Arc::new(Value::FormatType(0..0));
                        context.tys.push((*entry.key(), ty));
                        entry.insert(core_item);
                    }
                    Entry::Occupied(entry) => report(diagnostics::item_redefinition(
                        Severity::Error,
                        context.file_id,
                        entry.key(),
                        struct_ty.range.clone(),
                        entry.get().range(),
                    )),
                }
            }
        }
    }

    core_items
}

/// Translate surface structure type fields into core structure type fields in
/// the core syntax, while validating that they are well-formed.
pub fn from_struct_ty_fields(
    context: &Context<'_>,
    surface_fields: &[surface::TypeField],
    report: &mut dyn FnMut(Diagnostic<usize>),
) -> Vec<core::TypeField> {
    // Field names that have previously seen, along with the source
    // range where they were introduced (for diagnostic reporting).
    let mut seen_field_names = HashMap::new();
    // Fields that have been elaborated into the core syntax.
    let mut core_fields = Vec::with_capacity(surface_fields.len());

    for field in surface_fields {
        use std::collections::hash_map::Entry;

        let field_range = field.name.0.start..field.term.range().end;
        let format_ty = Arc::new(Value::FormatType(0..0));
        let ty = from_term_check_ty(&context, &field.term, &format_ty, report);

        match seen_field_names.entry(field.name.1.clone()) {
            Entry::Vacant(entry) => {
                core_fields.push(core::TypeField {
                    doc: field.doc.clone(),
                    start: field_range.start,
                    name: entry.key().clone(),
                    term: Arc::new(ty),
                });

                entry.insert(field_range);
            }
            Entry::Occupied(entry) => report(diagnostics::field_redeclaration(
                Severity::Error,
                context.file_id,
                entry.key(),
                field_range,
                entry.get().clone(),
            )),
        }
    }

    core_fields
}

/// Validate that a surface term is a type, and translate it into the core syntax.
pub fn from_term_to_ty(
    context: &Context<'_>,
    surface_term: &surface::Term,
    report: &mut dyn FnMut(Diagnostic<usize>),
) -> core::Term {
    match surface_term {
        surface::Term::FormatType(range) => core::Term::FormatType(range.clone()),
        surface::Term::TypeType(range) => core::Term::TypeType(range.clone()),
        surface_term => {
            let (core_term, ty) = from_term_synth_ty(context, surface_term, report);
            match ty.as_ref() {
                Value::FormatType(_) | Value::TypeType(_) | Value::Error(_) => core_term,
                _ => {
                    let range = surface_term.range();
                    report(diagnostics::universe_mismatch(
                        Severity::Error,
                        context.file_id,
                        range.clone(),
                        &ty,
                    ));
                    core::Term::Error(range)
                }
            }
        }
    }
}

/// Check that a surface term is an element of a type, and translate it into the
/// core syntax.
pub fn from_term_check_ty(
    context: &Context<'_>,
    surface_term: &surface::Term,
    expected_ty: &Arc<Value>,
    report: &mut dyn FnMut(Diagnostic<usize>),
) -> core::Term {
    match (surface_term, expected_ty.as_ref()) {
        (surface::Term::Error(range), _) => core::Term::Error(range.clone()),
        (surface_term, Value::Error(_)) => core::Term::Error(surface_term.range()),
        (surface::Term::NumberLiteral(range, literal), _) => {
            let error = |report: &mut dyn FnMut(Diagnostic<usize>)| {
                report(diagnostics::error::numeric_literal_not_supported(
                    context.file_id,
                    range.clone(),
                    expected_ty,
                ));
                core::Term::Error(surface_term.range())
            };
            match expected_ty.as_ref() {
                // TODO: Lookup globals in environment
                Value::Neutral(Head::Global(_, name), elims) if elims.is_empty() => {
                    match name.as_str() {
                        "Int" => match literal.parse_big_int(context.file_id, report) {
                            Some(value) => {
                                core::Term::Constant(range.clone(), core::Constant::Int(value))
                            }
                            None => core::Term::Error(range.clone()),
                        },
                        "F32" => match literal.parse_float(context.file_id, report) {
                            Some(value) => {
                                core::Term::Constant(range.clone(), core::Constant::F32(value))
                            }
                            None => core::Term::Error(range.clone()),
                        },
                        "F64" => match literal.parse_float(context.file_id, report) {
                            Some(value) => {
                                core::Term::Constant(range.clone(), core::Constant::F64(value))
                            }
                            None => core::Term::Error(range.clone()),
                        },
                        _ => error(report),
                    }
                }
                _ => error(report),
            }
        }
        (surface::Term::If(range, surface_head, surface_if_true, surface_if_false), _) => {
            // TODO: Lookup globals in environment
            let bool_ty = Arc::new(Value::global(0..0, "Bool"));
            let head = from_term_check_ty(context, surface_head, &bool_ty, report);
            let if_true = from_term_check_ty(context, surface_if_true, expected_ty, report);
            let if_false = from_term_check_ty(context, surface_if_false, expected_ty, report);

            core::Term::BoolElim(
                range.clone(),
                Arc::new(head),
                Arc::new(if_true),
                Arc::new(if_false),
            )
        }
        (surface::Term::Match(range, surface_head, surface_branches), _) => {
            let (head, head_ty) = from_term_synth_ty(context, surface_head, report);
            let error = |report: &mut dyn FnMut(Diagnostic<usize>)| {
                report(diagnostics::error::unsupported_pattern_ty(
                    context.file_id,
                    surface_head.range(),
                    &head_ty,
                ));
                core::Term::Error(range.clone())
            };

            match head_ty.as_ref() {
                Value::Neutral(Head::Global(_, name), elims) if elims.is_empty() => {
                    // TODO: Lookup globals in environment
                    match name.as_str() {
                        "Bool" => {
                            let (if_true, if_false) =
                                from_bool_branches(context, surface_branches, expected_ty, report);
                            core::Term::BoolElim(range.clone(), Arc::new(head), if_true, if_false)
                        }
                        "Int" => {
                            let (branches, default) = from_int_branches(
                                context,
                                surface_head.range(),
                                surface_branches,
                                expected_ty,
                                report,
                            );
                            core::Term::IntElim(range.clone(), Arc::new(head), branches, default)
                        }
                        _ => error(report),
                    }
                }
                Value::Error(_) => core::Term::Error(range.clone()),
                _ => error(report),
            }
        }
        (surface_term, expected_ty) => {
            let (core_term, synth_ty) = from_term_synth_ty(context, surface_term, report);

            if semantics::equal(&synth_ty, expected_ty) {
                core_term
            } else {
                report(diagnostics::type_mismatch(
                    Severity::Error,
                    context.file_id,
                    surface_term.range(),
                    expected_ty,
                    &synth_ty,
                ));
                core::Term::Error(surface_term.range())
            }
        }
    }
}

/// Synthesize the type of a surface term, and elaborate it into the core syntax.
pub fn from_term_synth_ty(
    context: &Context<'_>,
    surface_term: &surface::Term,
    report: &mut dyn FnMut(Diagnostic<usize>),
) -> (core::Term, Arc<Value>) {
    match surface_term {
        surface::Term::Ann(surface_term, surface_ty) => {
            let core_ty = from_term_to_ty(context, surface_ty, report);
            let ty = semantics::eval(context.globals, &context.items, &core_ty);
            let core_term = from_term_check_ty(context, surface_term, &ty, report);
            (core::Term::Ann(Arc::new(core_term), Arc::new(core_ty)), ty)
        }
        surface::Term::Name(range, name) => {
            if let Some((ty, _)) = context.globals.get(name) {
                return (
                    core::Term::Global(range.clone(), name.to_owned()),
                    semantics::eval(context.globals, &context.items, ty),
                );
            }
            if let Some(ty) = context.lookup_ty(name) {
                return (core::Term::Item(range.clone(), name.to_owned()), ty.clone());
            }

            report(diagnostics::error::var_name_not_found(
                context.file_id,
                name.as_str(),
                range.clone(),
            ));
            (
                core::Term::Error(range.clone()),
                Arc::new(Value::Error(0..0)),
            )
        }
        surface::Term::TypeType(range) | surface::Term::FormatType(range) => {
            report(diagnostics::term_has_no_type(
                Severity::Error,
                context.file_id,
                range.clone(),
            ));
            (
                core::Term::Error(range.clone()),
                Arc::new(Value::Error(0..0)),
            )
        }
        surface::Term::FunctionType(param_ty, body_ty) => {
            let core_param_ty = from_term_to_ty(context, param_ty, report);
            let core_body_ty = from_term_to_ty(context, body_ty, report);

            match (&core_param_ty, &core_body_ty) {
                (core::Term::Error(_), _) | (_, core::Term::Error(_)) => (
                    core::Term::Error(surface_term.range()),
                    Arc::new(Value::Error(0..0)),
                ),
                (_, _) => (
                    core::Term::FunctionType(Arc::new(core_param_ty), Arc::new(core_body_ty)),
                    Arc::new(Value::TypeType(0..0)),
                ),
            }
        }
        surface::Term::FunctionElim(head, arguments) => {
            let range = surface_term.range();
            let (mut core_head, mut head_type) = from_term_synth_ty(context, head, report);

            for argument in arguments {
                match head_type.as_ref() {
                    Value::FunctionType(param_type, body_type) => {
                        core_head = core::Term::FunctionElim(
                            Arc::new(core_head),
                            Arc::new(from_term_check_ty(context, argument, &param_type, report)),
                        );
                        head_type = body_type.clone();
                    }
                    Value::Error(_) => {
                        return (core::Term::Error(range), Arc::new(Value::Error(0..0)));
                    }
                    head_ty => {
                        report(diagnostics::not_a_function(
                            Severity::Error,
                            context.file_id,
                            head.range(),
                            head_ty,
                            argument.range(),
                        ));
                        return (core::Term::Error(range), Arc::new(Value::Error(0..0)));
                    }
                }
            }

            (core_head, head_type)
        }
        surface::Term::NumberLiteral(range, _) => {
            report(diagnostics::error::ambiguous_numeric_literal(
                context.file_id,
                range.clone(),
            ));

            (
                core::Term::Error(range.clone()),
                Arc::new(Value::Error(0..0)),
            )
        }
        surface::Term::If(range, surface_head, surface_if_true, surface_if_false) => {
            // TODO: Lookup globals in environment
            let bool_ty = Arc::new(Value::global(0..0, "Bool"));
            let head = from_term_check_ty(context, surface_head, &bool_ty, report);
            let (if_true, if_true_ty) = from_term_synth_ty(context, surface_if_true, report);
            let (if_false, if_false_ty) = from_term_synth_ty(context, surface_if_false, report);

            if semantics::equal(&if_true_ty, &if_false_ty) {
                (
                    core::Term::BoolElim(
                        range.clone(),
                        Arc::new(head),
                        Arc::new(if_true),
                        Arc::new(if_false),
                    ),
                    if_true_ty,
                )
            } else {
                report(diagnostics::type_mismatch(
                    Severity::Error,
                    context.file_id,
                    surface_if_false.range(),
                    &if_true_ty,
                    &if_false_ty,
                ));
                (
                    core::Term::Error(range.clone()),
                    Arc::new(Value::Error(range.clone())),
                )
            }
        }
        surface::Term::Match(range, _, _) => {
            report(diagnostics::ambiguous_match_expression(
                Severity::Error,
                context.file_id,
                range.clone(),
            ));
            (
                core::Term::Error(range.clone()),
                Arc::new(Value::Error(range.clone())),
            )
        }
        surface::Term::Error(range) => (
            core::Term::Error(range.clone()),
            Arc::new(Value::Error(0..0)),
        ),
    }
}

#[allow(unused_variables)]
fn from_bool_branches(
    context: &Context<'_>,
    surface_branches: &[(surface::Pattern, surface::Term)],
    expected_ty: &Value,
    report: &mut dyn FnMut(Diagnostic<usize>),
) -> (Arc<core::Term>, Arc<core::Term>) {
    unimplemented!("boolean eliminators")
}

fn from_int_branches(
    context: &Context<'_>,
    range: Range<usize>,
    surface_branches: &[(surface::Pattern, surface::Term)],
    expected_ty: &Arc<Value>,
    report: &mut dyn FnMut(Diagnostic<usize>),
) -> (BTreeMap<BigInt, Arc<core::Term>>, Arc<core::Term>) {
    use std::collections::btree_map::Entry;

    let mut branches = BTreeMap::new();
    let mut default = None;

    for (pattern, surface_term) in surface_branches {
        match pattern {
            surface::Pattern::NumberLiteral(range, literal) => {
                let core_term = from_term_check_ty(context, surface_term, expected_ty, report);
                if let Some(value) = literal.parse_big_int(context.file_id, report) {
                    match &default {
                        None => match branches.entry(value) {
                            Entry::Occupied(_) => {
                                report(diagnostics::warning::unreachable_pattern(
                                    context.file_id,
                                    range.clone(),
                                ))
                            }
                            Entry::Vacant(entry) => {
                                entry.insert(Arc::new(core_term));
                            }
                        },
                        Some(_) => report(diagnostics::warning::unreachable_pattern(
                            context.file_id,
                            range.clone(),
                        )),
                    }
                }
            }
            surface::Pattern::Name(range, _name) => {
                // TODO: check if name is bound
                // - if so compare for equality
                // - otherwise bind local variable
                let core_term = from_term_check_ty(context, surface_term, expected_ty, report);
                match &default {
                    None => default = Some(Arc::new(core_term)),
                    Some(_) => report(diagnostics::warning::unreachable_pattern(
                        context.file_id,
                        range.clone(),
                    )),
                }
            }
        }
    }

    let default = default.unwrap_or_else(|| {
        report(diagnostics::error::no_default_pattern(
            context.file_id,
            range,
        ));
        Arc::new(core::Term::Error(0..0))
    });

    (branches, default)
}
