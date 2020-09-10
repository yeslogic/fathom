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

use crate::diagnostics;
use crate::lang::core::semantics::{self, Head, Value};
use crate::lang::{core, surface};

/// Translate a surface module into a core module, while validating that it is
/// well-formed.
pub fn from_module(
    globals: &core::Globals,
    surface_module: &surface::Module,
    report: &mut dyn FnMut(Diagnostic<usize>),
) -> core::Module {
    core::Module {
        file_id: surface_module.file_id,
        doc: surface_module.doc.clone(),
        items: Context::new(globals, surface_module.file_id)
            .from_items(&surface_module.items, report),
    }
}

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
    types: Vec<(&'me str, Arc<Value>)>,
}

impl<'me> Context<'me> {
    /// Create a new context.
    pub fn new(globals: &'me core::Globals, file_id: usize) -> Context<'me> {
        Context {
            globals,
            file_id,
            items: HashMap::new(),
            types: Vec::new(),
        }
    }

    /// Lookup the type of a binding corresponding to `name` in the context,
    /// returning `None` if `name` was not yet bound.
    pub fn lookup_type(&self, name: &str) -> Option<&Arc<Value>> {
        Some(&self.types.iter().rev().find(|(n, _)| *n == name)?.1)
    }

    /// Evaluate a [`core::Term`] into a [`Value`] in the current elaboration context.
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    /// [`core::Term`]: crate::lang::core::Term
    pub fn eval(&self, term: &core::Term) -> Arc<Value> {
        semantics::eval(self.globals, &self.items, term)
    }

    /// Check that one [`Value`] is [computationally equal]
    /// to another [`Value`] in the current elaboration context.
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    /// [computationally equal]: https://ncatlab.org/nlab/show/equality#computational_equality
    pub fn is_equal(&self, value0: &Value, value1: &Value) -> bool {
        semantics::is_equal(value0, value1)
    }

    /// Translate surface items into core items, while validating that they are
    /// well-formed.
    pub fn from_items(
        mut self,
        surface_items: &'me [surface::Item],
        report: &mut dyn FnMut(Diagnostic<usize>),
    ) -> Vec<core::Item> {
        let mut core_items = Vec::new();

        for item in surface_items.iter() {
            use std::collections::hash_map::Entry;

            match item {
                surface::Item::Alias(alias) => {
                    let (core_term, r#type) = match &alias.type_ {
                        Some(surface_type) => {
                            let core_type = self.is_type(surface_type, report);
                            let r#type = self.eval(&core_type);
                            let core_term = self.check_type(&alias.term, &r#type, report);
                            (
                                core::Term::Ann(Arc::new(core_term), Arc::new(core_type)),
                                r#type,
                            )
                        }
                        None => self.synth_type(&alias.term, report),
                    };

                    // FIXME: Avoid shadowing builtin definitions
                    match self.items.entry(&alias.name.1) {
                        Entry::Vacant(entry) => {
                            let item = core::Alias {
                                range: alias.range.clone(),
                                doc: alias.doc.clone(),
                                name: entry.key().to_string(),
                                term: Arc::new(core_term),
                            };

                            let core_item = core::Item::Alias(item);
                            core_items.push(core_item.clone());
                            self.types.push((*entry.key(), r#type));
                            entry.insert(core_item);
                        }
                        Entry::Occupied(entry) => report(diagnostics::item_redefinition(
                            Severity::Error,
                            self.file_id,
                            entry.key(),
                            alias.range.clone(),
                            entry.get().range(),
                        )),
                    }
                }
                surface::Item::Struct(struct_type) => {
                    let core_fields = self.is_fields(&struct_type.fields, report);

                    // FIXME: Avoid shadowing builtin definitions
                    match self.items.entry(&struct_type.name.1) {
                        Entry::Vacant(entry) => {
                            let item = core::StructType {
                                range: struct_type.range.clone(),
                                doc: struct_type.doc.clone(),
                                name: entry.key().to_string(),
                                fields: core_fields,
                            };

                            let core_item = core::Item::Struct(item);
                            core_items.push(core_item.clone());
                            let r#type = Arc::new(Value::FormatType(0..0));
                            self.types.push((*entry.key(), r#type));
                            entry.insert(core_item);
                        }
                        Entry::Occupied(entry) => report(diagnostics::item_redefinition(
                            Severity::Error,
                            self.file_id,
                            entry.key(),
                            struct_type.range.clone(),
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
    pub fn is_fields(
        &self,
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
            let format_type = Arc::new(Value::FormatType(0..0));
            let r#type = self.check_type(&field.term, &format_type, report);

            match seen_field_names.entry(field.name.1.clone()) {
                Entry::Vacant(entry) => {
                    core_fields.push(core::TypeField {
                        doc: field.doc.clone(),
                        start: field_range.start,
                        name: entry.key().clone(),
                        term: Arc::new(r#type),
                    });

                    entry.insert(field_range);
                }
                Entry::Occupied(entry) => report(diagnostics::field_redeclaration(
                    Severity::Error,
                    self.file_id,
                    entry.key(),
                    field_range,
                    entry.get().clone(),
                )),
            }
        }

        core_fields
    }

    /// Validate that a surface term is a type, and translate it into the core syntax.
    pub fn is_type(
        &self,
        surface_term: &surface::Term,
        report: &mut dyn FnMut(Diagnostic<usize>),
    ) -> core::Term {
        match surface_term {
            surface::Term::FormatType(range) => core::Term::FormatType(range.clone()),
            surface::Term::TypeType(range) => core::Term::TypeType(range.clone()),
            surface_term => {
                let (core_term, found_type) = self.synth_type(surface_term, report);
                match found_type.as_ref() {
                    Value::FormatType(_) | Value::TypeType(_) | Value::Error(_) => core_term,
                    _ => {
                        let range = surface_term.range();
                        report(diagnostics::universe_mismatch(
                            Severity::Error,
                            self.file_id,
                            range.clone(),
                            &found_type,
                        ));
                        core::Term::Error(range)
                    }
                }
            }
        }
    }

    /// Check that a surface term is an element of a type, and translate it into the
    /// core syntax.
    pub fn check_type(
        &self,
        surface_term: &surface::Term,
        expected_type: &Arc<Value>,
        report: &mut dyn FnMut(Diagnostic<usize>),
    ) -> core::Term {
        match (surface_term, expected_type.as_ref()) {
            (surface::Term::Error(range), _) => core::Term::Error(range.clone()),
            (surface_term, Value::Error(_)) => core::Term::Error(surface_term.range()),
            (surface::Term::NumberLiteral(range, literal), _) => {
                use crate::lang::core::Constant::{Int, F32, F64};

                let error = |report: &mut dyn FnMut(Diagnostic<usize>)| {
                    report(diagnostics::error::numeric_literal_not_supported(
                        self.file_id,
                        range.clone(),
                        expected_type,
                    ));
                    core::Term::Error(surface_term.range())
                };
                match expected_type.as_ref() {
                    // TODO: Lookup globals in environment
                    Value::Stuck(Head::Global(_, name), elims) if elims.is_empty() => {
                        match name.as_str() {
                            "Int" => match literal.parse_big_int(self.file_id, report) {
                                Some(value) => core::Term::Constant(range.clone(), Int(value)),
                                None => core::Term::Error(range.clone()),
                            },
                            "F32" => match literal.parse_float(self.file_id, report) {
                                Some(value) => core::Term::Constant(range.clone(), F32(value)),
                                None => core::Term::Error(range.clone()),
                            },
                            "F64" => match literal.parse_float(self.file_id, report) {
                                Some(value) => core::Term::Constant(range.clone(), F64(value)),
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
                let bool_type = Arc::new(Value::global(0..0, "Bool"));
                let head = self.check_type(surface_head, &bool_type, report);
                let if_true = self.check_type(surface_if_true, expected_type, report);
                let if_false = self.check_type(surface_if_false, expected_type, report);

                core::Term::BoolElim(
                    range.clone(),
                    Arc::new(head),
                    Arc::new(if_true),
                    Arc::new(if_false),
                )
            }
            (surface::Term::Match(range, surface_head, surface_branches), _) => {
                let (head, head_type) = self.synth_type(surface_head, report);
                let error = |report: &mut dyn FnMut(Diagnostic<usize>)| {
                    report(diagnostics::error::unsupported_pattern_type(
                        self.file_id,
                        surface_head.range(),
                        &head_type,
                    ));
                    core::Term::Error(range.clone())
                };

                match head_type.as_ref() {
                    Value::Stuck(Head::Global(_, name), elims) if elims.is_empty() => {
                        // TODO: Lookup globals in environment
                        match name.as_str() {
                            "Bool" => {
                                let (if_true, if_false) = self.from_bool_branches(
                                    surface_branches,
                                    expected_type,
                                    report,
                                );
                                core::Term::BoolElim(
                                    range.clone(),
                                    Arc::new(head),
                                    if_true,
                                    if_false,
                                )
                            }
                            "Int" => {
                                let (branches, default) = self.from_int_branches(
                                    surface_head.range(),
                                    surface_branches,
                                    expected_type,
                                    report,
                                );
                                core::Term::IntElim(
                                    range.clone(),
                                    Arc::new(head),
                                    branches,
                                    default,
                                )
                            }
                            _ => error(report),
                        }
                    }
                    Value::Error(_) => core::Term::Error(range.clone()),
                    _ => error(report),
                }
            }
            (surface_term, expected_type) => match self.synth_type(surface_term, report) {
                (core_term, found_type) if self.is_equal(&found_type, expected_type) => core_term,
                (_, found_type) => {
                    report(diagnostics::type_mismatch(
                        Severity::Error,
                        self.file_id,
                        surface_term.range(),
                        expected_type,
                        &found_type,
                    ));
                    core::Term::Error(surface_term.range())
                }
            },
        }
    }

    /// Synthesize the type of a surface term, and elaborate it into the core syntax.
    pub fn synth_type(
        &self,
        surface_term: &surface::Term,
        report: &mut dyn FnMut(Diagnostic<usize>),
    ) -> (core::Term, Arc<Value>) {
        match surface_term {
            surface::Term::Ann(surface_term, surface_type) => {
                let core_type = self.is_type(surface_type, report);
                let r#type = self.eval(&core_type);
                let core_term = self.check_type(surface_term, &r#type, report);
                (
                    core::Term::Ann(Arc::new(core_term), Arc::new(core_type)),
                    r#type,
                )
            }
            surface::Term::Name(range, name) => {
                if let Some((r#type, _)) = self.globals.get(name) {
                    return (
                        core::Term::Global(range.clone(), name.to_owned()),
                        self.eval(r#type),
                    );
                }
                if let Some(r#type) = self.lookup_type(name) {
                    return (
                        core::Term::Item(range.clone(), name.to_owned()),
                        r#type.clone(),
                    );
                }

                report(diagnostics::error::var_name_not_found(
                    self.file_id,
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
                    self.file_id,
                    range.clone(),
                ));
                (
                    core::Term::Error(range.clone()),
                    Arc::new(Value::Error(0..0)),
                )
            }
            surface::Term::FunctionType(param_type, body_type) => {
                let core_param_type = self.is_type(param_type, report);
                let core_body_type = self.is_type(body_type, report);

                match (&core_param_type, &core_body_type) {
                    (core::Term::Error(_), _) | (_, core::Term::Error(_)) => (
                        core::Term::Error(surface_term.range()),
                        Arc::new(Value::Error(0..0)),
                    ),
                    (_, _) => (
                        core::Term::FunctionType(
                            Arc::new(core_param_type),
                            Arc::new(core_body_type),
                        ),
                        Arc::new(Value::TypeType(0..0)),
                    ),
                }
            }
            surface::Term::FunctionElim(head, arguments) => {
                let range = surface_term.range();
                let (mut core_head, mut head_type) = self.synth_type(head, report);

                for argument in arguments {
                    match head_type.as_ref() {
                        Value::FunctionType(param_type, body_type) => {
                            core_head = core::Term::FunctionElim(
                                Arc::new(core_head),
                                Arc::new(self.check_type(argument, &param_type, report)),
                            );
                            head_type = body_type.clone();
                        }
                        Value::Error(_) => {
                            return (core::Term::Error(range), Arc::new(Value::Error(0..0)));
                        }
                        head_type => {
                            report(diagnostics::not_a_function(
                                Severity::Error,
                                self.file_id,
                                head.range(),
                                head_type,
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
                    self.file_id,
                    range.clone(),
                ));

                (
                    core::Term::Error(range.clone()),
                    Arc::new(Value::Error(0..0)),
                )
            }
            surface::Term::If(range, surface_head, surface_if_true, surface_if_false) => {
                // TODO: Lookup globals in environment
                let bool_type = Arc::new(Value::global(0..0, "Bool"));
                let head = self.check_type(surface_head, &bool_type, report);
                let (if_true, if_true_type) = self.synth_type(surface_if_true, report);
                let (if_false, if_false_type) = self.synth_type(surface_if_false, report);

                if semantics::is_equal(&if_true_type, &if_false_type) {
                    (
                        core::Term::BoolElim(
                            range.clone(),
                            Arc::new(head),
                            Arc::new(if_true),
                            Arc::new(if_false),
                        ),
                        if_true_type,
                    )
                } else {
                    report(diagnostics::type_mismatch(
                        Severity::Error,
                        self.file_id,
                        surface_if_false.range(),
                        &if_true_type,
                        &if_false_type,
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
                    self.file_id,
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
        &self,
        surface_branches: &[(surface::Pattern, surface::Term)],
        expected_type: &Value,
        report: &mut dyn FnMut(Diagnostic<usize>),
    ) -> (Arc<core::Term>, Arc<core::Term>) {
        unimplemented!("boolean eliminators")
    }

    fn from_int_branches(
        &self,
        range: Range<usize>,
        surface_branches: &[(surface::Pattern, surface::Term)],
        expected_type: &Arc<Value>,
        report: &mut dyn FnMut(Diagnostic<usize>),
    ) -> (BTreeMap<BigInt, Arc<core::Term>>, Arc<core::Term>) {
        use std::collections::btree_map::Entry;

        let mut branches = BTreeMap::new();
        let mut default = None;

        for (pattern, surface_term) in surface_branches {
            match pattern {
                surface::Pattern::NumberLiteral(range, literal) => {
                    let core_term = self.check_type(surface_term, expected_type, report);
                    if let Some(value) = literal.parse_big_int(self.file_id, report) {
                        match &default {
                            None => match branches.entry(value) {
                                Entry::Occupied(_) => {
                                    report(diagnostics::warning::unreachable_pattern(
                                        self.file_id,
                                        range.clone(),
                                    ))
                                }
                                Entry::Vacant(entry) => {
                                    entry.insert(Arc::new(core_term));
                                }
                            },
                            Some(_) => report(diagnostics::warning::unreachable_pattern(
                                self.file_id,
                                range.clone(),
                            )),
                        }
                    }
                }
                surface::Pattern::Name(range, _name) => {
                    // TODO: check if name is bound
                    // - if so compare for equality
                    // - otherwise bind local variable
                    let core_term = self.check_type(surface_term, expected_type, report);
                    match &default {
                        None => default = Some(Arc::new(core_term)),
                        Some(_) => report(diagnostics::warning::unreachable_pattern(
                            self.file_id,
                            range.clone(),
                        )),
                    }
                }
            }
        }

        let default = default.unwrap_or_else(|| {
            report(diagnostics::error::no_default_pattern(self.file_id, range));
            Arc::new(core::Term::Error(0..0))
        });

        (branches, default)
    }
}
