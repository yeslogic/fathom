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
use crate::lang::core;
use crate::lang::core::semantics::{self, Head, Value};
use crate::lang::surface::{ItemData, Module, Pattern, PatternData, Term, TermData};

/// Contextual information to be used during elaboration.
pub struct Context<'me> {
    /// The global environment.
    globals: &'me core::Globals,
    /// Labels that have previously been used for items.
    items: HashMap<String, core::Item>,
    /// List of types currently bound in this context.
    /// These could either refer to items or local bindings.
    types: Vec<(String, Arc<Value>)>,
    /// Diagnostics collected during elaboration.
    diagnostics: Vec<Diagnostic<usize>>,
}

impl<'me> Context<'me> {
    /// Create a new context.
    pub fn new(globals: &'me core::Globals) -> Context<'me> {
        Context {
            globals,
            items: HashMap::new(),
            types: Vec::new(),
            diagnostics: Vec::new(),
        }
    }

    /// Store a diagnostic message in the context for later reporting.
    fn push_diagnostic(&mut self, diagnostic: Diagnostic<usize>) {
        self.diagnostics.push(diagnostic);
    }

    /// Drain the collected diagnostics from the context.
    pub fn drain_diagnostics<'a>(&'a mut self) -> impl 'a + Iterator<Item = Diagnostic<usize>> {
        self.diagnostics.drain(..)
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
        semantics::is_equal(self.globals, &self.items, value0, value1)
    }

    /// Translate a surface module into a core module,
    /// while validating that it is well-formed.
    pub fn from_module(&mut self, surface_module: &Module) -> core::Module {
        let file_id = surface_module.file_id;
        let mut core_items = Vec::new();

        for item in surface_module.items.iter() {
            use std::collections::hash_map::Entry;

            match &item.data {
                ItemData::Alias(alias) => {
                    let (core_term, r#type) = match &alias.type_ {
                        Some(surface_type) => {
                            let core_type = self.is_type(file_id, surface_type);
                            let r#type = self.eval(&core_type);
                            let core_term = self.check_type(file_id, &alias.term, &r#type);
                            let term_data =
                                core::TermData::Ann(Arc::new(core_term), Arc::new(core_type));

                            (core::Term::new(alias.term.range(), term_data), r#type)
                        }
                        None => self.synth_type(file_id, &alias.term),
                    };

                    // FIXME: Avoid shadowing builtin definitions
                    match self.items.entry(alias.name.data.clone()) {
                        Entry::Vacant(entry) => {
                            let item_data = core::ItemData::Alias(core::Alias {
                                doc: alias.doc.clone(),
                                name: alias.name.data.clone(),
                                term: Arc::new(core_term),
                            });

                            let core_item = core::Item::new(item.range(), item_data);
                            core_items.push(core_item.clone());
                            self.types.push((entry.key().clone(), r#type));
                            entry.insert(core_item);
                        }
                        Entry::Occupied(entry) => {
                            let original_range = entry.get().range();
                            self.push_diagnostic(diagnostics::item_redefinition(
                                Severity::Error,
                                file_id,
                                &alias.name.data,
                                item.range.clone(),
                                original_range,
                            ))
                        }
                    }
                }
                ItemData::Struct(struct_type) => {
                    // Field names that have previously seen, along with the source
                    // range where they were introduced (for diagnostic reporting).
                    let mut seen_field_names = HashMap::new();
                    // Fields that have been elaborated into the core syntax.
                    let mut core_fields = Vec::with_capacity(struct_type.fields.len());

                    for field in &struct_type.fields {
                        let field_range = field.name.range().start..field.term.range().end;
                        let format_type = Arc::new(Value::FormatType);
                        let r#type = self.check_type(file_id, &field.term, &format_type);

                        match seen_field_names.entry(field.name.data.clone()) {
                            Entry::Vacant(entry) => {
                                core_fields.push(core::TypeField {
                                    doc: field.doc.clone(),
                                    name: field.name.data.clone(),
                                    term: Arc::new(r#type),
                                });

                                entry.insert(field_range);
                            }
                            Entry::Occupied(entry) => {
                                self.push_diagnostic(diagnostics::error::field_redeclaration(
                                    file_id,
                                    entry.key(),
                                    field_range,
                                    entry.get().clone(),
                                ));
                            }
                        }
                    }

                    // FIXME: Avoid shadowing builtin definitions
                    match self.items.entry(struct_type.name.data.clone()) {
                        Entry::Vacant(entry) => {
                            let item_data = core::ItemData::Struct(core::StructType {
                                doc: struct_type.doc.clone(),
                                name: struct_type.name.data.clone(),
                                fields: core_fields,
                            });

                            let core_item = core::Item::new(item.range(), item_data);
                            core_items.push(core_item.clone());
                            self.types
                                .push((entry.key().clone(), Arc::new(Value::FormatType)));
                            entry.insert(core_item);
                        }
                        Entry::Occupied(entry) => {
                            let original_range = entry.get().range();
                            self.push_diagnostic(diagnostics::item_redefinition(
                                Severity::Error,
                                file_id,
                                &struct_type.name.data,
                                item.range.clone(),
                                original_range,
                            ));
                        }
                    }
                }
            }
        }

        self.items.clear();
        self.types.clear();

        core::Module {
            file_id,
            doc: surface_module.doc.clone(),
            items: core_items,
        }
    }

    /// Validate that a surface term is a type, and translate it into the core syntax.
    pub fn is_type(&mut self, file_id: usize, surface_term: &Term) -> core::Term {
        let range = surface_term.range();

        match &surface_term.data {
            TermData::FormatType => core::Term::new(range, core::TermData::FormatType),
            TermData::TypeType => core::Term::new(range, core::TermData::TypeType),
            _ => {
                let (core_term, found_type) = self.synth_type(file_id, surface_term);
                match found_type.as_ref() {
                    Value::FormatType | Value::TypeType | Value::Error => core_term,
                    _ => {
                        self.push_diagnostic(diagnostics::universe_mismatch(
                            Severity::Error,
                            file_id,
                            range.clone(),
                            &found_type,
                        ));
                        core::Term::new(range, core::TermData::Error)
                    }
                }
            }
        }
    }

    /// Check that a surface term is an element of a type, and translate it into the
    /// core syntax.
    pub fn check_type(
        &mut self,
        file_id: usize,
        surface_term: &Term,
        expected_type: &Arc<Value>,
    ) -> core::Term {
        let range = surface_term.range();

        match (&surface_term.data, expected_type.as_ref()) {
            (TermData::Error, _) => core::Term::new(range, core::TermData::Error),
            (_, Value::Error) => core::Term::new(range, core::TermData::Error),
            (TermData::NumberLiteral(literal), _) => {
                use crate::lang::core::Constant::{Int, F32, F64};

                let numeric_literal_not_supported = || {
                    diagnostics::error::numeric_literal_not_supported(
                        file_id,
                        range.clone(),
                        expected_type,
                    )
                };

                let term_data = match expected_type.as_ref() {
                    // TODO: Lookup globals in environment
                    Value::Stuck(Head::Global(name), elims) if elims.is_empty() => {
                        let mut report = |diagnostic| self.push_diagnostic(diagnostic);
                        match name.as_str() {
                            "Int" => match literal.parse_big_int(file_id, &mut report) {
                                Some(value) => core::TermData::Constant(Int(value)),
                                None => core::TermData::Error,
                            },
                            "F32" => match literal.parse_float(file_id, &mut report) {
                                Some(value) => core::TermData::Constant(F32(value)),
                                None => core::TermData::Error,
                            },
                            "F64" => match literal.parse_float(file_id, &mut report) {
                                Some(value) => core::TermData::Constant(F64(value)),
                                None => core::TermData::Error,
                            },
                            _ => {
                                self.push_diagnostic(numeric_literal_not_supported());
                                core::TermData::Error
                            }
                        }
                    }
                    Value::Error => core::TermData::Error,
                    _ => {
                        self.push_diagnostic(numeric_literal_not_supported());
                        core::TermData::Error
                    }
                };

                core::Term::new(range, term_data)
            }
            (TermData::If(surface_head, surface_if_true, surface_if_false), _) => {
                // TODO: Lookup globals in environment
                let bool_type = Arc::new(Value::global("Bool"));
                let head = self.check_type(file_id, surface_head, &bool_type);
                let if_true = self.check_type(file_id, surface_if_true, expected_type);
                let if_false = self.check_type(file_id, surface_if_false, expected_type);

                core::Term::new(
                    range,
                    core::TermData::BoolElim(Arc::new(head), Arc::new(if_true), Arc::new(if_false)),
                )
            }
            (TermData::Match(surface_head, surface_branches), _) => {
                let (head, head_type) = self.synth_type(file_id, surface_head);

                let unsupported_pattern_type = || {
                    diagnostics::error::unsupported_pattern_type(
                        file_id,
                        surface_head.range(),
                        &head_type,
                    )
                };

                let term_data = match head_type.as_ref() {
                    Value::Stuck(Head::Global(name), elims) if elims.is_empty() => {
                        // TODO: Lookup globals in environment
                        match name.as_str() {
                            "Bool" => {
                                self.push_diagnostic(diagnostics::bug::not_yet_implemented(
                                    file_id,
                                    range.clone(),
                                    "boolean patterns",
                                ));
                                core::TermData::Error
                            }
                            "Int" => {
                                let (branches, default) = self.from_int_branches(
                                    file_id,
                                    surface_head.range(),
                                    surface_branches,
                                    expected_type,
                                );
                                core::TermData::IntElim(Arc::new(head), branches, default)
                            }
                            _ => {
                                self.push_diagnostic(unsupported_pattern_type());
                                core::TermData::Error
                            }
                        }
                    }
                    Value::Error => core::TermData::Error,
                    _ => {
                        self.push_diagnostic(unsupported_pattern_type());
                        core::TermData::Error
                    }
                };

                core::Term::new(range, term_data)
            }
            (_, expected_type) => match self.synth_type(file_id, surface_term) {
                (core_term, found_type) if self.is_equal(&found_type, expected_type) => core_term,
                (_, found_type) => {
                    self.push_diagnostic(diagnostics::type_mismatch(
                        Severity::Error,
                        file_id,
                        range.clone(),
                        expected_type,
                        &found_type,
                    ));
                    core::Term::new(range, core::TermData::Error)
                }
            },
        }
    }

    /// Synthesize the type of a surface term, and elaborate it into the core syntax.
    pub fn synth_type(&mut self, file_id: usize, surface_term: &Term) -> (core::Term, Arc<Value>) {
        let range = surface_term.range();

        match &surface_term.data {
            TermData::Ann(surface_term, surface_type) => {
                let core_type = self.is_type(file_id, surface_type);
                let r#type = self.eval(&core_type);
                let core_term = self.check_type(file_id, surface_term, &r#type);
                let term_data = core::TermData::Ann(Arc::new(core_term), Arc::new(core_type));

                (core::Term::new(surface_term.range(), term_data), r#type)
            }
            TermData::Name(name) => {
                if let Some((r#type, _)) = self.globals.get(name) {
                    let core_term = core::Term::new(range, core::TermData::Global(name.to_owned()));
                    return (core_term, self.eval(r#type));
                }
                if let Some(r#type) = self.lookup_type(name) {
                    let core_term = core::Term::new(range, core::TermData::Item(name.to_owned()));
                    return (core_term, r#type.clone());
                }

                self.push_diagnostic(diagnostics::error::var_name_not_found(
                    file_id,
                    name.as_str(),
                    surface_term.range(),
                ));
                (
                    core::Term::new(range, core::TermData::Error),
                    Arc::new(Value::Error),
                )
            }
            TermData::TypeType | TermData::FormatType => {
                self.push_diagnostic(diagnostics::term_has_no_type(
                    Severity::Error,
                    file_id,
                    surface_term.range(),
                ));
                (
                    core::Term::new(range, core::TermData::Error),
                    Arc::new(Value::Error),
                )
            }
            TermData::FunctionType(param_type, body_type) => {
                let core_param_type = self.is_type(file_id, param_type);
                let core_body_type = self.is_type(file_id, body_type);

                match (&core_param_type.data, &core_body_type.data) {
                    (core::TermData::Error, _) | (_, core::TermData::Error) => (
                        core::Term::new(range, core::TermData::Error),
                        Arc::new(Value::Error),
                    ),
                    (_, _) => (
                        core::Term::new(
                            range,
                            core::TermData::FunctionType(
                                Arc::new(core_param_type),
                                Arc::new(core_body_type),
                            ),
                        ),
                        Arc::new(Value::TypeType),
                    ),
                }
            }
            TermData::FunctionElim(head, arguments) => {
                let (mut core_head, mut head_type) = self.synth_type(file_id, head);

                for argument in arguments {
                    match head_type.as_ref() {
                        Value::FunctionType(param_type, body_type) => {
                            core_head = core::Term::new(
                                range.clone(),
                                core::TermData::FunctionElim(
                                    Arc::new(core_head),
                                    Arc::new(self.check_type(file_id, argument, &param_type)),
                                ),
                            );
                            head_type = body_type.clone();
                        }
                        Value::Error => {
                            return (
                                core::Term::new(range, core::TermData::Error),
                                Arc::new(Value::Error),
                            );
                        }
                        head_type => {
                            self.push_diagnostic(diagnostics::not_a_function(
                                Severity::Error,
                                file_id,
                                head.range(),
                                head_type,
                                argument.range(),
                            ));
                            return (
                                core::Term::new(range, core::TermData::Error),
                                Arc::new(Value::Error),
                            );
                        }
                    }
                }

                (core_head, head_type)
            }
            TermData::NumberLiteral(_) => {
                self.push_diagnostic(diagnostics::error::ambiguous_numeric_literal(
                    file_id,
                    surface_term.range(),
                ));

                (
                    core::Term::new(range, core::TermData::Error),
                    Arc::new(Value::Error),
                )
            }
            TermData::If(surface_head, surface_if_true, surface_if_false) => {
                // TODO: Lookup globals in environment
                let bool_type = Arc::new(Value::global("Bool"));
                let head = self.check_type(file_id, surface_head, &bool_type);
                let (if_true, if_true_type) = self.synth_type(file_id, surface_if_true);
                let (if_false, if_false_type) = self.synth_type(file_id, surface_if_false);

                if self.is_equal(&if_true_type, &if_false_type) {
                    let term_data = core::TermData::BoolElim(
                        Arc::new(head),
                        Arc::new(if_true),
                        Arc::new(if_false),
                    );
                    (core::Term::new(range, term_data), if_true_type)
                } else {
                    self.push_diagnostic(diagnostics::type_mismatch(
                        Severity::Error,
                        file_id,
                        surface_if_false.range(),
                        &if_true_type,
                        &if_false_type,
                    ));
                    (
                        core::Term::new(range, core::TermData::Error),
                        Arc::new(Value::Error),
                    )
                }
            }
            TermData::Match(_, _) => {
                self.push_diagnostic(diagnostics::ambiguous_match_expression(
                    Severity::Error,
                    file_id,
                    surface_term.range(),
                ));
                (
                    core::Term::new(range, core::TermData::Error),
                    Arc::new(Value::Error),
                )
            }
            TermData::Error => (
                core::Term::new(range, core::TermData::Error),
                Arc::new(Value::Error),
            ),
        }
    }

    fn from_int_branches(
        &mut self,
        file_id: usize,
        range: Range<usize>,
        surface_branches: &[(Pattern, Term)],
        expected_type: &Arc<Value>,
    ) -> (BTreeMap<BigInt, Arc<core::Term>>, Arc<core::Term>) {
        use std::collections::btree_map::Entry;

        let mut branches = BTreeMap::new();
        let mut default = None;

        for (pattern, surface_term) in surface_branches {
            let unreachable_pattern =
                || diagnostics::warning::unreachable_pattern(file_id, pattern.range());

            match &pattern.data {
                PatternData::NumberLiteral(literal) => {
                    let core_term = self.check_type(file_id, surface_term, expected_type);
                    let mut report = |diagnostic| self.push_diagnostic(diagnostic);
                    if let Some(value) = literal.parse_big_int(file_id, &mut report) {
                        match &default {
                            None => match branches.entry(value) {
                                Entry::Occupied(_) => self.push_diagnostic(unreachable_pattern()),
                                Entry::Vacant(entry) => {
                                    entry.insert(Arc::new(core_term));
                                }
                            },
                            Some(_) => self.push_diagnostic(unreachable_pattern()),
                        }
                    }
                }
                PatternData::Name(_name) => {
                    // TODO: check if name is bound
                    // - if so compare for equality
                    // - otherwise bind local variable
                    let core_term = self.check_type(file_id, surface_term, expected_type);
                    match &default {
                        None => default = Some(Arc::new(core_term)),
                        Some(_) => self.push_diagnostic(unreachable_pattern()),
                    }
                }
            }
        }

        let default = default.unwrap_or_else(|| {
            self.push_diagnostic(diagnostics::error::no_default_pattern(
                file_id,
                range.clone(),
            ));
            Arc::new(core::Term::new(range, core::TermData::Error))
        });

        (branches, default)
    }
}
