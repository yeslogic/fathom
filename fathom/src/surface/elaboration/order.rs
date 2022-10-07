//! Determination of elaboration order and cycle detection.
//!
//! This module determines the order in which elaboration should proceed, given
//! out-of-order definitions in a module.
//!
//! The algorithm is as follows:
//!
//! 1. Traverse the terms within each module item and note dependencies on other
//!    items.
//!    *  Names in scope are tracked so that when a local name shadows an item
//!       name we know not to add a dependency on the item.
//! 2. Build up a list of item indices that will indicate the order in which
//!    they should be elaborated.
//!    *  Recursively visit each item's dependencies and push the index of leaf
//!       items to the output list. If an item is already in the list is it not
//!       added again.
//!    *  Keep track of the stack of items in the depth-first traversal. If we
//!       re-enter an item already in the stack report an error indicating a
//!       cycle has been detected.

use fxhash::{FxHashMap, FxHashSet};

use crate::source::ByteRange;
use crate::surface::elaboration::reporting::Message;
use crate::surface::{elaboration, FormatField, Item, Module, Pattern, Term};
use crate::StringId;

enum Error {
    CycleDetected,
}

pub fn elaboration_order(
    elab_context: &mut elaboration::Context,
    surface_module: &Module<'_, ByteRange>,
) -> Vec<usize> {
    let item_names = item_names(surface_module);
    let item_deps = collect_item_dependencies(surface_module, &item_names);

    let context = ModuleOrderContext::new(elab_context);
    context.determine_order(surface_module.items, &item_names, &item_deps)
}

fn item_names(surface_module: &Module<'_, ByteRange>) -> FxHashMap<StringId, usize> {
    surface_module
        .items
        .iter()
        .enumerate()
        .filter_map(|(i, item)| match item {
            Item::Def(item) => Some((item.label.1, i)),
            Item::ReportedError(_) => None,
        })
        .collect()
}

fn collect_item_dependencies(
    surface_module: &Module<'_, ByteRange>,
    item_names: &FxHashMap<StringId, usize>,
) -> Vec<Vec<StringId>> {
    let mut local_names = Vec::new();
    surface_module
        .items
        .iter()
        .map(|item| item_dependencies(item, item_names, &mut local_names))
        .collect()
}

struct ModuleOrderContext<'a, 'interner, 'arena> {
    elab_context: &'a mut elaboration::Context<'interner, 'arena>,
    output: Vec<usize>,
    visited: FxHashSet<StringId>,
    stack: Vec<StringId>,
}

impl<'a, 'interner, 'arena> ModuleOrderContext<'a, 'interner, 'arena> {
    fn new(elab_context: &'a mut elaboration::Context<'interner, 'arena>) -> Self {
        ModuleOrderContext {
            elab_context,
            output: Vec::new(),
            visited: FxHashSet::default(),
            stack: Vec::new(),
        }
    }

    fn determine_order(
        mut self,
        items: &[Item<'_, ByteRange>],
        item_names: &FxHashMap<StringId, usize>,
        dependencies: &[Vec<StringId>],
    ) -> Vec<usize> {
        let mut erroneous = FxHashSet::default();
        for item in items {
            match item {
                Item::Def(item) => {
                    if erroneous.contains(&item.label.1) {
                        continue;
                    }
                    match self.visit_item(item.label.1, item_names, dependencies) {
                        Ok(()) => self.stack.clear(),
                        Err(Error::CycleDetected) => erroneous.extend(self.stack.drain(..)),
                    }
                }
                Item::ReportedError(_) => {}
            }
        }
        self.output
    }

    fn visit_item(
        &mut self,
        name: StringId,
        item_names: &FxHashMap<StringId, usize>,
        dependencies: &[Vec<StringId>],
    ) -> Result<(), Error> {
        if self.visited.contains(&name) {
            return Ok(());
        }

        if self.stack.contains(&name) {
            self.stack.push(name);
            self.elab_context.push_message(Message::CycleDetected {
                names: self.stack.clone(),
            });
            return Err(Error::CycleDetected);
        }

        match item_names.get(&name) {
            Some(index) => {
                let index = *index;
                self.stack.push(name);
                (dependencies[index].iter())
                    .try_for_each(|dep| self.visit_item(*dep, item_names, dependencies))?;
                self.stack.pop();
                self.visited.insert(name);
                self.output.push(index);
                Ok(())
            }
            // not a module item
            None => Ok(()),
        }
    }
}

fn item_dependencies(
    item: &Item<'_, ByteRange>,
    item_names: &FxHashMap<StringId, usize>,
    local_names: &mut Vec<StringId>,
) -> Vec<StringId> {
    let mut deps = Vec::new();
    match item {
        Item::Def(item) => {
            let initial_locals_names_len = local_names.len();
            push_pattern_deps(item.patterns, item_names, local_names, &mut deps);
            if let Some(r#type) = item.type_ {
                term_deps(r#type, item_names, local_names, &mut deps);
            }
            term_deps(item.expr, item_names, local_names, &mut deps);
            local_names.truncate(initial_locals_names_len);
        }
        Item::ReportedError(_) => {}
    }
    deps
}

fn term_deps(
    term: &Term<ByteRange>,
    item_names: &FxHashMap<StringId, usize>,
    local_names: &mut Vec<StringId>,
    deps: &mut Vec<StringId>,
) {
    match term {
        Term::Name(_, name) => {
            if local_names.iter().rev().any(|local| name == local) {
                // local binding, do nothing
            } else if item_names.contains_key(name) && deps.last() != Some(name) {
                // Only push if it's not a duplicate of the last item. This is a basic way
                // to reduce the number of duplicate dependencies that are pushed.
                deps.push(*name);
            }
        }
        Term::Ann(_, expr, r#type) => {
            term_deps(expr, item_names, local_names, deps);
            term_deps(r#type, item_names, local_names, deps);
        }
        Term::Let(_, pattern, r#type, def_expr, body_expr) => {
            push_pattern(pattern, local_names);
            if let Some(r#type) = r#type {
                term_deps(r#type, item_names, local_names, deps);
            }
            term_deps(def_expr, item_names, local_names, deps);
            term_deps(body_expr, item_names, local_names, deps);
            pop_pattern(pattern, local_names);
        }
        Term::Match(_, scrutinee, equations) => {
            let initial_locals_names_len = local_names.len();
            term_deps(scrutinee, item_names, local_names, deps);
            for (pattern, body) in *equations {
                push_pattern(pattern, local_names);
                term_deps(body, item_names, local_names, deps);
            }
            local_names.truncate(initial_locals_names_len);
        }
        Term::Arrow(_, param_type, body_type) => {
            term_deps(param_type, item_names, local_names, deps);
            term_deps(body_type, item_names, local_names, deps);
        }
        Term::FunType(_, patterns, body_type) => {
            let initial_locals_names_len = local_names.len();
            push_pattern_deps(*patterns, item_names, local_names, deps);
            term_deps(body_type, item_names, local_names, deps);
            local_names.truncate(initial_locals_names_len);
        }
        Term::FunLiteral(_, patterns, body_type) => {
            let initial_locals_names_len = local_names.len();
            push_pattern_deps(*patterns, item_names, local_names, deps);
            term_deps(body_type, item_names, local_names, deps);
            local_names.truncate(initial_locals_names_len);
        }
        Term::App(_, head_expr, arg_exprs) => {
            term_deps(head_expr, item_names, local_names, deps);
            for arg_expr in *arg_exprs {
                term_deps(arg_expr, item_names, local_names, deps);
            }
        }
        Term::RecordType(_, type_fields) => {
            let initial_locals_names_len = local_names.len();
            for type_field in *type_fields {
                term_deps(&type_field.type_, item_names, local_names, deps);
                local_names.push(type_field.label.1);
            }
            local_names.truncate(initial_locals_names_len);
        }
        Term::RecordLiteral(_, expr_fields) => {
            let initial_locals_names_len = local_names.len();
            for expr_field in *expr_fields {
                term_deps(&expr_field.expr, item_names, local_names, deps);
                local_names.push(expr_field.label.1);
            }
            local_names.truncate(initial_locals_names_len);
        }
        Term::Proj(_, head_expr, _) => {
            term_deps(head_expr, item_names, local_names, deps);
        }
        Term::ArrayLiteral(_, terms) => {
            for term in *terms {
                term_deps(term, item_names, local_names, deps);
            }
        }
        Term::FormatRecord(_, fields) => {
            field_deps(fields, item_names, local_names, deps);
        }
        Term::FormatOverlap(_, format_fields) => {
            field_deps(format_fields, item_names, local_names, deps);
        }
        Term::FormatCond(_, (_, name), format, cond) => {
            local_names.push(*name);
            term_deps(format, item_names, local_names, deps);
            term_deps(cond, item_names, local_names, deps);
            local_names.pop();
        }
        Term::BinOp(_, lhs, _, rhs) => {
            term_deps(lhs, item_names, local_names, deps);
            term_deps(rhs, item_names, local_names, deps);
        }
        Term::Hole(_, _)
        | Term::Placeholder(_)
        | Term::Universe(_)
        | Term::UnitLiteral(_)
        | Term::StringLiteral(_, _)
        | Term::NumberLiteral(_, _)
        | Term::BooleanLiteral(_, _)
        | Term::ReportedError(_) => {}
    }
}

fn push_pattern_deps(
    patterns: &[(Pattern<ByteRange>, Option<&Term<'_, ByteRange>>)],
    item_names: &FxHashMap<StringId, usize>,
    local_names: &mut Vec<StringId>,
    deps: &mut Vec<StringId>,
) {
    for (pattern, r#type) in patterns {
        if let Some(r#type) = r#type {
            term_deps(r#type, item_names, local_names, deps);
        }
        push_pattern(pattern, local_names);
    }
}

fn field_deps(
    fields: &[FormatField<ByteRange>],
    item_names: &FxHashMap<StringId, usize>,
    local_names: &mut Vec<StringId>,
    deps: &mut Vec<StringId>,
) {
    let initial_locals_names_len = local_names.len();
    for field in fields {
        match field {
            FormatField::Format {
                label: (_, label),
                format,
                ..
            } => {
                term_deps(format, item_names, local_names, deps);
                local_names.push(*label)
            }
            FormatField::Computed {
                label: (_, label),
                type_,
                expr,
            } => {
                if let Some(r#type) = type_ {
                    term_deps(r#type, item_names, local_names, deps);
                }
                term_deps(expr, item_names, local_names, deps);
                local_names.push(*label)
            }
        }
    }
    local_names.truncate(initial_locals_names_len);
}

fn push_pattern(pattern: &Pattern<ByteRange>, local_names: &mut Vec<StringId>) {
    match pattern {
        Pattern::Name(_, name) => local_names.push(*name),
        Pattern::Placeholder(_) => {}
        Pattern::StringLiteral(_, _) => {}
        Pattern::NumberLiteral(_, _) => {}
        Pattern::BooleanLiteral(_, _) => {}
    }
}

fn pop_pattern(pattern: &Pattern<ByteRange>, local_names: &mut Vec<StringId>) {
    match pattern {
        Pattern::Name(_, _) => {
            local_names.pop();
        }
        Pattern::Placeholder(_) => {}
        Pattern::StringLiteral(_, _) => {}
        Pattern::NumberLiteral(_, _) => {}
        Pattern::BooleanLiteral(_, _) => {}
    }
}
