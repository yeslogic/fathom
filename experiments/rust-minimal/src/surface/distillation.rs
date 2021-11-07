//! Bidirectional distillation of the core language into the surface language.

use scoped_arena::Scope;
use std::cell::RefCell;

use crate::env::{self, EnvLen, LocalVar, UniqueEnv};
use crate::surface::{Pattern, Term};
use crate::{core, StringId, StringInterner};

/// Distillation context.
pub struct Context<'interner, 'arena, 'env> {
    interner: &'interner RefCell<StringInterner>,
    /// Scoped arena for storing distilled terms.
    scope: &'arena Scope<'arena>,
    /// Rigid name environment.
    rigid_names: &'env mut UniqueEnv<Option<StringId>>,

    placeholder_string: StringId,
}

impl<'interner, 'arena, 'env> Context<'interner, 'arena, 'env> {
    /// Construct a new distillation context.
    pub fn new(
        interner: &'interner RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
        rigid_names: &'env mut UniqueEnv<Option<StringId>>,
    ) -> Context<'interner, 'arena, 'env> {
        let placeholder_string = interner.borrow_mut().get_or_intern("_");

        Context {
            interner,
            scope,
            rigid_names,
            placeholder_string,
        }
    }

    fn rigid_len(&mut self) -> EnvLen {
        self.rigid_names.len()
    }

    fn get_rigid_name(&self, var: LocalVar) -> Option<StringId> {
        self.rigid_names.get_local(var).copied().flatten()
    }

    fn push_rigid(&mut self, name: Option<StringId>) -> StringId {
        let name = name.unwrap_or(self.placeholder_string); // TODO: choose a better name?

        // TODO: avoid globals
        // TODO: ensure we chose a correctly bound name
        self.rigid_names.push(Some(name));
        name
    }

    fn pop_rigid(&mut self) {
        self.rigid_names.pop();
    }

    fn truncate_rigid(&mut self, len: EnvLen) {
        self.rigid_names.truncate(len);
    }

    fn check_number_literal<T: std::fmt::Display>(&mut self, number: T) -> Term<'arena, ()> {
        let number = self.interner.borrow_mut().get_or_intern(number.to_string());
        Term::NumberLiteral((), number)
    }

    fn synth_prim(&mut self, prim: core::Prim) -> Term<'arena, ()> {
        // FIXME: Check if shadowed
        let name = self.interner.borrow_mut().get_or_intern_static(prim.name());
        Term::Name((), name)
    }

    fn synth_number_literal<T: std::fmt::Display>(
        &mut self,
        number: T,
        prim_type: core::Prim,
    ) -> Term<'arena, ()> {
        let expr = self.check_number_literal(number);
        let r#type = self.synth_prim(prim_type);

        Term::Ann((), self.scope.to_scope(expr), self.scope.to_scope(r#type))
    }

    /// Distill a core term into a surface term, in a 'checkable' context.
    pub fn check(&mut self, core_term: &core::Term<'_>) -> Term<'arena, ()> {
        match core_term {
            core::Term::Ann(expr, _) => {
                // Avoid adding extraneous type annotations!
                self.check(expr)
            }
            core::Term::Let(def_name, def_expr, output_expr) => {
                let (def_expr, def_type) = match self.synth(def_expr) {
                    Term::Ann(_, expr, r#type) => (expr, Some(r#type)),
                    expr => (self.scope.to_scope(expr) as &_, None),
                };

                let def_name = self.push_rigid(*def_name);
                let output_expr = self.check(output_expr);
                self.pop_rigid();

                let def_pattern = Pattern::Name((), def_name);
                let output_expr = self.scope.to_scope(output_expr);

                Term::Let((), def_pattern, def_type, def_expr, output_expr)
            }
            core::Term::FunIntro(input_name, output_expr) => {
                let input_name = self.push_rigid(*input_name);
                let output_expr = self.check(output_expr);
                self.pop_rigid();

                Term::FunLiteral(
                    (),
                    Pattern::Name((), input_name),
                    self.scope.to_scope(output_expr),
                )
            }
            core::Term::RecordType(labels, _) if labels.is_empty() => Term::UnitLiteral(()),
            core::Term::RecordIntro(labels, _) if labels.is_empty() => Term::UnitLiteral(()),
            core::Term::RecordIntro(labels, exprs) => {
                let scope = self.scope;
                let expr_fields = Iterator::zip(labels.iter(), exprs.iter())
                    .map(|(label, expr)| (((), *label), self.check(expr)));

                Term::RecordLiteral((), scope.to_scope_from_iter(expr_fields))
            }
            core::Term::ArrayIntro(elem_exprs) => {
                let scope = self.scope;
                let elem_exprs = elem_exprs.iter().map(|elem_exprs| self.check(elem_exprs));

                Term::ArrayLiteral((), scope.to_scope_from_iter(elem_exprs))
            }

            core::Term::Const(r#const) => match r#const {
                core::Const::U8(number) => self.check_number_literal(number),
                core::Const::U16(number) => self.check_number_literal(number),
                core::Const::U32(number) => self.check_number_literal(number),
                core::Const::U64(number) => self.check_number_literal(number),
                core::Const::S8(number) => self.check_number_literal(number),
                core::Const::S16(number) => self.check_number_literal(number),
                core::Const::S32(number) => self.check_number_literal(number),
                core::Const::S64(number) => self.check_number_literal(number),
                core::Const::F32(number) => self.check_number_literal(number),
                core::Const::F64(number) => self.check_number_literal(number),
            },

            _ => self.synth(core_term),
        }
    }

    /// Distill a core term into a surface term, in a 'synthesizable' context.
    pub fn synth(&mut self, core_term: &core::Term<'_>) -> Term<'arena, ()> {
        match core_term {
            core::Term::RigidVar(var) => match self.get_rigid_name(*var) {
                Some(name) => Term::Name((), name),
                None => todo!("misbound variable"), // TODO: error?
            },
            core::Term::FlexibleVar(_var) => {
                Term::Placeholder(()) // TODO: lookup flexible variable name
            }
            core::Term::FlexibleInsertion(var, rigid_infos) => {
                let mut head_expr = self.synth(&core::Term::FlexibleVar(*var));

                for (var, info) in Iterator::zip(env::global_vars(), rigid_infos.iter()) {
                    match info {
                        core::EntryInfo::Concrete => {}
                        core::EntryInfo::Abstract => {
                            let var = self.rigid_len().global_to_local(var).unwrap();
                            let input_expr = self.synth(&core::Term::RigidVar(var));
                            head_expr = Term::FunElim(
                                (),
                                self.scope.to_scope(head_expr),
                                self.scope.to_scope(input_expr),
                            );
                        }
                    }
                }

                head_expr
            }
            core::Term::Ann(expr, r#type) => {
                let r#type = self.check(r#type);
                let expr = self.check(expr);

                Term::Ann((), self.scope.to_scope(expr), self.scope.to_scope(r#type))
            }
            core::Term::Let(def_name, def_expr, output_expr) => {
                let (def_expr, def_type) = match self.synth(def_expr) {
                    Term::Ann(_, expr, r#type) => (expr, Some(r#type)),
                    expr => (self.scope.to_scope(expr) as &_, None),
                };

                let def_name = self.push_rigid(*def_name);
                let output_expr = self.synth(output_expr);
                self.pop_rigid();

                let def_pattern = Pattern::Name((), def_name);
                let output_expr = self.scope.to_scope(output_expr);

                Term::Let((), def_pattern, def_type, def_expr, output_expr)
            }
            core::Term::Universe => Term::Universe(()),
            core::Term::FunType(input_name, input_type, output_type) => {
                let input_type = self.check(input_type);

                let input_name = self.push_rigid(*input_name);
                let output_type = self.check(output_type);
                self.pop_rigid();

                // TODO: distill to arrow if `input_name` is not bound in `output_type`
                Term::FunType(
                    (),
                    Pattern::Name((), input_name),
                    self.scope.to_scope(input_type),
                    self.scope.to_scope(output_type),
                )
            }
            core::Term::FunIntro(input_name, output_expr) => {
                let input_name = self.push_rigid(*input_name);
                let output_expr = self.synth(output_expr);
                self.pop_rigid();

                Term::FunLiteral(
                    (),
                    Pattern::Name((), input_name),
                    self.scope.to_scope(output_expr),
                )
            }
            core::Term::FunElim(head_expr, input_expr) => {
                let head_expr = self.synth(head_expr);
                let input_expr = self.synth(input_expr);

                Term::FunElim(
                    (),
                    self.scope.to_scope(head_expr),
                    self.scope.to_scope(input_expr),
                )
            }
            core::Term::RecordType(labels, _) if labels.is_empty() => {
                Term::Ann((), &Term::UnitLiteral(()), &Term::Universe(()))
            }
            core::Term::RecordType(labels, types) => {
                let initial_rigid_len = self.rigid_len();
                let type_fields = (self.scope).to_scope_from_iter(
                    Iterator::zip(labels.iter(), types.iter()).map(|(label, r#type)| {
                        let r#type = self.check(r#type);
                        self.push_rigid(Some(*label));
                        (((), *label), r#type)
                    }),
                );
                self.truncate_rigid(initial_rigid_len);

                Term::RecordType((), type_fields)
            }
            core::Term::RecordIntro(labels, _) if labels.is_empty() => Term::UnitLiteral(()),
            core::Term::RecordIntro(labels, exprs) => {
                let scope = self.scope;
                let expr_fields = Iterator::zip(labels.iter(), exprs.iter())
                    .map(|(label, expr)| (((), *label), self.synth(expr)));

                // TODO: type annotations?
                Term::RecordLiteral((), scope.to_scope_from_iter(expr_fields))
            }
            core::Term::RecordElim(head_expr, label) => {
                let head_expr = self.synth(head_expr);

                Term::RecordElim((), self.scope.to_scope(head_expr), ((), *label))
            }
            core::Term::ArrayIntro(elem_exprs) => {
                let scope = self.scope;
                let elem_exprs = elem_exprs.iter().map(|elem_exprs| self.check(elem_exprs));

                // FIXME: Type annotations
                Term::ArrayLiteral((), scope.to_scope_from_iter(elem_exprs))
            }
            core::Term::FormatRecord(labels, _) if labels.is_empty() => {
                let format_type = self.synth_prim(core::Prim::FormatType);
                Term::Ann((), &Term::UnitLiteral(()), self.scope.to_scope(format_type))
            }
            core::Term::FormatRecord(labels, formats) => {
                let initial_rigid_len = self.rigid_len();
                let type_fields = (self.scope).to_scope_from_iter(
                    Iterator::zip(labels.iter(), formats.iter()).map(|(label, format)| {
                        let format = self.check(format);
                        self.push_rigid(Some(*label));
                        (((), *label), format)
                    }),
                );
                self.truncate_rigid(initial_rigid_len);

                Term::FormatRecord((), type_fields)
            }
            core::Term::Prim(prim) => self.synth_prim(*prim),
            core::Term::Const(r#const) => match r#const {
                core::Const::U8(number) => self.synth_number_literal(number, core::Prim::U8Type),
                core::Const::U16(number) => self.synth_number_literal(number, core::Prim::U16Type),
                core::Const::U32(number) => self.synth_number_literal(number, core::Prim::U32Type),
                core::Const::U64(number) => self.synth_number_literal(number, core::Prim::U64Type),
                core::Const::S8(number) => self.synth_number_literal(number, core::Prim::S8Type),
                core::Const::S16(number) => self.synth_number_literal(number, core::Prim::S16Type),
                core::Const::S32(number) => self.synth_number_literal(number, core::Prim::S32Type),
                core::Const::S64(number) => self.synth_number_literal(number, core::Prim::S64Type),
                core::Const::F32(number) => self.synth_number_literal(number, core::Prim::F32Type),
                core::Const::F64(number) => self.synth_number_literal(number, core::Prim::F64Type),
            },
        }
    }
}
