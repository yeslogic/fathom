//! Bidirectional distillation of the core language into the surface language.

use scoped_arena::Scope;
use std::cell::RefCell;

use crate::env::{self, EnvLen, LocalVar, UniqueEnv};
use crate::surface::Term;
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

    fn synth_prim(&mut self, name: &'static str) -> Term<'arena, ()> {
        Term::Name((), self.interner.borrow_mut().get_or_intern_static(name))
    }

    fn synth_number_literal<T: std::fmt::Display>(
        &mut self,
        number: T,
        type_name: &'static str,
    ) -> Term<'arena, ()> {
        let expr = self.check_number_literal(number);
        let r#type = self.synth_prim(type_name);

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

                let def_name = self.push_rigid(Some(*def_name));
                let output_expr = self.check(output_expr);
                self.pop_rigid();

                let output_expr = self.scope.to_scope(output_expr);

                Term::Let((), ((), def_name), def_type, def_expr, output_expr)
            }
            core::Term::FunIntro(input_name, output_expr) => {
                let input_name = self.push_rigid(*input_name);
                let output_expr = self.check(output_expr);
                self.pop_rigid();

                Term::FunIntro((), ((), input_name), self.scope.to_scope(output_expr))
            }
            core::Term::RecordType(labels, _) if labels.is_empty() => Term::RecordEmpty(()),
            core::Term::RecordIntro(labels, _) if labels.is_empty() => Term::RecordEmpty(()),
            core::Term::RecordIntro(labels, exprs) => {
                let scope = self.scope;
                let expr_fields = Iterator::zip(labels.iter(), exprs.iter())
                    .map(|(label, expr)| (((), *label), self.check(expr)));

                Term::RecordIntro((), scope.to_scope_from_iter(expr_fields))
            }

            core::Term::U8Intro(number) => self.check_number_literal(number),
            core::Term::U16Intro(number) => self.check_number_literal(number),
            core::Term::U32Intro(number) => self.check_number_literal(number),
            core::Term::U64Intro(number) => self.check_number_literal(number),
            core::Term::S8Intro(number) => self.check_number_literal(number),
            core::Term::S16Intro(number) => self.check_number_literal(number),
            core::Term::S32Intro(number) => self.check_number_literal(number),
            core::Term::S64Intro(number) => self.check_number_literal(number),
            core::Term::F32Intro(number) => self.check_number_literal(number),
            core::Term::F64Intro(number) => self.check_number_literal(number),

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
                let name = None; // TODO: lookup flexible variable name
                Term::Hole((), name)
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

                let def_name = self.push_rigid(Some(*def_name));
                let output_expr = self.synth(output_expr);
                self.pop_rigid();

                let output_expr = self.scope.to_scope(output_expr);

                Term::Let((), ((), def_name), def_type, def_expr, output_expr)
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
                    ((), input_name),
                    self.scope.to_scope(input_type),
                    self.scope.to_scope(output_type),
                )
            }
            core::Term::FunIntro(input_name, output_expr) => {
                let input_name = self.push_rigid(*input_name);
                let output_expr = self.synth(output_expr);
                self.pop_rigid();

                Term::FunIntro((), ((), input_name), self.scope.to_scope(output_expr))
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
                Term::Ann((), &Term::RecordEmpty(()), &Term::Universe(()))
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
            core::Term::RecordIntro(labels, _) if labels.is_empty() => Term::RecordEmpty(()),
            core::Term::RecordIntro(labels, exprs) => {
                let scope = self.scope;
                let expr_fields = Iterator::zip(labels.iter(), exprs.iter())
                    .map(|(label, expr)| (((), *label), self.synth(expr)));
                // TODO: type annotations?

                Term::RecordIntro((), scope.to_scope_from_iter(expr_fields))
            }
            core::Term::RecordElim(head_expr, label) => {
                let head_expr = self.synth(head_expr);

                Term::RecordElim((), self.scope.to_scope(head_expr), ((), *label))
            }

            core::Term::U8Type => self.synth_prim("U8"),
            core::Term::U16Type => self.synth_prim("U16"),
            core::Term::U32Type => self.synth_prim("U32"),
            core::Term::U64Type => self.synth_prim("U64"),
            core::Term::S8Type => self.synth_prim("S8"),
            core::Term::S16Type => self.synth_prim("S16"),
            core::Term::S32Type => self.synth_prim("S32"),
            core::Term::S64Type => self.synth_prim("S64"),
            core::Term::F32Type => self.synth_prim("F32"),
            core::Term::F64Type => self.synth_prim("F64"),

            core::Term::U8Intro(number) => self.synth_number_literal(number, "U8"),
            core::Term::U16Intro(number) => self.synth_number_literal(number, "U16"),
            core::Term::U32Intro(number) => self.synth_number_literal(number, "U32"),
            core::Term::U64Intro(number) => self.synth_number_literal(number, "U64"),
            core::Term::S8Intro(number) => self.synth_number_literal(number, "S8"),
            core::Term::S16Intro(number) => self.synth_number_literal(number, "S16"),
            core::Term::S32Intro(number) => self.synth_number_literal(number, "S32"),
            core::Term::S64Intro(number) => self.synth_number_literal(number, "S64"),
            core::Term::F32Intro(number) => self.synth_number_literal(number, "F32"),
            core::Term::F64Intro(number) => self.synth_number_literal(number, "F64"),

            core::Term::FormatType => self.synth_prim("Format"),
            core::Term::FormatRecord(labels, _) if labels.is_empty() => {
                let format_type = self.synth_prim("Format");
                Term::Ann((), &Term::RecordEmpty(()), self.scope.to_scope(format_type))
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
            core::Term::FormatFail => self.synth_prim("fail"),
            core::Term::FormatU8 => self.synth_prim("u8"),
            core::Term::FormatU16Be => self.synth_prim("u16be"),
            core::Term::FormatU16Le => self.synth_prim("u16le"),
            core::Term::FormatU32Be => self.synth_prim("u32be"),
            core::Term::FormatU32Le => self.synth_prim("u32le"),
            core::Term::FormatU64Be => self.synth_prim("u64be"),
            core::Term::FormatU64Le => self.synth_prim("u64le"),
            core::Term::FormatS8 => self.synth_prim("s8"),
            core::Term::FormatS16Be => self.synth_prim("s16be"),
            core::Term::FormatS16Le => self.synth_prim("s16le"),
            core::Term::FormatS32Be => self.synth_prim("s32be"),
            core::Term::FormatS32Le => self.synth_prim("s32le"),
            core::Term::FormatS64Be => self.synth_prim("s64be"),
            core::Term::FormatS64Le => self.synth_prim("s64le"),
            core::Term::FormatF32Be => self.synth_prim("f32be"),
            core::Term::FormatF32Le => self.synth_prim("f32le"),
            core::Term::FormatF64Be => self.synth_prim("f64be"),
            core::Term::FormatF64Le => self.synth_prim("f64le"),
            core::Term::FormatRepr(expr) => {
                let repr = self.synth_prim("Repr");
                let expr = self.check(expr);

                Term::FunElim((), self.scope.to_scope(repr), self.scope.to_scope(expr))
            }

            // NOTE: Not sure if this is a great approach!
            core::Term::ReportedError => Term::Hole((), None),
        }
    }
}
