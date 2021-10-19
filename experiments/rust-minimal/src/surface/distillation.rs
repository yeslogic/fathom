//! Bidirectional distillation of the core language into the surface language.

use scoped_arena::Scope;
use std::cell::RefCell;

use crate::env::{self, EnvLen, LocalVar, UniqueEnv};
use crate::surface::Term;
use crate::{core, StringId, StringInterner};

/// Distillation context.
pub struct Context<'interner, 'arena> {
    interner: &'interner RefCell<StringInterner>,
    /// Scoped arena for storing distilled terms.
    scope: &'arena Scope<'arena>,
    /// Rigid name environment.
    rigid_names: UniqueEnv<StringId>,

    placeholder_string: StringId,
}

impl<'interner, 'arena> Context<'interner, 'arena> {
    /// Construct a new distillation context.
    pub fn new(
        interner: &'interner RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
    ) -> Context<'interner, 'arena> {
        let placeholder_string = interner.borrow_mut().get_or_intern("_");

        Context {
            interner,
            scope,
            rigid_names: UniqueEnv::new(),
            placeholder_string,
        }
    }

    fn rigid_len(&mut self) -> EnvLen {
        self.rigid_names.len()
    }

    fn get_rigid_name(&self, var: LocalVar) -> Option<StringId> {
        self.rigid_names.get_local(var).copied()
    }

    fn push_rigid(&mut self, name: Option<StringId>) -> StringId {
        let name = name.unwrap_or(self.placeholder_string); // TODO: choose a better name?
        self.rigid_names.push(name); // TODO: ensure we chose a correctly bound name
        name
    }

    fn pop_rigid(&mut self) {
        self.rigid_names.pop();
    }

    fn truncate_rigid(&mut self, len: EnvLen) {
        self.rigid_names.truncate(len);
    }

    fn numeric_literal<T: std::fmt::Display>(&mut self, number: T) -> Term<'arena, ()> {
        Term::NumberLiteral(
            (),
            self.interner.borrow_mut().get_or_intern(number.to_string()),
        )
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

                Term::Let(
                    (),
                    ((), def_name),
                    def_type,
                    def_expr,
                    self.scope.to_scope(output_expr),
                )
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

            core::Term::U8Intro(number) => self.numeric_literal(number),
            core::Term::U16Intro(number) => self.numeric_literal(number),
            core::Term::U32Intro(number) => self.numeric_literal(number),
            core::Term::U64Intro(number) => self.numeric_literal(number),
            core::Term::S8Intro(number) => self.numeric_literal(number),
            core::Term::S16Intro(number) => self.numeric_literal(number),
            core::Term::S32Intro(number) => self.numeric_literal(number),
            core::Term::S64Intro(number) => self.numeric_literal(number),
            core::Term::F32Intro(number) => self.numeric_literal(number),
            core::Term::F64Intro(number) => self.numeric_literal(number),

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

                Term::Let(
                    (),
                    ((), def_name),
                    def_type,
                    def_expr,
                    self.scope.to_scope(output_expr),
                )
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

            core::Term::U8Type => Term::U8Type(()),
            core::Term::U16Type => Term::U16Type(()),
            core::Term::U32Type => Term::U32Type(()),
            core::Term::U64Type => Term::U64Type(()),
            core::Term::S8Type => Term::S8Type(()),
            core::Term::S16Type => Term::S16Type(()),
            core::Term::S32Type => Term::S32Type(()),
            core::Term::S64Type => Term::S64Type(()),
            core::Term::F32Type => Term::F32Type(()),
            core::Term::F64Type => Term::F64Type(()),

            // FIXME: annotations
            core::Term::U8Intro(number) => self.numeric_literal(number),
            core::Term::U16Intro(number) => self.numeric_literal(number),
            core::Term::U32Intro(number) => self.numeric_literal(number),
            core::Term::U64Intro(number) => self.numeric_literal(number),
            core::Term::S8Intro(number) => self.numeric_literal(number),
            core::Term::S16Intro(number) => self.numeric_literal(number),
            core::Term::S32Intro(number) => self.numeric_literal(number),
            core::Term::S64Intro(number) => self.numeric_literal(number),
            core::Term::F32Intro(number) => self.numeric_literal(number),
            core::Term::F64Intro(number) => self.numeric_literal(number),

            core::Term::FormatType => Term::FormatType(()),
            core::Term::FormatRecord(labels, _) if labels.is_empty() => {
                Term::Ann((), &Term::RecordEmpty(()), &Term::FormatType(()))
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
            core::Term::FormatFail => Term::FormatFail(()),
            core::Term::FormatU8 => Term::FormatU8(()),
            core::Term::FormatU16Be => Term::FormatU16Be(()),
            core::Term::FormatU16Le => Term::FormatU16Le(()),
            core::Term::FormatU32Be => Term::FormatU32Be(()),
            core::Term::FormatU32Le => Term::FormatU32Le(()),
            core::Term::FormatU64Be => Term::FormatU64Be(()),
            core::Term::FormatU64Le => Term::FormatU64Le(()),
            core::Term::FormatS8 => Term::FormatS8(()),
            core::Term::FormatS16Be => Term::FormatS16Be(()),
            core::Term::FormatS16Le => Term::FormatS16Le(()),
            core::Term::FormatS32Be => Term::FormatS32Be(()),
            core::Term::FormatS32Le => Term::FormatS32Le(()),
            core::Term::FormatS64Be => Term::FormatS64Be(()),
            core::Term::FormatS64Le => Term::FormatS64Le(()),
            core::Term::FormatF32Be => Term::FormatF32Be(()),
            core::Term::FormatF32Le => Term::FormatF32Le(()),
            core::Term::FormatF64Be => Term::FormatF64Be(()),
            core::Term::FormatF64Le => Term::FormatF64Le(()),
            core::Term::FormatRepr(expr) => {
                let expr = self.check(expr);

                Term::FormatRepr((), self.scope.to_scope(expr))
            }

            // NOTE: Not sure if this is a great approach!
            core::Term::ReportedError => Term::Hole((), None),
        }
    }
}
