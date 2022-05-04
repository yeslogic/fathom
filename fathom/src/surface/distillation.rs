//! Bidirectional distillation of the core language into the surface language.

use scoped_arena::Scope;
use std::cell::RefCell;

use crate::core::UIntStyle;
use crate::env::{self, EnvLen, GlobalVar, LocalVar, UniqueEnv};
use crate::surface::elaboration::FlexSource;
use crate::surface::{Pattern, Term};
use crate::{core, StringId, StringInterner};

/// Distillation context.
pub struct Context<'interner, 'arena, 'env> {
    interner: &'interner RefCell<StringInterner>,
    /// Scoped arena for storing distilled terms.
    scope: &'arena Scope<'arena>,
    /// Rigid name environment.
    rigid_names: &'env mut UniqueEnv<Option<StringId>>,
    /// Flexible sources.
    flexible_sources: &'env UniqueEnv<FlexSource>,
}

impl<'interner, 'arena, 'env> Context<'interner, 'arena, 'env> {
    /// Construct a new distillation context.
    pub fn new(
        interner: &'interner RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
        rigid_names: &'env mut UniqueEnv<Option<StringId>>,
        flexible_sources: &'env UniqueEnv<FlexSource>,
    ) -> Context<'interner, 'arena, 'env> {
        Context {
            interner,
            scope,
            rigid_names,
            flexible_sources,
        }
    }

    fn rigid_len(&mut self) -> EnvLen {
        self.rigid_names.len()
    }

    fn get_rigid_name(&self, var: LocalVar) -> Option<StringId> {
        self.rigid_names.get_local(var).copied().flatten()
    }

    fn push_rigid(&mut self, name: Option<StringId>) -> StringId {
        let name = name.unwrap_or_else(|| {
            self.interner.borrow_mut().get_or_intern_static("_") // TODO: choose a better name?
        });

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

    fn get_flexible_name(&self, var: GlobalVar) -> Option<StringId> {
        match self.flexible_sources.get_global(var)? {
            FlexSource::HoleExpr(_, name) => Some(*name),
            _ => None,
        }
    }

    fn check_number_literal_styled<T: core::UIntStyled<N>, const N: usize>(
        &mut self,
        number: T,
        style: UIntStyle,
    ) -> Term<'arena, ()> {
        let string = style.format(number);
        let number = self.interner.borrow_mut().get_or_intern(string);
        Term::NumberLiteral((), number)
    }

    fn check_number_literal<T: std::fmt::Display>(&mut self, number: T) -> Term<'arena, ()> {
        let number = self.interner.borrow_mut().get_or_intern(number.to_string());
        Term::NumberLiteral((), number)
    }

    fn check_boolean_pattern(&mut self, boolean: bool) -> Pattern<()> {
        let name = match boolean {
            true => self.interner.borrow_mut().get_or_intern("true"),
            false => self.interner.borrow_mut().get_or_intern("false"),
        };
        Pattern::Name((), name)
    }

    fn check_number_pattern<T: std::fmt::Display>(&mut self, number: T) -> Pattern<()> {
        let number = self.interner.borrow_mut().get_or_intern(number.to_string());
        Pattern::NumberLiteral((), number)
    }

    fn check_number_pattern_styled<T: core::UIntStyled<N>, const N: usize>(
        &mut self,
        number: T,
        style: UIntStyle,
    ) -> Pattern<()> {
        // TODO: Share with check_number_literal_styled
        let string = style.format(number);
        let number = self.interner.borrow_mut().get_or_intern(string);
        Pattern::NumberLiteral((), number)
    }

    fn check_constant_pattern(&mut self, r#const: &core::Const) -> Pattern<()> {
        match r#const {
            core::Const::Bool(boolean) => self.check_boolean_pattern(*boolean),
            core::Const::U8(number, style) => self.check_number_pattern_styled(number, *style),
            core::Const::U16(number, style) => self.check_number_pattern_styled(number, *style),
            core::Const::U32(number, style) => self.check_number_pattern_styled(number, *style),
            core::Const::U64(number, style) => self.check_number_pattern_styled(number, *style),
            core::Const::S8(number) => self.check_number_pattern(number),
            core::Const::S16(number) => self.check_number_pattern(number),
            core::Const::S32(number) => self.check_number_pattern(number),
            core::Const::S64(number) => self.check_number_pattern(number),
            core::Const::F32(number) => self.check_number_pattern(number),
            core::Const::F64(number) => self.check_number_pattern(number),
            core::Const::Pos(number) => self.check_number_pattern(number),
            core::Const::Ref(number) => self.check_number_pattern(number),
        }
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

    fn synth_number_literal_styled<T: core::UIntStyled<N>, const N: usize>(
        &mut self,
        number: T,
        style: UIntStyle,
        prim_type: core::Prim,
    ) -> Term<'arena, ()> {
        let expr = self.check_number_literal_styled(number, style);
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
            core::Term::Let(def_name, def_type, def_expr, output_expr) => {
                let def_type = self.synth(def_type);
                let def_expr = self.check(def_expr);

                let def_name = self.push_rigid(*def_name);
                let output_expr = self.check(output_expr);
                self.pop_rigid();

                Term::Let(
                    (),
                    Pattern::Name((), def_name),
                    Some(self.scope.to_scope(def_type)),
                    self.scope.to_scope(def_expr),
                    self.scope.to_scope(output_expr),
                )
            }
            core::Term::FunLit(input_name, output_expr) => {
                let input_name = self.push_rigid(*input_name);
                let output_expr = self.check(output_expr);
                self.pop_rigid();

                Term::FunLiteral(
                    (),
                    Pattern::Name((), input_name),
                    None,
                    self.scope.to_scope(output_expr),
                )
            }
            core::Term::RecordType(labels, _) if labels.is_empty() => Term::UnitLiteral(()),
            core::Term::RecordLit(labels, _) if labels.is_empty() => Term::UnitLiteral(()),
            core::Term::RecordLit(labels, exprs) => {
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
            core::Term::FormatRecord(labels, _) if labels.is_empty() => Term::UnitLiteral(()),
            core::Term::ConstLit(r#const) => match r#const {
                core::Const::Bool(boolean) => Term::BooleanLiteral((), *boolean),
                core::Const::U8(number, style) => self.check_number_literal_styled(number, *style),
                core::Const::U16(number, style) => self.check_number_literal_styled(number, *style),
                core::Const::U32(number, style) => self.check_number_literal_styled(number, *style),
                core::Const::U64(number, style) => self.check_number_literal_styled(number, *style),
                core::Const::S8(number) => self.check_number_literal(number),
                core::Const::S16(number) => self.check_number_literal(number),
                core::Const::S32(number) => self.check_number_literal(number),
                core::Const::S64(number) => self.check_number_literal(number),
                core::Const::F32(number) => self.check_number_literal(number),
                core::Const::F64(number) => self.check_number_literal(number),
                core::Const::Pos(number) => self.check_number_literal(number),
                core::Const::Ref(number) => self.check_number_literal(number),
            },
            core::Term::ConstCase(head_expr, branches, default_expr) => {
                let head_expr = self.synth(head_expr);
                match default_expr {
                    Some(default_expr) => {
                        let default_branch = {
                            let name = self.push_rigid(None);
                            let default_expr = self.check(default_expr);
                            self.pop_rigid();

                            (Pattern::Name((), name), default_expr)
                        };

                        Term::Match(
                            (),
                            self.scope.to_scope(head_expr),
                            self.scope.to_scope_from_iter(
                                branches
                                    .iter()
                                    .map(|(r#const, output_expr)| {
                                        let pattern = self.check_constant_pattern(r#const);
                                        let output_expr = self.check(output_expr);
                                        (pattern, output_expr)
                                    })
                                    .chain(std::iter::once(default_branch)),
                            ),
                        )
                    }
                    None => Term::Match(
                        (),
                        self.scope.to_scope(head_expr),
                        self.scope.to_scope_from_iter(branches.iter().map(
                            |(r#const, output_expr)| {
                                let pattern = self.check_constant_pattern(r#const);
                                let output_expr = self.check(output_expr);
                                (pattern, output_expr)
                            },
                        )),
                    ),
                }
            }

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
            core::Term::FlexibleVar(var) => match self.get_flexible_name(*var) {
                Some(name) => Term::Hole((), name),
                None => Term::Placeholder(()),
            },
            core::Term::FlexibleInsertion(var, rigid_infos) => {
                let mut head_expr = self.synth(&core::Term::FlexibleVar(*var));

                for (var, info) in Iterator::zip(env::global_vars(), rigid_infos.iter()) {
                    match info {
                        core::EntryInfo::Definition => {}
                        core::EntryInfo::Parameter => {
                            let var = self.rigid_len().global_to_local(var).unwrap();
                            let input_expr = self.check(&core::Term::RigidVar(var));
                            head_expr = Term::App(
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
            core::Term::Let(def_name, def_type, def_expr, output_expr) => {
                let def_type = self.synth(def_type);
                let def_expr = self.check(def_expr);

                let def_name = self.push_rigid(*def_name);
                let output_expr = self.synth(output_expr);
                self.pop_rigid();

                Term::Let(
                    (),
                    Pattern::Name((), def_name),
                    Some(self.scope.to_scope(def_type)),
                    self.scope.to_scope(def_expr),
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
                    Pattern::Name((), input_name),
                    Some(self.scope.to_scope(input_type)),
                    self.scope.to_scope(output_type),
                )
            }
            core::Term::FunLit(input_name, output_expr) => {
                let input_name = self.push_rigid(*input_name);
                let output_expr = self.synth(output_expr);
                self.pop_rigid();

                Term::FunLiteral(
                    (),
                    Pattern::Name((), input_name),
                    None,
                    self.scope.to_scope(output_expr),
                )
            }
            core::Term::FunApp(head_expr, input_expr) => {
                let head_expr = self.synth(head_expr);
                let input_expr = self.check(input_expr);

                Term::App(
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
            core::Term::RecordLit(labels, _) if labels.is_empty() => Term::UnitLiteral(()),
            core::Term::RecordLit(labels, exprs) => {
                let scope = self.scope;
                let expr_fields = Iterator::zip(labels.iter(), exprs.iter())
                    .map(|(label, expr)| (((), *label), self.synth(expr)));

                // TODO: type annotations?
                Term::RecordLiteral((), scope.to_scope_from_iter(expr_fields))
            }
            core::Term::RecordProj(head_expr, label) => {
                let head_expr = self.synth(head_expr);

                Term::Proj((), self.scope.to_scope(head_expr), ((), *label))
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
                Term::FormatRecord((), self.synth_format_fields(labels, formats))
            }
            core::Term::FormatOverlap(labels, formats) => {
                Term::FormatOverlap((), self.synth_format_fields(labels, formats))
            }
            core::Term::Prim(prim) => self.synth_prim(*prim),
            core::Term::ConstLit(r#const) => match r#const {
                core::Const::Bool(boolean) => Term::BooleanLiteral((), *boolean),
                core::Const::U8(number, style) => {
                    self.synth_number_literal_styled(number, *style, core::Prim::U8Type)
                }
                core::Const::U16(number, style) => {
                    self.synth_number_literal_styled(number, *style, core::Prim::U16Type)
                }
                core::Const::U32(number, style) => {
                    self.synth_number_literal_styled(number, *style, core::Prim::U32Type)
                }
                core::Const::U64(number, style) => {
                    self.synth_number_literal_styled(number, *style, core::Prim::U64Type)
                }
                core::Const::S8(number) => self.synth_number_literal(number, core::Prim::S8Type),
                core::Const::S16(number) => self.synth_number_literal(number, core::Prim::S16Type),
                core::Const::S32(number) => self.synth_number_literal(number, core::Prim::S32Type),
                core::Const::S64(number) => self.synth_number_literal(number, core::Prim::S64Type),
                core::Const::F32(number) => self.synth_number_literal(number, core::Prim::F32Type),
                core::Const::F64(number) => self.synth_number_literal(number, core::Prim::F64Type),
                core::Const::Pos(number) => self.synth_number_literal(number, core::Prim::PosType),
                core::Const::Ref(number) => self.synth_number_literal(number, core::Prim::RefType),
            },
            core::Term::ConstCase(head_expr, branches, default_expr) => {
                let head_expr = self.synth(head_expr);
                match default_expr {
                    Some(default_expr) => {
                        let default_branch = {
                            let name = self.push_rigid(None);
                            let default_expr = self.synth(default_expr);
                            self.pop_rigid();

                            (Pattern::Name((), name), default_expr)
                        };

                        Term::Match(
                            (),
                            self.scope.to_scope(head_expr),
                            self.scope.to_scope_from_iter(
                                branches
                                    .iter()
                                    .map(|(r#const, output_expr)| {
                                        let pattern = self.check_constant_pattern(r#const);
                                        let output_expr = self.synth(output_expr);
                                        (pattern, output_expr)
                                    })
                                    .chain(std::iter::once(default_branch)),
                            ),
                        )
                    }
                    None => Term::Match(
                        (),
                        self.scope.to_scope(head_expr),
                        self.scope.to_scope_from_iter(branches.iter().map(
                            |(r#const, output_expr)| {
                                let pattern = self.check_constant_pattern(r#const);
                                let output_expr = self.synth(output_expr);
                                (pattern, output_expr)
                            },
                        )),
                    ),
                }
            }
        }
    }

    fn synth_format_fields(
        &mut self,
        labels: &[StringId],
        core_formats: &[core::Term<'_>],
    ) -> &'arena [(((), StringId), Term<'arena, ()>)] {
        let initial_rigid_len = self.rigid_len();
        let format_fields = (self.scope).to_scope_from_iter(
            Iterator::zip(labels.iter(), core_formats.iter()).map(|(label, format)| {
                let format = self.check(format);
                self.push_rigid(Some(*label));
                (((), *label), format)
            }),
        );
        self.truncate_rigid(initial_rigid_len);

        format_fields
    }
}
