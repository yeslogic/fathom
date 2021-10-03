//! Bidirectional distillation of the core language into the surface language.

use scoped_arena::Scope;

use crate::env::{self, EnvLen, LocalVar, UniqueEnv};
use crate::surface::Term;
use crate::{core, BytePos, ByteRange, StringId, StringInterner};

const PLACEHOLDER_POS: BytePos = 0;
const PLACEHOLDER_RANGE: ByteRange = ByteRange::new(PLACEHOLDER_POS, PLACEHOLDER_POS);

/// Distillation context.
pub struct Context<'arena> {
    /// Scoped arena for storing distilled terms.
    scope: &'arena Scope<'arena>,
    /// Rigid name environment.
    rigid_names: UniqueEnv<StringId>,

    placeholder_string: StringId,
}

impl<'arena> Context<'arena> {
    /// Construct a new distillation context.
    pub fn new(interner: &mut StringInterner, scope: &'arena Scope<'arena>) -> Context<'arena> {
        let placeholder_string = interner.get_or_intern("_");

        Context {
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

    /// Distill a core term into a surface term, in a 'checkable' context.
    pub fn check(&mut self, core_term: &core::Term<'_>) -> Term<'arena> {
        match core_term {
            core::Term::Ann(expr, _) => {
                // Avoid adding extraneous type annotations!
                self.check(expr)
            }
            core::Term::Let(def_name, def_expr, output_expr) => {
                let (def_expr, def_type) = match self.synth(def_expr) {
                    Term::Ann(expr, r#type) => (expr, Some(r#type)),
                    expr => (self.scope.to_scope(expr) as &_, None),
                };

                let def_name = self.push_rigid(Some(*def_name));
                let output_expr = self.check(output_expr);
                self.pop_rigid();

                Term::Let(
                    PLACEHOLDER_POS,
                    (PLACEHOLDER_RANGE, def_name),
                    def_type,
                    def_expr,
                    self.scope.to_scope(output_expr),
                )
            }
            core::Term::FunIntro(input_name, output_expr) => {
                let input_name = self.push_rigid(*input_name);
                let output_expr = self.check(output_expr);
                self.pop_rigid();

                Term::FunIntro(
                    PLACEHOLDER_POS,
                    (PLACEHOLDER_RANGE, input_name),
                    self.scope.to_scope(output_expr),
                )
            }
            core::Term::RecordType(labels, _) if labels.is_empty() => {
                Term::RecordEmpty(PLACEHOLDER_RANGE)
            }
            core::Term::RecordIntro(labels, _) if labels.is_empty() => {
                Term::RecordEmpty(PLACEHOLDER_RANGE)
            }
            core::Term::RecordIntro(labels, exprs) => {
                let scope = self.scope;
                let expr_fields = Iterator::zip(labels.iter(), exprs.iter())
                    .map(|(label, expr)| ((PLACEHOLDER_RANGE, *label), self.check(expr)));

                Term::RecordIntro(PLACEHOLDER_RANGE, scope.to_scope_from_iter(expr_fields))
            }
            _ => self.synth(core_term),
        }
    }

    /// Distill a core term into a surface term, in a 'synthesizable' context.
    pub fn synth(&mut self, core_term: &core::Term<'_>) -> Term<'arena> {
        match core_term {
            core::Term::RigidVar(var) => match self.get_rigid_name(*var) {
                Some(name) => Term::Name(PLACEHOLDER_RANGE, name),
                None => todo!("misbound variable"), // TODO: error?
            },
            core::Term::FlexibleVar(_var) => {
                let name = None; // TODO: lookup flexible variable name
                Term::Hole(PLACEHOLDER_RANGE, name)
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

                Term::Ann(self.scope.to_scope(expr), self.scope.to_scope(r#type))
            }
            core::Term::Let(def_name, def_expr, output_expr) => {
                let (def_expr, def_type) = match self.synth(def_expr) {
                    Term::Ann(expr, r#type) => (expr, Some(r#type)),
                    expr => (self.scope.to_scope(expr) as &_, None),
                };

                let def_name = self.push_rigid(Some(*def_name));
                let output_expr = self.synth(output_expr);
                self.pop_rigid();

                Term::Let(
                    PLACEHOLDER_POS,
                    (PLACEHOLDER_RANGE, def_name),
                    def_type,
                    def_expr,
                    self.scope.to_scope(output_expr),
                )
            }
            core::Term::Universe => Term::Universe(PLACEHOLDER_RANGE),
            core::Term::FunType(input_name, input_type, output_type) => {
                let input_type = self.check(input_type);

                let input_name = self.push_rigid(*input_name);
                let output_type = self.check(output_type);
                self.pop_rigid();

                // TODO: distill to arrow if `input_name` is not bound in `output_type`
                Term::FunType(
                    PLACEHOLDER_POS,
                    (PLACEHOLDER_RANGE, input_name),
                    self.scope.to_scope(input_type),
                    self.scope.to_scope(output_type),
                )
            }
            core::Term::FunIntro(input_name, output_expr) => {
                let input_name = self.push_rigid(*input_name);
                let output_expr = self.synth(output_expr);
                self.pop_rigid();

                Term::FunIntro(
                    PLACEHOLDER_POS,
                    (PLACEHOLDER_RANGE, input_name),
                    self.scope.to_scope(output_expr),
                )
            }
            core::Term::FunElim(head_expr, input_expr) => {
                let head_expr = self.synth(head_expr);
                let input_expr = self.synth(input_expr);

                Term::FunElim(
                    self.scope.to_scope(head_expr),
                    self.scope.to_scope(input_expr),
                )
            }
            core::Term::RecordType(labels, _) if labels.is_empty() => Term::Ann(
                self.scope.to_scope(Term::RecordEmpty(PLACEHOLDER_RANGE)),
                self.scope.to_scope(Term::Universe(PLACEHOLDER_RANGE)),
            ),
            core::Term::RecordType(labels, types) => {
                let initial_rigid_len = self.rigid_len();
                let type_fields = (self.scope).to_scope_from_iter(
                    Iterator::zip(labels.iter(), types.iter()).map(|(label, r#type)| {
                        let r#type = self.check(r#type);
                        self.push_rigid(Some(*label));
                        ((PLACEHOLDER_RANGE, *label), r#type)
                    }),
                );
                self.truncate_rigid(initial_rigid_len);

                Term::RecordType(PLACEHOLDER_RANGE, type_fields)
            }
            core::Term::RecordIntro(labels, _) if labels.is_empty() => {
                Term::RecordEmpty(PLACEHOLDER_RANGE)
            }
            core::Term::RecordIntro(labels, exprs) => {
                let scope = self.scope;
                let expr_fields = Iterator::zip(labels.iter(), exprs.iter())
                    .map(|(label, expr)| ((PLACEHOLDER_RANGE, *label), self.synth(expr)));
                // TODO: type annotations?

                Term::RecordIntro(PLACEHOLDER_RANGE, scope.to_scope_from_iter(expr_fields))
            }
            core::Term::RecordElim(head_expr, label) => {
                let head_expr = self.synth(head_expr);

                Term::RecordElim(self.scope.to_scope(head_expr), (PLACEHOLDER_RANGE, *label))
            }
            // NOTE: Not sure if this is a great approach!
            core::Term::ReportedError => Term::Hole(PLACEHOLDER_RANGE, None),
        }
    }
}
