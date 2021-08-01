//! Bidirectional distillation of the core language into the surface language.

use crate::env::{self, LocalVar, UniqueEnv};
use crate::{core, surface, BytePos, ByteRange, StringId, StringInterner};

const PLACEHOLDER_POS: BytePos = 0;
const PLACEHOLDER_RANGE: ByteRange = ByteRange::new(PLACEHOLDER_POS, PLACEHOLDER_POS);

/// Distillation context.
pub struct Context<'arena> {
    /// Arena for storing distilled terms.
    arena: &'arena surface::Arena<'arena>,
    /// Name environment.
    names: UniqueEnv<StringId>,

    placeholder_string: StringId,
}

impl<'arena> Context<'arena> {
    /// Construct a new distillation context.
    pub fn new(
        interner: &mut StringInterner,
        arena: &'arena surface::Arena<'arena>,
    ) -> Context<'arena> {
        let placeholder_string = interner.get_or_intern("_");

        Context {
            arena,
            names: UniqueEnv::new(),
            placeholder_string,
        }
    }

    fn get_rigid_name(&self, var: LocalVar) -> Option<StringId> {
        self.names.get_local(var).copied()
    }

    fn push_rigid(&mut self, name: Option<StringId>) -> StringId {
        let name = name.unwrap_or(self.placeholder_string); // TODO: choose a better name
        self.names.push(name); // TODO: ensure we chose a correctly bound name
        name
    }

    fn pop_rigid(&mut self) {
        self.names.pop();
    }

    /// Distill a core term into a surface term, in a 'checkable' context.
    pub fn check(&mut self, core_term: &core::Term<'_>) -> surface::Term<'arena> {
        match core_term {
            core::Term::Ann(expr, _) => {
                // Avoid adding extraneous type annotations!
                self.check(expr)
            }
            core::Term::Let(def_name, def_expr, output_expr) => {
                let (def_expr, def_type) = match self.synth(def_expr) {
                    surface::Term::Ann(expr, r#type) => (expr, Some(r#type)),
                    expr => (self.arena.alloc_term(expr) as &_, None),
                };

                let def_name = self.push_rigid(Some(*def_name));
                let output_expr = self.check(output_expr);
                self.pop_rigid();

                surface::Term::Let(
                    PLACEHOLDER_POS,
                    (PLACEHOLDER_RANGE, def_name),
                    def_type,
                    def_expr,
                    self.arena.alloc_term(output_expr),
                )
            }
            core::Term::FunIntro(input_name, output_expr) => {
                let input_name = self.push_rigid(*input_name);
                let output_expr = self.check(output_expr);
                self.pop_rigid();

                surface::Term::FunIntro(
                    PLACEHOLDER_POS,
                    (PLACEHOLDER_RANGE, input_name),
                    self.arena.alloc_term(output_expr),
                )
            }
            _ => self.synth(core_term),
        }
    }

    /// Distill a core term into a surface term, in a 'synthesizable' context.
    pub fn synth(&mut self, core_term: &core::Term<'_>) -> surface::Term<'arena> {
        match core_term {
            core::Term::RigidVar(var) => match self.get_rigid_name(*var) {
                Some(name) => surface::Term::Name(PLACEHOLDER_RANGE, name),
                None => todo!("misbound variable"), // TODO: error?
            },
            core::Term::FlexibleVar(_var) => {
                let name = None; // TODO: lookup flexible variable name
                surface::Term::Hole(PLACEHOLDER_RANGE, name)
            }
            core::Term::FlexibleInsertion(var, rigid_infos) => {
                let mut head_expr = self.synth(&core::Term::FlexibleVar(*var));

                for (var, info) in Iterator::zip(env::global_vars(), rigid_infos.iter()) {
                    match info {
                        core::EntryInfo::Concrete => {}
                        core::EntryInfo::Abstract => {
                            let var = self.names.len().global_to_local(var).unwrap();
                            let input_expr = self.synth(&core::Term::RigidVar(var));
                            head_expr = surface::Term::FunElim(
                                self.arena.alloc_term(head_expr),
                                self.arena.alloc_term(input_expr),
                            );
                        }
                    }
                }

                head_expr
            }
            core::Term::Ann(expr, r#type) => {
                let r#type = self.synth(r#type);
                let expr = self.check(expr);

                surface::Term::Ann(self.arena.alloc_term(expr), self.arena.alloc_term(r#type))
            }
            core::Term::Let(def_name, def_expr, output_expr) => {
                let (def_expr, def_type) = match self.synth(def_expr) {
                    surface::Term::Ann(expr, r#type) => (expr, Some(r#type)),
                    expr => (self.arena.alloc_term(expr) as &_, None),
                };

                let def_name = self.push_rigid(Some(*def_name));
                let output_expr = self.synth(output_expr);
                self.pop_rigid();

                surface::Term::Let(
                    PLACEHOLDER_POS,
                    (PLACEHOLDER_RANGE, def_name),
                    def_type,
                    def_expr,
                    self.arena.alloc_term(output_expr),
                )
            }
            core::Term::Universe => surface::Term::Universe(PLACEHOLDER_RANGE),
            core::Term::FunType(input_name, input_type, output_type) => {
                let input_type = self.check(input_type);

                let input_name = self.push_rigid(*input_name);
                let output_type = self.check(output_type);
                self.pop_rigid();

                // TODO: distill to arrow if `input_name` is not bound in `output_type`
                surface::Term::FunType(
                    PLACEHOLDER_POS,
                    (PLACEHOLDER_RANGE, input_name),
                    self.arena.alloc_term(input_type),
                    self.arena.alloc_term(output_type),
                )
            }
            core::Term::FunIntro(input_name, output_expr) => {
                let input_name = self.push_rigid(*input_name);
                let output_expr = self.synth(output_expr);
                self.pop_rigid();

                surface::Term::FunIntro(
                    PLACEHOLDER_POS,
                    (PLACEHOLDER_RANGE, input_name),
                    self.arena.alloc_term(output_expr),
                )
            }
            core::Term::FunElim(head_expr, input_expr) => {
                let head_expr = self.synth(head_expr);
                let input_expr = self.synth(input_expr);

                surface::Term::FunElim(
                    self.arena.alloc_term(head_expr),
                    self.arena.alloc_term(input_expr),
                )
            }
            // NOTE: Not sure if this is a great approach!
            core::Term::ReportedError => surface::Term::Hole(PLACEHOLDER_RANGE, None),
        }
    }
}
