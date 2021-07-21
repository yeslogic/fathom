//! Bidirectional distillation of the core language into the surface language.

use crate::env::{self, LocalVar, UniqueEnv};
use crate::{core, surface, BytePos, ByteRange, StringId};

const PLACEHOLDER_POS: BytePos = 0;
const PLACEHOLDER_RANGE: ByteRange = ByteRange::new(PLACEHOLDER_POS, PLACEHOLDER_POS);

/// Distillation context.
pub struct Context<'arena> {
    /// Arena for storing distilled terms.
    arena: &'arena surface::Arena<'arena>,
    /// Name environment.
    names: UniqueEnv<StringId>,
}

impl<'arena> Context<'arena> {
    /// Construct a new distillation context.
    pub fn new(arena: &'arena surface::Arena<'arena>) -> Context<'arena> {
        Context {
            arena,
            names: UniqueEnv::new(),
        }
    }

    fn get_name(&self, var: LocalVar) -> Option<StringId> {
        self.names.get_local(var).copied()
    }

    fn push_binding(&mut self, name: StringId) -> StringId {
        self.names.push(name); // TODO: ensure we chose a correctly bound name
        name
    }

    fn pop_binding(&mut self) {
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

                let def_name = self.push_binding(*def_name);
                let output_expr = self.check(output_expr);
                self.pop_binding();

                surface::Term::Let(
                    PLACEHOLDER_POS,
                    (PLACEHOLDER_RANGE, def_name),
                    def_type,
                    def_expr,
                    self.arena.alloc_term(output_expr),
                )
            }
            core::Term::FunIntro(input_name, output_expr) => {
                let input_name = self.push_binding(input_name.unwrap()); // TODO: Unwrap
                let output_expr = self.check(output_expr);
                self.pop_binding();

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
            core::Term::BoundVar(var) => match self.get_name(*var) {
                Some(name) => surface::Term::Name(PLACEHOLDER_RANGE, name),
                None => todo!("misbound variable"), // TODO: error?
            },
            core::Term::ProblemVar(_var) => {
                let name = None; // TODO: lookup problem name
                surface::Term::Hole(PLACEHOLDER_RANGE, name)
            }
            core::Term::InsertedProblem(var, bindings) => {
                let mut head_expr = self.synth(&core::Term::ProblemVar(*var));

                for (var, mode) in Iterator::zip(env::global_vars(), bindings.iter()) {
                    match mode {
                        core::BindingMode::Defined => {}
                        core::BindingMode::Assumed => {
                            let var = self.names.len().global_to_local(var).unwrap();
                            let input_expr = self.synth(&core::Term::BoundVar(var));
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

                let def_name = self.push_binding(*def_name);
                let output_expr = self.synth(output_expr);
                self.pop_binding();

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

                let input_name = self.push_binding(input_name.unwrap()); // TODO: Unwrap
                let output_type = self.check(output_type);
                self.pop_binding();

                surface::Term::FunType(
                    PLACEHOLDER_POS,
                    (PLACEHOLDER_RANGE, input_name),
                    self.arena.alloc_term(input_type),
                    self.arena.alloc_term(output_type),
                )
            }
            core::Term::FunIntro(input_name, output_expr) => {
                let input_name = self.push_binding(input_name.unwrap()); // TODO: Unwrap
                let output_expr = self.synth(output_expr);
                self.pop_binding();

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
