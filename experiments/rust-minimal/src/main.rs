use fathom_minimal::{elaboration, surface, Interner};
use structopt::StructOpt;
use typed_arena::Arena;

#[derive(StructOpt)]
enum Options {
    /// Read and elaborate a term, and print its normal form and type
    Normalise { term: String },
    /// Read and elaborate a term, and print its type
    Type { term: String },
}

fn main() {
    match Options::from_args() {
        Options::Normalise { term } => {
            let mut interner = Interner::new();
            let surface_arena = Arena::new();
            let core_arena = Arena::new();

            let mut context = elaboration::Context::new(&core_arena);
            let surface_term = surface::Term::parse(&mut interner, &surface_arena, &term).unwrap();
            let (core_term, r#type) = context.synth(&surface_term).unwrap();
            let core_term = context.normalize(&core_arena, &core_term).unwrap();

            // TODO: Pretty print term and type
        }
        Options::Type { term } => {
            let mut interner = Interner::new();
            let surface_arena = Arena::new();
            let core_arena = Arena::new();

            let mut context = elaboration::Context::new(&core_arena);
            let surface_term = surface::Term::parse(&mut interner, &surface_arena, &term).unwrap();
            let (_, r#type) = context.synth(&surface_term).unwrap();

            // TODO: Pretty print type
        }
    }
}
