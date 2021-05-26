use fathom_minimal::elaboration;
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
            let term_arena = Arena::new();
            // TODO: Parse term
            let context = elaboration::Context::new(&term_arena);
            // TODO: Elaborate term and synthesize its type
            // TODO: Normalize term
            // TODO: Print term and type
        }
        Options::Type { term } => {
            let term_arena = Arena::new();
            // TODO: Parse term
            let context = elaboration::Context::new(&term_arena);
            // TODO: Elaborate term and synthesize its type
            // TODO: Print type
        }
    }
}
