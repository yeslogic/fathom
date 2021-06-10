use fathom_minimal::{distillation, elaboration, surface, StringInterner};
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
            let mut interner = StringInterner::new();
            let surface_arena = Arena::new();
            let core_arena = Arena::new();

            let mut context = elaboration::Context::new(&core_arena);
            let surface_term = surface::Term::parse(&mut interner, &surface_arena, &term).unwrap();

            if let Some((term, r#type)) = context.synth(&surface_term) {
                if let (Some(term), Some(r#type)) = (
                    context.normalize(&core_arena, &term),
                    context.readback(&core_arena, &r#type),
                ) {
                    use pretty::DocAllocator;

                    let mut context = distillation::Context::new(&surface_arena);
                    let term = context.check(&term);
                    let r#type = context.synth(&r#type);

                    let doc_arena = pretty::Arena::<()>::new();
                    let doc = (doc_arena.nil())
                        .append(term.pretty(&interner, &doc_arena))
                        .append(doc_arena.space())
                        .append(doc_arena.text(":"))
                        .append(doc_arena.space())
                        .append(r#type.pretty(&interner, &doc_arena))
                        .into_doc();

                    println!("{}", doc.pretty(usize::MAX));
                }
            }

            for message in context.drain_messages() {
                println!("{}", message);
            }
        }
        Options::Type { term } => {
            let mut interner = StringInterner::new();
            let surface_arena = Arena::new();
            let core_arena = Arena::new();

            let mut context = elaboration::Context::new(&core_arena);
            let surface_term = surface::Term::parse(&mut interner, &surface_arena, &term).unwrap();

            if let Some((_, r#type)) = context.synth(&surface_term) {
                if let Some(r#type) = context.readback(&core_arena, &r#type) {
                    let mut context = distillation::Context::new(&surface_arena);
                    let r#type = context.synth(&r#type);

                    let doc_arena = pretty::Arena::<()>::new();
                    let doc = r#type.pretty(&interner, &doc_arena).into_doc();

                    println!("{}", doc.pretty(usize::MAX));
                }
            }

            for message in context.drain_messages() {
                println!("{}", message);
            }
        }
    }
}
