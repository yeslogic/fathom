use fathom_minimal::{core, surface, StringInterner};
use std::io::Read;
use std::path::PathBuf;
use structopt::StructOpt;

/// CLI for the programming language prototype.
#[derive(StructOpt)]
#[structopt(after_help = r#"EXAMPLES:

Using arguments

    fathom-minimal elab --surface-term=example-file
    fathom-minimal normalise --surface-term=example-file

Using pipes and redirects

    echo "fun (A : Type) -> fun (a : A) -> A" | fathom-minimal elab
    cat example-file | fathom-minimal elab
    fathom-minimal elab < example-file

Using heredocs

    fathom-minimal elab <<< "fun (A : Type) -> fun (a : A) -> A"

    fathom-minimal normalise <<EOF
        let id : fun (A : Type) -> fun (a : A) -> A
          = fun A => fun a => a;

        id Type Type
    EOF
"#)]
enum Options {
    /// Parse and elaborate a term, printing the elaborated term and type
    Elab(Args),
    /// Parse and elaborate a term, printing its normal form and type
    Normalise(Args),
    /// Parse and elaborate a term, printing its type
    Type(Args),
}

#[derive(StructOpt)]
struct Args {
    /// Path to a file containing the surface term (`-` to read from stdin)
    #[structopt(
        long = "surface-term",
        name = "FILE",
        default_value = "-",
        possible_values = &["-", "<path>"],
        parse(from_str),
    )]
    surface_term: Input,
}

enum Input {
    StdIn,
    Path(PathBuf),
}

impl From<&str> for Input {
    fn from(src: &str) -> Input {
        match src {
            "-" => Input::StdIn,
            _ => Input::Path(PathBuf::from(src)),
        }
    }
}

const MAX_PRETTY_WIDTH: usize = 80;

fn main() {
    let mut interner = StringInterner::new();
    let surface_arena = surface::Arena::new();
    let core_arena = core::Arena::new();
    let pretty_arena = pretty::Arena::<()>::new();
    let mut context = surface::elaboration::Context::new(&core_arena);

    let term_width = termsize::get().map_or(usize::MAX, |size| usize::from(size.cols));
    let pretty_width = std::cmp::min(term_width, MAX_PRETTY_WIDTH);

    match Options::from_args() {
        Options::Elab(Args { surface_term }) => {
            let surface_term = parse_term(&mut interner, &surface_arena, &surface_term);

            if let Some((term, r#type)) = context.synth(&surface_term) {
                if let Some(r#type) = context.readback(&core_arena, &r#type) {
                    let mut context = surface::distillation::Context::new(&surface_arena);
                    let term = context.check(&term);
                    let r#type = context.synth(&r#type);

                    let context = surface::pretty::Context::new(&interner, &pretty_arena);
                    let doc = context.ann(&term, &r#type).into_doc();

                    println!("{}", doc.pretty(pretty_width));
                }
            }
        }
        Options::Normalise(Args { surface_term }) => {
            let surface_term = parse_term(&mut interner, &surface_arena, &surface_term);

            if let Some((term, r#type)) = context.synth(&surface_term) {
                if let (Some(term), Some(r#type)) = (
                    context.normalize(&core_arena, &term),
                    context.readback(&core_arena, &r#type),
                ) {
                    let mut context = surface::distillation::Context::new(&surface_arena);
                    let term = context.check(&term);
                    let r#type = context.synth(&r#type);

                    let context = surface::pretty::Context::new(&interner, &pretty_arena);
                    let doc = context.ann(&term, &r#type).into_doc();

                    println!("{}", doc.pretty(pretty_width));
                }
            }
        }
        Options::Type(Args { surface_term }) => {
            let surface_term = parse_term(&mut interner, &surface_arena, &surface_term);

            if let Some((_, r#type)) = context.synth(&surface_term) {
                if let Some(r#type) = context.readback(&core_arena, &r#type) {
                    let mut context = surface::distillation::Context::new(&surface_arena);
                    let r#type = context.synth(&r#type);

                    let context = surface::pretty::Context::new(&interner, &pretty_arena);
                    let doc = context.term(&r#type).into_doc();

                    println!("{}", doc.pretty(pretty_width));
                }
            }
        }
    }

    // Print diagnostics to stderr
    for message in context.drain_messages() {
        eprintln!("{}", message);
    }
}

fn parse_term<'arena>(
    interner: &mut StringInterner,
    arena: &'arena surface::Arena<'arena>,
    term: &Input,
) -> surface::Term<'arena> {
    // FIXME: error handling

    let mut source = String::new();
    match term {
        Input::StdIn => std::io::stdin().read_to_string(&mut source).unwrap(),
        Input::Path(path) => std::fs::File::open(path)
            .unwrap()
            .read_to_string(&mut source)
            .unwrap(),
    };

    surface::Term::parse(interner, arena, &source).unwrap()
}
