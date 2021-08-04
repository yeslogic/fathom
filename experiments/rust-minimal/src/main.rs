use fathom_minimal::surface::{distillation, elaboration};
use fathom_minimal::{core, surface, StringInterner};
use std::io::Read;
use std::path::PathBuf;
use structopt::StructOpt;

/// CLI for the programming language prototype.
#[derive(StructOpt)]
#[structopt(after_help = r#"EXAMPLES:

Using arguments

    fathom-minimal elab --surface-term=examples/prelude.txt
    fathom-minimal norm --surface-term=examples/prelude.txt

Using pipes and redirects

    echo "fun (A : Type) -> A -> A" | fathom-minimal elab
    cat examples/prelude.txt | fathom-minimal elab
    fathom-minimal elab < examples/prelude.txt

Using heredocs

    fathom-minimal elab <<< "fun (A : Type) -> A -> A"

    fathom-minimal norm <<EOF
        let id : fun (A : Type) -> A -> A
          = fun A => fun a => a;

        id Type Type
    EOF
"#)]
enum Options {
    /// Parse and elaborate a term, printing the elaborated term and type
    Elab(Args),
    /// Parse and elaborate a term, printing its normal form and type
    Norm(Args),
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
        // possible_values = &["-", "<path>"],
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
    let term_width = termsize::get().map_or(usize::MAX, |size| usize::from(size.cols));
    let pretty_width = std::cmp::min(term_width, MAX_PRETTY_WIDTH);

    match Options::from_args() {
        Options::Elab(Args { surface_term }) => {
            let surface_term = parse_term(&mut interner, &surface_arena, &surface_term);

            let mut context = elaboration::Context::new(&core_arena);
            let (term, r#type) = context.synth(&surface_term);
            let r#type = context.readback(&core_arena, &r#type);

            if check_elaboration_messages(&mut context) {
                let mut context = distillation::Context::new(&mut interner, &surface_arena);
                let term = context.check(&term);
                let r#type = context.synth(&r#type);

                let context = surface::pretty::Context::new(&interner, &pretty_arena);
                let doc = context.ann(&term, &r#type).into_doc();

                println!("{}", doc.pretty(pretty_width));
            } else {
                std::process::exit(1);
            }
        }
        Options::Norm(Args { surface_term }) => {
            let surface_term = parse_term(&mut interner, &surface_arena, &surface_term);

            let mut context = elaboration::Context::new(&core_arena);
            let (term, r#type) = context.synth(&surface_term);
            let term = context.normalize(&core_arena, &term);
            let r#type = context.readback(&core_arena, &r#type);

            if check_elaboration_messages(&mut context) {
                let mut context = distillation::Context::new(&mut interner, &surface_arena);
                let term = context.check(&term);
                let r#type = context.synth(&r#type);

                let context = surface::pretty::Context::new(&interner, &pretty_arena);
                let doc = context.ann(&term, &r#type).into_doc();

                println!("{}", doc.pretty(pretty_width));
            } else {
                std::process::exit(1);
            }
        }
        Options::Type(Args { surface_term }) => {
            let surface_term = parse_term(&mut interner, &surface_arena, &surface_term);

            let mut context = elaboration::Context::new(&core_arena);
            let (_, r#type) = context.synth(&surface_term);
            let r#type = context.readback(&core_arena, &r#type);

            if check_elaboration_messages(&mut context) {
                let mut context = distillation::Context::new(&mut interner, &surface_arena);
                let r#type = context.synth(&r#type);

                let context = surface::pretty::Context::new(&interner, &pretty_arena);
                let doc = context.term(&r#type).into_doc();

                println!("{}", doc.pretty(pretty_width));
            } else {
                std::process::exit(1);
            }
        }
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

fn check_elaboration_messages(context: &mut elaboration::Context<'_>) -> bool {
    let mut is_ok = true;

    for message in context.drain_messages() {
        eprintln!("{:?}", message);
        is_ok = false;
    }

    is_ok
}
