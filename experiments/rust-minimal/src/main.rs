use codespan_reporting::diagnostic::Severity;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{BufferedStandardStream, ColorChoice};
use fathom_minimal::surface::{distillation, elaboration};
use fathom_minimal::{core, surface, StringInterner};
use std::io::{Read, Write};
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
        parse(from_str)
    )]
    surface_term: Input,
    /// Continue even if errors were encountered.
    #[structopt(long = "allow-errors")]
    allow_errors: bool,
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
    let mut writer = BufferedStandardStream::stderr(ColorChoice::Auto);
    let term_config = codespan_reporting::term::Config::default();

    let mut interner = StringInterner::new();
    let surface_arena = surface::Arena::new();
    let core_arena = core::Arena::new();

    let pretty_arena = pretty::Arena::<()>::new();
    let term_width = termsize::get().map_or(usize::MAX, |size| usize::from(size.cols));
    let pretty_width = std::cmp::min(term_width, MAX_PRETTY_WIDTH);

    match Options::from_args() {
        Options::Elab(args) => {
            let file = load_input(&args.surface_term);
            let surface_term = parse_term(&mut interner, &surface_arena, &file);

            let mut context = elaboration::Context::new(&core_arena);
            let (term, r#type) = context.synth(&surface_term);
            let r#type = context.readback(&core_arena, &r#type);

            if check_elaboration(&mut writer, &term_config, &file, &interner, &mut context)
                || args.allow_errors
            {
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
        Options::Norm(args) => {
            let file = load_input(&args.surface_term);
            let surface_term = parse_term(&mut interner, &surface_arena, &file);

            let mut context = elaboration::Context::new(&core_arena);
            let (term, r#type) = context.synth(&surface_term);
            let term = context.normalize(&core_arena, &term);
            let r#type = context.readback(&core_arena, &r#type);

            if check_elaboration(&mut writer, &term_config, &file, &interner, &mut context)
                || args.allow_errors
            {
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
        Options::Type(args) => {
            let file = load_input(&args.surface_term);
            let surface_term = parse_term(&mut interner, &surface_arena, &file);

            let mut context = elaboration::Context::new(&core_arena);
            let (_, r#type) = context.synth(&surface_term);
            let r#type = context.readback(&core_arena, &r#type);

            if check_elaboration(&mut writer, &term_config, &file, &interner, &mut context)
                || args.allow_errors
            {
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

fn load_input(input: &Input) -> SimpleFile<String, String> {
    let mut source = String::new();

    let name = match input {
        Input::StdIn => {
            std::io::stdin().read_to_string(&mut source).unwrap();
            "<stdin>".to_owned()
        }
        Input::Path(path) => {
            let mut file = std::fs::File::open(path).unwrap(); // TODO: report errors
            file.read_to_string(&mut source).unwrap();
            path.to_string_lossy().into()
        }
    };

    SimpleFile::new(name, source)
}

fn parse_term<'arena>(
    interner: &mut StringInterner,
    arena: &'arena surface::Arena<'arena>,
    file: &SimpleFile<String, String>,
) -> surface::Term<'arena> {
    surface::Term::parse(interner, arena, file.source()).unwrap() // TODO: report errors
}

fn check_elaboration(
    writer: &mut BufferedStandardStream,
    config: &codespan_reporting::term::Config,
    files: &SimpleFile<String, String>,
    interner: &StringInterner,
    context: &mut elaboration::Context<'_>,
) -> bool {
    let mut is_ok = true;

    for message in context.drain_messages() {
        let diagnostic = message.to_diagnostic(interner);

        codespan_reporting::term::emit(writer, config, files, &diagnostic).unwrap();
        writer.flush().unwrap();

        is_ok &= diagnostic.severity < Severity::Error;
    }

    is_ok
}
