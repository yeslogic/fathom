use codespan_reporting::diagnostic::{Diagnostic, Severity};
use codespan_reporting::files::{SimpleFile, SimpleFiles};
use codespan_reporting::term::termcolor::{BufferedStandardStream, ColorChoice};
use fathom_minimal::core::semantics;
use fathom_minimal::surface::elaboration;
use fathom_minimal::{surface, ByteRange, StringInterner};
use scoped_arena::Scope;
use std::cell::RefCell;
use std::io::{Read, Write};
use std::panic;
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
    /// Elaborate a term, printing the elaborated term and type
    Elab {
        /// Path to a file containing the surface term (`-` to read from stdin)
        #[structopt(long = "term", name = "FILE", default_value = "-", parse(from_str))]
        term_input: Input,
        /// Continue even if errors were encountered.
        #[structopt(long = "allow-errors")]
        allow_errors: bool,
    },
    /// Elaborate a term, printing its normal form and type
    Norm {
        /// Path to a file containing the surface term (`-` to read from stdin)
        #[structopt(long = "term", name = "FILE", default_value = "-", parse(from_str))]
        term_input: Input,
        /// Continue even if errors were encountered.
        #[structopt(long = "allow-errors")]
        allow_errors: bool,
    },
    /// Elaborate a term, printing its type
    Type {
        /// Path to a file containing the surface term (`-` to read from stdin)
        #[structopt(long = "term", name = "FILE", default_value = "-", parse(from_str))]
        term_input: Input,
        /// Continue even if errors were encountered.
        #[structopt(long = "allow-errors")]
        allow_errors: bool,
    },
}

enum Input {
    StdIn,
    File(PathBuf),
}

impl From<&str> for Input {
    fn from(src: &str) -> Input {
        match src {
            "-" => Input::StdIn,
            _ => Input::File(PathBuf::from(src)),
        }
    }
}

const MAX_PRETTY_WIDTH: usize = 80;

fn main() -> ! {
    install_panic_hook();

    let mut writer = BufferedStandardStream::stderr(ColorChoice::Auto);
    let term_config = codespan_reporting::term::Config::default();

    let interner = RefCell::new(StringInterner::new());
    let mut surface_scope = Scope::new(); // Short-term storage of surface terms
    let core_scope = Scope::new(); // Long-term storage of core terms

    match Options::from_args() {
        Options::Elab {
            term_input,
            allow_errors,
        } => {
            let file = load_input(&term_input);
            let surface_term = parse_term(&interner, &surface_scope, &file);

            let mut context = elaboration::Context::new(&interner, &core_scope);
            let (term, r#type) = context.synth(&surface_term);
            let r#type = context.quote_context(&core_scope).quote(&r#type);

            if check_elaboration(&mut writer, &term_config, &file, &interner, &mut context)
                || allow_errors
            {
                surface_scope.reset(); // Reuse the surface scope for distillation
                let mut context = context.distillation_context(&surface_scope);
                let term = context.check(&term);
                let r#type = context.check(&r#type);

                {
                    let context = surface::pretty::Context::new(&interner, &surface_scope);
                    let doc = context
                        .term(&surface::Term::Ann((), &term, &r#type))
                        .into_doc();

                    println!("{}", doc.pretty(get_pretty_width()));
                }

                std::process::exit(0);
            } else {
                std::process::exit(1);
            }
        }
        Options::Norm {
            term_input,
            allow_errors,
        } => {
            let file = load_input(&term_input);
            let surface_term = parse_term(&interner, &surface_scope, &file);

            let mut context = elaboration::Context::new(&interner, &core_scope);
            let (term, r#type) = context.synth(&surface_term);
            let term = context.eval_context().normalise(&core_scope, &term);
            let r#type = context.quote_context(&core_scope).quote(&r#type);

            if check_elaboration(&mut writer, &term_config, &file, &interner, &mut context)
                || allow_errors
            {
                surface_scope.reset(); // Reuse the surface scope for distillation
                let mut context = context.distillation_context(&surface_scope);
                let term = context.check(&term);
                let r#type = context.check(&r#type);

                {
                    let context = surface::pretty::Context::new(&interner, &surface_scope);
                    let doc = context
                        .term(&surface::Term::Ann((), &term, &r#type))
                        .into_doc();

                    println!("{}", doc.pretty(get_pretty_width()));
                }

                std::process::exit(0);
            } else {
                std::process::exit(1);
            }
        }
        Options::Type {
            term_input,
            allow_errors,
        } => {
            let file = load_input(&term_input);
            let surface_term = parse_term(&interner, &surface_scope, &file);

            let mut context = elaboration::Context::new(&interner, &core_scope);
            let (_, r#type) = context.synth(&surface_term);
            let r#type = context.quote_context(&core_scope).quote(&r#type);

            if check_elaboration(&mut writer, &term_config, &file, &interner, &mut context)
                || allow_errors
            {
                surface_scope.reset(); // Reuse the surface scope for distillation
                let mut context = context.distillation_context(&surface_scope);
                let r#type = context.check(&r#type);

                {
                    let context = surface::pretty::Context::new(&interner, &surface_scope);
                    let doc = context.term(&r#type).into_doc();

                    println!("{}", doc.pretty(get_pretty_width()));
                }

                std::process::exit(0);
            } else {
                std::process::exit(1);
            }
        }
    }
}

const BUG_REPORT_URL: &str = concat!(env!("CARGO_PKG_REPOSITORY"), "/issues/new");

fn install_panic_hook() {
    panic::set_hook(Box::new(move |info| {
        let location = info.location();
        let message = if let Some(error) = info.payload().downcast_ref::<semantics::Error>() {
            error.description()
        } else if let Some(message) = info.payload().downcast_ref::<String>() {
            message.as_str()
        } else if let Some(message) = info.payload().downcast_ref::<&str>() {
            message
        } else {
            "unknown panic type"
        };

        let diagnostic = Diagnostic::bug()
            .with_message(format!("compiler panicked at '{}'", message))
            .with_notes(vec![
                match location {
                    Some(location) => format!("panicked at: {}", location),
                    None => format!("panicked at: unknown location"),
                },
                format!("please file a bug report at: {}", BUG_REPORT_URL),
                // TODO: print rust backtrace
                // TODO: print fathom backtrace
            ]);

        let mut writer = BufferedStandardStream::stderr(ColorChoice::Auto);
        let term_config = codespan_reporting::term::Config::default();
        let dummy_files = SimpleFiles::<String, String>::new();

        codespan_reporting::term::emit(&mut writer, &term_config, &dummy_files, &diagnostic)
            .unwrap();
    }));
}

fn load_input(input: &Input) -> SimpleFile<String, String> {
    let mut source = String::new();

    let name = match input {
        Input::StdIn => {
            std::io::stdin().read_to_string(&mut source).unwrap();
            "<stdin>".to_owned()
        }
        Input::File(path) => {
            let mut file = std::fs::File::open(path).unwrap(); // TODO: report errors
            file.read_to_string(&mut source).unwrap();
            path.to_string_lossy().into()
        }
    };

    SimpleFile::new(name, source)
}

fn get_pretty_width() -> usize {
    let term_width = termsize::get().map_or(usize::MAX, |size| usize::from(size.cols));
    std::cmp::min(term_width, MAX_PRETTY_WIDTH)
}

fn parse_term<'arena>(
    interner: &RefCell<StringInterner>,
    scope: &'arena Scope<'arena>,
    file: &SimpleFile<String, String>,
) -> surface::Term<'arena, ByteRange> {
    surface::Term::parse(interner, scope, file.source()).unwrap() // TODO: report errors
}

fn check_elaboration(
    writer: &mut BufferedStandardStream,
    config: &codespan_reporting::term::Config,
    files: &SimpleFile<String, String>,
    interner: &RefCell<StringInterner>,
    context: &mut elaboration::Context<'_, '_>,
) -> bool {
    let mut is_ok = true;

    for message in context.drain_messages() {
        let diagnostic = message.to_diagnostic(&interner.borrow());

        codespan_reporting::term::emit(writer, config, files, &diagnostic).unwrap();
        writer.flush().unwrap();

        is_ok &= diagnostic.severity < Severity::Error;
    }

    is_ok
}
