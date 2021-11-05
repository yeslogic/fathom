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
    /// Manipulate binary data
    Data {
        /// Path to a file containing the surface term (`-` to read from stdin)
        #[structopt(long = "format", name = "FILE", parse(from_str))]
        format_input: Input,
        /// Continue even if errors were encountered
        #[structopt(long = "allow-errors")]
        allow_errors: bool,
        /// The binary file to read (`-` to read from stdin)
        #[structopt(name = "BINARY", default_value = "-", parse(from_str))]
        binary_input: Input, // TODO: parse multiple binary files?
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

fn get_pretty_width() -> usize {
    let term_width = termsize::get().map_or(usize::MAX, |size| usize::from(size.cols));
    std::cmp::min(term_width, MAX_PRETTY_WIDTH)
}

fn main() -> ! {
    match Options::from_args() {
        Options::Elab {
            term_input,
            allow_errors,
        } => {
            let mut driver = fathom_minimal::Driver::new();
            driver.install_panic_hook();
            driver.set_allow_errors(allow_errors);
            driver.set_emit_width(get_pretty_width());

            let file_id = match term_input {
                Input::StdIn => driver.read_source("<stdin>", std::io::stdin()),
                Input::File(path) => driver.read_source_path(&path),
            };

            let status = driver.elaborate(file_id);

            std::process::exit(status.exit_code());
        }
        Options::Norm {
            term_input,
            allow_errors,
        } => {
            let mut driver = fathom_minimal::Driver::new();
            driver.install_panic_hook();
            driver.set_allow_errors(allow_errors);
            driver.set_emit_width(get_pretty_width());

            let file_id = match term_input {
                Input::StdIn => driver.read_source("<stdin>", std::io::stdin()),
                Input::File(path) => driver.read_source_path(&path),
            };

            let status = driver.normalise(file_id);

            std::process::exit(status.exit_code());
        }
        Options::Type {
            term_input,
            allow_errors,
        } => {
            let mut driver = fathom_minimal::Driver::new();
            driver.install_panic_hook();
            driver.set_allow_errors(allow_errors);
            driver.set_emit_width(get_pretty_width());

            let file_id = match term_input {
                Input::StdIn => driver.read_source("<stdin>", std::io::stdin()),
                Input::File(path) => driver.read_source_path(&path),
            };

            let status = driver.r#type(file_id);

            std::process::exit(status.exit_code());
        }
        Options::Data {
            format_input,
            allow_errors,
            binary_input,
        } => {
            let mut driver = fathom_minimal::Driver::new();
            driver.install_panic_hook();
            driver.set_allow_errors(allow_errors);
            driver.set_emit_width(get_pretty_width());

            let file_id = match format_input {
                Input::StdIn => driver.read_source("<stdin>", std::io::stdin()),
                Input::File(path) => driver.read_source_path(&path),
            };

            let status = match binary_input {
                Input::StdIn => driver.read_format(file_id, &mut std::io::stdin()),
                Input::File(path) => {
                    let mut reader = std::fs::File::open(path).unwrap(); // TODO: report errors
                    driver.read_format(file_id, &mut reader)
                }
            };

            std::process::exit(status.exit_code());
        }
    }
}
