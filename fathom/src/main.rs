use clap::Parser;
use std::path::PathBuf;

/// CLI for the programming language prototype.
#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
#[clap(after_help = r#"EXAMPLES:

Using arguments

    fathom elab --term=examples/prelude.txt
    fathom norm --term=examples/prelude.txt

Using pipes and redirects

    echo "fun (A : Type) -> A -> A" | fathom elab
    cat examples/prelude.txt | fathom elab
    fathom elab < examples/prelude.txt

Using heredocs

    fathom elab <<< "fun (A : Type) -> A -> A"

    fathom norm <<EOF
        let id : fun (A : Type) -> A -> A
          = fun A => fun a => a;

        id Type Type
    EOF
"#)]
enum Options {
    /// Elaborate a term, printing the elaborated term and type
    Elab {
        /// Path to a file containing the surface term (`-` to read from stdin)
        #[clap(long = "term", name = "FILE", default_value = "-", parse(from_str))]
        term_input: Input,
        /// Continue even if errors were encountered.
        #[clap(long = "allow-errors")]
        allow_errors: bool,
    },
    /// Elaborate a term, printing its normal form and type
    Norm {
        /// Path to a file containing the surface term (`-` to read from stdin)
        #[clap(long = "term", name = "FILE", default_value = "-", parse(from_str))]
        term_input: Input,
        /// Continue even if errors were encountered.
        #[clap(long = "allow-errors")]
        allow_errors: bool,
    },
    /// Elaborate a term, printing its type
    Type {
        /// Path to a file containing the surface term (`-` to read from stdin)
        #[clap(long = "term", name = "FILE", default_value = "-", parse(from_str))]
        term_input: Input,
        /// Continue even if errors were encountered. v
        #[clap(long = "allow-errors")]
        allow_errors: bool,
    },
    /// Manipulate binary data
    Data {
        /// Path to a file containing the surface term (`-` to read from stdin)
        #[clap(long = "format", name = "FILE", parse(from_str))]
        format_input: Input,
        /// Continue even if errors were encountered
        #[clap(long = "allow-errors")]
        allow_errors: bool,
        /// The binary file to read
        #[clap(name = "BINARY", parse(from_str))]
        binary_path: PathBuf, // TODO: parse multiple binary files?
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
    match Options::parse() {
        Options::Elab {
            term_input,
            allow_errors,
        } => {
            let mut driver = fathom::Driver::new();
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
            let mut driver = fathom::Driver::new();
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
            let mut driver = fathom::Driver::new();
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
            binary_path,
        } => {
            use std::io::Read;

            let mut driver = fathom::Driver::new();
            driver.install_panic_hook();
            driver.set_allow_errors(allow_errors);
            driver.set_emit_width(get_pretty_width());

            let file_id = match format_input {
                Input::StdIn => driver.read_source("<stdin>", std::io::stdin()),
                Input::File(path) => driver.read_source_path(&path),
            };

            // TODO: report errors
            let mut file = std::fs::File::open(binary_path).unwrap();
            let mut data = Vec::new();
            file.read_to_end(&mut data).unwrap();
            let buffer = fathom::core::binary::Buffer::from(data.as_slice());

            let status = driver.read_format(file_id, buffer);

            std::process::exit(status.exit_code());
        }
    }
}
