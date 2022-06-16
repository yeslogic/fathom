use clap::Parser;
use std::path::PathBuf;

/// A language for declaratively specifying binary data formats
#[derive(Parser)]
#[clap(author, version, about)]
enum Options {
    /// Elaborate a term, printing the elaborated term and type
    Elab {
        /// Path to a file containing the surface term
        #[clap(long = "term", name = "FILE", default_value = "-")]
        term_file: PathOrStdin,
        /// Continue even if errors were encountered
        #[clap(long = "allow-errors")]
        allow_errors: bool,
    },
    /// Elaborate a term, printing its normal form and type
    Norm {
        /// Path to a file containing the surface term
        #[clap(long = "term", name = "FILE", default_value = "-")]
        term_file: PathOrStdin,
        /// Continue even if errors were encountered
        #[clap(long = "allow-errors")]
        allow_errors: bool,
    },
    /// Elaborate a term, printing its type
    Type {
        /// Path to a file containing the surface term
        #[clap(long = "term", name = "FILE", default_value = "-")]
        term_file: PathOrStdin,
        /// Continue even if errors were encountered
        #[clap(long = "allow-errors")]
        allow_errors: bool,
    },
    /// Manipulate binary data
    Data {
        /// Path to a file containing the surface term
        #[clap(long = "format", name = "FILE")]
        format_file: PathOrStdin,
        /// Continue even if errors were encountered
        #[clap(long = "allow-errors")]
        allow_errors: bool,
        /// The binary file to read
        #[clap(name = "BINARY")]
        binary_path: PathBuf, // TODO: parse multiple binary files?
    },
}

enum PathOrStdin {
    StdIn,
    Path(PathBuf),
}

impl std::str::FromStr for PathOrStdin {
    type Err = &'static str; // Unused, but satisfies `clap`. Could be `!` in the future.

    fn from_str(src: &str) -> Result<PathOrStdin, &'static str> {
        match src {
            "-" => Ok(PathOrStdin::StdIn),
            _ => Ok(PathOrStdin::Path(PathBuf::from(src))),
        }
    }
}

fn read_source(driver: &mut fathom::Driver, file: PathOrStdin) -> fathom::source::FileId {
    match file {
        PathOrStdin::StdIn => driver.read_source("<stdin>", std::io::stdin()),
        PathOrStdin::Path(path) => driver.read_source_path(&path),
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
            term_file,
            allow_errors,
        } => {
            let mut driver = fathom::Driver::new();
            driver.install_panic_hook();
            driver.set_allow_errors(allow_errors);
            driver.set_emit_width(get_pretty_width());

            let file_id = read_source(&mut driver, term_file);
            let status = driver.elaborate(file_id);

            std::process::exit(status.exit_code());
        }
        Options::Norm {
            term_file,
            allow_errors,
        } => {
            let mut driver = fathom::Driver::new();
            driver.install_panic_hook();
            driver.set_allow_errors(allow_errors);
            driver.set_emit_width(get_pretty_width());

            let file_id = read_source(&mut driver, term_file);
            let status = driver.normalise(file_id);

            std::process::exit(status.exit_code());
        }
        Options::Type {
            term_file,
            allow_errors,
        } => {
            let mut driver = fathom::Driver::new();
            driver.install_panic_hook();
            driver.set_allow_errors(allow_errors);
            driver.set_emit_width(get_pretty_width());

            let file_id = read_source(&mut driver, term_file);
            let status = driver.r#type(file_id);

            std::process::exit(status.exit_code());
        }
        Options::Data {
            format_file,
            allow_errors,
            binary_path,
        } => {
            use std::io::Read;

            let mut driver = fathom::Driver::new();
            driver.install_panic_hook();
            driver.set_allow_errors(allow_errors);
            driver.set_emit_width(get_pretty_width());

            let file_id = read_source(&mut driver, format_file);

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
