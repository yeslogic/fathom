use clap::Parser;
use std::path::PathBuf;

/// A language for declaratively specifying binary data formats
#[derive(Parser)]
#[clap(author, version, about)]
enum Cli {
    /// Elaborate a Fathom module or term, printing the result to stdout
    Elab {
        /// Path to a module to elaborate
        #[clap(
            long = "module",
            name = "MODULE_FILE",
            group = "input",
            required_unless_present = "input",
            display_order = 0
        )]
        module_file: Option<PathOrStdin>,
        /// Path to a term to elaborate
        #[clap(
            long = "term",
            name = "TERM_FILE",
            group = "input",
            required_unless_present = "input",
            display_order = 1
        )]
        term_file: Option<PathOrStdin>,
        /// Continue even if errors were encountered
        #[clap(long = "allow-errors")]
        allow_errors: bool,
    },
    /// Normalise a Fathom term, printing its normal form and type
    Norm {
        /// Path to a term to normalise
        #[clap(long = "term", name = "TERM_FILE", display_order = 0)]
        term_file: PathOrStdin,
        /// Continue even if errors were encountered
        #[clap(long = "allow-errors")]
        allow_errors: bool,
    },
    /// Manipulate binary data based on a Fathom format
    #[clap(after_help = DATA_COMMAND_AFTER_HELP)]
    #[clap(after_long_help = DATA_COMMAND_AFTER_LONG_HELP)]
    Data {
        /// Path to a module to load when reading
        #[clap(long = "module", name = "MODULE_FILE", display_order = 0)]
        module_file: Option<PathOrStdin>,
        /// Format used when reading the binary data
        ///
        /// The term provided by `FORMAT` must be of type `Format`.
        ///
        /// Required unless `--module` is present.
        #[clap(
            long = "format",
            name = "FORMAT",
            default_value = "main",
            required_unless_present = "MODULE_FILE",
            display_order = 1
        )]
        format: String,
        /// Path to the binary data to read from
        #[clap(name = "BINARY_FILE")]
        binary_file: PathOrStdin,
        /// Continue even if errors were encountered
        #[clap(long = "allow-errors")]
        allow_errors: bool,
    },
    /// Generate code to read Fathom format
    Compile {
        /// Path to a module to load when reading
        #[clap(long = "module", name = "MODULE_FILE", display_order = 0)]
        module_file: Option<PathOrStdin>,
        /// Format used when reading the binary data
        ///
        /// The term provided by `FORMAT` must be of type `Format`.
        ///
        /// Required unless `--module` is present.
        #[clap(
            long = "format",
            name = "FORMAT",
            default_value = "main",
            required_unless_present = "MODULE_FILE",
            display_order = 1
        )]
        format: String,
    },
}

const DATA_COMMAND_AFTER_HELP: &str = "\
Examples:

  $ fathom data --format '{ magic <- u32be where u32_eq magic \"icns\" }' AppIcon.icns
  $ fathom data --module formats/opentype.fathom Monaco.ttf
  $ fathom data --module formats/icns.fathom --format header AppIcon.icns
";

const DATA_COMMAND_AFTER_LONG_HELP: &str = "\
Binary data can be read using a term supplied by the `--format` option:

  $ fathom data --format '{ magic <- u32be where u32_eq magic \"icns\" }' AppIcon.icns

Alternatively data can be read using a module:

  $ fathom data --module formats/opentype.fathom Monaco.ttf
  $ fathom data --module formats/stl-binary.fathom cube.stl

When a module is specified the binary data is read assuming that it contains a
`main` definition, but this can be overridden using the `--format` option:

  $ fathom data --module formats/icns.fathom --format header AppIcon.icns
";

#[derive(Clone, Debug)]
enum PathOrStdin {
    StdIn,
    Path(PathBuf),
}

impl std::str::FromStr for PathOrStdin {
    type Err = std::convert::Infallible;

    fn from_str(src: &str) -> Result<PathOrStdin, std::convert::Infallible> {
        match src {
            "-" => Ok(PathOrStdin::StdIn),
            _ => Ok(PathOrStdin::Path(PathBuf::from(src))),
        }
    }
}

fn unwrap_or_exit<T>(option: Option<T>) -> T {
    option.unwrap_or_else(|| std::process::exit(fathom::Status::Error.exit_code()))
}

fn load_file_or_exit(driver: &mut fathom::Driver, file: PathOrStdin) -> fathom::source::FileId {
    unwrap_or_exit(match file {
        PathOrStdin::StdIn => driver.load_source("<stdin>".to_owned(), std::io::stdin()),
        PathOrStdin::Path(path) => driver.load_source_path(&path),
    })
}

fn read_bytes_or_exit(driver: &mut fathom::Driver, file: PathOrStdin) -> Vec<u8> {
    unwrap_or_exit(match file {
        PathOrStdin::StdIn => driver.read_bytes("<stdio>".to_owned(), std::io::stdin()),
        PathOrStdin::Path(path) => driver.read_bytes_path(&path),
    })
}

const MAX_PRETTY_WIDTH: usize = 80;

fn get_pretty_width() -> usize {
    let term_width = termsize::get().map_or(usize::MAX, |size| usize::from(size.cols));
    std::cmp::min(term_width, MAX_PRETTY_WIDTH)
}

fn main() -> ! {
    match Cli::parse() {
        Cli::Elab {
            module_file,
            term_file,
            allow_errors,
        } => {
            let mut driver = fathom::Driver::new();
            driver.install_panic_hook();
            driver.set_allow_errors(allow_errors);
            driver.set_emit_width(get_pretty_width());

            let status = match (module_file, term_file) {
                (Some(module_file), None) => {
                    let file_id = load_file_or_exit(&mut driver, module_file);
                    driver.elaborate_and_emit_module(file_id)
                }
                (None, Some(term_file)) => {
                    let file_id = load_file_or_exit(&mut driver, term_file);
                    driver.elaborate_and_emit_term(file_id)
                }
                (Some(_), Some(_)) | (None, None) => {
                    unreachable!(r#"guarded by `required_unless_present = "input"`"#)
                }
            };

            std::process::exit(status.exit_code());
        }
        Cli::Norm {
            term_file,
            allow_errors,
        } => {
            let mut driver = fathom::Driver::new();
            driver.install_panic_hook();
            driver.set_allow_errors(allow_errors);
            driver.set_emit_width(get_pretty_width());

            let file_id = load_file_or_exit(&mut driver, term_file);
            let status = driver.normalise_and_emit_term(file_id);

            std::process::exit(status.exit_code());
        }
        Cli::Data {
            module_file,
            format,
            binary_file,
            allow_errors,
        } => {
            let mut driver = fathom::Driver::new();
            driver.install_panic_hook();
            driver.set_allow_errors(allow_errors);
            driver.set_emit_width(get_pretty_width());

            let module_file_id = module_file.map(|input| load_file_or_exit(&mut driver, input));
            let format_file_id = driver.load_source_string("<FORMAT>".to_owned(), format);

            let data = read_bytes_or_exit(&mut driver, binary_file);
            let status = driver.read_and_emit_format(module_file_id, format_file_id, &data);

            std::process::exit(status.exit_code());
        }
        Cli::Compile {
            module_file,
            format,
        } => {
            let mut driver = fathom::Driver::new();
            driver.install_panic_hook();
            driver.set_emit_width(get_pretty_width());

            let module_file_id = module_file.map(|input| load_file_or_exit(&mut driver, input));
            let format_file_id = driver.load_source_string("<FORMAT>".to_owned(), format);

            let status = driver.compile_and_emit_format(module_file_id, format_file_id);

            std::process::exit(status.exit_code());
        }
    }
}
