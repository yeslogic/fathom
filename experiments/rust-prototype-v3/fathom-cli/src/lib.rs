use codespan_reporting::term::termcolor::ColorChoice;
use fathom::driver::TermWidth;
use structopt::StructOpt;

mod commands;

/// Tools for working with Fathom binary format descriptions.
#[derive(StructOpt, Debug)]
pub struct Options {
    /// Configure coloring of output
    #[structopt(
        long = "color",
        name = "WHEN",
        default_value = "auto",
        case_insensitive = true,
        possible_values = &["auto", "always", "ansi", "never"],
        parse(try_from_str = parse_color_choice),
    )]
    color: ColorChoice,
    /// The width of terminal to use when wrapping diagnostic output
    #[structopt(
        long = "term-width",
        name = "WIDTH",
        default_value = "auto",
        case_insensitive = true,
        possible_values = &["auto", "none", "<number>"],
        parse(try_from_str = parse_term_width),
    )]
    term_width: TermWidth,
    #[structopt(subcommand)]
    command: Command,
}

// TODO: repl: interactive mode for Fathom formats
// TODO: language-server: LSP-based editor integration
#[derive(StructOpt, Debug)]
enum Command {
    /// Manipulate binary data
    #[structopt(name = "data")]
    Data(commands::data::Options),
    /// Compile a binary format for a given target
    #[structopt(name = "compile")]
    Compile(commands::compile::Options),
    /// Check binary formats are valid
    #[structopt(name = "check")]
    Check(commands::check::Options),
    /// Generate documentation for binary formats
    #[structopt(name = "doc")]
    Doc(commands::doc::Options),
}

fn parse_color_choice(src: &str) -> Result<ColorChoice, &'static str> {
    match () {
        () if src.eq_ignore_ascii_case("auto") => Ok(ColorChoice::Auto),
        () if src.eq_ignore_ascii_case("always") => Ok(ColorChoice::Always),
        () if src.eq_ignore_ascii_case("ansi") => Ok(ColorChoice::AlwaysAnsi),
        () if src.eq_ignore_ascii_case("never") => Ok(ColorChoice::Never),
        () => Err("valid values: auto, always, ansi, never"),
    }
}

fn parse_term_width(src: &str) -> Result<TermWidth, &'static str> {
    match () {
        () if src.eq_ignore_ascii_case("auto") => Ok(TermWidth::Auto),
        () if src.eq_ignore_ascii_case("none") => Ok(TermWidth::None),
        () => match src.parse() {
            Ok(number) => Ok(TermWidth::Explicit(number)),
            Err(_) => Err("valid values: auto, none, <number>"),
        },
    }
}

pub fn run(options: Options) -> anyhow::Result<()> {
    match &options.command {
        Command::Data(command_options) => commands::data::run(&options, command_options),
        Command::Compile(command_options) => commands::compile::run(&options, command_options),
        Command::Check(command_options) => commands::check::run(&options, command_options),
        Command::Doc(command_options) => commands::doc::run(&options, command_options),
    }
}
