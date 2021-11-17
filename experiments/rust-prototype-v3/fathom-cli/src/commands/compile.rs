use std::path::PathBuf;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
pub struct Options {
    /// The Fathom format file to use when reading
    #[structopt(long = "format-file", name = "FORMAT-PATH")]
    format_file: PathBuf, // TODO: specify formats by name, eg. 'opentype'
    /// Checks that the core module is well-formed after elaboration
    #[structopt(long = "validate-core")]
    validate_core: bool,
    /// Target to generate
    #[structopt(
        long = "target",
        name = "TARGET",
        case_insensitive = true,
        possible_values = &["rust"],
        parse(try_from_str = parse_target),
    )]
    target: Target,
}

#[derive(StructOpt, Debug)]
enum Target {
    Rust,
}

fn parse_target(src: &str) -> Result<Target, &'static str> {
    match () {
        () if src.eq_ignore_ascii_case("rust") => Ok(Target::Rust),
        () => Err("valid values: rust"),
    }
}

pub fn run(_options: &crate::Options, command_options: &Options) -> anyhow::Result<()> {
    match command_options.target {
        Target::Rust => Err(anyhow::anyhow!("error: not yet implemented")),
    }
}
