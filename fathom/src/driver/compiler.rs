use codespan_reporting::files::{Files, SimpleFiles};
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{BufferWriter, ColorChoice, StandardStream};
use std::env;
use std::fs;
use std::path::PathBuf;

use crate::pass::{core_to_pretty, core_to_surface, surface_to_core, surface_to_doc};
use crate::reporting;

lazy_static::lazy_static! {
    static ref INPUT_DIR: PathBuf = env::current_dir().unwrap(); // FIXME
}

pub fn run_compiler(module_name: &str, fathom_path: &PathBuf) {
    let mut files = SimpleFiles::new();
    let mut compiler = Compiler::setup(
        &mut files,
        module_name,
        fathom_path.clone().into_os_string().to_str().unwrap(),
    );

    // Run stages

    eprintln!("trying to compile");

    // let surface_module = compiler.parse_surface(&files);
    // compiler.compile_doc(&surface_module);
    // let core_module = compiler.elaborate(&files, &surface_module);
    // compiler.roundtrip_surface_to_core(&files, &core_module);
    // compiler.roundtrip_pretty_core(&mut files, &core_module);
    // compiler.binary_parse_tests();

    // compiler.finish(&files);
}

pub struct Compiler {
    module_name: String,
    term_config: codespan_reporting::term::Config,
    input_fathom_path: PathBuf,
    input_fathom_file_id: usize,
    // snapshot_filename: PathBuf,
    // directives: directives::Directives,
    // failed_checks: Vec<&'static str>,
    found_messages: Vec<reporting::Message>,
}

impl Compiler {
    pub fn setup(
        files: &mut SimpleFiles<String, String>,
        module_name: &str,
        fathom_path: &str,
    ) -> Compiler {
        // Set up output streams

        let term_config = term::Config::default();
        let stdout = StandardStream::stdout(ColorChoice::Auto);

        // Set up files

        let input_fathom_path = INPUT_DIR.join(fathom_path);
        // let snapshot_filename = SNAPSHOTS_DIR.join(fathom_path).with_extension("");
        let source = fs::read_to_string(&input_fathom_path).unwrap_or_else(|error| {
            panic!("error reading `{}`: {}", input_fathom_path.display(), error)
        });
        let input_fathom_file_id = files.add(input_fathom_path.display().to_string(), source);

        Compiler {
            module_name: module_name.to_owned(),
            term_config,
            input_fathom_path,
            input_fathom_file_id,
            // snapshot_filename,
            // directives,
            // failed_checks: Vec::new(),
            found_messages: Vec::new(),
        }
    }
}
