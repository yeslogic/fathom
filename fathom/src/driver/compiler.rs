use codespan_reporting::files::{Files, SimpleFiles};
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{BufferWriter, ColorChoice, StandardStream};
use std::env;
use std::fs;
use std::path::PathBuf;

use crate::lang;
use crate::pass::{core_to_pretty, core_to_surface, surface_to_core, surface_to_doc};
use crate::reporting;

lazy_static::lazy_static! {
    static ref INPUT_DIR: PathBuf = env::current_dir().unwrap(); // FIXME
    static ref GLOBALS: lang::core::Globals = lang::core::Globals::default();
}

pub fn run_compiler(module_name: &str, fathom_path: &PathBuf) -> lang::core::Module {
    let mut files = SimpleFiles::new();
    let mut compiler = Compiler::setup(
        &mut files,
        module_name,
        fathom_path.clone().into_os_string().to_str().unwrap(),
    );

    // Run stages

    eprintln!("trying to compile");

    let surface_module = compiler.parse_surface(&files);
    // compiler.compile_doc(&surface_module);
    let core_module = compiler.elaborate(&files, &surface_module);
    // compiler.roundtrip_surface_to_core(&files, &core_module);
    // compiler.roundtrip_pretty_core(&mut files, &core_module);
    // compiler.binary_parse_tests();

    // compiler.finish(&files);
    //todo return diagnostics
    core_module
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
    surface_module: Option<lang::surface::Module>,
    core_module: Option<lang::core::Module>,
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
            surface_module: None,
            core_module: None,
        }
    }

    pub fn parse_surface(&mut self, files: &SimpleFiles<String, String>) -> lang::surface::Module {
        let file_id = self.input_fathom_file_id;
        let source = files.source(file_id).unwrap();
        let surface_module =
            lang::surface::Module::parse(file_id, source, &mut self.found_messages);
        self.surface_module = Some(surface_module.clone());

        return surface_module;
    }

    pub fn elaborate(
        &mut self,
        files: &SimpleFiles<String, String>,
        surface_module: &lang::surface::Module,
    ) -> lang::core::Module {
        let mut context = surface_to_core::Context::new(&GLOBALS);
        let core_module = context.from_module(&surface_module);
        self.found_messages.extend(context.drain_messages());

        // The core syntax from the elaborator should always be well-formed!
        let mut context = lang::core::typing::Context::new(&GLOBALS);
        context.is_module(&core_module);
        let validation_messages = context.drain_messages().collect::<Vec<_>>();

        if !validation_messages.is_empty() {
            let pretty_arena = pretty::Arena::new();
            let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();

            for message in &validation_messages {
                let diagnostic = message.to_diagnostic(&pretty_arena);
                term::emit(&mut buffer, &self.term_config, files, &diagnostic).unwrap();
            }

            eprintln!("  • elaborate: validate");
            eprintln!();
            eprintln_indented(4, "", "---- found diagnostics ----");
            eprintln_indented(4, "| ", &String::from_utf8_lossy(buffer.as_slice()));
            eprintln!();
        }
        self.core_module = Some(core_module.clone());

        core_module
    }
}

fn eprintln_indented(indent: usize, prefix: &str, output: &str) {
    for line in output.lines() {
        eprintln!(
            "{space: >indent$}{prefix}{line}",
            space = "",
            indent = indent,
            prefix = prefix,
            line = line,
        );
    }
}
