use codespan_reporting::diagnostic::Severity;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{BufferedStandardStream, ColorChoice, WriteColor};
use std::fmt;
use std::io;
use std::io::Write;
use std::path::Path;

use crate::lang::{core, surface};
use crate::pass::{core_to_pretty, surface_to_core, surface_to_doc, surface_to_pretty};
use crate::reporting::Message;

lazy_static::lazy_static! {
    static ref GLOBALS: core::Globals = core::Globals::default();
}

/// The width of the terminal to use when printing diagnostics.
#[derive(Debug, Copy, Clone)]
pub enum TermWidth {
    /// Detect wrapping from the terminal width
    Auto,
    /// No wrapping
    None,
    /// Explicit terminal width
    Explicit(u16),
}

impl TermWidth {
    fn compute(self) -> usize {
        match self {
            TermWidth::Auto => termsize::get().map_or(usize::MAX, |size| usize::from(size.cols)),
            TermWidth::None => usize::MAX,
            TermWidth::Explicit(count) => usize::from(count),
        }
    }
}

/// Fathom compiler driver
pub struct Driver {
    validate_core: bool,
    emit_core: bool,
    emit_width: TermWidth,
    emit_writer: Box<dyn WriteColor>,
    codespan_config: codespan_reporting::term::Config,
    diagnostic_writer: Box<dyn WriteColor>,

    files: SimpleFiles<String, String>,
    surface_to_core: surface_to_core::Context<'static>,
    surface_to_doc: surface_to_doc::Context,
    core_typing: core::typing::Context<'static>,
    messages: Vec<Message>,
}

impl Driver {
    /// Create a new compiler driver
    pub fn new() -> Driver {
        Driver {
            validate_core: false,
            emit_core: false,
            emit_width: TermWidth::Auto,
            emit_writer: Box::new(BufferedStandardStream::stdout(ColorChoice::Auto)),
            codespan_config: codespan_reporting::term::Config::default(),
            diagnostic_writer: Box::new(BufferedStandardStream::stderr(ColorChoice::Auto)),

            files: SimpleFiles::new(),
            surface_to_core: surface_to_core::Context::new(&GLOBALS),
            surface_to_doc: surface_to_doc::Context::new(),
            core_typing: core::typing::Context::new(&GLOBALS),
            messages: Vec::new(),
        }
    }

    /// Set to `true` to print the core language after elaboration.
    pub fn set_emit_core(&mut self, emit_core: bool) {
        self.emit_core = emit_core;
    }

    /// Set to `true` to validate the core language after elaboration.
    pub fn set_validate_core(&mut self, validate_core: bool) {
        self.validate_core = validate_core;
    }

    /// Set the width to use for printing diagnostics.
    pub fn set_emit_width(&mut self, emit_width: TermWidth) {
        self.emit_width = emit_width;
    }

    /// Set the writer to use when emitting data and intermediate languages
    pub fn set_emit_writer(&mut self, stream: impl 'static + WriteColor) {
        self.emit_writer = Box::new(stream) as Box<dyn WriteColor>;
    }

    /// Set the writer to use when rendering diagnostics
    pub fn set_diagnostic_writer(&mut self, stream: impl 'static + WriteColor) {
        self.diagnostic_writer = Box::new(stream) as Box<dyn WriteColor>;
    }

    /// Read a binary data file using a format module
    pub fn read_data(
        &mut self,
        format_path: &Path,
        item_name: &str,
        binary_path: &Path,
    ) -> Result<(), ReadDataError> {
        let surface_module = match self.add_source_file(format_path) {
            Some(file_id) => self.parse_surface_module(file_id),
            None => return Ok(()),
        };

        let core_module = self.surface_to_core_module(&surface_module);
        let mut core_binary_read = core::binary::read::Context::new(&GLOBALS, &core_module);

        // TODO: Avoid needing to read the buffer all at once
        let buffer = match std::fs::read(binary_path) {
            Ok(buffer) => buffer,
            Err(error) => {
                self.messages.push(Message::ReadFile {
                    path: binary_path.to_owned(),
                    error: error.to_string(),
                });
                return Ok(());
            }
        };

        // TODO: Force diagnostics to be rendered here?

        let read_scope = fathom_runtime::ReadScope::new(&buffer);
        // TODO: Make the reading of binary data more lazy
        let (main_value, links) =
            core_binary_read.read_item(&mut read_scope.reader(), item_name)?;

        let pretty_arena = pretty::Arena::new(); // TODO: reuse arenas
        let main_term = self.surface_to_core.read_back_to_surface(&main_value);
        let pretty::DocBuilder(_, doc) = surface_to_pretty::from_term(&pretty_arena, &main_term);

        write!(
            &mut self.emit_writer,
            "{name} = {term}",
            name = item_name,
            term = doc.pretty(self.emit_width.compute())
        )?;
        self.emit_writer.flush()?;

        for (link_pos, link_value) in links {
            let pretty_arena = pretty::Arena::new(); // TODO: reuse arenas
            let link_term = self.surface_to_core.read_back_to_surface(&link_value);
            let pretty::DocBuilder(_, doc) =
                surface_to_pretty::from_term(&pretty_arena, &link_term);

            write!(
                &mut self.emit_writer,
                "{pos} = {term}",
                pos = link_pos,
                term = doc.pretty(self.emit_width.compute())
            )?;
            self.emit_writer.flush()?;
        }

        Ok(())
    }

    /// Elaborate the surface language into the core language
    pub fn check(&mut self, format_path: &Path) -> Result<(), io::Error> {
        let surface_module = match self.add_source_file(format_path) {
            Some(file_id) => self.parse_surface_module(file_id),
            None => return Ok(()),
        };

        let core_module = self.surface_to_core_module(&surface_module);

        if self.emit_core {
            let pretty_arena = pretty::Arena::new();
            let pretty::DocBuilder(_, doc) =
                core_to_pretty::from_module(&pretty_arena, &core_module);
            let emit_width = self.emit_width.compute();
            write!(&mut self.emit_writer, "{}", doc.pretty(emit_width))?;
            self.emit_writer.flush()?;
        }

        Ok(())
    }

    /// Compile documentation for a format module
    pub fn write_doc(&mut self, format_path: &Path) -> Result<(), io::Error> {
        let surface_module = match self.add_source_file(format_path) {
            Some(file_id) => self.parse_surface_module(file_id),
            None => return Ok(()),
        };

        self.surface_to_doc
            .from_module(&mut io::stdout().lock(), &surface_module)?; // TODO: allow for writer to be customised?

        Ok(())
    }

    /// Write diagnostics to the diagnostics writer
    // TODO: stream diagnostics rather than having to wait util compilation completes
    pub fn check_diagnostics(&mut self) -> Result<bool, codespan_reporting::files::Error> {
        let pretty_arena = pretty::Arena::new();

        let mut is_ok = true;
        for message in &self.messages {
            let diagnostic = message.to_diagnostic(&pretty_arena);
            is_ok &= diagnostic.severity < Severity::Error;
            term::emit(
                &mut self.diagnostic_writer,
                &self.codespan_config,
                &self.files,
                &diagnostic,
            )?;
            self.diagnostic_writer.flush()?;
        }
        self.messages.clear();

        Ok(is_ok)
    }

    // Internals

    fn add_source_file(&mut self, path: &Path) -> Option<usize> {
        match std::fs::read_to_string(path) {
            Ok(source) => Some(self.files.add(path.display().to_string(), source)),
            Err(error) => {
                self.messages.push(Message::ReadFile {
                    path: path.to_owned(),
                    error: error.to_string(),
                });
                None
            }
        }
    }

    fn parse_surface_module(&mut self, file_id: usize) -> surface::Module {
        let file = self.files.get(file_id).unwrap();
        surface::Module::parse(file_id, file.source(), &mut self.messages)
    }

    fn surface_to_core_module(&mut self, surface_module: &surface::Module) -> core::Module {
        let core_module = self.surface_to_core.from_module(&surface_module);
        self.messages.extend(self.surface_to_core.drain_messages());

        if self.validate_core {
            self.core_typing.is_module(&core_module);
            self.messages.extend(self.core_typing.drain_messages());
        }

        core_module
    }
}

/// An error produced while reading binary data.
#[derive(Debug)]
pub enum ReadDataError {
    Io(io::Error),
    Read(fathom_runtime::ReadError),
}

impl fmt::Display for ReadDataError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ReadDataError::Io(error) => error.fmt(f),
            ReadDataError::Read(error) => error.fmt(f),
        }
    }
}

impl std::error::Error for ReadDataError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ReadDataError::Io(error) => Some(error),
            ReadDataError::Read(error) => Some(error),
        }
    }
}

impl From<io::Error> for ReadDataError {
    fn from(error: io::Error) -> ReadDataError {
        ReadDataError::Io(error)
    }
}

impl From<fathom_runtime::ReadError> for ReadDataError {
    fn from(error: fathom_runtime::ReadError) -> ReadDataError {
        ReadDataError::Read(error)
    }
}
