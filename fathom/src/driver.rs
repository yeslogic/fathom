use codespan_reporting::diagnostic::{Diagnostic, Severity};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{BufferedStandardStream, ColorChoice, WriteColor};
use std::cell::RefCell;
use std::io::Read;
use std::path::Path;

use crate::core::binary;
use crate::source::{ByteRange, FileId};
use crate::surface::{self, elaboration};
use crate::StringInterner;

#[derive(Debug, Copy, Clone)]
pub enum Status {
    Ok,
    Error,
}

impl Status {
    pub fn exit_code(self) -> i32 {
        match self {
            Status::Ok => 0,
            Status::Error => 1,
        }
    }
}

pub struct Driver<'surface, 'core> {
    files: SimpleFiles<String, String>,
    interner: RefCell<StringInterner>,
    surface_scope: scoped_arena::Scope<'surface>,
    core_scope: scoped_arena::Scope<'core>,

    allow_errors: bool,
    codespan_config: codespan_reporting::term::Config,
    diagnostic_writer: RefCell<Box<dyn WriteColor>>,

    emit_width: usize,
    emit_writer: RefCell<Box<dyn WriteColor>>,
}

impl<'surface, 'core> Driver<'surface, 'core> {
    pub fn new() -> Driver<'surface, 'core> {
        Driver {
            interner: RefCell::new(StringInterner::new()),
            surface_scope: scoped_arena::Scope::new(),
            core_scope: scoped_arena::Scope::new(),
            files: SimpleFiles::new(),

            allow_errors: false,
            codespan_config: codespan_reporting::term::Config::default(),
            diagnostic_writer: RefCell::new(Box::new(BufferedStandardStream::stderr(
                if atty::is(atty::Stream::Stderr) {
                    ColorChoice::Auto
                } else {
                    ColorChoice::Never
                },
            ))),

            emit_width: usize::MAX,
            emit_writer: RefCell::new(Box::new(BufferedStandardStream::stdout(
                if atty::is(atty::Stream::Stdout) {
                    ColorChoice::Auto
                } else {
                    ColorChoice::Never
                },
            ))),
        }
    }

    /// Setup a global panic hook
    pub fn install_panic_hook(&self) {
        use crate::core::semantics;

        const BUG_REPORT_URL: &str = concat!(env!("CARGO_PKG_REPOSITORY"), "/issues/new");

        // Use the currently set codespan configuration
        let term_config = self.codespan_config.clone();
        // Fetch the default hook (which prints the panic message and an optional backtrace)
        let default_hook = std::panic::take_hook();

        std::panic::set_hook(Box::new(move |info| {
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

            let mut writer = BufferedStandardStream::stderr(if atty::is(atty::Stream::Stderr) {
                ColorChoice::Auto
            } else {
                ColorChoice::Never
            });
            let dummy_files = SimpleFiles::<String, String>::new();

            default_hook(info);
            eprintln!();
            codespan_reporting::term::emit(&mut writer, &term_config, &dummy_files, &diagnostic)
                .unwrap();
        }));
    }

    /// Set to true if we should attempt to continue after encountering errors
    pub fn set_allow_errors(&mut self, allow_errors: bool) {
        self.allow_errors = allow_errors;
    }

    /// Set the writer to use when rendering diagnostics
    pub fn set_diagnostic_writer(&mut self, stream: impl 'static + WriteColor) {
        self.diagnostic_writer = RefCell::new(Box::new(stream) as Box<dyn WriteColor>);
    }

    /// Set the width to use when emitting data and intermediate languages
    pub fn set_emit_width(&mut self, emit_width: usize) {
        self.emit_width = emit_width;
    }

    /// Set the writer to use when emitting data and intermediate languages
    pub fn set_emit_writer(&mut self, stream: impl 'static + WriteColor) {
        self.emit_writer = RefCell::new(Box::new(stream) as Box<dyn WriteColor>);
    }

    /// Read a source file using a reader.
    pub fn read_source(&mut self, name: &str, mut reader: impl Read) -> FileId {
        // TODO: render diagnostics
        let mut source = String::new();
        reader.read_to_string(&mut source).unwrap();
        self.files.add(name.to_owned(), source)
    }

    /// Read a source file at the given path.
    pub fn read_source_path(&mut self, path: &Path) -> FileId {
        // TODO: render diagnostics
        let source = std::fs::read_to_string(path).unwrap();
        self.files.add(path.display().to_string(), source)
    }

    pub fn elaborate(&mut self, file_id: FileId) -> Status {
        let (surface_term, parse_diagnostics) = self.parse_term(file_id);
        let err_scope = scoped_arena::Scope::new();
        let mut context = elaboration::Context::new(&self.interner, &self.core_scope, &err_scope);
        let (term, r#type) = context.synth(&surface_term);
        let r#type = context.quote_context(&self.core_scope).quote(&r#type);

        let diagnostics = {
            let elab_messages = context.drain_messages();
            parse_diagnostics.chain(elab_messages.map(|m| m.to_diagnostic(&self.interner, file_id)))
        };

        if !(self.emit_diagnostics(diagnostics) || self.allow_errors) {
            return Status::Error;
        }

        self.surface_scope.reset(); // Reuse the surface scope for distillation
        let mut context = context.distillation_context(&self.surface_scope);
        let term = context.check(&term);
        let r#type = context.check(&r#type);

        self.emit_term(&surface::Term::Ann((), &term, &r#type));

        Status::Ok
    }

    pub fn normalise(&mut self, file_id: FileId) -> Status {
        let (surface_term, parse_diagnostics) = self.parse_term(file_id);
        let err_scope = scoped_arena::Scope::new();
        let mut context = elaboration::Context::new(&self.interner, &self.core_scope, &err_scope);
        let (term, r#type) = context.synth(&surface_term);
        let term = context.eval_context().normalise(&self.core_scope, &term);
        let r#type = context.quote_context(&self.core_scope).quote(&r#type);

        let diagnostics = {
            let elab_messages = context.drain_messages();
            parse_diagnostics.chain(elab_messages.map(|m| m.to_diagnostic(&self.interner, file_id)))
        };

        if !(self.emit_diagnostics(diagnostics) || self.allow_errors) {
            return Status::Error;
        }

        self.surface_scope.reset(); // Reuse the surface scope for distillation
        let mut context = context.distillation_context(&self.surface_scope);
        let term = context.check(&term);
        let r#type = context.check(&r#type);

        self.emit_term(&surface::Term::Ann((), &term, &r#type));

        Status::Ok
    }

    pub fn r#type(&mut self, file_id: FileId) -> Status {
        let (surface_term, parse_diagnostics) = self.parse_term(file_id);
        let err_scope = scoped_arena::Scope::new();
        let mut context = elaboration::Context::new(&self.interner, &self.core_scope, &err_scope);
        let (_, r#type) = context.synth(&surface_term);
        let r#type = context.quote_context(&self.core_scope).quote(&r#type);

        let diagnostics = {
            let elab_messages = context.drain_messages();
            parse_diagnostics.chain(elab_messages.map(|m| m.to_diagnostic(&self.interner, file_id)))
        };

        if !(self.emit_diagnostics(diagnostics) || self.allow_errors) {
            return Status::Error;
        }

        self.surface_scope.reset(); // Reuse the surface scope for distillation
        let mut context = context.distillation_context(&self.surface_scope);
        let r#type = context.check(&r#type);

        self.emit_term(&r#type);

        Status::Ok
    }

    pub fn read_format<'data>(&mut self, file_id: FileId, buffer: binary::Buffer<'data>) -> Status {
        use itertools::Itertools;
        use pretty::DocAllocator;
        use std::sync::Arc;

        use crate::core::semantics::Value;
        use crate::core::Prim;

        let (surface_term, parse_diagnostics) = self.parse_term(file_id);
        let err_scope = scoped_arena::Scope::new();
        let mut context = elaboration::Context::new(&self.interner, &self.core_scope, &err_scope);
        let format = context.check(&surface_term, &Arc::new(Value::prim(Prim::FormatType, [])));

        let diagnostics = {
            let elab_messages = context.drain_messages();
            parse_diagnostics.chain(elab_messages.map(|m| m.to_diagnostic(&self.interner, file_id)))
        };

        if !(self.emit_diagnostics(diagnostics) || self.allow_errors) {
            return Status::Error;
        }

        let format = context.eval_context().eval(&format);
        let refs = context
            .binary_context()
            .read_entrypoint(buffer, format)
            .unwrap(); // TODO: render nicer errors

        for (pos, parsed_refs) in refs.into_iter().sorted_by_key(|(pos, _)| *pos) {
            self.surface_scope.reset(); // Reuse the surface scope for distillation

            let exprs = parsed_refs
                .iter()
                .map(|parsed_ref| {
                    let expr = context
                        .quote_context(&self.core_scope)
                        .quote(&parsed_ref.expr);
                    context
                        .distillation_context(&self.surface_scope)
                        .check(&expr)
                })
                .collect::<Vec<_>>();

            let context = surface::pretty::Context::new(&self.interner, &self.surface_scope);
            let pos = pos.to_string();
            let doc = context
                .concat([
                    context.text(&pos),
                    context.space(),
                    context.text("="),
                    context.space(),
                    context.sequence(
                        context.text("["),
                        exprs.iter().map(|expr| context.term(&expr)),
                        context.text(","),
                        context.text("]"),
                    ),
                ])
                .into_doc();

            self.emit_doc(doc);
        }

        Status::Ok
    }

    fn parse_term(
        &'surface self,
        file_id: FileId,
    ) -> (
        surface::Term<'surface, ByteRange>,
        impl Iterator<Item = Diagnostic<FileId>>,
    ) {
        let source = self.files.get(file_id).unwrap().source();
        let (term, messages) = surface::Term::parse(&self.interner, &self.surface_scope, source);
        let diagnostics = messages.into_iter().map(move |m| m.to_diagnostic(file_id));

        (term, diagnostics)
    }

    fn emit_term(&self, term: &surface::Term<'_, ()>) {
        let context = surface::pretty::Context::new(&self.interner, &self.surface_scope);
        self.emit_doc(context.term(term).into_doc());
    }

    fn emit_doc(&self, doc: pretty::RefDoc) {
        let mut emit_writer = self.emit_writer.borrow_mut();
        writeln!(emit_writer, "{}", doc.pretty(self.emit_width)).unwrap();
        emit_writer.flush().unwrap();
    }

    fn emit_diagnostics(&self, diagnostics: impl Iterator<Item = Diagnostic<FileId>>) -> bool {
        let mut is_ok = true;

        for diagnostic in diagnostics {
            let mut diagnostic_writer = self.diagnostic_writer.borrow_mut();
            codespan_reporting::term::emit(
                &mut *diagnostic_writer,
                &self.codespan_config,
                &self.files,
                &diagnostic,
            )
            .unwrap();
            diagnostic_writer.flush().unwrap();

            is_ok &= diagnostic.severity < Severity::Error;
        }

        is_ok
    }
}
