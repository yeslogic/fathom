use codespan::{FileId, Files, Span};
use codespan_reporting::{Diagnostic, Label, Severity};

use super::{Directives, ExpectedDiagnostic, Status, Token};

pub struct Parser<'a> {
    files: &'a Files,
    file_id: FileId,
    directives: Directives,
    diagnostics: Vec<Diagnostic>,
}

impl<'a> Parser<'a> {
    pub fn new(files: &'a Files, file_id: FileId) -> Parser<'a> {
        Parser {
            files,
            file_id,
            directives: Directives::default(),
            diagnostics: Vec::new(),
        }
    }

    pub fn expect_directives(&mut self, tokens: impl Iterator<Item = Result<Token, Diagnostic>>) {
        for directive in tokens {
            match directive {
                Ok((span, key, value)) => match (key.as_ref(), value) {
                    ("SKIP", reason) => match reason {
                        None => self.diagnostics.push(Diagnostic::new_error(
                            "`SKIP` directive must have a reason",
                            self.label(span, "missing skip reason"),
                        )),
                        Some(_) if self.directives.skip.is_some() => {
                            self.duplicate_directive(span, "SKIP");
                        }
                        Some(reason) => self.directives.skip = Some(reason),
                    },
                    ("PARSE", status) => self.expect_parse_stage(span, status),
                    ("COMPILE/RUST", status) => self.expect_compile_rust_stage(span, status),
                    ("COMPILE/DOC", status) => self.expect_compile_doc_stage(span, status),
                    ("bug", pattern) => self.expect_bug(span, pattern),
                    ("error", pattern) => self.expect_error(span, pattern),
                    ("warning", pattern) => self.expect_warning(span, pattern),
                    ("note", pattern) => self.expect_note(span, pattern),
                    ("help", pattern) => self.expect_help(span, pattern),
                    (_, _) => self.diagnostics.push(
                        Diagnostic::new_error(
                            format!("unknown directive `{}`", key),
                            self.label(span, "unknown directive"),
                        )
                        .with_notes(vec![unindent::unindent(
                            "
                                perhaps you meant:
                                    - SKIP:         <reason>
                                    - PARSE:        (ok|fail)
                                    - COMPILE/RUST: (ok|fail)
                                    - COMPILE/DOC:  (ok|fail)
                                    - bug:          <regex>
                                    - error:        <regex>
                                    - warning:      <regex>
                                    - note:         <regex>
                                    - help:         <regex>
                            ",
                        )]),
                    ),
                },
                Err(diagnostic) => self.diagnostics.push(diagnostic),
            }
        }
    }

    pub fn finish(self) -> (Directives, Vec<Diagnostic>) {
        (self.directives, self.diagnostics)
    }

    fn label(&self, span: impl Into<Span>, message: impl Into<String>) -> Label {
        Label::new(self.file_id, span, message)
    }

    fn duplicate_directive(&mut self, span: Span, directive: &str) {
        self.diagnostics.push(Diagnostic::new_error(
            format!("`{}` directive already set", directive),
            self.label(span, "duplicate directive"),
        ));
    }

    fn expect_parse_stage(&mut self, span: Span, status: Option<String>) {
        if self.directives.parse.is_some() {
            self.duplicate_directive(span, "PARSE");
        } else if let Some(status) = self.expect_stage_status(span, status) {
            self.directives.parse = Some(status);
        }
    }

    fn expect_compile_rust_stage(&mut self, span: Span, status: Option<String>) {
        if self.directives.compile_rust.is_some() {
            self.duplicate_directive(span, "COMPILE/RUST");
        } else if let Some(status) = self.expect_stage_status(span, status) {
            self.directives.compile_rust = Some(status);
        }
    }

    fn expect_compile_doc_stage(&mut self, span: Span, status: Option<String>) {
        if self.directives.compile_doc.is_some() {
            self.duplicate_directive(span, "COMPILE/DOC");
        } else if let Some(status) = self.expect_stage_status(span, status) {
            self.directives.compile_doc = Some(status);
        }
    }

    fn expect_stage_status(&mut self, span: Span, status: Option<String>) -> Option<Status> {
        match status.as_ref().map(String::as_ref) {
            Some("ok") => return Some(Status::Ok),
            Some("fail") => return Some(Status::Fail),
            Some(status) => self.diagnostics.push(
                Diagnostic::new_error(
                    format!("unknown expected status `{}`", status),
                    self.label(span, "unknown expected status"),
                )
                .with_notes(vec![unindent::unindent(
                    "
                        perhaps you meant:
                            - ok
                            - fail
                    ",
                )]),
            ),
            None => self.diagnostics.push(
                Diagnostic::new_error(
                    "stage directive must have an expected status",
                    self.label(span, "missing stage status"),
                )
                .with_notes(vec![unindent::unindent(
                    "
                        perhaps you meant:
                            - ok
                            - fail
                    ",
                )]),
            ),
        }
        None
    }

    fn expect_bug(&mut self, span: Span, pattern: Option<String>) {
        self.expect_diagnostic(span, Severity::Bug, pattern);
    }

    fn expect_error(&mut self, span: Span, pattern: Option<String>) {
        self.expect_diagnostic(span, Severity::Error, pattern);
    }

    fn expect_warning(&mut self, span: Span, pattern: Option<String>) {
        self.expect_diagnostic(span, Severity::Warning, pattern);
    }

    fn expect_note(&mut self, span: Span, pattern: Option<String>) {
        self.expect_diagnostic(span, Severity::Note, pattern);
    }

    fn expect_help(&mut self, span: Span, pattern: Option<String>) {
        self.expect_diagnostic(span, Severity::Help, pattern);
    }

    fn expect_diagnostic(&mut self, span: Span, severity: Severity, pattern: Option<String>) {
        use regex::Regex;
        use std::str::FromStr;

        let location = self
            .files
            .location(self.file_id, span.start())
            .expect("diagnostic_location");

        let pattern = match pattern {
            None => Regex::from_str(".*"),
            Some(pattern) => Regex::from_str(&pattern),
        };

        match pattern {
            Ok(pattern) => {
                self.directives
                    .expected_diagnostics
                    .push(ExpectedDiagnostic {
                        line: location.line,
                        severity,
                        pattern,
                    });
            }
            Err(error) => {
                self.diagnostics.push(
                    Diagnostic::new_error(
                        "failed to compile regex",
                        self.label(span, "invalid regex"),
                    )
                    .with_notes(vec![error.to_string()]),
                );
            }
        }
    }
}
