use codespan::{FileId, Files, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};

use super::{Directives, ExpectedDiagnostic, SpannedString, Token};

pub struct Parser<'a> {
    files: &'a Files<String>,
    file_id: FileId,
    directives: Directives,
    diagnostics: Vec<Diagnostic>,
}

impl<'a> Parser<'a> {
    pub fn new(files: &'a Files<String>, file_id: FileId) -> Parser<'a> {
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
                Ok((span, key, value)) => match (key.as_str(), value) {
                    ("SKIP", reason) => match reason {
                        None => self.diagnostics.push(Diagnostic::new_error(
                            "`SKIP` directive must have a reason",
                            self.label(span, "missing skip reason"),
                        )),
                        Some(_) if self.directives.skip.is_some() => {
                            self.duplicate_directive(&key);
                        }
                        Some(reason) => self.directives.skip = Some(reason.to_string()),
                    },
                    ("bug", pattern) => self.expect_bug(span, pattern),
                    ("error", pattern) => self.expect_error(span, pattern),
                    ("warning", pattern) => self.expect_warning(span, pattern),
                    ("note", pattern) => self.expect_note(span, pattern),
                    ("help", pattern) => self.expect_help(span, pattern),
                    (_, _) => self.diagnostics.push(
                        Diagnostic::new_error(
                            format!("unknown directive `{}`", key),
                            self.label(key.span(), "unknown directive"),
                        )
                        .with_notes(vec![unindent::unindent(
                            "
                                perhaps you meant:
                                    - SKIP:         <reason>
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

    fn duplicate_directive(&mut self, directive: &SpannedString) {
        self.diagnostics.push(Diagnostic::new_error(
            format!("`{}` directive already set", directive),
            self.label(directive.span(), "duplicate directive"),
        ));
    }

    fn expect_bug(&mut self, span: Span, pattern: Option<SpannedString>) {
        self.expect_diagnostic(span, Severity::Bug, pattern);
    }

    fn expect_error(&mut self, span: Span, pattern: Option<SpannedString>) {
        self.expect_diagnostic(span, Severity::Error, pattern);
    }

    fn expect_warning(&mut self, span: Span, pattern: Option<SpannedString>) {
        self.expect_diagnostic(span, Severity::Warning, pattern);
    }

    fn expect_note(&mut self, span: Span, pattern: Option<SpannedString>) {
        self.expect_diagnostic(span, Severity::Note, pattern);
    }

    fn expect_help(&mut self, span: Span, pattern: Option<SpannedString>) {
        self.expect_diagnostic(span, Severity::Help, pattern);
    }

    fn expect_diagnostic(
        &mut self,
        span: Span,
        severity: Severity,
        pattern: Option<SpannedString>,
    ) {
        use regex::Regex;
        use std::str::FromStr;

        let location = self
            .files
            .location(self.file_id, span.start())
            .expect("diagnostic_location");

        let pattern = match pattern {
            None => Ok(Regex::from_str(".*").unwrap()),
            Some(pattern) => {
                Regex::from_str(pattern.as_str()).map_err(|error| (pattern.span(), error))
            }
        };

        match pattern {
            Ok(pattern) => {
                self.directives
                    .expected_diagnostics
                    .push(ExpectedDiagnostic {
                        file_id: self.file_id,
                        line: location.line,
                        severity,
                        pattern,
                    });
            }
            Err((pattern_span, error)) => {
                self.diagnostics.push(
                    Diagnostic::new_error(
                        "failed to compile regex",
                        self.label(pattern_span, "invalid regex"),
                    )
                    .with_notes(vec![error.to_string()]),
                );
            }
        }
    }
}
