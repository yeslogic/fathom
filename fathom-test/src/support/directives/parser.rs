use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
use codespan_reporting::files::{Files, SimpleFiles};
use std::ops::Range;

use super::{Directives, ExpectedDiagnostic, SpannedString, Token};

pub struct Parser<'a> {
    files: &'a SimpleFiles<String, String>,
    file_id: usize,
    directives: Directives,
    diagnostics: Vec<Diagnostic<usize>>,
}

impl<'a> Parser<'a> {
    pub fn new(files: &'a SimpleFiles<String, String>, file_id: usize) -> Parser<'a> {
        Parser {
            files,
            file_id,
            directives: Directives::default(),
            diagnostics: Vec::new(),
        }
    }

    pub fn expect_directives(
        &mut self,
        tokens: impl Iterator<Item = Result<Token, Diagnostic<usize>>>,
    ) {
        for directive in tokens {
            match directive {
                Ok((range, key, value)) => match (key.as_str(), value) {
                    ("SKIP", reason) => match reason {
                        None => self.diagnostics.push(
                            Diagnostic::error()
                                .with_message("`SKIP` directive must have a reason")
                                .with_labels(vec![self.label(range, "missing skip reason")]),
                        ),
                        Some(_) if self.directives.skip.is_some() => {
                            self.duplicate_directive(&key);
                        }
                        Some(reason) => self.directives.skip = Some(reason.to_string()),
                    },
                    ("bug", pattern) => self.expect_bug(range, pattern),
                    ("error", pattern) => self.expect_error(range, pattern),
                    ("warning", pattern) => self.expect_warning(range, pattern),
                    ("note", pattern) => self.expect_note(range, pattern),
                    ("help", pattern) => self.expect_help(range, pattern),
                    (_, _) => self.diagnostics.push(
                        Diagnostic::error()
                            .with_message(format!("unknown directive `{}`", key))
                            .with_labels(vec![self.label(key.range(), "unknown directive")])
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

    pub fn finish(self) -> (Directives, Vec<Diagnostic<usize>>) {
        (self.directives, self.diagnostics)
    }

    fn label(&self, range: Range<usize>, message: impl Into<String>) -> Label<usize> {
        Label::primary(self.file_id, range).with_message(message)
    }

    fn duplicate_directive(&mut self, directive: &SpannedString) {
        self.diagnostics.push(
            Diagnostic::error()
                .with_message(format!("`{}` directive already set", directive))
                .with_labels(vec![self.label(directive.range(), "duplicate directive")]),
        );
    }

    fn expect_bug(&mut self, range: Range<usize>, pattern: Option<SpannedString>) {
        self.expect_diagnostic(range, Severity::Bug, pattern);
    }

    fn expect_error(&mut self, range: Range<usize>, pattern: Option<SpannedString>) {
        self.expect_diagnostic(range, Severity::Error, pattern);
    }

    fn expect_warning(&mut self, range: Range<usize>, pattern: Option<SpannedString>) {
        self.expect_diagnostic(range, Severity::Warning, pattern);
    }

    fn expect_note(&mut self, range: Range<usize>, pattern: Option<SpannedString>) {
        self.expect_diagnostic(range, Severity::Note, pattern);
    }

    fn expect_help(&mut self, range: Range<usize>, pattern: Option<SpannedString>) {
        self.expect_diagnostic(range, Severity::Help, pattern);
    }

    fn expect_diagnostic(
        &mut self,
        range: Range<usize>,
        severity: Severity,
        pattern: Option<SpannedString>,
    ) {
        use regex::Regex;
        use std::str::FromStr;

        let pattern = match pattern {
            None => Ok(Regex::from_str(".*").unwrap()),
            Some(pattern) => {
                Regex::from_str(pattern.as_str()).map_err(|error| (pattern.range(), error))
            }
        };

        match pattern {
            Ok(pattern) => {
                self.directives
                    .expected_diagnostics
                    .push(ExpectedDiagnostic {
                        file_id: self.file_id,
                        line_index: self.files.line_index(self.file_id, range.start).unwrap(),
                        location: self.files.location(self.file_id, range.start).unwrap(),
                        severity,
                        pattern,
                    });
            }
            Err((pattern_range, error)) => {
                self.diagnostics.push(
                    Diagnostic::error()
                        .with_message("failed to compile regex")
                        .with_labels(vec![self.label(pattern_range, "invalid regex")])
                        .with_notes(vec![error.to_string()]),
                );
            }
        }
    }
}
