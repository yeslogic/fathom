use codespan::{ByteIndex, ByteOffset, FileId, LineIndex, Span};
use codespan_reporting::diagnostic::Severity;
use regex::Regex;
use std::fmt;

mod lexer;
mod parser;

pub use self::lexer::{Lexer, Token};
pub use self::parser::Parser;

/// The test directives collected for a source file.
#[derive(Clone, Debug)]
pub struct Directives {
    /// Skip directive, if it exists.
    ///
    /// ```text
    /// //~ SKIP: reason
    /// ```
    pub skip: Option<String>,
    /// Diagnostic directives:
    ///
    /// ```text
    /// //~ bug: regex
    /// //~ error: regex
    /// //~ warning: regex
    /// //~ note: regex
    /// //~ help: regex
    /// ```
    pub expected_diagnostics: Vec<ExpectedDiagnostic>,
}

impl Default for Directives {
    fn default() -> Directives {
        Directives {
            skip: None,
            expected_diagnostics: Vec::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExpectedDiagnostic {
    pub file_id: FileId,
    pub line: LineIndex,
    pub severity: Severity,
    pub pattern: Regex,
}

/// A string that is located in a source file.
#[derive(Debug, Clone)]
pub struct SpannedString {
    pub start: ByteIndex,
    pub inner: String,
}

impl SpannedString {
    pub fn new(start: impl Into<ByteIndex>, inner: impl Into<String>) -> SpannedString {
        SpannedString {
            start: start.into(),
            inner: inner.into(),
        }
    }

    pub fn span(&self) -> Span {
        Span::new(
            self.start,
            self.start + ByteOffset::from_str_len(&self.inner),
        )
    }

    pub fn as_str(&self) -> &str {
        &self.inner
    }
}

impl fmt::Display for SpannedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}
