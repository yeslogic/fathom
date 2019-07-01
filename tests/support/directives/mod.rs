use codespan::{FileId, LineIndex};
use codespan_reporting::Severity;
use regex::Regex;

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
