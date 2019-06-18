use codespan::{FileId, LineIndex};
use codespan_reporting::Severity;
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
    /// ```text
    /// //~ PARSE: ok
    /// //~ PARSE: fail
    /// ```
    pub parse: Option<Status>,
    /// ```text
    /// //~ COMPILE/RUST: ok
    /// //~ COMPILE/RUST: fail
    /// ```
    pub compile_rust: Option<Status>,
    /// ```text
    /// //~ COMPILE/DOC: ok
    /// //~ COMPILE/DOC: fail
    /// ```
    pub compile_doc: Option<Status>,
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
            parse: None,
            compile_rust: None,
            compile_doc: None,
            expected_diagnostics: Vec::new(),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Status {
    Ok,
    Fail,
}

impl fmt::Display for Status {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Status::Ok => write!(f, "ok"),
            Status::Fail => write!(f, "fail"),
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
