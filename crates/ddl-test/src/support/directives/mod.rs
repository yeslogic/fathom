use codespan::LineIndex;
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
    /// ```text
    /// //~ PARSE: ok
    /// //~ PARSE: fail
    /// ```
    pub parse: Option<StageStatus>,
    /// ```text
    /// //~ COMPILE/RUST: ok
    /// //~ COMPILE/RUST: fail
    /// ```
    pub compile_rust: Option<StageStatus>,
    /// ```text
    /// //~ COMPILE/DOC: ok
    /// //~ COMPILE/DOC: fail
    /// ```
    pub compile_doc: Option<StageStatus>,
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

#[derive(Clone, Debug)]
pub enum StageStatus {
    Ok,
    Fail,
}

#[derive(Clone, Debug)]
pub struct ExpectedDiagnostic {
    line: LineIndex,
    severity: Severity,
    message_pattern: Regex,
}
