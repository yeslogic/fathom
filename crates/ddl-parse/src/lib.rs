//! Parser for the data description language.

#![warn(rust_2018_idioms)]

use codespan::{ByteIndex, FileId, Files};
use codespan_reporting::{Diagnostic, Label};
use ddl_concrete::Module;
use lalrpop_util::ParseError;
use std::fmt;

mod lexer;

use self::lexer::{Lexer, Token};

mod grammar {
    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}

pub fn parse_module(files: &Files, file_id: FileId) -> (Module, Vec<Diagnostic>) {
    let mut diagnostics = Vec::new();
    let lexer = Lexer::new(files, file_id);

    let module = grammar::ModuleParser::new()
        .parse(&mut diagnostics, lexer)
        .unwrap_or_else(|error| {
            diagnostics.push(parse_error_to_diagnostic(file_id, error));
            Module { items: Vec::new() }
        });

    (module, diagnostics)
}

/// Convert an LALRPOP error to a codespan-reporting diagnostic.
fn parse_error_to_diagnostic(
    file_id: FileId,
    error: ParseError<ByteIndex, Token, Diagnostic>,
) -> Diagnostic {
    match error {
        ParseError::InvalidToken { location: _ } => unreachable!(),

        ParseError::UnrecognizedEOF { location, expected } => Diagnostic::new_error(
            "unexpected end of file",
            Label::new(file_id, location..location, "unexpected end of file"),
        )
        .with_notes(vec![format!(
            "expected one of {}",
            display_expected(&expected),
        )]),

        ParseError::UnrecognizedToken {
            token: (start, token, end),
            expected,
        } => Diagnostic::new_error(
            format!("unexpected token \"{}\"", token),
            Label::new(file_id, start..end, "unexpected token"),
        )
        .with_notes(vec![format!(
            "expected one of {}",
            display_expected(&expected),
        )]),

        ParseError::ExtraToken {
            token: (start, token, end),
        } => Diagnostic::new_error(
            format!("extra token \"{}\"", token),
            Label::new(file_id, start..end, "extra token"),
        ),

        ParseError::User { error } => error,
    }
}

fn display_expected<'a, Item: fmt::Display>(items: &'a [Item]) -> impl 'a + fmt::Display {
    struct DisplayExpected<'a, Item>(&'a [Item]);

    impl<'a, Item: fmt::Display> fmt::Display for DisplayExpected<'a, Item> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            for (i, item) in self.0.iter().enumerate() {
                match i {
                    0 => write!(f, "{}", item)?,
                    i if i >= self.0.len() => write!(f, ", or {}", item)?,
                    _ => write!(f, ", {}", item)?,
                }
            }

            Ok(())
        }
    }

    DisplayExpected(items)
}

fn concat_docs(lines: Vec<String>) -> String {
    use itertools::Itertools;

    lines
        .iter()
        .map(|line| match line {
            line if line.starts_with(" ") => &line[" ".len()..],
            line => &line[..],
        })
        .join("\n")
}
