//! Parser for the data description language.

use codespan::{ByteIndex, FileId};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lalrpop_util::ParseError;
use std::fmt;

use crate::concrete::Module;
use crate::lexer::{Lexer, Token};

mod grammar {
    include!(concat!(env!("OUT_DIR"), "/parse/grammar.rs"));
}

pub fn parse_module(
    file_id: FileId,
    lexer: Lexer<'_>,
    report: &mut dyn FnMut(Diagnostic),
) -> Module {
    let module = grammar::ModuleParser::new()
        .parse(file_id, report, lexer)
        .unwrap_or_else(|error| {
            report(parse_error_to_diagnostic(file_id, error));
            Module {
                file_id,
                items: Vec::new(),
            }
        });

    module
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
