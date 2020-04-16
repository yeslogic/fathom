use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::{Files, SimpleFiles};
use std::ops::Range;

use super::SpannedString;

pub type Token = (Range<usize>, SpannedString, Option<SpannedString>);

/// An iterator over the test directives in a file.
///
/// # Grammar
///
/// ```text
/// top         ::= line* EOF
///
/// line        :: (not "\\")* (comment | directive)? eol
///
/// comment     ::= "//" (not "~" | not eol)? (not eol)*
/// directive   ::= "//~" key (":" value)?
///
/// key         ::= not ":"
/// value       ::= not eol
/// eol         ::= "\n"
/// ```
pub struct Lexer<'input> {
    file_id: usize,
    eof: usize,
    chars: std::str::CharIndices<'input>,
}

impl<'input> Lexer<'input> {
    pub fn new(files: &'input SimpleFiles<String, String>, file_id: usize) -> Lexer<'input> {
        let source = files.source(file_id).unwrap();
        Lexer {
            file_id,
            eof: source.len(),
            chars: source.char_indices(),
        }
    }

    fn unexpected_eol<T>(
        &self,
        eol: usize,
        expected: &str,
    ) -> Option<Result<T, Diagnostic<usize>>> {
        let range = eol..eol;
        Some(Err(Diagnostic::error()
            .with_message("unexpected end of line")
            .with_labels(vec![Label::primary(self.file_id, range)
                .with_message(format!("{} expected here", expected))])))
    }

    fn unexpected_eof<T>(&self, expected: &str) -> Option<Result<T, Diagnostic<usize>>> {
        let range = self.eof..self.eof;
        Some(Err(Diagnostic::error()
            .with_message("unexpected end of file")
            .with_labels(vec![Label::primary(self.file_id, range)
                .with_message(format!("{} expected here", expected))])))
    }

    fn next_char(&mut self) -> Option<(usize, char)> {
        self.chars.next()
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<Token, Diagnostic<usize>>;

    fn next(&mut self) -> Option<Result<Token, Diagnostic<usize>>> {
        fn emit(
            range: impl Into<Range<usize>>,
            (key_start, key): (usize, String),
            value: Option<(usize, String)>,
        ) -> Option<Result<Token, Diagnostic<usize>>> {
            Some(Ok((
                range.into(),
                SpannedString::new(key_start, key.trim().to_string()),
                value.map(|(value_start, value)| {
                    SpannedString::new(value_start, value.trim().to_string())
                }),
            )))
        }

        // haha I wanted to see how much fun I could have with labelled loops
        //
        // sorry
        'top: loop {
            // look for the start of a comment, ie. `//`
            let start = match self.next_char()? {
                (i, '/') => i,
                _ => continue 'top,
            };
            match self.next_char()? {
                (_, '/') => {}
                _ => continue 'top,
            }

            match self.next_char()? {
                // in comment directive
                (_, '~') => loop {
                    // skip space
                    let (key_start, mut key) = 'space0: loop {
                        match self.next_char() {
                            None => return self.unexpected_eof("key"),
                            Some((i, '\n')) => return self.unexpected_eol(i, "key"),
                            Some((_, ' ')) => {}
                            Some((key_start, ch)) => {
                                let mut key = String::new();
                                key.push(ch);
                                break 'space0 (key_start, key);
                            }
                        }
                    };

                    // in key
                    'key: loop {
                        match self.next_char()? {
                            (_, ':') => break 'key,
                            (end, '\n') => return emit(start..end, (key_start, key), None),
                            (_, ch) => key.push(ch),
                        }
                    }

                    // skip space
                    let (value_start, mut value) = 'space1: loop {
                        match self.next_char() {
                            None => return self.unexpected_eof("value"),
                            Some((i, '\n')) => return self.unexpected_eol(i, "value"),
                            Some((_, ' ')) => {}
                            Some((value_start, ch)) => {
                                let mut value = String::new();
                                value.push(ch);
                                break 'space1 (value_start, value);
                            }
                        }
                    };

                    // in key
                    let end = 'value: loop {
                        match self.next_char() {
                            None => break 'value self.eof,
                            Some((end, '\n')) => break 'value end,
                            Some((_, ch)) => value.push(ch),
                        }
                    };

                    return emit(start..end, (key_start, key), Some((value_start, value)));
                },
                // in comment
                _ => 'comment: loop {
                    match self.next_char()? {
                        (_, '\n') => continue 'top,
                        _ => continue 'comment,
                    }
                },
            }
        }
    }
}
