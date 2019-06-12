use codespan::{ByteIndex, FileId, Files, Span};
use codespan_reporting::{Diagnostic, Label};

pub type Token = (Span, String, Option<String>);

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
    file_id: FileId,
    eof: ByteIndex,
    chars: std::str::CharIndices<'input>,
}

impl<'input> Lexer<'input> {
    pub fn new(files: &'input Files, file_id: FileId) -> Lexer<'input> {
        Lexer {
            file_id,
            eof: files.source_span(file_id).end(),
            chars: files.source(file_id).char_indices(),
        }
    }

    fn label(&self, span: impl Into<Span>, message: impl Into<String>) -> Label {
        Label::new(self.file_id, span, message)
    }

    fn unexpected_eol<T>(&self, eol: ByteIndex, expected: &str) -> Option<Result<T, Diagnostic>> {
        Some(Err(Diagnostic::new_error(
            "unexpected end of line",
            self.label(eol..eol, format!("{} expected here", expected)),
        )))
    }

    fn unexpected_eof<T>(&self, expected: &str) -> Option<Result<T, Diagnostic>> {
        Some(Err(Diagnostic::new_error(
            "unexpected end of file",
            self.label(self.eof..self.eof, format!("{} expected here", expected)),
        )))
    }

    fn next_char(&mut self) -> Option<(ByteIndex, char)> {
        let (i, ch) = self.chars.next()?;
        Some((ByteIndex::from(i as u32), ch))
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<Token, Diagnostic>;

    fn next(&mut self) -> Option<Result<Token, Diagnostic>> {
        fn emit(
            span: impl Into<Span>,
            key: String,
            value: Option<String>,
        ) -> Option<Result<Token, Diagnostic>> {
            Some(Ok((
                span.into(),
                key.trim().to_string(),
                value.map(|value| value.trim().to_string()),
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
                    let mut key = String::new();
                    let mut value = String::new();

                    // skip space
                    'space0: loop {
                        match self.next_char() {
                            None => return self.unexpected_eof("key"),
                            Some((i, '\n')) => return self.unexpected_eol(i, "key"),
                            Some((_, ' ')) => {}
                            Some((_, ch)) => {
                                key.push(ch);
                                break 'space0;
                            }
                        }
                    }

                    // in key
                    'key: loop {
                        match self.next_char()? {
                            (_, ':') => break 'key,
                            (end, '\n') => return emit(start..end, key, None),
                            (_, ch) => key.push(ch),
                        }
                    }

                    // skip space
                    'space1: loop {
                        match self.next_char() {
                            None => return self.unexpected_eof("value"),
                            Some((i, '\n')) => return self.unexpected_eol(i, "value"),
                            Some((_, ' ')) => {}
                            Some((_, ch)) => {
                                value.push(ch);
                                break 'space1;
                            }
                        }
                    }

                    // in key
                    let end = 'value: loop {
                        match self.next_char() {
                            None => break 'value self.eof,
                            Some((end, '\n')) => break 'value end,
                            Some((_, ch)) => value.push(ch),
                        }
                    };

                    return emit(start..end, key, Some(value));
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
