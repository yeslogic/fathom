use codespan::{ByteIndex, ByteOffset, FileId, Files, Span};
use codespan_reporting::{Diagnostic, Label};
use std::fmt;

#[derive(Debug, Clone)]
pub enum Token {
    Identifier(String),
}

pub type SpannedToken = (ByteIndex, Token, ByteIndex);

/// A lexer for the DDL.
pub struct Lexer<'input> {
    file_id: FileId,
    /// An iterator of unicode characters to consume.
    chars: std::str::Chars<'input>,
    /// One character of lookahead, making this lexer LR(1).
    peeked: Option<char>,
    /// The start of the next token to be emitted.
    token_start: ByteIndex,
    /// The end of the next token to be emitted.
    token_end: ByteIndex,
}

impl<'input> Lexer<'input> {
    /// Create a new lexer with the given file.
    pub fn new(files: &'input Files, file_id: FileId) -> Lexer<'input> {
        let mut chars = files.source(file_id).chars();
        let peeked = chars.next();

        Lexer {
            file_id,
            chars,
            peeked,
            token_start: ByteIndex::from(0),
            token_end: ByteIndex::from(0),
        }
    }

    /// Emit a token and reset the start position, ready for the next token.
    fn emit(&mut self, token: Token) -> SpannedToken {
        let start = self.token_start;
        let end = self.token_end;
        self.token_start = self.token_end;
        (start, token, end)
    }

    /// Peek at the current lookahead character.
    fn peek(&self) -> Option<char> {
        self.peeked
    }

    /// Consume the current character and load the next one. Return the old character.
    fn advance(&mut self) -> Option<char> {
        let current = std::mem::replace(&mut self.peeked, self.chars.next());
        self.token_end += current.map_or(ByteOffset::from(0), ByteOffset::from_char_len);
        current
    }

    fn reset_start(&mut self) {
        self.token_start = self.token_end;
    }

    /// Create a label for a diagnostic.
    fn label(&self, span: impl Into<Span>, message: impl Into<String>) -> Label {
        Label::new(self.file_id, span, message)
    }

    fn unexpected_char<T>(
        &self,
        start: ByteIndex,
        found: char,
        expected: &[&str],
    ) -> Option<Result<T, Diagnostic>> {
        let end = start + ByteOffset::from_char_len(found);
        Some(Err(Diagnostic::new_error(
            format!("unexpected character `{}`", found),
            self.label(start..end, "unexpected character"),
        )
        .with_notes(vec![ExpectedNote(expected).to_string()])))
    }

    fn unexpected_eof<T>(&self, expected: &[&str]) -> Option<Result<T, Diagnostic>> {
        Some(Err(Diagnostic::new_error(
            "unexpected end of file",
            self.label(self.token_end..self.token_end, "unexpected end of file"),
        )
        .with_notes(vec![ExpectedNote(expected).to_string()])))
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<SpannedToken, Diagnostic>;

    fn next(&mut self) -> Option<Result<SpannedToken, Diagnostic>> {
        'top: loop {
            let start = self.token_end;
            match self.advance()? {
                '/' => {
                    let expected = &["`/`"];
                    let start = self.token_end;
                    match self.advance() {
                        Some('/') => 'comment: loop {
                            match self.advance() {
                                Some('\n') => {
                                    self.reset_start();
                                    continue 'top;
                                }
                                Some(_) => continue 'comment,
                                None => return None,
                            }
                        },
                        Some(ch) => return self.unexpected_char(start, ch, expected),
                        None => return self.unexpected_eof(expected),
                    }
                }
                ch if is_identifier_start(ch) => {
                    let mut ident = String::new();
                    ident.push(ch);
                    'ident: loop {
                        match self.peek() {
                            Some(ch) if is_identifier_continue(ch) => {
                                ident.push(ch);
                                self.advance();
                            }
                            None | Some(_) => return Some(Ok(self.emit(Token::Identifier(ident)))),
                        }
                    }
                }
                ch if is_whitespace(ch) => {
                    self.reset_start();
                    continue 'top;
                }
                ch => {
                    let expected = &["comment", "identifier", "whitespace"];
                    return self.unexpected_char(start, ch, expected);
                }
            }
        }
    }
}

fn is_whitespace(ch: char) -> bool {
    match ch {
        | '\u{0009}' // horizontal tab, '\t'
        | '\u{000A}' // line feed, '\n'
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // carriage return, '\r'
        | '\u{0020}' // space, ' '
        | '\u{0085}' // next line
        | '\u{200E}' // left-to-right mark
        | '\u{200F}' // right-to-left mark
        | '\u{2028}' // line separator
        | '\u{2029}' // paragraph separator
        => true,
        _ => false,
    }
}

fn is_identifier_start(ch: char) -> bool {
    match ch {
        'a'..='z' | 'A'..='Z' | '_' => true,
        _ => false,
    }
}

fn is_identifier_continue(ch: char) -> bool {
    match ch {
        '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => true,
        _ => false,
    }
}

struct ExpectedNote<'a>(&'a [&'a str]);

impl<'a> fmt::Display for ExpectedNote<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, token) in self.0.iter().enumerate() {
            match i {
                0 => write!(f, "expected one of {}", token)?,
                i if i >= self.0.len() => write!(f, ", or {} here", token)?,
                _ => write!(f, ", {}", token)?,
            }
        }

        Ok(())
    }
}
