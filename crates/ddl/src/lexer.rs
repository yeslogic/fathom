use codespan::{ByteIndex, ByteOffset, FileId, Files, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use maplit::hashmap;
use std::collections::HashMap;
use std::fmt;

type Keywords = HashMap<String, Token>;

lazy_static::lazy_static! {
    pub static ref CONCRETE_KEYWORDS: Keywords = hashmap! {
        "struct".to_owned() => Token::Struct,
    };

    pub static ref CORE_KEYWORDS: Keywords = hashmap! {
        "item".to_owned() => Token::Item,
        "struct".to_owned() => Token::Struct,
    };
}

/// Tokens that will be produces during lexing.
#[derive(Debug, Clone)]
pub enum Token {
    /// Identifiers
    Identifier(String),
    /// Doc comments,
    DocComment(String),

    /// Keyword `item`
    Item,
    /// Keyword `struct`
    Struct,

    /// Open curly brace: `{`
    OpenBrace,
    /// Close curly brace:  `}`
    CloseBrace,
    /// Open parenthesis: `(`
    OpenParen,
    /// Close parenthesis:  `)`
    CloseParen,

    /// Bang: `!`
    Bang,
    /// Colon: `:`
    Colon,
    /// Comma: `,`
    Comma,
    /// Equals: `=`
    Equals,
    /// Semicolon: `;`
    Semi,
}

impl<'a> fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Identifier(name) => write!(f, "{}", name),
            Token::DocComment(name) => write!(f, "///{}", name),

            Token::Item => write!(f, "item"),
            Token::Struct => write!(f, "struct"),

            Token::OpenBrace => write!(f, "{{"),
            Token::CloseBrace => write!(f, "}}"),
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),

            Token::Bang => write!(f, "!"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::Equals => write!(f, "="),
            Token::Semi => write!(f, ";"),
        }
    }
}

pub type SpannedToken = (ByteIndex, Token, ByteIndex);

/// A lexer for the DDL.
pub struct Lexer<'input, 'keywords> {
    file_id: FileId,
    keywords: &'keywords Keywords,
    /// An iterator of unicode characters to consume.
    chars: std::str::Chars<'input>,
    /// One character of lookahead, making this lexer LR(1).
    peeked: Option<char>,
    /// The start of the next token to be emitted.
    token_start: ByteIndex,
    /// The end of the next token to be emitted.
    token_end: ByteIndex,
}

impl<'input, 'keywords> Lexer<'input, 'keywords> {
    /// Create a new lexer with the given file.
    pub fn new(
        files: &'input Files,
        file_id: FileId,
        keywords: &'keywords Keywords,
    ) -> Lexer<'input, 'keywords> {
        let mut chars = files.source(file_id).chars();
        let peeked = chars.next();

        Lexer {
            file_id,
            keywords,
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
        .with_notes(vec![format!(
            "expected one of {}",
            display_expected(&expected),
        )])))
    }

    fn unexpected_eof<T>(&self, expected: &[&str]) -> Option<Result<T, Diagnostic>> {
        Some(Err(Diagnostic::new_error(
            "unexpected end of file",
            self.label(self.token_end..self.token_end, "unexpected end of file"),
        )
        .with_notes(vec![format!(
            "expected one of {}",
            display_expected(&expected),
        )])))
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

impl<'input, 'keywords> Iterator for Lexer<'input, 'keywords> {
    type Item = Result<SpannedToken, Diagnostic>;

    fn next(&mut self) -> Option<Result<SpannedToken, Diagnostic>> {
        'top: loop {
            let start = self.token_end;
            match self.advance()? {
                '/' => {
                    let expected = &["`/`"];
                    let start = self.token_end;
                    match self.advance() {
                        Some('/') if self.peek() == Some('/') => {
                            let mut doc = String::new();
                            self.advance();
                            'doc_comment: loop {
                                match self.advance() {
                                    Some('\n') | Some('\r') | None => {
                                        return Some(Ok(self.emit(Token::DocComment(doc))))
                                    }
                                    Some(ch) => {
                                        doc.push(ch);
                                        continue 'doc_comment;
                                    }
                                }
                            }
                        }
                        Some('/') => 'comment: loop {
                            match self.advance() {
                                Some('\n') | Some('\r') => {
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
                '{' => return Some(Ok(self.emit(Token::OpenBrace))),
                '}' => return Some(Ok(self.emit(Token::CloseBrace))),
                '(' => return Some(Ok(self.emit(Token::OpenParen))),
                ')' => return Some(Ok(self.emit(Token::CloseParen))),
                '!' => return Some(Ok(self.emit(Token::Bang))),
                ':' => return Some(Ok(self.emit(Token::Colon))),
                ',' => return Some(Ok(self.emit(Token::Comma))),
                '=' => return Some(Ok(self.emit(Token::Equals))),
                ';' => return Some(Ok(self.emit(Token::Semi))),
                ch if is_identifier_start(ch) => {
                    let mut ident = String::new();
                    ident.push(ch);
                    'ident: loop {
                        match self.peek() {
                            Some(ch) if is_identifier_continue(ch) => {
                                ident.push(ch);
                                self.advance();
                            }
                            None | Some(_) => {
                                return Some(Ok(self.emit(match self.keywords.get(&ident) {
                                    Some(token) => token.clone(),
                                    None => Token::Identifier(ident),
                                })));
                            }
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
