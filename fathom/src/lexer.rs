use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::files::SimpleFiles;
use maplit::hashmap;
use std::collections::HashMap;
use std::fmt;

use crate::diagnostics;
use crate::literal::{self, Sign};

type Keywords = HashMap<String, Token>;

lazy_static::lazy_static! {
    pub static ref SURFACE_KEYWORDS: Keywords = hashmap! {
        "if".to_owned() => Token::If,
        "else".to_owned() => Token::Else,
        "Format".to_owned() => Token::Format,
        "match".to_owned() => Token::Match,
        "struct".to_owned() => Token::Struct,
        "Type".to_owned() => Token::Type,
    };

    pub static ref CORE_KEYWORDS: Keywords = hashmap! {
        "bool_elim".to_owned() => Token::BoolElim,
        "f32".to_owned() => Token::F32,
        "f64".to_owned() => Token::F64,
        "Format".to_owned() => Token::Format,
        "global".to_owned() => Token::Global,
        "int".to_owned() => Token::Int,
        "int_elim".to_owned() => Token::IntElim,
        "item".to_owned() => Token::Item,
        "struct".to_owned() => Token::Struct,
        "Type".to_owned() => Token::Type,
    };
}

/// Tokens that will be produces during lexing.
#[derive(Debug, Clone)]
pub enum Token {
    /// Doc comments,
    DocComment(String),
    /// Inner doc comment
    InnerDocComment(String),
    /// Identifiers.
    Identifier(String),
    /// Numeric literals.
    NumberLiteral(literal::Number),
    /// String literals.
    StringLiteral(literal::String),
    /// Character literals.
    CharLiteral(literal::Char),

    /// Keyword: `bool_elim`.
    BoolElim,
    /// Keyword: `else`.
    Else,
    /// Keyword: `f32`.
    F32,
    /// Keyword: `f64`.
    F64,
    /// Keyword: `Format`.
    Format,
    /// Keyword: `global`.
    Global,
    /// Keyword: `if`.
    If,
    /// Keyword: `int`.
    Int,
    /// Keyword: `int_elim`.
    IntElim,
    /// Keyword: `item`.
    Item,
    /// Keyword: `match`.
    Match,
    /// Keyword: `struct`.
    Struct,
    /// Keyword: `Type`.
    Type,

    /// Open curly brace: `{`.
    OpenBrace,
    /// Close curly brace: `}`.
    CloseBrace,
    /// Open parenthesis: `(`.
    OpenParen,
    /// Close parenthesis: `)`.
    CloseParen,

    /// Bang: `!`.
    Bang,
    /// Colon: `:`.
    Colon,
    /// Comma: `,`.
    Comma,
    /// Equals: `=`.
    Equals,
    /// Equals greater: `=>`.
    EqualsGreater,
    // Hyphen greater: `->`.
    HyphenGreater,
    /// Semicolon: `;`.
    Semi,
}

impl<'a> fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::DocComment(comment) => write!(f, "///{}", comment),
            Token::InnerDocComment(comment) => write!(f, "//!{}", comment),
            Token::Identifier(name) => write!(f, "{}", name),
            Token::NumberLiteral(literal) => write!(f, "{}", literal),
            Token::StringLiteral(literal) => write!(f, "{}", literal),
            Token::CharLiteral(literal) => write!(f, "{}", literal),

            Token::BoolElim => write!(f, "bool_elim"),
            Token::Else => write!(f, "else"),
            Token::F32 => write!(f, "f32"),
            Token::F64 => write!(f, "f64"),
            Token::Format => write!(f, "Format"),
            Token::Global => write!(f, "global"),
            Token::If => write!(f, "if"),
            Token::Int => write!(f, "int"),
            Token::IntElim => write!(f, "int_elim"),
            Token::Item => write!(f, "item"),
            Token::Match => write!(f, "match"),
            Token::Struct => write!(f, "struct"),
            Token::Type => write!(f, "Type"),

            Token::OpenBrace => write!(f, "{{"),
            Token::CloseBrace => write!(f, "}}"),
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),

            Token::Bang => write!(f, "!"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::Equals => write!(f, "="),
            Token::EqualsGreater => write!(f, "=>"),
            Token::HyphenGreater => write!(f, "->"),
            Token::Semi => write!(f, ";"),
        }
    }
}

pub type SpannedToken = (usize, Token, usize);

/// A lexer for the fathom.
pub struct Lexer<'input, 'keywords> {
    file_id: usize,
    keywords: &'keywords Keywords,
    /// An iterator of unicode characters to consume.
    chars: std::str::Chars<'input>,
    /// One character of lookahead, making this lexer LR(1).
    peeked: Option<char>,
    /// The start of the next token to be emitted.
    token_start: usize,
    /// The end of the next token to be emitted.
    token_end: usize,
}

impl<'input, 'keywords> Lexer<'input, 'keywords> {
    /// Create a new lexer with the given file.
    pub fn new(
        files: &'input SimpleFiles<String, String>,
        file_id: usize,
        keywords: &'keywords Keywords,
    ) -> Lexer<'input, 'keywords> {
        let mut chars = files.get(file_id).unwrap().source().chars();
        let peeked = chars.next();

        Lexer {
            file_id,
            keywords,
            chars,
            peeked,
            token_start: 0,
            token_end: 0,
        }
    }

    /// Emit a token and reset the start position, ready for the next token.
    fn emit(&mut self, token: Token) -> Option<Result<SpannedToken, Diagnostic<usize>>> {
        let start = self.token_start;
        let end = self.token_end;
        self.token_start = self.token_end;
        Some(Ok((start, token, end)))
    }

    /// Peek at the current lookahead character.
    fn peek(&self) -> Option<char> {
        self.peeked
    }

    /// Consume the current character and load the next one. Return the old character.
    fn advance(&mut self) -> Option<char> {
        let current = std::mem::replace(&mut self.peeked, self.chars.next());
        self.token_end += current.map_or(0, char::len_utf8);
        current
    }

    fn reset_start(&mut self) {
        self.token_start = self.token_end;
    }

    fn unexpected_char<T>(
        &self,
        start: usize,
        found: char,
        expected: &[&str],
    ) -> Option<Result<T, Diagnostic<usize>>> {
        Some(Err(diagnostics::error::unexpected_char(
            self.file_id,
            start,
            found,
            expected,
        )))
    }

    fn unexpected_eof<T>(&self, expected: &[&str]) -> Option<Result<T, Diagnostic<usize>>> {
        Some(Err(diagnostics::error::unexpected_eof(
            self.file_id,
            self.token_end,
            expected,
        )))
    }

    fn consume_number(
        &mut self,
        start: usize,
        sign: Option<Sign>,
        first_digit: char,
    ) -> Option<Result<SpannedToken, Diagnostic<usize>>> {
        let mut number = String::new();
        number.push(first_digit);

        while let Some(ch) = self.peek() {
            if is_identifier_continue(ch) || ['.', '-', '+'].contains(&ch) {
                number.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        let number_range = start..self.token_end;
        let literal = literal::Number::new(sign, (number_range, number));
        self.emit(Token::NumberLiteral(literal))
    }

    fn consume_identifier(
        &mut self,
        start_ch: char,
    ) -> Option<Result<SpannedToken, Diagnostic<usize>>> {
        let mut ident = String::new();
        ident.push(start_ch);

        while let Some(ch) = self.peek() {
            if is_identifier_continue(ch) {
                ident.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        match self.keywords.get(&ident) {
            Some(token) => self.emit(token.clone()),
            None => self.emit(Token::Identifier(ident)),
        }
    }
}

impl<'input, 'keywords> Iterator for Lexer<'input, 'keywords> {
    type Item = Result<SpannedToken, Diagnostic<usize>>;

    fn next(&mut self) -> Option<Result<SpannedToken, Diagnostic<usize>>> {
        'top: loop {
            let start = self.token_end;
            return match self.advance()? {
                '/' => match self.advance() {
                    Some('/') => match self.peek() {
                        Some('/') => {
                            let mut doc = String::new();
                            self.advance();
                            'doc_comment: loop {
                                match self.advance() {
                                    Some('\n') | Some('\r') | None => {
                                        return self.emit(Token::DocComment(doc));
                                    }
                                    Some(ch) => {
                                        doc.push(ch);
                                        continue 'doc_comment;
                                    }
                                }
                            }
                        }
                        Some('!') => {
                            let mut doc = String::new();
                            self.advance();
                            'inner_doc_comment: loop {
                                match self.advance() {
                                    Some('\n') | Some('\r') | None => {
                                        return self.emit(Token::InnerDocComment(doc));
                                    }
                                    Some(ch) => {
                                        doc.push(ch);
                                        continue 'inner_doc_comment;
                                    }
                                }
                            }
                        }
                        Some(_) | None => 'comment: loop {
                            match self.advance() {
                                Some('\n') | Some('\r') => {
                                    self.reset_start();
                                    continue 'top;
                                }
                                Some(_) => continue 'comment,
                                None => return None,
                            }
                        },
                    },
                    Some(ch) => self.unexpected_char(self.token_end, ch, &["`/`"]),
                    None => self.unexpected_eof(&["`/`"]),
                },
                '{' => self.emit(Token::OpenBrace),
                '}' => self.emit(Token::CloseBrace),
                '(' => self.emit(Token::OpenParen),
                ')' => self.emit(Token::CloseParen),
                '!' => self.emit(Token::Bang),
                ':' => self.emit(Token::Colon),
                ',' => self.emit(Token::Comma),
                '=' => match self.peek() {
                    Some('>') => {
                        self.advance();
                        self.emit(Token::EqualsGreater)
                    }
                    Some(_) | None => self.emit(Token::Equals),
                },
                ';' => self.emit(Token::Semi),
                '+' => match self.advance()? {
                    ch if is_dec_digit(ch) => self.consume_number(start, Some(Sign::Positive), ch),
                    ch => self.unexpected_char(start, ch, &["decimal digit"]),
                },
                '-' => match self.peek() {
                    Some('>') => {
                        self.advance();
                        self.emit(Token::HyphenGreater)
                    }
                    Some(_) | None => match self.advance()? {
                        ch if is_dec_digit(ch) => {
                            self.consume_number(start, Some(Sign::Negative), ch)
                        }
                        ch => self.unexpected_char(start, ch, &["decimal digit"]),
                    },
                },
                ch if is_dec_digit(ch) => self.consume_number(start, None, ch),
                ch if is_identifier_start(ch) => self.consume_identifier(ch),
                ch if is_whitespace(ch) => {
                    self.reset_start();
                    continue 'top;
                }
                ch => {
                    let expected = &[
                        "{",
                        "}",
                        "(",
                        ")",
                        "!",
                        ":",
                        ",",
                        "=",
                        ";",
                        "comment",    // `/`
                        "number",     // '+' | '-' | dec-digit
                        "identifier", // identifier-start
                        "whitespace", // whitespace
                    ];
                    self.unexpected_char(start, ch, expected)
                }
            };
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

fn is_dec_digit(ch: char) -> bool {
    match ch {
        '0'..='9' => true,
        _ => false,
    }
}