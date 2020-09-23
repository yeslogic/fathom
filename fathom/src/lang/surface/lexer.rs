use logos::Logos;
use std::fmt;

use crate::reporting::LexerMessage;

/// Tokens in the surface language.
#[derive(Debug, Clone, Logos)]
pub enum Token<'source> {
    #[regex(r"///(.*)\n", |lexer| lexer.slice()[3..].trim_end().to_owned())]
    DocComment(String),
    #[regex(r"//!(.*)\n", |lexer| lexer.slice()[3..].trim_end().to_owned())]
    InnerDocComment(String),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Name(&'source str),
    #[regex(r#"'([^'\\]|\\.)*'"#)]
    CharLiteral(&'source str),
    #[regex(r#""([^"\\]|\\.)*""#)] // workaround editor highlighting: "
    StringLiteral(&'source str),
    #[regex(r"[-+]?[0-9][a-zA-Z0-9_\.]*")]
    NumericLiteral(&'source str),

    #[token("bool_elim")]
    BoolElim,
    #[token("else")]
    Else,
    #[token("f32")]
    F32,
    #[token("f64")]
    F64,
    #[token("Format")]
    Format,
    #[token("global")]
    Global,
    #[token("if")]
    If,
    #[token("int")]
    Int,
    #[token("int_elim")]
    IntElim,
    #[token("item")]
    Item,
    #[token("match")]
    Match,
    #[token("struct")]
    Struct,
    #[token("Type")]
    Type,

    #[token("{")]
    OpenBrace,
    #[token("}")]
    CloseBrace,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,

    #[token("!")]
    Bang,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("=")]
    Equals,
    #[token("=>")]
    EqualsGreater,
    #[token("->")]
    HyphenGreater,
    #[token(";")]
    Semi,

    #[error]
    #[regex(r"\p{Whitespace}", logos::skip)]
    #[regex(r"//(.*)\n", logos::skip)]
    Error,
}

impl<'source> fmt::Display for Token<'source> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::DocComment(source) => write!(f, "{}", source),
            Token::InnerDocComment(source) => write!(f, "{}", source),

            Token::Name(source) => write!(f, "{}", source),
            Token::CharLiteral(source) => write!(f, "{}", source),
            Token::StringLiteral(source) => write!(f, "{}", source),
            Token::NumericLiteral(source) => write!(f, "{}", source),

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

            Token::Error => write!(f, "<error>"),
        }
    }
}

pub type Spanned<Tok, Loc> = (Loc, Tok, Loc);

pub fn tokens<'source>(
    file_id: usize,
    source: &'source str,
) -> impl 'source + Iterator<Item = Result<Spanned<Token<'source>, usize>, LexerMessage>> {
    Token::lexer(source)
        .spanned()
        .map(move |(token, range)| match token {
            Token::Error => Err(LexerMessage::InvalidToken { file_id, range }),
            token => Ok((range.start, token, range.end)),
        })
}
