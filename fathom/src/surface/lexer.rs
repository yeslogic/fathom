use codespan_reporting::diagnostic::{Diagnostic, Label};
use logos::{Filter, Logos};

use crate::files::FileId;
use crate::source::{BytePos, ByteRange, ProgramSource};

pub const KEYWORDS: &[&str] = &[
    "def", "else", "false", "fun", "if", "let", "match", "overlap", "then", "true", "Type", "where",
];

pub fn is_keyword(word: &str) -> bool {
    KEYWORDS.iter().any(|keyword| word == *keyword)
}

#[derive(Clone, Debug, Logos)]
pub enum Token<'source> {
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    #[regex(r"r#[a-zA-Z_][a-zA-Z0-9_]*", |lex| &lex.slice()[2..])]
    Name(&'source str),
    #[regex(r"\?[a-zA-Z_][a-zA-Z0-9_]*", |lex| &lex.slice()[1..])]
    #[regex(r"\?r#[a-zA-Z_][a-zA-Z0-9_]*", |lex| &lex.slice()[3..])]
    Hole(&'source str),
    #[regex(r#""([^"\\]|\\.)*""#, |lex| &lex.slice()[1..(lex.slice().len() - 1)])]
    StringLiteral(&'source str),
    #[regex(r"[+-]?[0-9][a-zA-Z0-9_]*")]
    NumberLiteral(&'source str),

    #[token("def")]
    KeywordDef,
    #[token("else")]
    KeywordElse,
    #[token("false")]
    KeywordFalse,
    #[token("fun")]
    KeywordFun,
    #[token("if")]
    KeywordIf,
    #[token("let")]
    KeywordLet,
    #[token("match")]
    KeywordMatch,
    #[token("overlap")]
    KeywordOverlap,
    #[token("then")]
    KeywordThen,
    #[token("true")]
    KeywordTrue,
    #[token("Type")]
    KeywordType,
    #[token("where")]
    KeywordWhere,

    #[token("@")]
    At,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("=")]
    Equals,
    #[token("!=")]
    BangEquals,
    #[token("==")]
    EqualsEquals,
    #[token("=>")]
    EqualsGreater,
    #[token(">=")]
    GreaterEquals,
    #[token(">")]
    Greater,
    #[token("<=")]
    LessEquals,
    #[token("<")]
    Less,
    #[token(".")]
    FullStop,
    #[token("/")]
    ForwardSlash,
    #[token("->")]
    HyphenGreater,
    #[token("<-")]
    LessHyphen,
    #[token("-")]
    Minus,
    #[token("|")]
    Pipe,
    #[token("+")]
    Plus,
    #[token(";")]
    Semicolon,
    #[token("*")]
    Star,
    #[token("_")]
    Underscore,
    #[token("{")]
    OpenBrace,
    #[token("}")]
    CloseBrace,
    #[token("[")]
    OpenBracket,
    #[token("]")]
    CloseBracket,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,

    #[error]
    #[regex(r"\p{Whitespace}", logos::skip)]
    #[regex(r"//(.*)\n", logos::skip)]
    Error,

    #[token(r"/*", block_comment)]
    ErrorData(Error),
}

#[derive(Clone, Debug, Logos)]
enum BlockComment {
    #[error]
    Skip,
    #[token("/*")]
    Open,
    #[token("*/")]
    Close,
}

fn lexer_range<'source, T>(lexer: &logos::Lexer<'source, T>) -> ByteRange
where
    T: logos::Logos<'source>,
{
    let span = lexer.span();
    ByteRange::new(span.start as BytePos, span.end as BytePos)
}

fn block_comment<'source>(lexer: &mut logos::Lexer<'source, Token<'source>>) -> Filter<Error> {
    let mut comment_lexer = lexer.to_owned().morph::<BlockComment>();
    let first_open = lexer_range(&comment_lexer);
    let mut last_close = first_open;

    let mut depth: u32 = 1;
    while let Some(token) = comment_lexer.next() {
        match token {
            BlockComment::Skip => {}
            BlockComment::Open => depth += 1,
            BlockComment::Close => {
                depth -= 1;
                last_close = lexer_range(&comment_lexer);
                if depth == 0 {
                    break;
                }
            }
        }
    }

    *lexer = comment_lexer.morph::<Token>();

    match depth {
        0 => Filter::Skip,
        _ => Filter::Emit(Error::UnclosedBlockComment {
            depth,
            first_open,
            last_close,
        }),
    }
}

pub type Spanned<Tok, Loc> = (Loc, Tok, Loc);

#[derive(Clone, Debug)]
pub enum Error {
    UnclosedBlockComment {
        depth: u32,
        first_open: ByteRange,
        last_close: ByteRange,
    },
    UnexpectedCharacter {
        range: ByteRange,
    },
}

impl Error {
    pub fn range(&self) -> ByteRange {
        match self {
            Error::UnexpectedCharacter { range } => *range,
            Error::UnclosedBlockComment { first_open, .. } => *first_open,
        }
    }

    pub fn to_diagnostic(&self, file_id: FileId) -> Diagnostic<FileId> {
        match self {
            Error::UnexpectedCharacter { range } => Diagnostic::error()
                .with_message("unexpected character")
                .with_labels(vec![Label::primary(file_id, *range)]),
            Error::UnclosedBlockComment {
                depth,
                first_open,
                last_close,
            } => Diagnostic::error()
                .with_message("unclosed block comment")
                .with_labels(vec![
                    Label::primary(file_id, *first_open).with_message("first `/*`"),
                    Label::primary(file_id, *last_close).with_message("last `*/`"),
                ])
                .with_notes(vec![format!("help: {depth} more `*/` needed")]),
        }
    }
}

pub fn tokens(
    source: &ProgramSource,
) -> impl Iterator<Item = Result<Spanned<Token<'_>, BytePos>, Error>> {
    Token::lexer(source).spanned().map(move |(token, range)| {
        let start = range.start as BytePos;
        let end = range.end as BytePos;
        match token {
            Token::ErrorData(err) => Err(err),
            Token::Error => Err(Error::UnexpectedCharacter {
                range: ByteRange::new(start, end),
            }),
            token => Ok((start, token, end)),
        }
    })
}

impl<'source> Token<'source> {
    pub fn description(&self) -> &'static str {
        match self {
            Token::Name(_) => "name",
            Token::Hole(_) => "hole",
            Token::StringLiteral(_) => "string literal",
            Token::NumberLiteral(_) => "number literal",
            Token::KeywordDef => "def",
            Token::KeywordElse => "else",
            Token::KeywordFalse => "false",
            Token::KeywordFun => "fun",
            Token::KeywordIf => "if",
            Token::KeywordLet => "let",
            Token::KeywordMatch => "match",
            Token::KeywordOverlap => "overlap",
            Token::KeywordThen => "then",
            Token::KeywordTrue => "true",
            Token::KeywordType => "Type",
            Token::KeywordWhere => "where",
            Token::At => "@",
            Token::Colon => ":",
            Token::Comma => ",",
            Token::Equals => "=>",
            Token::EqualsGreater => "=>",
            Token::ForwardSlash => "/",
            Token::FullStop => ".",
            Token::HyphenGreater => "->",
            Token::LessHyphen => "<-",
            Token::Minus => "-",
            Token::Semicolon => ";",
            Token::Star => "*",
            Token::Pipe => "|",
            Token::Plus => "+",
            Token::Underscore => "_",
            Token::OpenBrace => "{",
            Token::CloseBrace => "}",
            Token::OpenBracket => "[",
            Token::CloseBracket => "]",
            Token::OpenParen => "(",
            Token::CloseParen => ")",
            Token::Error | Token::ErrorData(_) => "error",
            Token::BangEquals => "!=",
            Token::EqualsEquals => "==",
            Token::GreaterEquals => ">=",
            Token::Greater => ">",
            Token::LessEquals => "<=",
            Token::Less => "<",
        }
    }
}
