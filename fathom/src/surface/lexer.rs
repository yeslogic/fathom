use codespan_reporting::diagnostic::{Diagnostic, Label};
use logos::{Filter, Logos};

use crate::{
    files::FileId,
    source::{BytePos, ByteRange},
};

pub const KEYWORDS: &[&str] = &[
    "def", "else", "false", "fun", "if", "let", "match", "overlap", "then", "true", "Type", "where",
];

pub fn is_keyword(word: &str) -> bool {
    KEYWORDS.iter().any(|keyword| word == *keyword)
}

#[derive(Clone, Debug, Logos)]
#[logos(extras = FileId)]
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

    #[token(r"/*", block_comment)]
    BlockComment(BlockCommentError),

    #[error]
    #[regex(r"\p{Whitespace}", logos::skip)]
    #[regex(r"//(.*)\n", logos::skip)]
    Error,
}

const OPEN: &str = "/*";
const CLOSE: &str = "*/";
const LEN: BytePos = OPEN.len() as BytePos;

fn block_comment<'source>(
    lexer: &mut logos::Lexer<'source, Token<'source>>,
) -> Filter<BlockCommentError> {
    let start = lexer.span().start as BytePos;
    let first_open_pos = start;
    let mut last_close_pos = start;
    let mut pos = start;

    let mut depth: u32 = 1;
    while let Some(c) = lexer.remainder().chars().next() {
        if lexer.remainder().starts_with(OPEN) {
            pos += LEN;
            lexer.bump(OPEN.len());
            depth += 1;
        } else if lexer.remainder().starts_with(CLOSE) {
            pos += LEN;
            last_close_pos = pos;
            lexer.bump(CLOSE.len());
            depth -= 1;
            if depth == 0 {
                break;
            }
        } else {
            pos += c.len_utf8() as BytePos;
            lexer.bump(c.len_utf8());
        }
    }

    let file_id = lexer.extras;
    match depth {
        0 => Filter::Skip,
        _ => Filter::Emit(BlockCommentError {
            depth,
            first_open: ByteRange::new(file_id, first_open_pos, first_open_pos + LEN),
            last_close: ByteRange::new(file_id, last_close_pos, last_close_pos + LEN),
        }),
    }
}

pub type Spanned<Tok, Loc> = (Loc, Tok, Loc);

#[derive(Clone, Debug)]
pub enum Error {
    UnclosedBlockComment(BlockCommentError),
    UnexpectedCharacter { range: ByteRange },
}

#[derive(Clone, Debug)]
pub struct BlockCommentError {
    depth: u32,
    first_open: ByteRange,
    last_close: ByteRange,
}

impl Error {
    pub fn range(&self) -> ByteRange {
        match self {
            Error::UnexpectedCharacter { range } => *range,
            Error::UnclosedBlockComment(BlockCommentError { first_open, .. }) => *first_open,
        }
    }

    pub fn to_diagnostic(&self) -> Diagnostic<FileId> {
        match self {
            Error::UnexpectedCharacter { range } => Diagnostic::error()
                .with_message("unexpected character")
                .with_labels(vec![Label::primary(range.file_id(), *range)]),
            Error::UnclosedBlockComment(BlockCommentError {
                depth,
                first_open,
                last_close,
            }) => Diagnostic::error()
                .with_message("unclosed block comment")
                .with_labels(vec![
                    Label::primary(first_open.file_id(), *first_open)
                        .with_message(format!("first `{OPEN}`")),
                    Label::primary(last_close.file_id(), *last_close)
                        .with_message(format!("last `{CLOSE}`")),
                ])
                .with_notes(vec![format!("Help: {depth} more `{CLOSE}` needed",)]),
        }
    }
}

pub fn tokens(
    file_id: FileId,
    source: &str,
) -> impl Iterator<Item = Result<Spanned<Token<'_>, BytePos>, Error>> {
    assert!(
        source.len() <= u32::MAX as usize,
        "`source` must be less than 4GiB in length"
    );

    Token::lexer_with_extras(source, file_id)
        .spanned()
        .map(move |(token, range)| {
            let start = range.start as BytePos;
            let end = range.end as BytePos;
            match token {
                Token::BlockComment(err) => Err(Error::UnclosedBlockComment(err)),
                Token::Error => Err(Error::UnexpectedCharacter {
                    range: ByteRange::new(file_id, start, end),
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
            Token::BlockComment(_) => "block comment",
            Token::Error => "error",
            Token::BangEquals => "!=",
            Token::EqualsEquals => "==",
            Token::GreaterEquals => ">=",
            Token::Greater => ">",
            Token::LessEquals => "<=",
            Token::Less => "<",
        }
    }
}
