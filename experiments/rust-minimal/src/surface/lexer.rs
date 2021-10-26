use logos::Logos;

#[derive(Clone, Debug, Logos)]
pub enum Token<'source> {
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Name(&'source str),
    #[regex(r"\?[a-zA-Z_][a-zA-Z0-9_]*", |lex| &lex.slice()[1..])]
    Hole(&'source str),
    #[regex(r"[+-]?[0-9][a-zA-Z0-9_]*")]
    Number(&'source str),

    #[token("fun")]
    KeywordFun,
    #[token("let")]
    KeywordLet,
    #[token("Type")]
    KeywordType,

    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("=")]
    Equals,
    #[token("=>")]
    EqualsGreater,
    #[token(".")]
    FullStop,
    #[token("->")]
    HyphenGreater,
    #[token("<-")]
    LessHyphen,
    #[token(";")]
    Semicolon,
    #[token("_")]
    Underscore,

    #[token("{")]
    OpenBrace,
    #[token("}")]
    CloseBrace,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,

    #[error]
    #[regex(r"\p{Whitespace}", logos::skip)]
    #[regex(r"//(.*)\n", logos::skip)]
    Error,
}

pub type Spanned<Tok, Loc> = (Loc, Tok, Loc);

pub fn tokens<'source>(
    source: &'source str,
) -> impl 'source + Iterator<Item = Result<Spanned<Token<'source>, usize>, ()>> {
    Token::lexer(source)
        .spanned()
        .map(|(token, range)| match token {
            Token::Error => Err(()),
            token => Ok((range.start, token, range.end)),
        })
}
