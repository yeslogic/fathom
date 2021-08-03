use logos::Logos;

#[derive(Clone, Debug, Logos)]
pub enum Token<'source> {
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident(&'source str),

    #[token("fun")]
    KeywordFun,
    #[token("let")]
    KeywordLet,
    #[token("Type")]
    KeywordType,

    #[token(":")]
    Colon,
    #[token("=")]
    Equals,
    #[token("=>")]
    EqualsGreater,
    #[token("->")]
    HyphenGreater,
    #[token("?")]
    QuestionMark,
    #[token(";")]
    Semicolon,

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