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

    #[token("U8")]
    KeywordU8Type,
    #[token("U16")]
    KeywordU16Type,
    #[token("U32")]
    KeywordU32Type,
    #[token("U64")]
    KeywordU64Type,
    #[token("S8")]
    KeywordS8Type,
    #[token("S16")]
    KeywordS16Type,
    #[token("S32")]
    KeywordS32Type,
    #[token("S64")]
    KeywordS64Type,
    #[token("F32")]
    KeywordF32Type,
    #[token("F64")]
    KeywordF64Type,

    #[token("Format")]
    KeywordFormatType,
    #[token("fail")]
    KeywordFormatFail,
    #[token("u8")]
    KeywordFormatU8,
    #[token("u16be")]
    KeywordFormatU16Be,
    #[token("u16le")]
    KeywordFormatU16Le,
    #[token("u32be")]
    KeywordFormatU32Be,
    #[token("u32le")]
    KeywordFormatU32Le,
    #[token("u64be")]
    KeywordFormatU64Be,
    #[token("u64le")]
    KeywordFormatU64Le,
    #[token("s8")]
    KeywordFormatS8,
    #[token("s16be")]
    KeywordFormatS16Be,
    #[token("s16le")]
    KeywordFormatS16Le,
    #[token("s32be")]
    KeywordFormatS32Be,
    #[token("s32le")]
    KeywordFormatS32Le,
    #[token("s64be")]
    KeywordFormatS64Be,
    #[token("s64le")]
    KeywordFormatS64Le,
    #[token("f32be")]
    KeywordFormatF32Be,
    #[token("f32le")]
    KeywordFormatF32Le,
    #[token("f64be")]
    KeywordFormatF64Be,
    #[token("f64le")]
    KeywordFormatF64Le,

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
    #[token("?")]
    QuestionMark,
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
