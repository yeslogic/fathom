use codespan::{ByteIndex, ByteOffset, ByteSpan, FileMap, RawOffset};
use codespan_reporting::{Diagnostic, Label};
use std::fmt;
use std::str::CharIndices;
use unicode_xid::UnicodeXID;

fn is_symbol(ch: char) -> bool {
    match ch {
        '&' | '!' | ':' | ',' | '.' | '=' | '/' | '>' | '<' | '-' | '|' | '+' | ';' | '*' => true,
        _ => false,
    }
}

fn is_ident_start(ch: char) -> bool {
    UnicodeXID::is_xid_start(ch) || ch == '_'
}

fn is_ident_continue(ch: char) -> bool {
    UnicodeXID::is_xid_continue(ch) || ch == '_'
}

fn is_bin_digit(ch: char) -> bool {
    ch.is_digit(2)
}

fn is_hex_digit(ch: char) -> bool {
    ch.is_digit(16)
}

fn is_dec_digit(ch: char) -> bool {
    ch.is_digit(10)
}

/// An error that occurred while lexing the source file
#[derive(Debug, Fail, Clone, PartialEq, Eq)]
pub enum LexerError {
    /// An unexpected character was encountered
    #[fail(display = "An unexpected character {:?} was found.", found)]
    UnexpectedCharacter { start: ByteIndex, found: char },
    /// Expected a binary literal
    #[fail(display = "Expected a binary literal.")]
    ExpectedBinLiteral { start: ByteIndex },
    /// Expected a hexidecimal literal
    #[fail(display = "Expected a hexidecimal literal.")]
    ExpectedHexLiteral { start: ByteIndex },
}

impl LexerError {
    pub fn to_diagnostic(&self) -> Diagnostic {
        match *self {
            LexerError::UnexpectedCharacter { start, found } => {
                let char_span = ByteSpan::from_offset(start, ByteOffset::from_char_utf8(found));
                Diagnostic::new_error(format!("unexpected character {:?}", found))
                    .with_label(Label::new_primary(char_span))
            }
            LexerError::ExpectedBinLiteral { start } => {
                Diagnostic::new_error("expected a binary literal")
                    .with_label(Label::new_primary(ByteSpan::new(start, start)))
            }
            LexerError::ExpectedHexLiteral { start } => {
                Diagnostic::new_error("expected a hexidecimal literal")
                    .with_label(Label::new_primary(ByteSpan::new(start, start)))
            }
        }
    }
}

/// A token in the source file, to be emitted by the `Lexer`
#[derive(Clone, Debug, PartialEq)]
pub enum Token<S> {
    // Data
    Ident(S),
    DocComment(S),
    BinLiteral(u64, S),
    DecLiteral(u64, S),
    HexLiteral(u64, S),
    FloatDecLiteral(f64, S),

    // Keywords
    As,      // as
    Cond,    // cond
    Compute, // compute
    From,    // from
    Struct,  // struct
    Union,   // union
    Where,   // where

    // Symbols
    Amp,          // &
    AmpAmp,       // &&
    Bang,         // !
    BangEqual,    // !=
    Colon,        // :
    Comma,        // ,
    Dot,          // .
    Equal,        // =
    EqualEqual,   // ==
    EqualGreater, // =>
    ForwardSlash, // /
    Greater,      // >
    GreaterEqual, // >=
    Less,         // <
    LessEqual,    // <=
    Minus,        // -
    Pipe,         // |
    PipePipe,     // ||
    Plus,         // +
    Semi,         // ;
    Star,         // *

    // Delimiters
    LParen,   // (
    RParen,   // )
    LBrace,   // {
    RBrace,   // }
    LBracket, // [
    RBracket, // ]
}

impl<S: fmt::Display> fmt::Display for Token<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::Ident(ref name) => write!(f, "{}", name),
            Token::DocComment(ref comment) => write!(f, "{}", comment),
            Token::BinLiteral(num, ref suffix) => write!(f, "{}{}", num, suffix),
            Token::DecLiteral(num, ref suffix) => write!(f, "{}{}", num, suffix),
            Token::HexLiteral(num, ref suffix) => write!(f, "{}{}", num, suffix),
            Token::FloatDecLiteral(num, ref suffix) => write!(f, "{}{}", num, suffix),
            Token::As => write!(f, "as"),
            Token::Cond => write!(f, "cond"),
            Token::Compute => write!(f, "compute"),
            Token::From => write!(f, "from"),
            Token::Struct => write!(f, "struct"),
            Token::Union => write!(f, "union"),
            Token::Where => write!(f, "where"),
            Token::Amp => write!(f, "&"),
            Token::AmpAmp => write!(f, "&&"),
            Token::Bang => write!(f, "!"),
            Token::BangEqual => write!(f, "!="),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Equal => write!(f, "="),
            Token::EqualEqual => write!(f, "=="),
            Token::EqualGreater => write!(f, "=>"),
            Token::ForwardSlash => write!(f, "/"),
            Token::Greater => write!(f, ">"),
            Token::GreaterEqual => write!(f, ">="),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::Minus => write!(f, "-"),
            Token::Pipe => write!(f, "|"),
            Token::PipePipe => write!(f, "||"),
            Token::Plus => write!(f, "+"),
            Token::Semi => write!(f, ";"),
            Token::Star => write!(f, "*"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
        }
    }
}

impl<'input> From<Token<&'input str>> for Token<String> {
    fn from(src: Token<&'input str>) -> Token<String> {
        match src {
            Token::Ident(name) => Token::Ident(String::from(name)),
            Token::DocComment(comment) => Token::DocComment(String::from(comment)),
            Token::BinLiteral(num, suffix) => Token::BinLiteral(num, String::from(suffix)),
            Token::DecLiteral(num, suffix) => Token::DecLiteral(num, String::from(suffix)),
            Token::HexLiteral(num, suffix) => Token::HexLiteral(num, String::from(suffix)),
            Token::FloatDecLiteral(num, suffix) => {
                Token::FloatDecLiteral(num, String::from(suffix))
            }
            Token::As => Token::As,
            Token::Cond => Token::Cond,
            Token::Compute => Token::Compute,
            Token::From => Token::From,
            Token::Struct => Token::Struct,
            Token::Union => Token::Union,
            Token::Where => Token::Where,
            Token::Amp => Token::Amp,
            Token::AmpAmp => Token::AmpAmp,
            Token::Bang => Token::Bang,
            Token::BangEqual => Token::BangEqual,
            Token::Colon => Token::Colon,
            Token::Comma => Token::Comma,
            Token::Dot => Token::Dot,
            Token::Equal => Token::Equal,
            Token::EqualEqual => Token::EqualEqual,
            Token::EqualGreater => Token::EqualGreater,
            Token::ForwardSlash => Token::ForwardSlash,
            Token::Greater => Token::Greater,
            Token::GreaterEqual => Token::GreaterEqual,
            Token::Less => Token::Less,
            Token::LessEqual => Token::LessEqual,
            Token::Minus => Token::Minus,
            Token::Pipe => Token::Pipe,
            Token::PipePipe => Token::PipePipe,
            Token::Plus => Token::Plus,
            Token::Semi => Token::Semi,
            Token::Star => Token::Star,
            Token::LParen => Token::LParen,
            Token::RParen => Token::RParen,
            Token::LBrace => Token::LBrace,
            Token::RBrace => Token::RBrace,
            Token::LBracket => Token::LBracket,
            Token::RBracket => Token::RBracket,
        }
    }
}

/// An iterator over a source string that yeilds `Token`s for subsequent use by
/// the parser
pub struct Lexer<'input> {
    filemap: &'input FileMap,
    chars: CharIndices<'input>,
    lookahead: Option<(usize, char)>,
}

impl<'input> Lexer<'input> {
    /// Create a new lexer from the source string
    pub fn new(filemap: &'input FileMap) -> Self {
        let mut chars = filemap.src().char_indices();

        Lexer {
            filemap,
            lookahead: chars.next(),
            chars,
        }
    }

    /// Return the next character in the source string
    fn lookahead(&self) -> Option<(ByteIndex, char)> {
        self.lookahead.map(|(index, ch)| {
            let off = ByteOffset(index as RawOffset);
            let index = self.filemap.span().start() + off;
            (index, ch)
        })
    }

    /// Bump the current position in the source string by one character,
    /// returning the current character and byte position.
    fn bump(&mut self) -> Option<(ByteIndex, char)> {
        let current = self.lookahead();
        self.lookahead = self.chars.next();
        current
    }

    /// Return a slice of the source string
    fn slice(&self, start: ByteIndex, end: ByteIndex) -> &'input str {
        self.filemap.src_slice(ByteSpan::new(start, end)).unwrap()
    }

    /// Test a predicate againt the next character in the source
    fn test_lookahead<F>(&self, mut pred: F) -> bool
    where
        F: FnMut(char) -> bool,
    {
        self.lookahead.map_or(false, |(_, ch)| pred(ch))
    }

    /// Consume characters while the predicate matches for the current
    /// character, then return the consumed slice and the end byte
    /// position.
    fn take_while<F>(&mut self, start: ByteIndex, mut keep_going: F) -> (ByteIndex, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        self.take_until(start, |ch| !keep_going(ch))
    }

    /// Consume characters until the predicate matches for the next character
    /// in the lookahead, then return the consumed slice and the end byte
    /// position.
    fn take_until<F>(&mut self, start: ByteIndex, mut terminate: F) -> (ByteIndex, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        while let Some((end, ch)) = self.lookahead() {
            if terminate(ch) {
                return (end, self.slice(start, end));
            } else {
                self.bump();
            }
        }

        let eof = self.filemap.span().end();
        (eof, self.slice(start, eof))
    }

    /// Consume a doc comment
    fn doc_comment(&mut self, start: ByteIndex) -> (ByteIndex, Token<&'input str>, ByteIndex) {
        let (end, mut comment) = self.take_until(start + ByteOffset(3), |ch| ch == '\n');

        // Skip preceding space
        if comment.starts_with(' ') {
            comment = &comment[1..];
        }

        (start, Token::DocComment(comment), end)
    }

    /// Consume an identifier token
    fn ident(&mut self, start: ByteIndex) -> (ByteIndex, Token<&'input str>, ByteIndex) {
        let (end, ident) = self.take_while(start, is_ident_continue);

        let token = match ident {
            "as" => Token::As,
            "cond" => Token::Cond,
            "compute" => Token::Compute,
            "from" => Token::From,
            "struct" => Token::Struct,
            "union" => Token::Union,
            "where" => Token::Where,
            ident => Token::Ident(ident),
        };

        (start, token, end)
    }

    /// Consume a literal suffix
    fn literal_suffix(&mut self, start: ByteIndex) -> (ByteIndex, &'input str) {
        if self.test_lookahead(is_ident_start) {
            self.bump(); // skip ident start
            self.take_while(start, is_ident_continue)
        } else {
            (start, "")
        }
    }

    /// Consume a binary literal token
    fn bin_literal(
        &mut self,
        start: ByteIndex,
    ) -> Result<(ByteIndex, Token<&'input str>, ByteIndex), LexerError> {
        self.bump(); // skip 'b'
        let (end, src) = self.take_while(start + ByteOffset(2), is_bin_digit);
        if src.is_empty() {
            Err(LexerError::ExpectedBinLiteral { start })
        } else {
            let int = u64::from_str_radix(src, 2).unwrap();
            let (end, suffix) = self.literal_suffix(end);

            Ok((start, Token::BinLiteral(int, suffix), end))
        }
    }

    /// Consume a hexidecimal literal token
    fn hex_literal(
        &mut self,
        start: ByteIndex,
    ) -> Result<(ByteIndex, Token<&'input str>, ByteIndex), LexerError> {
        self.bump(); // skip 'x'
        let (end, src) = self.take_while(start + ByteOffset(2), is_hex_digit);
        if src.is_empty() {
            Err(LexerError::ExpectedHexLiteral { start })
        } else {
            let int = u64::from_str_radix(src, 16).unwrap();
            let (end, suffix) = self.literal_suffix(end);

            Ok((start, Token::HexLiteral(int, suffix), end))
        }
    }

    /// Consume a decimal literal token
    fn dec_literal(&mut self, start: ByteIndex) -> (ByteIndex, Token<&'input str>, ByteIndex) {
        let (end, src) = self.take_while(start, is_dec_digit);

        if self.test_lookahead(|ch| ch == '.') {
            self.bump(); // skip '.'
            let (end, src) = self.take_while(start, is_dec_digit);
            let float = src.parse().unwrap();
            let (end, suffix) = self.literal_suffix(end);

            (start, Token::FloatDecLiteral(float, suffix), end)
        } else {
            let int = u64::from_str_radix(src, 10).unwrap();
            let (end, suffix) = self.literal_suffix(end);

            (start, Token::DecLiteral(int, suffix), end)
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<(ByteIndex, Token<&'input str>, ByteIndex), LexerError>;

    fn next(&mut self) -> Option<Result<(ByteIndex, Token<&'input str>, ByteIndex), LexerError>> {
        while let Some((start, ch)) = self.bump() {
            let end = start + ByteOffset(1);

            return Some(match ch {
                ch if is_symbol(ch) => {
                    let (end, symbol) = self.take_while(start, is_symbol);

                    match symbol {
                        "&" => Ok((start, Token::Amp, end)),
                        "&&" => Ok((start, Token::AmpAmp, end)),
                        "!" => Ok((start, Token::Bang, end)),
                        "!=" => Ok((start, Token::BangEqual, end)),
                        ":" => Ok((start, Token::Colon, end)),
                        "," => Ok((start, Token::Comma, end)),
                        "." => Ok((start, Token::Dot, end)),
                        "=" => Ok((start, Token::Equal, end)),
                        "==" => Ok((start, Token::EqualEqual, end)),
                        "=>" => Ok((start, Token::EqualGreater, end)),
                        "/" => Ok((start, Token::ForwardSlash, end)),
                        ">" => Ok((start, Token::Greater, end)),
                        ">=" => Ok((start, Token::GreaterEqual, end)),
                        "<" => Ok((start, Token::Less, end)),
                        "<=" => Ok((start, Token::LessEqual, end)),
                        "-" => Ok((start, Token::Minus, end)),
                        "|" => Ok((start, Token::Pipe, end)),
                        "||" => Ok((start, Token::PipePipe, end)),
                        "+" => Ok((start, Token::Plus, end)),
                        ";" => Ok((start, Token::Semi, end)),
                        "*" => Ok((start, Token::Star, end)),
                        symbol if symbol.starts_with("///") => Ok(self.doc_comment(start)),
                        symbol if symbol.starts_with("//") => {
                            self.take_until(start, |ch| ch == '\n');
                            continue;
                        }
                        _ => Err(LexerError::UnexpectedCharacter { start, found: ch }),
                    }
                }
                '(' => Ok((start, Token::LParen, end)),
                ')' => Ok((start, Token::RParen, end)),
                '{' => Ok((start, Token::LBrace, end)),
                '}' => Ok((start, Token::RBrace, end)),
                '[' => Ok((start, Token::LBracket, end)),
                ']' => Ok((start, Token::RBracket, end)),
                '0' if self.test_lookahead(|x| x == 'b') => self.bin_literal(start),
                '0' if self.test_lookahead(|x| x == 'x') => self.hex_literal(start),
                ch if is_dec_digit(ch) => Ok(self.dec_literal(start)),
                ch if is_ident_start(ch) => Ok(self.ident(start)),
                ch if ch.is_whitespace() => continue,
                _ => Err(LexerError::UnexpectedCharacter { start, found: ch }),
            });
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use codespan::{CodeMap, FileName};
    use codespan::RawIndex;

    use super::*;

    /// A handy macro to give us a nice syntax for declaring test cases
    ///
    /// This was inspired by the tests in the LALRPOP lexer
    macro_rules! test {
        ($src:expr, $($span:expr => $token:expr,)*) => {{
            let mut codemap = CodeMap::new();
            let filemap = codemap.add_filemap(FileName::virtual_("test"), $src.into());

            let lexed_tokens: Vec<_> = Lexer::new(&filemap).collect();
            let expected_tokens = vec![$({
                let start = ByteIndex($span.find("~").unwrap() as RawIndex + 1);
                let end = ByteIndex($span.rfind("~").unwrap() as RawIndex + 2);
                Ok((start, $token, end))
            }),*];

            assert_eq!(lexed_tokens, expected_tokens);
        }};
    }

    #[test]
    fn data() {
        test! {
            "  u8  0b0100101  0x6Ffa6u32  1234i8  ",
            "  ~~                                 " => Token::Ident("u8"),
            "      ~~~~~~~~~                      " => Token::BinLiteral(37, ""),
            "                 ~~~~~~~~~~          " => Token::HexLiteral(458662, "u32"),
            "                             ~~~~~~  " => Token::DecLiteral(1234, "i8"),
        };
    }

    #[test]
    fn doc_comment() {
        test! {
            "       /// hello this is dog",
            "       ~~~~~~~~~~~~~~~~~~~~~" => Token::DocComment("hello this is dog"),
        };
    }

    #[test]
    fn keywords() {
        test! {
            "  as  cond compute  from  struct  union  where ",
            "  ~~                                           " => Token::As,
            "      ~~~~                                     " => Token::Cond,
            "           ~~~~~~~                             " => Token::Compute,
            "                    ~~~~                       " => Token::From,
            "                          ~~~~~~               " => Token::Struct,
            "                                  ~~~~~        " => Token::Union,
            "                                         ~~~~~ " => Token::Where,
        };
    }

    #[test]
    fn symbols() {
        test! {
            " & && ! != : , . = == => / > >= < <= - | || + ; * ",
            " ~                                                " => Token::Amp,
            "   ~~                                             " => Token::AmpAmp,
            "      ~                                           " => Token::Bang,
            "        ~~                                        " => Token::BangEqual,
            "           ~                                      " => Token::Colon,
            "             ~                                    " => Token::Comma,
            "               ~                                  " => Token::Dot,
            "                 ~                                " => Token::Equal,
            "                   ~~                             " => Token::EqualEqual,
            "                      ~~                          " => Token::EqualGreater,
            "                         ~                        " => Token::ForwardSlash,
            "                           ~                      " => Token::Greater,
            "                             ~~                   " => Token::GreaterEqual,
            "                                ~                 " => Token::Less,
            "                                  ~~              " => Token::LessEqual,
            "                                     ~            " => Token::Minus,
            "                                       ~          " => Token::Pipe,
            "                                         ~~       " => Token::PipePipe,
            "                                            ~     " => Token::Plus,
            "                                              ~   " => Token::Semi,
            "                                                ~ " => Token::Star,
        }
    }

    #[test]
    fn delimiters() {
        test! {
            " ( ) { } [ ] ",
            " ~           " => Token::LParen,
            "   ~         " => Token::RParen,
            "     ~       " => Token::LBrace,
            "       ~     " => Token::RBrace,
            "         ~   " => Token::LBracket,
            "           ~ " => Token::RBracket,
        }
    }

    #[test]
    fn array_ty() {
        test! {
            "[u8; 34u32]",
            "~          " => Token::LBracket,
            " ~~        " => Token::Ident("u8"),
            "   ~       " => Token::Semi,
            "     ~~~~~ " => Token::DecLiteral(34, "u32"),
            "          ~" => Token::RBracket,
        };
    }

    #[test]
    fn struct_ty() {
        test! {
            "{ foo : u16 }",
            "~            " => Token::LBrace,
            "  ~~~        " => Token::Ident("foo"),
            "      ~      " => Token::Colon,
            "        ~~~  " => Token::Ident("u16"),
            "            ~" => Token::RBrace,
        };
    }
}
