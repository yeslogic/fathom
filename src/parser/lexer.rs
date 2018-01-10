use std::str::CharIndices;

use source::BytePos;
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
#[fail(display = "{:?} at: {:?}", code, location)] // FIXME: use better formatting
pub struct Error {
    /// The location where the lexer error occured
    pub location: BytePos,
    /// The error code
    pub code: ErrorCode,
}

fn error<T>(location: BytePos, code: ErrorCode) -> Result<T, Error> {
    Err(Error { location, code })
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErrorCode {
    /// An unexpected character was encountered
    UnexpectedCharacter,
    /// Expected a binary literal
    ExpectedBinLiteral,
    /// Expected a hexidecimal literal
    ExpectedHexLiteral,
}

/// A token in the source file, to be emitted by the `Lexer`
#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    // Data
    Ident(&'src str),
    DocComment(&'src str),
    BinLiteral(u64, &'src str),
    DecLiteral(u64, &'src str),
    HexLiteral(u64, &'src str),
    FloatDecLiteral(f64, &'src str),

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

/// An iterator over a source string that yeilds `Token`s for subsequent use by
/// the parser
pub struct Lexer<'src> {
    src: &'src str,
    chars: CharIndices<'src>,
    lookahead: Option<(usize, char)>,
}

impl<'src> Lexer<'src> {
    /// Create a new lexer from the source string
    pub fn new(src: &'src str) -> Self {
        let mut chars = src.char_indices();

        Lexer {
            src,
            lookahead: chars.next(),
            chars,
        }
    }

    /// Return the next character in the source string
    fn lookahead(&self) -> Option<(BytePos, char)> {
        self.lookahead.map(|(index, ch)| (BytePos(index), ch))
    }

    /// Bump the current position in the source string by one character,
    /// returning the current character and byte position.
    fn bump(&mut self) -> Option<(BytePos, char)> {
        let current = self.lookahead();
        self.lookahead = self.chars.next();
        current
    }

    /// Return a slice of the source string
    fn slice(&self, start: BytePos, end: BytePos) -> &'src str {
        &self.src[start.0..end.0]
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
    fn take_while<F>(&mut self, start: BytePos, mut keep_going: F) -> (BytePos, &'src str)
    where
        F: FnMut(char) -> bool,
    {
        self.take_until(start, |ch| !keep_going(ch))
    }

    /// Consume characters until the predicate matches for the next character
    /// in the lookahead, then return the consumed slice and the end byte
    /// position.
    fn take_until<F>(&mut self, start: BytePos, mut terminate: F) -> (BytePos, &'src str)
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

        let eof = BytePos(self.src.len());
        (eof, self.slice(start, eof))
    }

    /// Consume a doc comment
    fn doc_comment(&mut self, start: BytePos) -> (BytePos, Token<'src>, BytePos) {
        let (end, mut comment) = self.take_until(start.map(|x| x + 3), |ch| ch == '\n');

        // Skip preceding space
        if comment.starts_with(' ') {
            comment = &comment[1..];
        }

        (start, Token::DocComment(comment), end)
    }

    /// Consume an identifier token
    fn ident(&mut self, start: BytePos) -> (BytePos, Token<'src>, BytePos) {
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
    fn literal_suffix(&mut self, start: BytePos) -> (BytePos, &'src str) {
        if self.test_lookahead(is_ident_start) {
            self.bump(); // skip ident start
            self.take_while(start, is_ident_continue)
        } else {
            (start, "")
        }
    }

    /// Consume a binary literal token
    fn bin_literal(&mut self, start: BytePos) -> Result<(BytePos, Token<'src>, BytePos), Error> {
        self.bump(); // skip 'b'
        let (end, src) = self.take_while(start.map(|x| x + 2), is_bin_digit);
        if src.is_empty() {
            error(start, ErrorCode::ExpectedBinLiteral)
        } else {
            let int = u64::from_str_radix(src, 2).unwrap();
            let (end, suffix) = self.literal_suffix(end);

            Ok((start, Token::BinLiteral(int, suffix), end))
        }
    }

    /// Consume a hexidecimal literal token
    fn hex_literal(&mut self, start: BytePos) -> Result<(BytePos, Token<'src>, BytePos), Error> {
        self.bump(); // skip 'x'
        let (end, src) = self.take_while(start.map(|x| x + 2), is_hex_digit);
        if src.is_empty() {
            error(start, ErrorCode::ExpectedHexLiteral)
        } else {
            let int = u64::from_str_radix(src, 16).unwrap();
            let (end, suffix) = self.literal_suffix(end);

            Ok((start, Token::HexLiteral(int, suffix), end))
        }
    }

    /// Consume a decimal literal token
    fn dec_literal(&mut self, start: BytePos) -> (BytePos, Token<'src>, BytePos) {
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

impl<'src> Iterator for Lexer<'src> {
    type Item = Result<(BytePos, Token<'src>, BytePos), Error>;

    fn next(&mut self) -> Option<Result<(BytePos, Token<'src>, BytePos), Error>> {
        while let Some((start, ch)) = self.bump() {
            let end = start.map(|x| x + 1);

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
                        _ => error(start, ErrorCode::UnexpectedCharacter),
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
                _ => error(start, ErrorCode::UnexpectedCharacter),
            });
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A handy macro to give us a nice syntax for declaring test cases
    ///
    /// This was inspired by the tests in the LALRPOP lexer
    macro_rules! test {
        ($src:expr, $($span:expr => $token:expr,)*) => {{
            let lexed_tokens: Vec<_> = Lexer::new($src).collect();
            let expected_tokens = vec![$({
                let start = BytePos($span.find("~").unwrap());
                let end = BytePos($span.rfind("~").unwrap() + 1);
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
