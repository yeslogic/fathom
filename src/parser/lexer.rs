use std::str::CharIndices;

use source::BytePos;
use unicode_xid::UnicodeXID;

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
#[derive(Clone, Debug, PartialEq, Eq)]
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
    /// An unexpected token was encountered
    UnrecognizedToken,
    /// Expected a binary literal
    ExpectedBinLiteral,
    /// Expected a hexidecimal literal
    ExpectedHexLiteral,
}

/// A token in the source file, to be emitted by the `Lexer`
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token<'input> {
    // Data
    Ident(&'input str),
    BinLiteral(u64),
    HexLiteral(u64),
    DecLiteral(u64),

    // Keywords
    Struct,
    Union,

    // Symbols
    Equals, // =
    Semi, // ;
    Comma, // ,
    Pipe, // |
    Colon, // :
    Plus, // +
    Minus, // -
    Star, // *
    FSlash, // /

    // Delimiters
    LParen, // (
    RParen, // )
    LBrace, // {
    RBrace, // }
    LBracket, // [
    RBracket, // ]
}

/// An iterator over a source string that yeilds `Token`s for subsequent use by
/// the parser
pub struct Lexer<'input> {
    src: &'input str,
    chars: CharIndices<'input>,
    lookahead: Option<(usize, char)>,
}

impl<'input> Lexer<'input> {
    /// Create a new lexer from the source string
    pub fn new(src: &'input str) -> Self {
        let mut chars = src.char_indices();

        Lexer {
            src,
            lookahead: chars.next(),
            chars,
        }
    }

    /// Bump the current position in the source string by one character,
    /// returning the current character and byte position.
    fn bump(&mut self) -> Option<(BytePos, char)> {
        let current = self.lookahead;
        self.lookahead = self.chars.next();
        current.map(|(index, ch)| (BytePos(index), ch))
    }

    /// Return a slice of the source string
    fn slice(&self, start: BytePos, end: BytePos) -> &'input str {
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
    fn take_while<F>(&mut self, start: BytePos, mut keep_going: F) -> (BytePos, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        self.take_until(start, |ch| !keep_going(ch))
    }

    /// Consume characters until the predicate matches for the next character
    /// in the lookahead, then return the consumed slice and the end byte
    /// position.
    fn take_until<F>(&mut self, start: BytePos, mut terminate: F) -> (BytePos, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        while let Some((end, ch)) = self.lookahead {
            if terminate(ch) {
                return (BytePos(end), self.slice(start, BytePos(end)));
            } else {
                self.bump();
            }
        }

        let eof = BytePos(self.src.len());
        (eof, self.slice(start, eof))
    }

    /// Consume an identifier token
    fn ident(&mut self, start: BytePos) -> (BytePos, Token<'input>, BytePos) {
        let (end, ident) = self.take_while(start, is_ident_continue);

        let token = match ident {
            "struct" => Token::Struct,
            "union" => Token::Union,
            ident => Token::Ident(ident),
        };

        (start, token, end)
    }

    /// Consume a binary literal token
    fn bin_literal(&mut self, start: BytePos) -> Result<(BytePos, Token<'input>, BytePos), Error> {
        self.bump(); // skip 'b'
        let (end, src) = self.take_while(start.map(|x| x + 2), is_bin_digit);
        if src.is_empty() {
            error(start, ErrorCode::ExpectedBinLiteral)
        } else {
            let int = u64::from_str_radix(src, 2).unwrap();
            Ok((start, Token::BinLiteral(int), end))
        }
    }

    /// Consume a hexidecimal literal token
    fn hex_literal(&mut self, start: BytePos) -> Result<(BytePos, Token<'input>, BytePos), Error> {
        self.bump(); // skip 'x'
        let (end, src) = self.take_while(start.map(|x| x + 2), is_hex_digit);
        if src.is_empty() {
            error(start, ErrorCode::ExpectedHexLiteral)
        } else {
            let int = u64::from_str_radix(src, 16).unwrap();
            Ok((start, Token::HexLiteral(int), end))
        }
    }

    /// Consume a decimal literal token
    fn dec_literal(&mut self, start: BytePos) -> (BytePos, Token<'input>, BytePos) {
        let (end, src) = self.take_while(start, is_dec_digit);
        let int = u64::from_str_radix(src, 10).unwrap();
        (start, Token::DecLiteral(int), end)
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<(BytePos, Token<'input>, BytePos), Error>;

    fn next(&mut self) -> Option<Result<(BytePos, Token<'input>, BytePos), Error>> {
        while let Some((start, ch)) = self.bump() {
            let end = start.map(|x| x + 1);

            return Some(match ch {
                '=' => Ok((start, Token::Equals, end)),
                ';' => Ok((start, Token::Semi, end)),
                ',' => Ok((start, Token::Comma, end)),
                '|' => Ok((start, Token::Pipe, end)),
                ':' => Ok((start, Token::Colon, end)),
                '+' => Ok((start, Token::Plus, end)),
                '-' => Ok((start, Token::Minus, end)),
                '*' => Ok((start, Token::Star, end)),
                '/' if self.test_lookahead(|ch| ch == '/') => {
                    // Line comments
                    self.take_until(start, |ch| ch == '\n');
                    continue;
                }
                '/' => Ok((start, Token::FSlash, end)),
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
                _ => error(start, ErrorCode::UnrecognizedToken),
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
            "  u8  0b0100101  0x6Ffa6  1234   ",
            "  ~~                             " => Token::Ident("u8"),
            "      ~~~~~~~~~                  " => Token::BinLiteral(37),
            "                 ~~~~~~~         " => Token::HexLiteral(458662),
            "                          ~~~~   " => Token::DecLiteral(1234),
        };
    }

    #[test]
    fn keywords() {
        test! {
            "  struct  union  ",
            "  ~~~~~~         " => Token::Struct,
            "          ~~~~~  " => Token::Union,
        };
    }

    #[test]
    fn symbols() {
        test! {
            " = ; , | : + - * / ",
            " ~                 " => Token::Equals,
            "   ~               " => Token::Semi,
            "     ~             " => Token::Comma,
            "       ~           " => Token::Pipe,
            "         ~         " => Token::Colon,
            "           ~       " => Token::Plus,
            "             ~     " => Token::Minus,
            "               ~   " => Token::Star,
            "                 ~ " => Token::FSlash,
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
            "[u8; 34]",
            "~       " => Token::LBracket,
            " ~~     " => Token::Ident("u8"),
            "   ~    " => Token::Semi,
            "     ~~ " => Token::DecLiteral(34),
            "       ~" => Token::RBracket,
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
