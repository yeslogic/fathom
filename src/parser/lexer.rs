use std::str::CharIndices;

use source::BytePos;
use unicode_xid::UnicodeXID;

fn is_ident_start(ch: char) -> bool {
    UnicodeXID::is_xid_start(ch) || ch == '_'
}

fn is_ident_continue(ch: char) -> bool {
    UnicodeXID::is_xid_continue(ch) || ch == '_'
}

fn is_digit(ch: char) -> bool {
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErrorCode {
    /// An unexpected token was encountered while lexing the source file
    UnrecognizedToken,
}

/// A token in the sorce file, to be emitted by the `Lexer`
///
/// This will then be passed to the parser in order to construct an abstract
/// syntax tree.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token<'input> {
    // Data
    Ident(&'input str),
    IntLiteral(u64),

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

    // Delimeters
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
        (start, Token::Ident(ident), end)
    }

    /// Consume an integer literal token
    fn int_literal(&mut self, start: BytePos) -> (BytePos, Token<'input>, BytePos) {
        let (end, int) = self.take_while(start, is_digit);
        (start, Token::IntLiteral(int.parse().unwrap()), end)
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<(BytePos, Token<'input>, BytePos), Error>;

    fn next(&mut self) -> Option<Result<(BytePos, Token<'input>, BytePos), Error>> {
        while let Some((start, ch)) = self.bump() {
            let end = start.map(|x| x + 1);

            return match ch {
                '=' => Some(Ok((start, Token::Equals, end))),
                ';' => Some(Ok((start, Token::Semi, end))),
                ',' => Some(Ok((start, Token::Comma, end))),
                '|' => Some(Ok((start, Token::Pipe, end))),
                ':' => Some(Ok((start, Token::Colon, end))),
                '+' => Some(Ok((start, Token::Plus, end))),
                '-' => Some(Ok((start, Token::Minus, end))),
                '*' => Some(Ok((start, Token::Star, end))),
                '/' if self.test_lookahead(|ch| ch == '/') => {
                    // Line comments
                    self.take_until(start, |ch| ch == '\n');
                    continue;
                }
                '/' => Some(Ok((start, Token::FSlash, end))),
                '(' => Some(Ok((start, Token::LParen, end))),
                ')' => Some(Ok((start, Token::RParen, end))),
                '{' => Some(Ok((start, Token::LBrace, end))),
                '}' => Some(Ok((start, Token::RBrace, end))),
                '[' => Some(Ok((start, Token::LBracket, end))),
                ']' => Some(Ok((start, Token::RBracket, end))),
                ch if is_digit(ch) => Some(Ok(self.int_literal(start))),
                ch if is_ident_start(ch) => Some(Ok(self.ident(start))),
                ch if ch.is_whitespace() => continue,
                _ => Some(Err(Error {
                    code: ErrorCode::UnrecognizedToken,
                    location: start,
                })),
            };
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
            "  u8  1234   ",
            "  ~~         " => Token::Ident("u8"),
            "      ~~~~   " => Token::IntLiteral(1234),
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
            "     ~~ " => Token::IntLiteral(34),
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
