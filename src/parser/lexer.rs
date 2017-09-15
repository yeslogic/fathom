use std::str::CharIndices;

use source::BytePos;
use unicode_xid::UnicodeXID;

fn is_ident_start(c: char) -> bool {
    UnicodeXID::is_xid_start(c) || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    UnicodeXID::is_xid_continue(c) || c == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_digit(10)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    pub location: BytePos,
    pub code: ErrorCode,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErrorCode {
    UnrecognizedToken,
}

fn error<T>(code: ErrorCode, location: BytePos) -> Result<T, Error> {
    Err(Error { location, code })
}

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

pub struct Lexer<'input> {
    src: &'input str,
    chars: CharIndices<'input>,
    lookahead: Option<(usize, char)>,
}

impl<'input> Lexer<'input> {
    pub fn new(src: &'input str) -> Self {
        let mut chars = src.char_indices();

        Lexer {
            src,
            lookahead: chars.next(),
            chars,
        }
    }

    fn bump(&mut self) -> Option<(BytePos, char)> {
        let current = self.lookahead;
        self.lookahead = self.chars.next();
        current.map(|(index, ch)| (BytePos(index), ch))
    }

    fn slice(&self, start: BytePos, end: BytePos) -> &'input str {
        &self.src[start.0..end.0]
    }

    fn test_lookahead<F>(&self, mut pred: F) -> bool
    where
        F: FnMut(char) -> bool,
    {
        self.lookahead.map_or(false, |(_, ch)| pred(ch))
    }

    fn take_while<F>(&mut self, start: BytePos, mut keep_going: F) -> (BytePos, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        self.take_until(start, |ch| !keep_going(ch))
    }

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

    fn ident(&mut self, start: BytePos) -> (BytePos, Token<'input>, BytePos) {
        let (end, ident) = self.take_while(start, is_ident_continue);
        (start, Token::Ident(ident), end)
    }

    fn int_literal(&mut self, start: BytePos) -> (BytePos, Token<'input>, BytePos) {
        let (end, int) = self.take_while(start, is_digit);
        (start, Token::IntLiteral(int.parse().unwrap()), end)
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<(BytePos, Token<'input>, BytePos), Error>;

    fn next(&mut self) -> Option<Result<(BytePos, Token<'input>, BytePos), Error>> {
        loop {
            return match self.bump() {
                Some((start, ch)) => {
                    let end = start.map(|x| x + 1);
                    match ch {
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
                        _ => Some(error(ErrorCode::UnrecognizedToken, start)),
                    }
                }
                None => None,
            };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn ident() {
        test! {
            r#"  u8   "#,
            r#"  ~~   "# => Token::Ident("u8"),
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
