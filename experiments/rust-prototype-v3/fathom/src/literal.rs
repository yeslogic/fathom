//! Decoding of [literals] in the surface language into Rust datatypes.
//!
//! [literals]: https://en.wikipedia.org/wiki/Literal_%28computer_programming%29

use logos::Logos;
use num_bigint::BigInt;
use num_traits::Float;

use crate::lang::Location;
use crate::reporting::LiteralParseMessage::*;
use crate::reporting::Message;

/// The sign of a numeric literal.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Sign {
    Positive,
    Negative,
}

/// The [base] of a numeric digit.
///
/// [base]: https://en.wikipedia.org/wiki/Radix
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Base {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
}

impl Base {
    pub fn to_u8(self) -> u8 {
        match self {
            Base::Binary => 2,
            Base::Octal => 8,
            Base::Decimal => 10,
            Base::Hexadecimal => 16,
        }
    }
}

/// Convert the first byte of the source string to a digit.
fn ascii_digit<'source, Token>(lexer: &mut logos::Lexer<'source, Token>) -> Option<u8>
where
    Token: Logos<'source, Source = [u8]>,
{
    match lexer.slice().first()? {
        byte @ b'0'..=b'9' => Some(byte - b'0'),
        byte @ b'a'..=b'z' => Some(byte - b'a' + 10),
        byte @ b'A'..=b'Z' => Some(byte - b'A' + 10),
        _ => None,
    }
}

/// Numeric literal tokens.
#[derive(Debug, Clone, Logos)]
enum NumericLiteral {
    #[token(b"+", |_| Sign::Positive)]
    #[token(b"-", |_| Sign::Negative)]
    Sign(Sign),
    #[token(b"0b", |_| Base::Binary)]
    #[token(b"0o", |_| Base::Octal)]
    #[token(b"0x", |_| Base::Hexadecimal)]
    Base(Base),
    #[regex(b"[0-9]", ascii_digit)]
    Digit(u8),

    #[error]
    Error,
}

/// Digits up to base 32.
#[derive(Debug, Clone, Logos)]
enum Digit36 {
    #[regex(b"[0-9a-zA-Z]", ascii_digit)]
    Digit(u8),
    #[regex(b"_+")]
    Separator,

    #[error]
    Error,
}

/// Digits up to base 10.
#[derive(Debug, Clone, Logos)]
enum Digit10 {
    #[regex(b"[0-9]", ascii_digit)]
    Digit(u8),
    #[regex(b"_+")]
    Separator,
    #[token(b".")]
    StartFractional,
    #[token(b"e")]
    #[token(b"E")]
    StartExponent,

    #[error]
    Error,
}

/// Literal parser state.
pub struct State<'source, 'messages> {
    location: Location,
    source: &'source str,
    messages: &'messages mut Vec<Message>,
}

impl<'source, 'messages> State<'source, 'messages> {
    pub fn new(
        location: Location,
        source: &'source str,
        messages: &'messages mut Vec<Message>,
    ) -> State<'source, 'messages> {
        State {
            location,
            source,
            messages,
        }
    }

    /// Report a diagnostic message.
    fn report<T>(&mut self, error: impl Into<Message>) -> Option<T> {
        self.messages.push(error.into());
        None
    }

    /// Get the file-relative location of the current token.
    fn token_location<Token>(&self, lexer: &logos::Lexer<'source, Token>) -> Location
    where
        Token: Logos<'source>,
    {
        match self.location {
            Location::Generated => Location::Generated,
            Location::FileRange(file_id, range) => {
                let span = lexer.span();
                let range = (range.start + span.start)..(range.start + span.end);
                Location::file_range(file_id, range)
            }
        }
    }

    /// Expect another token to be present in the lexer, reporting an error if not.
    ///
    /// # Returns
    ///
    /// - `Some(_)`: If another token was found in the source stream
    /// - `None`: If we reached the end of the source stream and we need to terminate parsing
    fn expect_token<Token: Logos<'source>>(
        &mut self,
        lexer: &mut logos::Lexer<'source, Token>,
    ) -> Option<Token> {
        match lexer.next() {
            Some(token) => Some(token),
            None => {
                let location = self.token_location(&lexer);
                self.report(UnexpectedEndOfLiteral(location))
            }
        }
    }

    /// Parse a numeric literal into a big integer.
    ///
    /// # Returns
    ///
    /// - `Some(_)`: If the literal was parsed correctly.
    /// - `None`: If a fatal error when parsing the literal.
    pub fn number_to_big_int(mut self) -> Option<BigInt> {
        let mut lexer = NumericLiteral::lexer(self.source.as_bytes());

        let (sign, base, start_digit) = self.expect_numeric_literal_start(&mut lexer)?;

        let add_digit = |sign, base: Base, integer: &mut BigInt, digit: u8| {
            *integer *= base.to_u8();
            match sign {
                Sign::Positive => *integer += digit,
                Sign::Negative => *integer -= digit,
            }
        };

        let mut lexer = lexer.morph();
        let mut integer = BigInt::from(0);
        let mut num_digits = 0;

        if let Some(digit) = start_digit {
            add_digit(sign, base, &mut integer, digit);
            num_digits += 1;
        }

        while let Some(token) = lexer.next() {
            let location = self.token_location(&lexer);
            match token {
                Digit36::Digit(digit) if digit < base.to_u8() => {
                    add_digit(sign, base, &mut integer, digit);
                    num_digits += 1;
                }
                Digit36::Separator => match num_digits {
                    0 => return self.report(ExpectedDigit(location, base)),
                    _ => {}
                },
                Digit36::Digit(_) | Digit36::Error => match num_digits {
                    0 => return self.report(ExpectedDigit(location, base)),
                    _ => return self.report(ExpectedDigitOrSeparator(location, base)),
                },
            }
        }

        if num_digits == 0 {
            return self.report(UnexpectedEndOfLiteral(self.token_location(&lexer)));
        }

        Some(integer)
    }

    /// Parse a numeric literal into a float.
    ///
    /// # Returns
    ///
    /// - `Some(_)`: If the literal was parsed correctly.
    /// - `None`: If a fatal error when parsing the literal.
    pub fn number_to_float<T: Float + From<u8>>(mut self) -> Option<T> {
        // NOTE: This could probably be improved a great deal.
        // It might be worth looking at `lexical-core` crate as an alternative
        // to implementing our own parser: https://github.com/Alexhuszagh/rust-lexical/

        let mut lexer = NumericLiteral::lexer(self.source.as_bytes());

        let add_digit = |sign, base: Base, float: T, digit: u8| match sign {
            Sign::Positive => float * base.to_u8().into() + digit.into(),
            Sign::Negative => float * base.to_u8().into() - digit.into(),
        };

        let (sign, base, start_digit) = self.expect_numeric_literal_start(&mut lexer)?;

        let mut float = T::zero();
        let mut num_integer_digits = 0;

        if let Some(digit) = start_digit {
            float = add_digit(sign, base, float, digit);
            num_integer_digits += 1;
        }

        if base == Base::Decimal {
            let mut lexer = lexer.morph();
            let mut has_fractional = false;
            let mut has_exponent = false;

            while let Some(token) = lexer.next() {
                let location = self.token_location(&lexer);
                match token {
                    Digit10::Digit(digit) if digit < base.to_u8() => {
                        float = add_digit(sign, base, float, digit);
                        num_integer_digits += 1;
                    }
                    Digit10::Separator if num_integer_digits != 0 => {}
                    Digit10::Separator => return self.report(ExpectedDigit(location, base)),
                    Digit10::StartFractional => {
                        has_fractional = true;
                        break;
                    }
                    Digit10::StartExponent => {
                        has_exponent = true;
                        break;
                    }
                    Digit10::Digit(_) | Digit10::Error => match num_integer_digits {
                        0 => return self.report(ExpectedDigit(location, base)),
                        _ => {
                            return self.report(ExpectedDigitSeparatorFracOrExp(location, base));
                        }
                    },
                }
            }

            if num_integer_digits == 0 {
                let location = self.token_location(&lexer);
                return self.report(ExpectedDigit(location, base));
            }

            if has_fractional {
                let mut frac = T::zero();
                let mut num_frac_digits = 0;

                while let Some(token) = lexer.next() {
                    let location = self.token_location(&lexer);
                    match token {
                        Digit10::Digit(digit) if digit < base.to_u8() => {
                            frac = add_digit(sign, base, frac, digit);
                            num_frac_digits += 1;
                        }
                        Digit10::Separator if num_frac_digits != 0 => {}
                        Digit10::Separator => {
                            return self.report(ExpectedDigit(location, base));
                        }
                        Digit10::StartExponent => {
                            has_exponent = true;
                            break;
                        }
                        Digit10::Digit(_) | Digit10::StartFractional | Digit10::Error => {
                            match num_frac_digits {
                                0 => return self.report(ExpectedDigit(location, base)),
                                _ => {
                                    return self.report(ExpectedDigitSeparatorOrExp(location, base))
                                }
                            }
                        }
                    }
                }

                if num_frac_digits == 0 {
                    return self.report(ExpectedDigit(self.token_location(&lexer), base));
                }

                float = float + frac / T::powi(base.to_u8().into(), num_frac_digits);
            }

            if has_exponent {
                let location = self.token_location(&lexer);
                return self.report(FloatLiteralExponentNotSupported(location));
            }

            Some(float)
        } else {
            self.report(UnsupportedFloatLiteralBase(self.location, base))
        }
    }

    fn expect_numeric_literal_start(
        &mut self,
        lexer: &mut logos::Lexer<'source, NumericLiteral>,
    ) -> Option<(Sign, Base, Option<u8>)> {
        match self.expect_token(lexer)? {
            NumericLiteral::Sign(sign) => match self.expect_token(lexer)? {
                NumericLiteral::Base(base) => Some((sign, base, None)),
                NumericLiteral::Digit(digit) => Some((sign, Base::Decimal, Some(digit))),
                NumericLiteral::Sign(_) | NumericLiteral::Error => {
                    let location = self.token_location(&lexer);
                    self.report(ExpectedRadixOrDecimalDigit(location))
                }
            },
            NumericLiteral::Base(base) => Some((Sign::Positive, base, None)),
            NumericLiteral::Digit(digit) => Some((Sign::Positive, Base::Decimal, Some(digit))),
            NumericLiteral::Error => {
                let location = self.token_location(&lexer);
                self.report(ExpectedStartOfNumericLiteral(location))
            }
        }
    }
}
