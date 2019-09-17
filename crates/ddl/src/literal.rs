//! [Literals][literal-wikipedia] in the surface language.
//!
//! These will be translated into constants in the core language during
//! elaboration, once we know the type of the data we are translating to.
//!
//! [literal-wikipedia]: https://en.wikipedia.org/wiki/Literal_%28computer_programming%29

use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;
use num_bigint::BigInt;
use num_traits::{Float, Signed};
use std::fmt;

use crate::diagnostics;

#[derive(Debug, Copy, Clone)]
pub enum Sign {
    Positive,
    Negative,
}

impl fmt::Display for Sign {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Sign::Positive => write!(f, "+"),
            Sign::Negative => write!(f, "-"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
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

impl Default for Base {
    fn default() -> Base {
        Base::Decimal
    }
}

impl fmt::Display for Base {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Base::Binary => write!(f, "0b"),
            Base::Octal => write!(f, "0o"),
            Base::Decimal => Ok(()),
            Base::Hexadecimal => write!(f, "0x"),
        }
    }
}

/// Numeric literals.
#[derive(Debug, Clone)]
pub struct Number {
    pub sign: Option<Sign>,
    pub number: (Span, std::string::String),
}

impl Number {
    pub fn new(sign: Option<Sign>, number: (Span, std::string::String)) -> Number {
        Number { sign, number }
    }

    pub fn from_signed(span: Span, value: &(impl Signed + fmt::Display)) -> Number {
        let sign = if value.is_negative() {
            Some(Sign::Negative)
        } else {
            None
        };

        Number::new(sign, (span, value.abs().to_string()))
    }

    pub fn sign(&self) -> Sign {
        self.sign.unwrap_or(Sign::Positive)
    }

    fn char_spans<'a>(&'a self) -> impl Iterator<Item = (Span, char)> + 'a {
        use codespan::ByteOffset;

        (self.number.1.chars()).scan(self.number.0.start(), |current_byte, ch| {
            let start_byte = *current_byte;
            *current_byte += ByteOffset::from_char_len(ch);
            let span = Span::new(start_byte, *current_byte);
            Some((span, ch))
        })
    }

    fn parse_base_or_digit(
        &self,
        file_id: FileId,
        chars: &mut impl Iterator<Item = (Span, char)>,
        report: &mut dyn FnMut(Diagnostic),
    ) -> (Base, Option<u8>) {
        match chars.next() {
            Some((_, '0')) => match chars.next() {
                Some((_, 'b')) => (Base::Binary, Some(0)),
                Some((_, 'o')) => (Base::Octal, Some(0)),
                Some((_, 'x')) => (Base::Hexadecimal, Some(0)),
                Some((span, ch @ 'A'..='F')) => (
                    Base::Decimal,
                    Number::parse_digit(file_id, Base::Decimal, span, ch, report),
                ),
                Some((span, ch)) => {
                    report(diagnostics::bug::not_yet_implemented(
                        file_id,
                        span,
                        "invalid literal base error",
                    ));
                    (Base::Decimal, None)
                }
                None => (Base::Decimal, Some(0)),
            },
            Some((span, ch)) => (
                Base::Decimal,
                Number::parse_digit(file_id, Base::Decimal, span, ch, report),
            ),
            None => (Base::Decimal, None),
        }
    }

    fn parse_digit(
        file_id: FileId,
        base: Base,
        ch_span: Span,
        ch: char,
        report: &mut dyn FnMut(Diagnostic),
    ) -> Option<u8> {
        match (base, ch) {
            (Base::Binary, '0'..='1') => Some(ch as u8 - '0' as u8),
            (Base::Octal, '0'..='7') => Some(ch as u8 - '0' as u8),
            (Base::Decimal, '0'..='9') => Some(ch as u8 - '0' as u8),
            (Base::Hexadecimal, '0'..='9') => Some(ch as u8 - '0' as u8),
            (Base::Hexadecimal, 'a'..='f') => Some(ch as u8 - 'a' as u8 + 10),
            (Base::Hexadecimal, 'A'..='F') => Some(ch as u8 - 'A' as u8 + 10),
            (_, _) => {
                report(diagnostics::bug::not_yet_implemented(
                    file_id,
                    ch_span,
                    "invalid literal digit error",
                ));
                None
            }
        }
    }

    pub fn parse_big_int(
        &self,
        file_id: FileId,
        report: &mut dyn FnMut(Diagnostic),
    ) -> Option<BigInt> {
        let mut chars = self.char_spans();

        let (base, initial_value) = self.parse_base_or_digit(file_id, &mut chars, report);
        let mut value = initial_value.map(|v| match self.sign() {
            Sign::Positive => BigInt::from(v),
            Sign::Negative => -BigInt::from(v),
        });

        while let Some((span, ch)) = chars.next() {
            let digit = match ch {
                '_' => continue,
                ch => match Number::parse_digit(file_id, base, span, ch, report) {
                    Some(digit) => digit,
                    None => continue,
                },
            };

            if let Some(value) = &mut value {
                *value *= base.to_u8();
                match self.sign() {
                    Sign::Positive => *value += digit,
                    Sign::Negative => *value -= digit,
                }
            }
        }

        value
    }

    pub fn parse_float<T>(&self, file_id: FileId, report: &mut dyn FnMut(Diagnostic)) -> Option<T>
    where
        T: Float + From<u8> + std::ops::MulAssign + std::ops::AddAssign + std::ops::SubAssign,
    {
        let mut chars = self.char_spans();

        let (base, initial_value) = self.parse_base_or_digit(file_id, &mut chars, report);
        let mut value = initial_value.map(|v| match self.sign() {
            Sign::Positive => <T as From<u8>>::from(v),
            Sign::Negative => -<T as From<u8>>::from(v),
        });

        while let Some((span, ch)) = chars.next() {
            let digit = match ch {
                '_' => continue,
                '.' => panic!("fractional part"),
                ch => match Number::parse_digit(file_id, base, span, ch, report) {
                    Some(digit) => digit,
                    None => continue,
                },
            };

            if let Some(value) = &mut value {
                *value *= base.to_u8().into();
                match self.sign() {
                    Sign::Positive => *value += digit.into(),
                    Sign::Negative => *value -= digit.into(),
                }
            }
        }

        // TODO: parse fractional part
        // TODO: parse exponent

        match value? {
            value if value.is_infinite() => {
                report(diagnostics::bug::not_yet_implemented(
                    file_id,
                    self.number.0,
                    "overflow error",
                ));
                None
            }
            value => Some(value),
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.sign {
            None => write!(f, "{}", self.number.1),
            Some(sign) => write!(f, "{}{}", sign, self.number.1),
        }
    }
}

/// String literals.
#[derive(Debug, Clone)]
pub struct String {
    pub contents: (Span, std::string::String),
}

impl String {
    pub fn to_string(&self) -> Result<std::string::String, ()> {
        unimplemented!()
    }

    pub fn to_bytes(&self) -> Result<Vec<u8>, ()> {
        unimplemented!()
    }
}

impl fmt::Display for String {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.contents.1)
    }
}

/// Character literals.
#[derive(Debug, Clone)]
pub struct Char {
    pub contents: (Span, std::string::String),
}

impl Char {
    pub fn to_char(&self) -> Result<char, ()> {
        unimplemented!()
    }

    pub fn to_u8(&self) -> Result<u8, ()> {
        unimplemented!()
    }
}

impl fmt::Display for Char {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\'{}\'", self.contents.1)
    }
}
