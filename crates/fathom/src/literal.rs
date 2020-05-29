//! [Literals][literal-wikipedia] in the surface language.
//!
//! These will be translated into constants in the core language during
//! elaboration, once we know the type of the data we are translating to.
//!
//! [literal-wikipedia]: https://en.wikipedia.org/wiki/Literal_%28computer_programming%29

use codespan_reporting::diagnostic::Diagnostic;
use num_bigint::BigInt;
use num_traits::{Float, Signed};
use std::fmt;
use std::ops::Range;

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

/// An action to take after advancing a state machine
enum Action<State, Output> {
    Yield(State),
    Return(Output),
}

/// The state of an integer literal lexer.
enum IntegerLexerState {
    Top,
    ZeroOrBase,
    IntegerPart(Base, BigInt, u16),
}

impl IntegerLexerState {
    fn on_char(
        self,
        file_id: usize,
        ch: Option<(usize, char)>,
        report: &mut dyn FnMut(Diagnostic<usize>),
    ) -> Action<IntegerLexerState, Option<BigInt>> {
        use self::Action::{Return, Yield};
        use self::Base::*;
        use self::IntegerLexerState::*;

        match (self, ch) {
            (Top, Some((start, ch))) => match ch {
                '0' => Yield(ZeroOrBase),
                '1'..='9' => Yield(IntegerPart(Decimal, (ch as u8 - b'0').into(), 0)),
                _ => {
                    // TODO: bug?
                    report(diagnostics::error::unexpected_char(
                        file_id,
                        start,
                        ch,
                        &["decimal digit"],
                    ));
                    Return(None)
                }
            },
            (Top, None) => unimplemented!("bug: no characters in literal"),

            (ZeroOrBase, Some((start, ch))) => match ch {
                'b' => Yield(IntegerPart(Binary, 0.into(), 0)),
                'o' => Yield(IntegerPart(Octal, 0.into(), 0)),
                'x' => Yield(IntegerPart(Hexadecimal, 0.into(), 0)),
                '_' => Yield(IntegerPart(Decimal, 0.into(), 0)),
                '0'..='9' => Yield(IntegerPart(Decimal, (ch as u8 - b'0').into(), 2)),
                _ => {
                    report(diagnostics::error::unexpected_char(
                        file_id,
                        start,
                        ch,
                        &["digit", "base character"],
                    ));
                    Return(None)
                }
            },
            (ZeroOrBase, None) => Return(Some(0.into())),

            (IntegerPart(base, mut value, digits), Some((start, ch))) => {
                let digit = match (base, ch) {
                    (base, '_') => return Yield(IntegerPart(base, value, digits)),
                    (Binary, '0'..='1') => ch as u8 - b'0',
                    (Octal, '0'..='7') => ch as u8 - b'0',
                    (Decimal, '0'..='9') => ch as u8 - b'0',
                    (Hexadecimal, '0'..='9') => ch as u8 - b'0',
                    (Hexadecimal, 'a'..='f') => ch as u8 - b'a' + 10,
                    (Hexadecimal, 'A'..='F') => ch as u8 - b'A' + 10,
                    (_, _) => {
                        report(diagnostics::error::unexpected_char(
                            file_id,
                            start,
                            ch,
                            &["digit", "digit separator", "point", "exponent"],
                        ));
                        return Return(None);
                    }
                };

                value *= base.to_u8();
                value += digit;
                Yield(IntegerPart(base, value, digits + 1))
            }
            (IntegerPart(_, value, _), None) => Return(Some(value)), // TODO: Check num digits
        }
    }
}

/// The state of an floating point literal lexer.
enum FloatLexerState<T> {
    Top,
    ZeroOrBase,
    IntegerPart(Base, T, u16),
    FractionalPart(Base, T, T, u16),
    Exponent(T, u16, u16),
}

impl<T> FloatLexerState<T>
where
    T: Float + From<u8> + std::ops::MulAssign + std::ops::AddAssign + std::ops::SubAssign,
{
    fn on_char(
        self,
        file_id: usize,
        ch: Option<(usize, char)>,
        report: &mut dyn FnMut(Diagnostic<usize>),
    ) -> Action<FloatLexerState<T>, Option<T>> {
        use self::Action::{Return, Yield};
        use self::Base::*;
        use self::FloatLexerState::*;

        match (self, ch) {
            (Top, Some((start, ch))) => match ch {
                '0' => Yield(ZeroOrBase),
                '1'..='9' => Yield(IntegerPart(Base::Decimal, (ch as u8 - b'0').into(), 1)),
                _ => {
                    // TODO: bug?
                    report(diagnostics::error::unexpected_char(
                        file_id,
                        start,
                        ch,
                        &["decimal digit"],
                    ));
                    Return(None)
                }
            },
            (Top, None) => unimplemented!("bug: no characters in literal"),

            (ZeroOrBase, Some((start, ch))) => match ch {
                'b' => Yield(IntegerPart(Binary, 0.into(), 0)),
                'o' => Yield(IntegerPart(Octal, 0.into(), 0)),
                'x' => Yield(IntegerPart(Hexadecimal, 0.into(), 0)),
                '_' => Yield(IntegerPart(Decimal, 0.into(), 0)),
                '0'..='9' => Yield(IntegerPart(Decimal, (ch as u8 - b'0').into(), 2)),
                '.' => Yield(FractionalPart(Decimal, 0.into(), 0.into(), 0)),
                ch if ['e', 'E'].contains(&ch) => Yield(Exponent(0.into(), 0, 0)),
                ch if ['p', 'P'].contains(&ch) => Yield(Exponent(0.into(), 0, 0)),
                _ => {
                    report(diagnostics::error::unexpected_char(
                        file_id,
                        start,
                        ch,
                        &["digit", "base character"],
                    ));
                    Return(None)
                }
            },
            (ZeroOrBase, None) => Return(Some(0.into())),

            (IntegerPart(base, mut value, digits), Some((start, ch))) => {
                let digit = match (base, ch) {
                    (base, '_') => return Yield(IntegerPart(base, value, digits)),
                    (Binary, '0'..='1') => ch as u8 - b'0',
                    (Octal, '0'..='7') => ch as u8 - b'0',
                    (Decimal, '0'..='9') => ch as u8 - b'0',
                    (Hexadecimal, '0'..='9') => ch as u8 - b'0',
                    (Hexadecimal, 'a'..='f') => ch as u8 - b'a' + 10,
                    (Hexadecimal, 'A'..='F') => ch as u8 - b'A' + 10,
                    (base, '.') => return Yield(FractionalPart(base, value, 0.into(), 0)), // TODO: Check num digits
                    (Decimal, ch) if ['e', 'E'].contains(&ch) => {
                        // TODO: Check num digits
                        return Yield(Exponent(value, 0, 0));
                    }
                    (Binary, ch) | (Octal, ch) | (Hexadecimal, ch) if ['p', 'P'].contains(&ch) => {
                        // TODO: Check num digits
                        return Yield(Exponent(value, 0, 0));
                    }
                    (_, _) => {
                        report(diagnostics::error::unexpected_char(
                            file_id,
                            start,
                            ch,
                            &["digit", "digit separator", "point", "exponent"],
                        ));
                        return Return(None);
                    }
                };

                value *= base.to_u8().into();
                value += digit.into();
                Yield(IntegerPart(base, value, digits + 1))
            }
            (IntegerPart(_, value, _), None) => Return(Some(value)), // TODO: Check num digits

            (FractionalPart(base, value, mut frac, digits), Some((start, ch))) => {
                let digit = match (base, ch) {
                    (base, '_') => return Yield(FractionalPart(base, value, frac, digits)),
                    (Binary, '0'..='1') => ch as u8 - b'0',
                    (Octal, '0'..='7') => ch as u8 - b'0',
                    (Decimal, '0'..='9') => ch as u8 - b'0',
                    (Hexadecimal, '0'..='9') => ch as u8 - b'0',
                    (Hexadecimal, 'a'..='f') => ch as u8 - b'a' + 10,
                    (Hexadecimal, 'A'..='F') => ch as u8 - b'A' + 10,
                    (Decimal, ch) if ['e', 'E'].contains(&ch) => {
                        // TODO: Check num digits
                        let frac = frac / T::powi(base.to_u8().into(), digits.into());
                        return Yield(Exponent(value + frac, 0, 0));
                    }
                    (Binary, ch) | (Octal, ch) | (Hexadecimal, ch) if ['p', 'P'].contains(&ch) => {
                        // TODO: Check num digits
                        let frac = frac / T::powi(base.to_u8().into(), digits.into());
                        return Yield(Exponent(value + frac, 0, 0));
                    }
                    (_, _) => {
                        report(diagnostics::error::unexpected_char(
                            file_id,
                            start,
                            ch,
                            &["digit", "digit separator", "exponent"],
                        ));
                        return Return(None);
                    }
                };

                frac *= base.to_u8().into();
                frac += digit.into();
                Yield(FractionalPart(base, value, frac, digits + 1))
            }
            (FractionalPart(base, value, frac, digits), None) => {
                // TODO: Check num digits
                let frac = frac / T::powi(base.to_u8().into(), digits.into());
                Return(Some(value + frac))
            }
            (Exponent(value, exp, digits), Some((start, ch))) => match ch {
                // TODO: + or -
                '_' => Yield(Exponent(value, exp, digits)),
                '0'..='9' => {
                    let exp = exp + (ch as u8 - b'0') as u16;
                    Yield(Exponent(value, exp, digits + 1))
                }
                _ => {
                    report(diagnostics::error::unexpected_char(
                        file_id,
                        start,
                        ch,
                        &["decimal digit"],
                    ));
                    Return(None)
                }
            },
            (Exponent(value, exp, _), None) => Return(Some(value * T::powi(10.into(), exp as i32))), // TODO: Check num digits
        }
    }
}

/// Numeric literals.
#[derive(Debug, Clone)]
pub struct Number {
    pub sign: Option<Sign>,
    pub number: (Range<usize>, std::string::String),
}

impl Number {
    pub fn new(sign: Option<Sign>, number: (Range<usize>, std::string::String)) -> Number {
        Number { sign, number }
    }

    pub fn from_signed(range: Range<usize>, value: &(impl Signed + fmt::Display)) -> Number {
        let sign = if value.is_negative() {
            Some(Sign::Negative)
        } else {
            None
        };

        Number::new(sign, (range, value.abs().to_string()))
    }

    pub fn sign(&self) -> Sign {
        self.sign.unwrap_or(Sign::Positive)
    }

    fn chars<'a>(&'a self) -> impl Iterator<Item = (usize, char)> + 'a {
        (self.number.1.chars()).scan(self.number.0.start, |current, ch| {
            let start = *current;
            *current += ch.len_utf8();
            Some((start, ch))
        })
    }

    pub fn parse_big_int(
        &self,
        file_id: usize,
        report: &mut dyn FnMut(Diagnostic<usize>),
    ) -> Option<BigInt> {
        let mut chars = self.chars();
        let mut state = IntegerLexerState::Top;

        loop {
            state = match state.on_char(file_id, chars.next(), report) {
                Action::Yield(next) => next,
                Action::Return(value) => match self.sign() {
                    Sign::Positive => return value,
                    Sign::Negative => return value.map(|v| -v),
                },
            }
        }
    }

    pub fn parse_float<T>(
        &self,
        file_id: usize,
        report: &mut dyn FnMut(Diagnostic<usize>),
    ) -> Option<T>
    where
        T: Float + From<u8> + std::ops::MulAssign + std::ops::AddAssign + std::ops::SubAssign,
    {
        let mut chars = self.chars();
        let mut state = FloatLexerState::Top;

        loop {
            state = match state.on_char(file_id, chars.next(), report) {
                Action::Yield(next) => next,
                // TODO: Check infinity
                Action::Return(value) => match self.sign() {
                    Sign::Positive => return value,
                    Sign::Negative => return value.map(|v| -v),
                },
            }
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
    pub contents: (Range<usize>, std::string::String),
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
    pub contents: (Range<usize>, std::string::String),
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
