pub mod ast;
mod lexer;
mod grammar;

mod errors;
#[cfg(test)]
mod tests;

pub use self::lexer::{LexerError, Token};
pub use self::errors::{ExpectedTokens, ParseError};
pub(crate) use self::errors::from_lalrpop;

fn extend_vec<I: IntoIterator>(mut vec: Vec<I::Item>, last: I) -> Vec<I::Item> {
    vec.extend(last);
    vec
}
