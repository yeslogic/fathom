pub mod ast;
mod lexer;
mod grammar;

mod errors;
#[cfg(test)]
mod tests;

pub use self::lexer::{LexerError, Token};
pub use self::errors::{ExpectedTokens, ParseError};

use self::lexer::Lexer;

fn extend_vec<I: IntoIterator>(mut vec: Vec<I::Item>, last: I) -> Vec<I::Item> {
    vec.extend(last);
    vec
}

/// Attempt to parse a module from a source string
pub fn module<'input>(src: &'input str) -> Result<ast::Module<'input>, ParseError> {
    grammar::parse_Module(Lexer::new(src).map(|x| x.map_err(ParseError::from)))
        .map_err(|err| errors::from_lalrpop(src, err))
}

/// Attempt to parse a definition from a source string
pub fn definition<'input>(src: &'input str) -> Result<ast::Definition<'input>, ParseError> {
    grammar::parse_Definition(Lexer::new(src).map(|x| x.map_err(ParseError::from)))
        .map_err(|err| errors::from_lalrpop(src, err))
}

/// Attempt to parse a type from a source string
pub fn ty<'input>(src: &'input str) -> Result<ast::Type<'input>, ParseError> {
    grammar::parse_Type(Lexer::new(src).map(|x| x.map_err(ParseError::from)))
        .map_err(|err| errors::from_lalrpop(src, err))
}

/// Attempt to parse an expression from a source string
pub fn expr<'input>(src: &'input str) -> Result<ast::Expr<'input>, ParseError> {
    grammar::parse_Expr(Lexer::new(src).map(|x| x.map_err(ParseError::from)))
        .map_err(|err| errors::from_lalrpop(src, err))
}
