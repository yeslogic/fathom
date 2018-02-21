use codespan::FileMap;

use self::lexer::Lexer;
use syntax::concrete;

mod lexer;
mod grammar;

mod errors;
#[cfg(test)]
mod tests;

pub use self::lexer::{LexerError, Token};
pub use self::errors::{ExpectedTokens, ParseError};

fn extend_vec<I: IntoIterator>(mut vec: Vec<I::Item>, last: I) -> Vec<I::Item> {
    vec.extend(last);
    vec
}

/// Attempt to parse a module from a source string
pub fn module<'input>(filemap: &'input FileMap) -> Result<concrete::Module<'input>, ParseError> {
    grammar::parse_Module(Lexer::new(filemap).map(|x| x.map_err(ParseError::from)))
        .map_err(|err| errors::from_lalrpop(filemap, err))
}

/// Attempt to parse a definition from a source string
pub fn definition<'input>(
    filemap: &'input FileMap,
) -> Result<concrete::Definition<'input>, ParseError> {
    grammar::parse_Definition(Lexer::new(filemap).map(|x| x.map_err(ParseError::from)))
        .map_err(|err| errors::from_lalrpop(filemap, err))
}

/// Attempt to parse a type from a source string
pub fn ty<'input>(filemap: &'input FileMap) -> Result<concrete::Type<'input>, ParseError> {
    grammar::parse_Type(Lexer::new(filemap).map(|x| x.map_err(ParseError::from)))
        .map_err(|err| errors::from_lalrpop(filemap, err))
}

/// Attempt to parse an expression from a source string
pub fn expr<'input>(filemap: &'input FileMap) -> Result<concrete::Expr<'input>, ParseError> {
    grammar::parse_Expr(Lexer::new(filemap).map(|x| x.map_err(ParseError::from)))
        .map_err(|err| errors::from_lalrpop(filemap, err))
}
