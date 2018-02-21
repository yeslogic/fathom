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
pub fn module<'input>(filemap: &'input FileMap) -> (concrete::Module<'input>, Vec<ParseError>) {
    let mut errors = Vec::new();
    let lexer = Lexer::new(filemap).map(|x| x.map_err(ParseError::from));
    match grammar::parse_Module(&mut errors, filemap, lexer) {
        Ok(value) => (value, errors),
        Err(err) => {
            errors.push(errors::from_lalrpop(filemap, err));
            (concrete::Module::Error(filemap.span()), errors)
        }
    }
}

/// Attempt to parse a definition from a source string
pub fn definition<'input>(
    filemap: &'input FileMap,
) -> (concrete::Definition<'input>, Vec<ParseError>) {
    let mut errors = Vec::new();
    let lexer = Lexer::new(filemap).map(|x| x.map_err(ParseError::from));
    match grammar::parse_Definition(&mut errors, filemap, lexer) {
        Ok(value) => (value, errors),
        Err(err) => {
            errors.push(errors::from_lalrpop(filemap, err));
            (concrete::Definition::Error(filemap.span()), errors)
        }
    }
}

/// Attempt to parse a type from a source string
pub fn ty<'input>(filemap: &'input FileMap) -> (concrete::Type<'input>, Vec<ParseError>) {
    let mut errors = Vec::new();
    let lexer = Lexer::new(filemap).map(|x| x.map_err(ParseError::from));
    match grammar::parse_Type(&mut errors, filemap, lexer) {
        Ok(value) => (value, errors),
        Err(err) => {
            errors.push(errors::from_lalrpop(filemap, err));
            (concrete::Type::Error(filemap.span()), errors)
        }
    }
}

/// Attempt to parse an expression from a source string
pub fn expr<'input>(filemap: &'input FileMap) -> (concrete::Expr<'input>, Vec<ParseError>) {
    let mut errors = Vec::new();
    let lexer = Lexer::new(filemap).map(|x| x.map_err(ParseError::from));
    match grammar::parse_Expr(&mut errors, filemap, lexer) {
        Ok(value) => (value, errors),
        Err(err) => {
            errors.push(errors::from_lalrpop(filemap, err));
            (concrete::Expr::Error(filemap.span()), errors)
        }
    }
}
