#[doc(hidden)]
pub extern crate lazy_static;
#[doc(hidden)]
pub extern crate regex;
#[doc(hidden)]
pub extern crate smallvec;
extern crate thiserror;

pub mod lex;
pub mod parse;
#[cfg(test)]
mod test;
mod utils;

use crate::parse::FormattedParseError;
use crate::parse::Parseable;
use lex::{Token, TokenError, TokenValue};
use thiserror::Error;

/// Error type for the `parse` function.
#[non_exhaustive]
#[derive(Error, Debug)]
pub enum Error {
    #[error("{0}")]
    TokenError(#[from] TokenError),
    #[error("{0}")]
    ParseError(#[from] FormattedParseError),
}

pub fn parse<T: TokenValue, Grammar: Parseable<T>>(
    src: &str,
) -> Result<(Vec<Token<T>>, Grammar), Error> {
    let tokens = lex::tokenize(src)?;
    let ast = parse::build_ast::<T, Grammar>(&tokens).map_err(|e| e.formatted(&tokens, src))?;
    Ok((tokens, ast))
}
