//! A library for parsing type-safe context-free grammars with arbitrary lookahead.
//!
//! The `tokens!` and `grammar!` macros are used to define the language and
//! automatically implement parsers.
//!
//! Some tokens, like `Eof` and `StringToken` are provided in the `lex` module.
//!
//! ```
//! use gramma::{grammar, parse, tokens};
//! use gramma::lex::{Eof, RealDecimal, StringToken, Whitespace};
//!
//! tokens! {
//!     pub struct LBrace => symbol("{");
//!     pub struct RBrace => symbol("}");
//!     pub struct LBracket => symbol("[");
//!     pub struct RBracket => symbol("]");
//!     pub struct Colon => symbol(":");
//!     pub struct Comma => symbol(",");
//!     pub struct True => symbol("true");
//!     pub struct False => symbol("false");
//!     pub struct Null => symbol("null");
//!     pub enum JsonToken => {
//!         | LBrace | RBrace
//!         | LBracket | RBracket
//!         | Colon
//!         | Comma
//!         | Whitespace
//!         | StringToken
//!         | RealDecimal
//!         | True
//!         | False
//!         | Null
//!         | Eof
//!     };
//! }
//!
//! grammar! { JsonToken;
//!     pub struct Kvp => { key: StringToken, Colon, value: Value };
//!     pub struct List => { LBracket, vals: [(Value)(Comma)*], RBracket };
//!     pub struct Object => { LBrace, vals: [(Kvp)(Comma)*], RBrace };
//!     pub enum Bool => { True | False };
//!     pub enum Value => {
//!         | Object
//!         | List
//!         | String(StringToken)
//!         | Number(RealDecimal)
//!         | Bool
//!         | Null
//!     };
//!     pub struct Root => { value: Value, Eof };
//! }
//!
//! let (tokens, ast) = parse::<JsonToken, Root>(r#"{
//!     "x": "x",
//!     "y": ["z", [], {"a":["c"], "d": 3.5}],
//!     "z": {},
//!     "w": [true, null, 4, false]
//! }"#).unwrap();
//! ```

#[doc(hidden)]
pub extern crate lazy_static;
#[doc(hidden)]
pub extern crate regex;
#[doc(hidden)]
pub extern crate smallvec;
extern crate thiserror;

pub mod lex;
pub mod parse;
mod utils;

use crate::parse::FormattedParseError;
use crate::parse::Parseable;
use lex::{Token, TokenError};
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

pub fn parse<T: Token, Grammar: Parseable<T>>(src: &str) -> Result<(Vec<T>, Grammar), Error> {
    let (tokens, _) = lex::tokenize(src)?;
    let ast = parse::build_ast::<T, Grammar>(&tokens).map_err(|e| e.formatted(&tokens, src))?;
    Ok((tokens, ast))
}
