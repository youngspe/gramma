/*!
 * Types and functions for parsing a context-free grammar from a list of tokens.
 */
#[doc(hidden)]
pub mod ast;
mod macros;
#[doc(hidden)]
pub mod parsers;

use smallvec::SmallVec;
use std::fmt::{self, Debug, Display};

use crate::lex::{Token, TokenValue};

// traits
pub use ast::Ast;
pub use parsers::BeginParse;
pub use parsers::Parse;
pub use parsers::Parseable;

// types
pub use ast::Convert;
pub use ast::Empty;
pub use ast::EmptyList;
pub use ast::Enum2;
pub use ast::Enum2Def;
pub use ast::List;
pub use ast::NonEmptyList;
pub use ast::Seq2;
pub use ast::Seq2Def;
pub use ast::TokenAst;
pub use ast::Union2;
pub use ast::WrapAst;
pub use parsers::BoxParser;
pub use parsers::ConvertParser;
pub use parsers::EmptyListParser;
pub use parsers::EmptyParser;
pub use parsers::Enum2Parser;
pub use parsers::NonEmptyListParser;
pub use parsers::OptionParser;
pub use parsers::Seq2Parser;
pub use parsers::TokenParser;
pub use parsers::Union2Parser;

pub type ParseResult<T> = std::result::Result<T, ParseError>;
use ParseResult as PResult;

/// Error type for parsing operations.
#[derive(Clone, Debug)]
pub struct ParseError {
    /// Index of the invalid token
    pub index: usize,
    /// List of unique function pointers that produce representations of the expected tokens
    pub expected: SmallVec<[fn() -> String; 2]>,
}

impl ParseError {
    /// Combines two errors.
    /// Keep whichever has the greater index (i.e. the farthest we were able to parse).
    /// If the indices are the same, combine the two sets of expected tokens.
    pub fn combine(mut self, other: Self) -> Self {
        use std::cmp::Ordering::*;
        match self.index.cmp(&other.index) {
            Less => other,
            Equal => {
                for f in other.expected {
                    if !self.expected.contains(&f) {
                        self.expected.push(f)
                    }
                }
                self
            }
            Greater => self,
        }
    }

    /// Convert to an human-readable error format.
    pub fn formatted(self, tokens: &[Token<impl TokenValue>], src: &str) -> FormattedParseError {
        let mut expected = self
            .expected
            .iter()
            .copied()
            .map(|f| f())
            .collect::<Vec<_>>();
        expected.sort();

        FormattedParseError {
            actual: tokens[self.index].token_repr(src),
            expected,
            inner: self,
        }
    }
}

/// User-friendly type for parsing errors
/// Contains the representation of the actual and expected strings.
#[derive(Clone, Debug)]
pub struct FormattedParseError {
    pub actual: String,
    pub expected: Vec<String>,
    pub inner: ParseError,
}

impl Display for FormattedParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.expected.len() {
            0 => {
                f.write_str(&format!("Unexpected token {}", self.actual))?;
            }
            1 => {
                f.write_str(&format!(
                    "Expected {}; found {}",
                    self.expected[0], self.actual
                ))?;
            }
            _ => {
                f.write_str("Expected one of: { ")?;
                let joined = crate::utils::join(", ", &self.expected);
                f.write_str(&joined)?;
                f.write_str(&format!(" }}; found {}", self.actual))?;
            }
        }

        Ok(())
    }
}

impl std::error::Error for FormattedParseError {}

/// Constructs an abstract syntax tree of type `A` from the given tokens.
pub fn build_ast<T: TokenValue, A: Parseable<T>>(tokens: &[Token<T>]) -> PResult<A> {
    Parseable::parse(&tokens)
}
