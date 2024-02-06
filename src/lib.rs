#![no_std]
extern crate alloc;
extern crate either;
extern crate once_cell;
extern crate regex;

#[doc(hidden)]
pub use either::Either;
#[doc(hidden)]
pub use once_cell::sync::Lazy;
#[doc(hidden)]
pub use regex::Regex;

pub mod ast;
pub mod parse;
pub mod token;
pub(crate) mod utils;
pub(crate) mod internal_prelude {
    pub use alloc::{boxed::Box, vec::Vec};
}

pub use ast::{parse_tree, Rule};
pub use parse::ParseError;
pub use token::TokenDef;

#[doc(hidden)]
#[macro_export]
macro_rules! _lazy_regex {
    ($vis:vis static ref $NAME:ident => $regex:expr;) => {
        $vis static $NAME: $crate::Lazy<$crate::Regex> = $crate::Lazy::new(|| $crate::Regex::new($regex).unwrap());
    };
}
