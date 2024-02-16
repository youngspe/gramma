#![doc = include_str!("../README.md")]
#![no_std]
extern crate alloc;
extern crate either;
extern crate once_cell;
extern crate regex;

#[doc(hidden)]
pub use either::Either;
#[cfg(feature = "std")]
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

pub use ast::{display_tree, parse_tree, Rule};
pub use parse::ParseError;
pub use token::Token;

#[doc(hidden)]
#[macro_export]
macro_rules! _lazy_regex {
    ($vis:vis static ref $NAME:ident => $regex:expr;) => {
        $vis static $NAME: $crate::Lazy<$crate::Regex> = $crate::Lazy::new(|| $crate::Regex::new($regex).unwrap());
    };
}

#[cfg(not(feature = "std"))]
#[doc(hidden)]
pub struct Lazy<T> {
    inner: once_cell::race::OnceBox<T>,
    init: fn() -> T,
}

#[cfg(not(feature = "std"))]
#[doc(hidden)]
impl<T> Lazy<T> {
    pub const fn new(init: fn() -> T) -> Self {
        Self {
            inner: once_cell::race::OnceBox::new(),
            init,
        }
    }
}

#[cfg(not(feature = "std"))]
#[doc(hidden)]
impl<T> core::ops::Deref for Lazy<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner.get_or_init(|| (self.init)().into())
    }
}
