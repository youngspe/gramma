#![doc = include_str!("crate.md")]
#![no_std]
extern crate alloc;
extern crate either;
#[cfg(feature = "regex")]
extern crate once_cell;
#[cfg(feature = "regex")]
extern crate regex;

#[doc(hidden)]
pub use either::Either;
#[cfg(all(feature = "regex", feature = "std"))]
#[doc(hidden)]
pub use once_cell::sync::Lazy;
#[cfg(feature = "regex")]
#[doc(hidden)]
pub use regex::Regex;

#[macro_use]
pub(crate) mod utils;
pub mod ast;
pub mod parse;
pub mod token;
pub(crate) mod internal_prelude {
    pub use alloc::{boxed::Box, vec::Vec};
}
pub mod error;
pub mod string_matcher;

pub use ast::{display_tree, parse_tree, Rule};
pub use parse::ParseError;
pub use token::Token;

#[cfg(feature = "regex")]
#[doc(hidden)]
#[macro_export]
macro_rules! _lazy_regex {
    ($vis:vis static ref $NAME:ident => $regex:expr;) => {
        $vis static $NAME: $crate::Lazy<$crate::Regex> = $crate::Lazy::new(|| $crate::Regex::new($regex).unwrap());
    };
}

#[cfg(all(feature = "regex", not(feature = "std")))]
mod _lazy {
    pub struct Lazy<T> {
        inner: once_cell::race::OnceBox<T>,
        init: fn() -> T,
    }

    impl<T> Lazy<T> {
        pub const fn new(init: fn() -> T) -> Self {
            Self {
                inner: once_cell::race::OnceBox::new(),
                init,
            }
        }
    }

    impl<T> core::ops::Deref for Lazy<T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            self.inner.get_or_init(|| (self.init)().into())
        }
    }
}

#[cfg(all(feature = "regex", not(feature = "std")))]
#[doc(hidden)]
pub use _lazy::Lazy;
