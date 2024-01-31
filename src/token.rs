use std::{
    any::{Any, TypeId},
    cmp::Ordering,
    fmt::{self, Debug, Formatter},
};

use crate::{
    parse::{CxType, Location, LocationRange},
    utils::simple_name,
};

pub trait TokenDef: Any {
    fn try_lex(src: &str, location: Location) -> Option<LocationRange>;

    fn from_range(src: &str, range: LocationRange) -> Self;

    fn name() -> &'static str {
        simple_name::<Self>()
    }

    fn display_name() -> &'static str {
        Self::name()
    }

    fn print_debug(src: &str, range: LocationRange, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}({:?})",
            Self::name(),
            &src[range.start.position..range.end.position],
        )
    }

    fn print_display(src: &str, range: LocationRange, f: &mut Formatter) -> fmt::Result {
        Self::print_debug(src, range, f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AnyToken {
    pub token_type: &'static TokenType,
    pub range: LocationRange,
}

pub struct TokenType {
    name: fn() -> &'static str,
    token_id: fn() -> TypeId,
    try_lex: fn(&str, Location) -> Option<LocationRange>,
}

impl Debug for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

impl PartialEq for TokenType {
    fn eq(&self, other: &Self) -> bool {
        self.token_id() == other.token_id()
    }
}

impl PartialOrd for TokenType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TokenType {
    fn cmp(&self, other: &Self) -> Ordering {
        self.token_id().cmp(&other.token_id())
    }
}

impl Eq for TokenType {}

impl core::hash::Hash for TokenType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.token_id().hash(state);
    }
}

impl TokenType {
    pub const fn of<T: TokenDef>() -> &'static Self {
        &Self {
            name: T::name,
            token_id: TypeId::of::<T>,
            try_lex: T::try_lex,
        }
    }

    pub fn name(&self) -> &'static str {
        (self.name)()
    }

    pub fn token_id(&self) -> TypeId {
        (self.token_id)()
    }

    pub fn try_lex<Cx: CxType>(&'static self, src: &str, location: Location) -> Option<AnyToken> {
        Some(AnyToken {
            token_type: self,
            range: (self.try_lex)(src, location)?,
        })
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Eof;

impl TokenDef for Eof {
    fn try_lex(src: &str, location: Location) -> Option<LocationRange> {
        (location.position >= src.len()).then_some(LocationRange {
            start: location,
            end: location,
        })
    }

    fn from_range(_: &str, _: LocationRange) -> Self {
        Self
    }

    fn name() -> &'static str {
        "end-of-file"
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! _define_token {
    (@try_lex $Name:ident (regex = $pattern:literal $(, capture = $cap:literal)? $(,)?)) => {
        fn try_lex(src: &str, location: $crate::parse::Location) -> Option<$crate::parse::LocationRange> {
            $crate::lazy_static! {
                static ref PATTERN: $crate::Regex = $crate::Regex::new(::core::concat!(r"\A", $pattern)).unwrap();
            }
            $crate::parse::lex_regex(&PATTERN, 0 $(+ $cap)?, src, location)
        }

        fn name() -> &'static str {
            ::core::stringify!($Name)
        }

        fn print_debug(src: &str, range: $crate::parse::LocationRange, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            f.write_fmt(::core::format_args!(
                ::core::concat!("<",::core::stringify!($Name), " {:?}>"),
                &src[range.start.position..range.end.position]
            ))
        }
    };
    (@try_lex $Name:ident (exact = $pattern:literal)) => {
        fn try_lex(src: &str, location: $crate::parse::Location) -> Option<$crate::parse::LocationRange> {
            $crate::parse::lex_exact($pattern, src, location)
        }

        fn name() -> &'static str {
            ::core::concat!("'", $pattern, "'")
        }

        fn print_debug(src: &str, range: $crate::parse::LocationRange, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            f.write_fmt(::core::format_args!(
                ::core::concat!(::core::stringify!($Name), "({:?})"),
                &src[range.start.position..range.end.position]
            ))
        }

        fn print_display(_: &str, _: $crate::parse::LocationRange, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            f.write_str(::core::stringify!($pattern))
        }
    };
    ($(
        #[pattern $pattern:tt]
        $(#$attr:tt)*
        $vis:vis struct $Name:ident $(($Ty:ty))?;
    )*) => {$(
        $(#[$attr])*
        pub struct $Name $(($Ty))?;

        impl $crate::token::TokenDef for $Name {
            $crate::_define_token! { @try_lex $Name $pattern }

            fn display_name() -> &'static str {
                ::core::stringify!($Name)
            }

            fn from_range(_src: &str, _range: $crate::parse::LocationRange) -> Self {
                Self $( (::core::convert::TryInto::<$Ty>::try_into(
                    &_src[_range.start.position.._range.end.position]
                ).ok().unwrap()) )?
            }
        }
    )*};
}

#[macro_export]
macro_rules! define_token {
    ($(
        $(#$attr:tt)*
        $vis:vis struct $Name:ident $(($Ty:ty))?;
    )*) => {$(
        $crate::_define_token! {
            $(#$attr)*
            $vis struct $Name $(($Ty))?;
        }
    )*};
}
