use core::{
    any::{Any, TypeId},
    cmp::Ordering,
    fmt::{self, Debug, Formatter},
    ptr,
};

use crate::{
    ast::{DelegateRule, Discard, ReadToken},
    parse::{CxType, Location, LocationRange},
    utils::simple_name,
};

pub trait Token: Any {
    fn try_lex(src: &str, location: Location) -> Option<LocationRange>;

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
    pub token_type: TokenType,
    pub range: LocationRange,
}

pub struct TokenObject {
    name: fn() -> &'static str,
    token_id: fn() -> TypeId,
    try_lex: fn(&str, Location) -> Option<LocationRange>,
}

pub type TokenType = &'static TokenObject;

impl Debug for TokenObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

impl PartialEq for TokenObject {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self, other) || self.token_id() == other.token_id()
    }
}

impl PartialOrd for TokenObject {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TokenObject {
    fn cmp(&self, other: &Self) -> Ordering {
        self.token_id().cmp(&other.token_id())
    }
}

impl Eq for TokenObject {}

impl core::hash::Hash for TokenObject {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.token_id().hash(state);
    }
}

impl TokenObject {
    pub const fn of<T: Token>() -> &'static Self {
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

impl Token for Eof {
    fn try_lex(src: &str, location: Location) -> Option<LocationRange> {
        (location.position >= src.len()).then_some(LocationRange {
            start: location,
            end: location,
        })
    }

    fn name() -> &'static str {
        "end-of-file"
    }
}

impl DelegateRule for Eof {
    type Inner = Discard<ReadToken<Eof>>;

    fn from_inner(_: Self::Inner) -> Self {
        Self
    }
}

#[cfg(feature = "regex")]
#[doc(hidden)]
#[macro_export]
macro_rules! _define_token_regex {
    (@try_lex $Name:ident (regex = $pattern:literal $(, capture = $cap:literal)? $(,)?)) => {
        fn try_lex(src: &str, location: $crate::parse::Location) -> Option<$crate::parse::LocationRange> {
            $crate::_lazy_regex! {
                static ref PATTERN => ::core::concat!(r"\A(?:", $pattern, ")");
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
}

#[cfg(not(feature = "regex"))]
#[doc(hidden)]
#[macro_export]
macro_rules! _define_token_regex {
    (@try_lex $Name:ident (regex = $($rest:tt)*)) => {
        fn try_lex(
            src: &str,
            location: $crate::parse::Location,
        ) -> Option<$crate::parse::LocationRange> {
            ::core::compile_error!(::core::concat!(
                "Token '",
                stringify!($Name),
                "' uses regex pattern but 'regex' feature not enabled."
            ))
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! _define_token {
    (@try_lex $Name:ident (regex = $($rest:tt)*)) => {
        $crate::_define_token_regex! {
            @try_lex $Name (regex = $($rest)*)
        }
    };
    (@try_lex $Name:ident (exact = $pattern:literal)) => {
        fn try_lex(src: &str, location: $crate::parse::Location) -> Option<$crate::parse::LocationRange> {
            $crate::parse::lex_exact($pattern, src, location)
        }

        fn name() -> &'static str {
            ::core::stringify!($pattern)
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
    (@try_lex $Name:ident (matcher = $matcher:expr $(,)?)) => {
        fn try_lex(src: &str, location: $crate::parse::Location) -> Option<$crate::parse::LocationRange> {
            $crate::parse::lex_matcher($crate::string_matcher!($matcher), src, location)
        }

        fn name() -> &'static str {
            ::core::stringify!($Name)
        }

        fn print_debug(src: &str, range: $crate::parse::LocationRange, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            f.write_fmt(::core::format_args!(
                ::core::concat!(::core::stringify!($Name), "({:?})"),
                &src[range.start.position..range.end.position]
            ))
        }

        fn print_display(_: &str, _: $crate::parse::LocationRange, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            f.write_str(::core::stringify!($Name))
        }
    };
    (@try_lex $Name:ident ($type:ident $($rest:tt)*)) => {
        fn try_lex(
            src: &str,
            location: $crate::parse::Location,
        ) -> Option<$crate::parse::LocationRange> {
            ::core::compile_error!(::core::concat!(
                "Token '",
                stringify!($Name),
                "' uses unsupported pattern type '",
                stringify!($type),
                "'",
            ))
        }
    };
    (@impl_rule $Name:ident ($Ty:ty)) => {
        impl $crate::ast::DelegateRule for $Name {
            type Inner = $crate::ast::DualParse<$crate::ast::Discard<$crate::ast::ReadToken<$Name>>, $Ty>;

            fn from_inner(inner: Self::Inner) -> Self {
                Self(inner.inner)
            }
        }
    };
    (@impl_rule $Name:ident) => {
        impl $crate::ast::DelegateRule for $Name {
            type Inner = $crate::ast::ReadToken<$Name>;

            fn print_tree(&self, cx: &$crate::ast::print::PrintContext, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                if cx.is_debug() {
                    <Self as $crate::token::Token>::print_debug(cx.src(), self.range, f)
                } else {
                    <Self as $crate::token::Token>::print_display(cx.src(), self.range, f)
                }
            }

            fn from_inner(inner: Self::Inner) -> Self {
                Self { range: inner.range }
            }
        }
    };
    (@define_struct
        $(#$attr:tt)*
        $vis:vis struct $Name:ident ($Ty:ty);
    ) => {
        $(#$attr)*
        #[derive(Debug)]
        $vis struct $Name ($Ty);
    };
    (@define_struct
        $(#$attr:tt)*
        $vis:vis struct $Name:ident;
    ) => {
        $(#$attr)*
        #[derive(Debug)]
        $vis struct $Name { pub range: $crate::parse::LocationRange }
    };
    ($(
        #[pattern $pattern:tt]
        $(#$attr:tt)*
        $vis:vis struct $Name:ident $(($Ty:ty))?;
    )*) => {$(
        $crate::_define_token! {@define_struct
            $(#$attr)*
            $vis struct $Name $(($Ty))?;
        }

        impl $crate::token::Token for $Name {
            $crate::_define_token! { @try_lex $Name $pattern }

            fn display_name() -> &'static str {
                ::core::stringify!($Name)
            }
        }

        $crate::_define_token! { @impl_rule $Name $(($Ty))? }
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
