/*!
 * Types and functions for extracting tokens from a str.
 */
use std::convert::TryInto;
use std::fmt::{self, Display};
use std::marker::PhantomData;
use std::ops::Add;
use std::ops::Range;

/// Methods for tokens that can be used in a trait object
pub trait BasicToken: std::fmt::Debug {
    fn range(&self) -> Range<usize>;
    fn inner_range(&self) -> Range<usize> {
        self.range()
    }
    fn should_ignore(&self) -> bool {
        false
    }
    fn token_repr(&self, src: &str) -> String {
        format!("'{}'", &src[self.range()])
    }
}

// Methods for token types that can be recognized.
pub trait Token: Sized + Clone + BasicToken {
    fn recognize(src: &str, start: usize) -> Option<Self>;
    fn token_type_repr() -> String;
}

pub trait InnerRangeToken: Token {
    fn inner_range(&self) -> Range<usize>;
}

/// Alias for tokens with boxed dynamic values.
pub type AnyToken = Box<dyn BasicToken>;

/// Index for a particular type of token value
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct TokenIndex<T>(pub usize, PhantomData<T>);

impl<T> TokenIndex<T> {
    /// Creates a new instance referring to the given index.
    pub fn new(index: usize) -> Self {
        Self(index, PhantomData)
    }

    /// Returns a reference to the token at this index.
    /// Panics if the token is the wrong type.
    pub fn lookup<'a, U: 'a, A: ?Sized>(&self, tokens: &'a A) -> Option<&'a T>
    where
        A: std::ops::Index<usize, Output = U>,
        &'a U: TryInto<&'a T>,
    {
        (&tokens[self.0]).try_into().ok()
    }
}

/// Token that matches only the empty string. Used to mark the end of the input string.
///
/// ```
/// use gramma::lex::{BasicToken, Eof, Token};
///
/// assert_eq!(Eof::recognize("", 0).unwrap().range(), 0..0);
/// assert_eq!(Eof::recognize("foo", 0), None);
/// ```
#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct Eof(pub usize);
crate::_impl_parse_for_token!(Eof);

impl BasicToken for Eof {
    fn range(&self) -> Range<usize> {
        self.0..self.0
    }
    fn should_ignore(&self) -> bool {
        false
    }
    fn token_repr(&self, _: &str) -> String {
        Self::token_type_repr()
    }
}

impl Token for Eof {
    fn recognize(src: &str, start: usize) -> Option<Self> {
        if start == src.len() {
            Some(Self(start))
        } else {
            None
        }
    }
    fn token_type_repr() -> String {
        "end-of-file".to_string()
    }
}

/// Token that matches one or more unicode whitespace characters.
/// Will generally be ignored by a parser
///
/// ```
/// use gramma::lex::{BasicToken, Token, Whitespace};
///
/// // matches a single space:
/// assert_eq!(Whitespace::recognize(" ", 0).unwrap().range(), 0..1);
/// // matches multiple spaces:
/// assert_eq!(Whitespace::recognize("\t \ndone", 0).unwrap().range(), 0..3);
/// // does not match the empty string:
/// assert_eq!(Whitespace::recognize("", 0), None);
/// ```
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Whitespace(pub Range<usize>);
crate::_impl_parse_for_token!(Whitespace);

impl BasicToken for Whitespace {
    fn range(&self) -> Range<usize> {
        self.0.clone()
    }
    fn should_ignore(&self) -> bool {
        true
    }
    fn token_repr(&self, _: &str) -> String {
        Self::token_type_repr()
    }
}

impl Token for Whitespace {
    fn recognize(src: &str, start: usize) -> Option<Self> {
        let end = src[start..]
            .chars()
            .take_while(|c| c.is_whitespace())
            .map(char::len_utf8)
            .fold(start, Add::add);
        if end > start {
            Some(Self(start..end))
        } else {
            None
        }
    }
    fn token_type_repr() -> String {
        "whitespace".to_string()
    }
}

impl<T: BasicToken> BasicToken for Box<T> {
    fn range(&self) -> Range<usize> {
        <T as BasicToken>::range(self)
    }
    fn should_ignore(&self) -> bool {
        T::should_ignore(self)
    }
    fn token_repr(&self, src_slice: &str) -> String {
        T::token_repr(self, src_slice)
    }
}

impl<T: Token> Token for Box<T> {
    fn recognize(src: &str, start: usize) -> Option<Self> {
        T::recognize(src, start).map(Box::new)
    }
    fn token_type_repr() -> String {
        T::token_type_repr()
    }
}

// create pre-defined token definitions for decimal numbers and C-style identifiers.
crate::tokens! {
    /// Integer literal in base 10.
    ///
    /// ```
    /// use gramma::lex::{BasicToken, IntDecimal, Token};
    ///
    /// assert_eq!(IntDecimal::recognize("123", 0).unwrap().range(), 0..3);
    /// assert_eq!(IntDecimal::recognize("abc", 0), None);
    /// ```
    pub struct IntDecimal => regex("integer literal", r"^-?\d+");

    /// Numeric literal in base 10 with an optional fractional part.
    ///
    /// ```
    /// use gramma::lex::{BasicToken, RealDecimal, Token};
    ///
    /// assert_eq!(RealDecimal::recognize("123", 0).unwrap().range(), 0..3);
    /// assert_eq!(RealDecimal::recognize("123.456", 0).unwrap().range(), 0..7);
    /// assert_eq!(RealDecimal::recognize("abc", 0), None);
    /// ```
    pub struct RealDecimal => regex("real literal", r"^-?(\d*\.\d+|\d+)");

    /// Sequence of any ASCII letter, digit, or underscore, except the first character cannot be a digit
    ///
    /// ```
    /// use gramma::lex::{BasicToken, Identifier, Token};
    ///
    /// assert_eq!(Identifier::recognize("abc_123 ...", 0).unwrap().range(), 0..7);
    /// assert_eq!(Identifier::recognize("123abc", 0), None);
    /// ```
    pub struct Identifier => regex("identifier", r"^[a-zA-Z_][a-zA-Z_0-9]*");

    /// Token that matches string literals.
    /// Strings may start with a double or single quote.
    /// The quote character may be excaped by a backslash.
    ///
    /// ```
    /// use gramma::lex::{StringToken, Token};
    ///
    /// assert_eq!(
    ///     StringToken::recognize("'single quote'", 0),
    ///     Some(StringToken(0..14, 1..13)),
    /// );
    /// assert_eq!(
    ///     StringToken::recognize(r#""double quote" ..."#, 0),
    ///     Some(StringToken(0..14, 1..13)),
    /// );
    /// assert_eq!(
    ///     StringToken::recognize(r#""escape \"this\" quote""#, 0),
    ///     Some(StringToken(0..23, 1..22)),
    /// );
    /// assert_eq!(StringToken::recognize("\"unclosed", 0), None);
    /// ```
    pub struct StringToken => regex(
        "string",
        r#"^"((\\"|[^"])*)""# => m => (m.get(0), m.get(1)),
        r"'((\\'|[^'])*)'" => m => (m.get(0), m.get(1)),
    );
}

/// Error type for tokenization.
#[non_exhaustive]
#[derive(Debug)]
pub enum TokenError {
    Unreadable { at: usize },
}

impl Display for TokenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Unreadable { at } => {
                f.write_str(&format!("Unreadable token at position {}.", at))?;
            }
        }
        Ok(())
    }
}

impl std::error::Error for TokenError {}

/// Helper that lets us accept either a capture or pair of captures in a regex token expression.
#[doc(Hidden)]
pub struct FirstOptional<T>(pub T, pub T);

impl<T: Clone> From<T> for FirstOptional<T> {
    fn from(src: T) -> Self {
        Self(src.clone(), src)
    }
}

impl<T> From<(T, T)> for FirstOptional<T> {
    fn from((a, b): (T, T)) -> Self {
        Self(a, b)
    }
}

/// Converts a string to a Vec of tokens of type `T`.
/// Returns the tokens and the index of the string where tokenization stopped.
/// Returns `Err(TokenError::Unreadable)` if part of the string does not match any token
pub fn tokenize<T: Token>(src: &str) -> Result<(Vec<T>, usize), TokenError> {
    let mut index = 0;
    let mut tokens = Vec::new();

    while let Some(token) = T::recognize(src, index) {
        let range = token.range();
        index = range.end;
        tokens.push(token);

        if range.len() == 0 {
            // probably hit EOF
            return Ok((tokens, index));
        }

        if index > src.len() {
            panic!("Read past end of string while tokenizing.");
        }
    }

    if index == src.len() {
        Ok((tokens, index))
    } else {
        Err(TokenError::Unreadable { at: index })
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! _tokens {
    // do nothing if there are no token definitions left
    (@rule $m:ident) => {};
    // syntax for union of tokens
    // this token will implement From and TryInto for each of its variants
    (@rule $m:ident $(#[$meta:meta])* $vis:vis enum $name:ident => {$(|)? $($v:ident$(($ty:ty))?)|*}; $($rest:tt)*) => {
        $crate::_tokens!(
            @m $m enum ($vis $name ($(#[$meta])*) [$(
                ($v : $crate::_tokens!(@vartype $v $($ty)?))
            )*]));
        $crate::_tokens!(@rule $m $($rest)*);
    };
    // syntax for symbol/keyword tokens
    // matches strings that consist entirely of the given string
    (@rule $m:ident $(#[$meta:meta])* $vis:vis struct $name:ident => symbol($lit:literal $(,ignore=$ignore:expr)?); $($rest:tt)*) => {
        $crate::_tokens!(@m $m sym ($vis $name $lit ($(#[$meta])*) (ignore=$crate::_tokens!(@default ($($ignore)?) (false)))));
        $crate::_tokens!(@rule $m $($rest)*);
    };
    // syntax for regex tokens
    // matches strings that match the given regex. Hopefully the regex begins with '^'
    {
        @rule $m:ident
        $(#[$meta:meta])* $vis:vis struct $name:ident => regex(
            $repr:literal, $($reg:expr $(=> $caps:ident => $expr:expr)?),+ $(,)?
        );
            $($rest:tt)*
    } => {
        $crate::_tokens!(@m $m regex ($vis, $name, $repr, [$(($reg $(=>$caps=>$expr)?))+] ($(#[$meta])*) ));
        $crate::_tokens!(@rule $m $($rest)*);
    };
    {
        @rule $m:ident
        $(#[$meta:meta])* $vis:vis struct $name:ident => string(
            $repr:literal,
        );
    } => {};

    // implementation for union tokens:
    (@m token enum ($vis:vis $name:ident ($(#[$meta:meta])*) [ $(($v:ident : $ty:ty))* ])) => {
        $(#[$meta])*
        #[derive(Clone, PartialEq, Eq, Debug)]
        $vis enum $name {
            $($v($ty),)*
        }
        impl $crate::lex::BasicToken for $name {
            fn range(&self) -> std::ops::Range<usize> {
                match self {
                    $(Self::$v(x) => x.range(),)*
                }
            }
            fn should_ignore(&self) -> bool {
                match self {
                    $(Self::$v(x) => x.should_ignore(),)*
                }
            }
        }
        impl $crate::lex::Token for $name {
            fn recognize(src: &str, start: usize) -> Option<Self> {
                use $crate::lex::{BasicToken, Token};
                let mut best: Option<Self> = None;
                $(
                    best = match (best, <$ty as Token>::recognize(src, start).map(Into::into)) {
                        (None, t) => t,
                        (t, None) => t,
                        (Some(t1), Some(t2)) => {
                            if BasicToken::range(&t2).end > BasicToken::range(&t1).end {
                                Some(t2)
                            } else {
                                Some(t1)
                            }
                        }
                    };
                )*
                best
            }

            fn token_type_repr() -> std::string::String {
                stringify!($name).to_string()
            }
        }
        $(
            impl From<$ty> for $name {
                fn from(x: $ty) -> Self {
                    Self::$v(x)
                }
            }
            impl std::convert::TryFrom<$name> for $ty {
                type Error = ();
                fn try_from(x: $name) -> Result<Self, ()> {
                    match x {
                        $name::$v(x) => Ok(x),
                        _ => Err(()),
                    }
                }
            }
            impl<'a> std::convert::TryFrom<&'a $name> for &'a $ty {
                type Error = ();
                fn try_from(x: &'a $name) -> Result<Self, ()> {
                    match x {
                        $name::$v(x) => Ok(x),
                        _ => Err(()),
                    }
                }
            }
        )*
        $crate::_impl_parse_for_token!($name);
    };
    // implementation for symbol/keyword tokens:
    (@m token sym ($vis:vis $name:ident $lit:literal ($(#[$meta:meta])*) (ignore=$ignore:expr))) => {
        $(#[$meta])*
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
        $vis struct $name(pub usize);
        impl $crate::lex::BasicToken for $name {
            fn range(&self) -> std::ops::Range<usize> {
                self.0..self.0 + $lit.len()
            }
            fn should_ignore(&self) -> bool { $ignore }
        }
        impl $crate::lex::Token for $name {
            fn recognize(src: &str, start: usize) -> Option<Self> {
                if src[start..].starts_with($lit) { Some($name(start)) } else { None }
            }
            fn token_type_repr() -> std::string::String {
                concat!("'", $lit, "'").to_string()
            }
        }
        $crate::_impl_parse_for_token!($name);
    };
    // implementation for regex tokens:
    {
        @m token regex (
            $vis:vis, $name:ident, $repr:literal,
            [$(($reg:expr $(=> $caps:pat => $expr:expr)?))+]
            ($(#[$meta:meta])*)
        )
    } => {
        $(#[$meta])*
        #[derive(Clone, PartialEq, Eq, Debug)]
        $vis struct $name(pub std::ops::Range<usize>, pub std::ops::Range<usize>);

        impl $crate::lex::BasicToken for $name {
            fn range(&self) -> std::ops::Range<usize> {
                self.0.clone()
            }
            fn inner_range(&self) -> std::ops::Range<usize> {
                self.1.clone()
            }
        }

        impl $crate::lex::Token for $name {
            fn recognize(src: &str, start: usize) -> Option<Self> {
                let src = &src[start..];
                $(
                    if let Some((outer, inner)) = $crate::_tokens!(@regex src, $reg $(=> $caps => $expr)?) {
                        let end = outer.end + start;
                        let inner_start = inner.start + start;
                        let inner_end = inner.end + start;
                        return Some(Self(start..end, inner_start..inner_end))
                    }
                )+

                None
            }
            fn token_type_repr() -> std::string::String {
                $repr.to_string()
            }
        }
        $crate::_impl_parse_for_token!($name);
    };

    // @vartype: get the type of a token union variant
    (@vartype $v:ident) => { $v };
    (@vartype $v:ident $ty:ty) => { $ty };

    // @default: helper that replaces an empty token list with the given tokens
    (@default () ($($def:tt)*)) => {$($def)*};
    (@default ($($val:tt)+) ($($def:tt)*)) => {$($val)*};

    (@regex $s:expr, $reg:expr $(=> $caps:pat => $expr:expr )?) => {{
        $crate::lazy_static::lazy_static! {
            static ref RE: $crate::regex::Regex = $crate::regex::Regex::new($reg).unwrap();
        }
        if let Some(captures) = RE.captures($s) {
            if let $crate::lex::FirstOptional(Some(a), Some(b)) = $crate::_tokens!(@default ($({
                let $caps = captures;
                ($expr).into()
            })?) (
                captures.get(0).into()
            )) {
                Some((a.range(), b.range()))
            } else {
                None
            }
        } else {
            None
        }
    }};

    // catch all invalid token definitions:
    (@rule $($rest:tt)*) => {compile_error!(stringify!(invalid rule: $($rest)*));};
    // catch all invalid methods:
    (@m $($rest:tt)*) => {compile_error!(stringify!(invalid method: $($rest)*));};

    // the main rule for this macro:
    // should be a series of token definitions
    ($($rest:tt)*) => {
        $crate::_tokens!(@rule token $($rest)*);
    };
}

/**
 *  Used to define tokens that can be parsed from a str.
 *
 * ## Usage
 *
 * This macro may be used to define multiple kinds of tokens.
 *
 * ### Keywords and symbols
 *
 * Keyword and symbol tokens (tokens that match an exact string) may be defined as follows:
 *
 * ```
 *  use gramma::lex::{BasicToken, Token};
 *  use gramma::tokens;
 *
 *  tokens! {
 *      struct True => symbol("true");
 *      struct Plus => symbol("+");
 *  }
 *
 *  assert_eq!(True::recognize("true", 0).unwrap().range(), 0..4);
 *  // matches just the length of the symbol (in this case 1):
 *  assert_eq!(Plus::recognize("+1234", 0).unwrap().range(), 0..1);
 *  // returns None when the beginning of the string does not match the token.
 *  assert_eq!(True::recognize("false, true", 0), None);
 * ```
 *
 * ### Regular expressions
 *
 * These tokens are recognized when a regular expression matches the input starting at the current character.
 * You almost definitely want to begin the token with `^` so the token only matches the beginning of the string.
 * The first argument is a string representing the kind of token this is.
 * It will be used when displaying error messages when this kind of token is expected during parsing.
 *
 * ```
 *  use gramma::lex::{BasicToken, Token};
 *  use gramma::tokens;
 *
 *  tokens! {
 *      struct MyIdent => regex("identifier", r"^[a-zA-Z_][a-zA-Z_0-9]*");
 *  }
 *
 *  assert_eq!(MyIdent::recognize("foo123", 0).unwrap().range(), 0..6);
 *  assert_eq!(MyIdent::recognize("123bar", 0), None);
 * ```
 *
 * ### Unions
 *
 * These tokens are defined as enums where each variant contains a specific kind of token.
 * The type of variant can be inferred from its name or provided explicitly.
 * This token will recognize any of its variants, and the longest token recognized will be used.
 * If two recognized variants are the same length, the first defined here will be used.
 * This type will implement both `From` and `TryInto` for each of its variants._tokens!
 *
 * ```
 *  use gramma::tokens;
 *  use gramma::lex::Token;
 *  # tokens! {
 *      # struct True => symbol("true");
 *      # struct Plus => symbol("+");
 *      # struct MyIdent => regex("identifier", r"^[a-zA-Z_][a-zA-Z_0-9]*");
 *  # }
 *
 *  tokens! {
 *      enum MyToken => {
 *          | True
 *          | Plus
 *          | Ident(MyIdent)
 *      };
 *  }
 *
 *  // Notice how True takes precedence over MyIdent even though it matches both.
 *  assert!(matches!(MyToken::recognize("true", 0), Some(MyToken::True(..))));
 *  assert!(matches!(MyToken::recognize("+", 0), Some(MyToken::Plus(..))));
 *  // Notice how MyIdent takes precedence over True because the string it matches is longer.
 *  assert!(matches!(MyToken::recognize("truer", 0), Some(MyToken::Ident(..))));
 * ```
 *
 * ## Example
 *
 * ```
 *  use gramma::lex::{tokenize, Whitespace, Eof, BasicToken};
 *  use gramma::tokens;
 *
 *  tokens! {
 *      pub struct LParen => symbol("(");
 *      pub struct RParen => symbol(")");
 *      pub struct Plus => symbol("+");
 *      pub struct Minus => symbol("-");
 *      pub struct Num => regex("number", r"^\d+");
 *      pub enum MyToken => {
 *          | Whitespace
 *          | LParen
 *          | RParen
 *          | Plus
 *          | Minus
 *          | Num
 *          | Eof // required to be able to recognize the end of the string
 *      };
 *  }
 *
 *  let tokens = tokenize::<MyToken>("(+ 12 (- 345 6))").unwrap().0;
 *  assert!(matches![tokens[0], MyToken::LParen(..)]);
 *  assert!(matches![tokens[1], MyToken::Plus(..)]);
 *  assert!(matches![tokens[2], MyToken::Whitespace(..)]);
 *  assert!(matches![tokens[3], MyToken::Num(..)]);
 *
 *  assert_eq!(tokens[0].range(), 0..1);
 *  assert_eq!(tokens[1].range(), 1..2);
 *  assert_eq!(tokens[2].range(), 2..3);
 *  assert_eq!(tokens[3].range(), 3..5);
 * ```
 */
#[macro_export]
macro_rules! tokens {
    ($($x:tt)*) => {
        $crate::_tokens!($($x)*);
    };
}
