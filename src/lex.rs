/*!
 * Types and functions for extracting tokens from a str.
 */
use std::convert::TryInto;
use std::fmt::{self, Display};
use std::marker::PhantomData;
use std::ops::Range;

/// Contains a token value and the range it covers in the original string.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Token<T> {
    pub value: T,
    pub range: Range<usize>,
}

impl<T: BasicTokenValue> Token<T> {
    pub fn token_repr(&self, src: &str) -> String {
        self.value.token_repr(&src[self.range.clone()])
    }
}

impl<'a, T: TokenValue> From<&'a Token<T>> for TokenRef<'a, T> {
    fn from(other: &'a Token<T>) -> Self {
        Self {
            value: &other.value,
            range: other.range.clone(),
        }
    }
}

/// Methods for tokens that can be used in a trait object
pub trait BasicTokenValue: std::fmt::Debug {
    fn should_ignore(&self) -> bool {
        false
    }
    fn token_repr(&self, src_slice: &str) -> String {
        format!("'{}'", src_slice)
    }
}
// Methods for token types that can be recognized.
pub trait TokenValue: Sized + Clone + BasicTokenValue + 'static {
    fn recognize(src: &str) -> Option<(Self, usize)>;
    fn token_type_repr() -> String;
}

/// Alias for tokens with boxed dynamic values.
pub type AnyToken = Token<Box<dyn BasicTokenValue>>;

/// Contains a reference to a partiular token value, along with the range it covers.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TokenRef<'a, T: TokenValue> {
    pub value: &'a T,
    pub range: Range<usize>,
}

/// Index for a particular type of token value
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct TokenIndex<T: TokenValue>(pub usize, PhantomData<T>);

impl<T: TokenValue> TokenIndex<T> {
    /// Creates a new instance referring to the given index.
    pub fn new(index: usize) -> Self {
        Self(index, PhantomData)
    }

    /// Returns a reference to the token at this index.
    /// Panics if the token is the wrong type.
    pub fn lookup<'a, U: TokenValue, A: ?Sized>(&self, tokens: &'a A) -> TokenRef<'a, T>
    where
        A: std::ops::Index<usize, Output = Token<U>>,
        &'a U: TryInto<&'a T>,
    {
        let token = &tokens[self.0];
        let value = (&token.value)
            .try_into()
            .map_err(|_| {
                format!(
                    "Unable to convert {:?} to {:?}.",
                    &tokens[self.0],
                    std::any::type_name::<T>(),
                )
            })
            .unwrap();
        TokenRef {
            value,
            range: token.range.clone(),
        }
    }
}

impl<T: BasicTokenValue + 'static + Sized> From<Token<T>> for AnyToken {
    fn from(other: Token<T>) -> Self {
        Self {
            value: Box::new(other.value),
            range: other.range,
        }
    }
}

/// Token that matches only the empty string. Used to mark the end of the input string.
///
/// ```
/// use gramma::lex::{Eof, TokenValue};
///
/// assert_eq!(Eof::recognize("").unwrap(), (Eof, 0));
/// assert_eq!(Eof::recognize("foo"), None);
/// ```
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Default)]
pub struct Eof;
crate::_impl_parse_for_token!(Eof);

impl BasicTokenValue for Eof {
    fn should_ignore(&self) -> bool {
        false
    }
    fn token_repr(&self, _: &str) -> String {
        Self::token_type_repr()
    }
}

impl TokenValue for Eof {
    fn recognize(src: &str) -> Option<(Self, usize)> {
        if src.len() == 0 {
            Some((Self, 0))
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
/// use gramma::lex::{TokenValue, Whitespace};
///
/// // matches a single space:
/// assert_eq!(Whitespace::recognize(" ").unwrap(), (Whitespace, 1));
/// // matches multiple spaces:
/// assert_eq!(Whitespace::recognize("\t \ndone").unwrap(), (Whitespace, 3));
/// // does not match the empty string:
/// assert_eq!(Whitespace::recognize(""), None);
/// ```
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Whitespace;
crate::_impl_parse_for_token!(Whitespace);

impl BasicTokenValue for Whitespace {
    fn should_ignore(&self) -> bool {
        true
    }
    fn token_repr(&self, _: &str) -> String {
        Self::token_type_repr()
    }
}

impl TokenValue for Whitespace {
    fn recognize(src: &str) -> Option<(Self, usize)> {
        let len = src.chars().take_while(|c| c.is_whitespace()).count();
        if len > 0 {
            Some((Self, len))
        } else {
            None
        }
    }
    fn token_type_repr() -> String {
        "whitespace".to_string()
    }
}

impl<T: BasicTokenValue> BasicTokenValue for Box<T> {
    fn should_ignore(&self) -> bool {
        T::should_ignore(self)
    }
    fn token_repr(&self, src_slice: &str) -> String {
        T::token_repr(self, src_slice)
    }
}

impl<T: TokenValue> TokenValue for Box<T> {
    fn recognize(src: &str) -> Option<(Self, usize)> {
        let (token, len) = T::recognize(src)?;
        Some((Box::new(token), len))
    }
    fn token_type_repr() -> String {
        T::token_type_repr()
    }
}

/// token that matches string literals.
/// strings may start with a double or single quote, or a backtick
/// the quote character may be excaped by a backslash.
///
/// ```
/// use gramma::lex::{StringToken, TokenValue};
///
/// assert_eq!(
///     StringToken::recognize("'single quote'").unwrap(),
///     (StringToken { quote_char: '\'', inner_range: 1..13 }, 14),
/// );
/// assert_eq!(
///     StringToken::recognize("\"double quote\" ...").unwrap(),
///     (StringToken { quote_char: '"', inner_range: 1..13 }, 14),
/// );
/// assert_eq!(
///     StringToken::recognize("`backticks`").unwrap(),
///     (StringToken { quote_char: '`', inner_range: 1..10 }, 11),
/// );
/// assert_eq!(StringToken::recognize("\"unclosed"), None);
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringToken {
    pub quote_char: char,
    pub inner_range: Range<usize>,
}
crate::_impl_parse_for_token!(StringToken);

impl BasicTokenValue for StringToken {
    fn should_ignore(&self) -> bool {
        false
    }
    fn token_repr(&self, _: &str) -> String {
        Self::token_type_repr()
    }
}

impl TokenValue for StringToken {
    fn recognize(src: &str) -> Option<(Self, usize)> {
        let mut chars = src.char_indices();
        let (_, first) = chars.next()?;

        let mut last = match first {
            '"' | '\'' | '`' => first,
            _ => return None,
        };

        for (i, cur) in chars {
            if cur == first && last != '\\' {
                return Some((
                    Self {
                        quote_char: first,
                        inner_range: first.len_utf8()..i,
                    },
                    i + cur.len_utf8(),
                ));
            }

            last = cur;
        }

        return None;
    }

    fn token_type_repr() -> String {
        "string".to_string()
    }
}

// create pre-defined token definitions for decimal numbers and C-style identifiers.
crate::tokens! {
    /// Integer literal in base 10.
    ///
    /// ```
    /// use gramma::lex::{IntDecimal, TokenValue};
    ///
    /// assert!(matches!(IntDecimal::recognize("123").unwrap(), (IntDecimal(..), 3)));
    /// assert_eq!(IntDecimal::recognize("abc"), None);
    /// ```
    pub struct IntDecimal => regex("integer literal", r"^-?\d+");

    /// Numeric literal in base 10 with an optional fractional part.
    ///
    /// ```
    /// use gramma::lex::{RealDecimal, TokenValue};
    ///
    /// assert!(matches!(RealDecimal::recognize("123").unwrap(), (RealDecimal(..), 3)));
    /// assert!(matches!(RealDecimal::recognize("123.456").unwrap(), (RealDecimal(..), 7)));
    /// assert_eq!(RealDecimal::recognize("abc"), None);
    /// ```
    pub struct RealDecimal => regex("real literal", r"^-?(\d*\.\d+|\d+)");

    /// Sequence of any ASCII letter, digit, or underscore, except the first character cannot be a digit
    ///
    /// ```
    /// use gramma::lex::{Identifier, TokenValue};
    ///
    /// assert!(matches!(Identifier::recognize("abc_123 ...").unwrap(), (Identifier(..), 7)));
    /// assert_eq!(Identifier::recognize("123abc"), None);
    /// ```
    pub struct Identifier => regex("identifier", r"^[a-zA-Z_][a-zA-Z_0-9]*");
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

/// Converts a string to a Vec of tokens of type `T`.
/// Returns `Err(TokenError::Unreadable)` if part of the string does not match any token
pub fn tokenize<T: TokenValue>(mut src: &str) -> Result<Vec<Token<T>>, TokenError> {
    let mut index = 0;
    let mut tokens = Vec::new();

    while let Some((token, len)) = T::recognize(src) {
        tokens.push(Token {
            value: token,
            range: index..index + len,
        });
        if src.is_empty() {
            return Ok(tokens);
        }
        src = &src[len..];
        index += len;
    }

    Err(TokenError::Unreadable { at: index })
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
    (@rule $m:ident $(#[$meta:meta])* $vis:vis struct $name:ident => regex($repr:literal, $reg:literal $(,)?); $($rest:tt)*) => {
        $crate::_tokens!(@m $m regex ($vis $name $repr $reg ($(#[$meta])*) ));
        $crate::_tokens!(@rule $m $($rest)*);
    };

    // implementation for union tokens:
    (@m token enum ($vis:vis $name:ident ($(#[$meta:meta])*) [ $(($v:ident : $ty:ty))* ])) => {
        $(#[$meta])*
        #[derive(Clone, PartialEq, Eq, Debug)]
        $vis enum $name {
            $($v($ty),)*
        }
        impl $crate::lex::BasicTokenValue for $name {
            fn should_ignore(&self) -> bool {
                match self {
                    $(Self::$v(x) => x.should_ignore(),)*
                }
            }
        }
        impl $crate::lex::TokenValue for $name {
            fn recognize(src: &str) -> Option<(Self, usize)> {
                fn convert<T: Into<$name>>((token, len): (T, usize)) -> ($name, usize) {
                    (token.into(), len)
                }

                let mut best: Option<(Self, usize)> = None;
                $(
                    best = match (best, <$ty as $crate::lex::TokenValue>::recognize(src).map(convert)) {
                        (None, t) => t,
                        (t, None) => t,
                        (Some((t1, l1)), Some((t2, l2))) => {
                            if l2 > l1 { Some((t2, l2)) } else { Some((t1, l1)) }
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
        $vis struct $name;
        impl $crate::lex::BasicTokenValue for $name {
            fn should_ignore(&self) -> bool { $ignore }
        }
        impl $crate::lex::TokenValue for $name {
            fn recognize(src: &str) -> Option<(Self, usize)> {
                if src.starts_with($lit) { Some(($name, $lit.len())) } else { None }
            }
            fn token_type_repr() -> std::string::String {
                concat!("'", $lit, "'").to_string()
            }
        }
        $crate::_impl_parse_for_token!($name);
    };
    // implementation for regex tokens:
    (@m token regex ($vis:vis $name:ident $repr:literal $reg:literal ($(#[$meta:meta])*))) => {
        $(#[$meta])*
        #[derive(Clone, PartialEq, Eq, Debug)]
        $vis struct $name(pub std::ops::Range<usize>);
        impl $crate::lex::BasicTokenValue for $name {}
        impl $crate::lex::TokenValue for $name {
            fn recognize(src: &str) -> Option<(Self, usize)> {
                $crate::lazy_static::lazy_static! {
                    static ref RE: $crate::regex::Regex = $crate::regex::Regex::new($reg).unwrap();
                }
                let found = RE.find(src)?;
                Some((Self(found.range()), found.end()))
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
 *  use gramma::lex::TokenValue;
 *  use gramma::tokens;
 *
 *  tokens! {
 *      struct True => symbol("true");
 *      struct Plus => symbol("+");
 *  }
 *
 *  assert!(matches!(True::recognize("true"), Some((True, 4))));
 *  // matches just the length of the symbol (in this case 1):
 *  assert!(matches!(Plus::recognize("+1234"), Some((Plus, 1))));
 *  // returns None when the beginning of the string does not match the token.
 *  assert!(matches!(True::recognize("false, true"), None));
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
 *  use gramma::lex::TokenValue;
 *  use gramma::tokens;
 *
 *  tokens! {
 *      struct MyIdent => regex("identifier", r"^[a-zA-Z_][a-zA-Z_0-9]*");
 *  }
 *
 *  assert!(matches!(MyIdent::recognize("foo123"), Some((MyIdent(..), 6))));
 *  assert!(matches!(MyIdent::recognize("123bar"), None));
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
 *  use gramma::lex::TokenValue;
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
 *  assert!(matches!(MyToken::recognize("true"), Some((MyToken::True(..), 4))));
 *  assert!(matches!(MyToken::recognize("+"), Some((MyToken::Plus(..), 1))));
 *  // Notice how MyIdent takes precedence over True because the string it matches is longer.
 *  assert!(matches!(MyToken::recognize("truer"), Some((MyToken::Ident(..), 5))));
 * ```
 *
 * ## Example
 *
 * ```
 *  use gramma::lex::{tokenize, Whitespace, Eof};
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
 *  let tokens = tokenize::<MyToken>("(+ 12 (- 345 6))").unwrap();
 *  assert!(matches![tokens[0].value, MyToken::LParen(..)]);
 *  assert!(matches![tokens[1].value, MyToken::Plus(..)]);
 *  assert!(matches![tokens[2].value, MyToken::Whitespace(..)]);
 *  assert!(matches![tokens[3].value, MyToken::Num(..)]);
 *
 *  assert_eq!(tokens[0].range, 0..1);
 *  assert_eq!(tokens[1].range, 1..2);
 *  assert_eq!(tokens[2].range, 2..3);
 *  assert_eq!(tokens[3].range, 3..5);
 * ```
 */
#[macro_export]
macro_rules! tokens {
    ($($x:tt)*) => {
        $crate::_tokens!($($x)*);
    };
}
