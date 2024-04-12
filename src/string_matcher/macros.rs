#[allow(unused)]
use crate::string_matcher::{patterns, StringMatcher, StringPattern};

/// Creates a [StringMatcher] from the given [StringPattern].
/// Approximately a shorthand for `&string_pattern(pattern).matcher()`.
///
/// Matchers are self-referential, so the output can't be moved after it's created.
/// All predefined patterns in the [patterns] module are brought into scope for the pattern expression.
///
/// # Example
///
/// ```
/// let quotes_matcher = gramma::string_matcher!(
///     char('"') + char(..).repeat(..).lazy() + !follows(char('\\')) + char('"')
/// );
///
/// assert!(quotes_matcher.match_string(4, r#"s = "hello, \"world\"!";"#).is_some());
/// ```
#[macro_export]
macro_rules! string_matcher {
    ($expr:expr $(,)?) => {
        &$crate::string_matcher::StringPattern::matcher({
            #[allow(unused)]
            use $crate::string_matcher::patterns::*;
            $expr
        }) as &$crate::string_matcher::StringMatcher<_>
    };
}

/// Compose a [StringPattern].
/// All predefined patterns in the [patterns] module are brought into scope.
///
/// # Example
///
/// ```
/// let quotes_pattern = gramma::string_pattern!(
///     char('"') + char(..).repeat(..).lazy() + !follows(char('\\')) + char('"')
/// );
///
/// assert!(quotes_pattern.matcher().match_string(4, r#"s = "hello, \"world\"!";"#).is_some());
/// ```
#[macro_export]
macro_rules! string_pattern {
    ($expr:expr $(,)?) => {
        $crate::string_matcher::StringPattern::_validate_string_pattern({
            #[allow(unused)]
            use $crate::string_matcher::patterns::*;
            $expr
        })
    };
}

///
/// ```
/// # use gramma::{define_string_pattern, string_matcher};
/// define_string_pattern!(
///     fn identifier(max_len: impl Into<Option<u32>>) {
///         !precedes(ascii_digit())
///             + word().repeat(1..=max_len.into().unwrap_or(u32::MAX)).simple()
///             + word_boundary()
///     }
/// );
///
/// assert_eq!(string_matcher!(identifier(4)).match_string(0, "foo"), Some(0..3));
/// assert_eq!(string_matcher!(identifier(4)).match_string(0, "foobar"), None);
/// assert_eq!(string_matcher!(identifier(None)).match_string(0, "foobarbaz"), Some(0..9));
/// ```
#[macro_export]
macro_rules! define_string_pattern {
    { $(
        $vis:vis fn $Name:ident (
            $($arg:tt $($arg1:ident)* $(: $Arg:ty)?),* $(,)?
        ) $body:block
    )* } => { $(
        $vis fn $Name ( $($arg $($arg1)* $(: $Arg)?),* ) ->
            $crate::string_matcher::StringPattern<impl $crate::string_matcher::IntoMatchString>
            { $crate::string_pattern! ($body) }
    )* };
}
