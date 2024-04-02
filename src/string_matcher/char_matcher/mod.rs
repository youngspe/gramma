use core::{
    fmt,
    ops::{Range, RangeFrom, RangeInclusive, RangeTo, RangeToInclusive},
};

use fmt::Debug;

use crate::utils::{default, DebugFn};

use super::{
    traits::{IntersectPattern, IntoMatchString, NegatePattern},
    DebugPrecedence, Link, Links, MatchString, StringPattern,
};

pub trait MatchChar {
    fn match_char(&self, ch: char) -> bool;

    #[inline(always)]
    fn match_start(&self, src: &str) -> bool {
        src.starts_with(|ch| self.match_char(ch))
    }

    #[inline(always)]
    fn match_end(&self, src: &str) -> bool {
        src.ends_with(|ch| self.match_char(ch))
    }

    fn fmt_char_matcher(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(core::any::type_name::<Self>())
    }

    fn as_debug(&self) -> impl Debug + '_ {
        DebugFn(|f| self.fmt_char_matcher(f))
    }
}

impl MatchChar for char {
    #[inline(always)]
    fn match_char(&self, ch: char) -> bool {
        *self == ch
    }

    #[inline(always)]
    fn match_start(&self, src: &str) -> bool {
        src.starts_with(*self)
    }

    #[inline(always)]
    fn match_end(&self, src: &str) -> bool {
        src.ends_with(*self)
    }

    fn fmt_char_matcher(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt(f)
    }
}

impl MatchChar for &str {
    #[inline(always)]
    fn match_char(&self, ch: char) -> bool {
        self.contains(ch)
    }

    fn fmt_char_matcher(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt(f)
    }
}

impl<F> MatchChar for F
where
    F: Fn(char) -> bool,
{
    #[inline(always)]
    fn match_char(&self, ch: char) -> bool {
        self(ch)
    }

    #[inline(always)]
    fn match_start(&self, src: &str) -> bool {
        src.starts_with(self)
    }

    #[inline(always)]
    fn match_end(&self, src: &str) -> bool {
        src.ends_with(self)
    }
}

macro_rules! contains_impl {
    (@is_pattern [pattern]) => {
        #[inline(always)]
        fn match_start(&self, src: &str) -> bool {
            src.starts_with(&**self)
        }

        #[inline(always)]
        fn match_end(&self, src: &str) -> bool {
            src.ends_with(&**self)
        }
    };
    (@is_pattern) => {};
    ($($Type:ty $(as $is_pattern:ident)?),* $(,)?) => {$(
        impl MatchChar for $Type {
            #[inline(always)]
            fn match_char(&self, ch: char) -> bool {
                self.contains(&ch)
            }

            contains_impl!(@is_pattern $([$is_pattern])?);

            fn fmt_char_matcher(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.fmt(f)
            }
        }
    )*};
}

impl MatchChar for core::ops::RangeFull {
    fn match_char(&self, _: char) -> bool {
        true
    }
}

contains_impl! {
    Range<char>, RangeTo<char>, RangeFrom<char>,
    RangeInclusive<char>, RangeToInclusive<char>,
    &[char] as pattern, &mut [char] as pattern,
}

impl<const N: usize> MatchChar for [char; N] {
    fn match_char(&self, ch: char) -> bool {
        self.contains(&ch)
    }
}

impl MatchChar for () {
    fn match_char(&self, _: char) -> bool {
        false
    }
}

macro_rules! tuple_impl {
    (@impl $($T:ident)+ ) => {
        impl <$($T: MatchChar),*> MatchChar for ($($T,)*) {
            #[inline(always)]
            fn match_char(&self, ch: char) -> bool {
                #[allow(non_snake_case)]
                let ($($T,)*) = self;
                $($T.match_char(ch) ||)* false
            }

            #[inline(always)]
            fn match_start(&self, src: &str) -> bool {
                #[allow(non_snake_case)]
                let ($($T,)*) = self;
                $($T.match_start(src) ||)* false
            }

            #[inline(always)]
            fn match_end(&self, src: &str) -> bool {
                #[allow(non_snake_case)]
                let ($($T,)*) = self;
                $($T.match_end(src) ||)* false
            }
        }
    };
    () => {};
    ($T0:ident $($T:ident)* ) => {
        tuple_impl!($($T)*);
        tuple_impl!(@impl $T0 $($T)*);
    }
}

tuple_impl!(T0 T1 T2 T3 T4 T5 T6 T7);

struct MatchCharRef<'lt, M: ?Sized> {
    inner: &'lt M,
}

impl<M: ?Sized + MatchChar> fmt::Debug for MatchCharRef<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt_char_matcher(f)
    }
}

impl<M: MatchChar + ?Sized> MatchChar for MatchCharRef<'_, M> {
    fn match_char(&self, ch: char) -> bool {
        self.inner.match_char(ch)
    }
}

impl<'m, M: MatchChar> MatchChar for StringPattern<CharMatcher<'m, M>> {
    #[inline(always)]
    fn match_char(&self, ch: char) -> bool {
        self.inner.inner.match_char(ch)
    }

    #[inline(always)]
    fn match_start(&self, src: &str) -> bool {
        self.inner.inner.match_start(src)
    }

    #[inline(always)]
    fn match_end(&self, src: &str) -> bool {
        self.inner.inner.match_end(src)
    }
}

pub struct IntersectChar<M1, M2> {
    matcher1: M1,
    matcher2: M2,
}

impl<M1, M2> MatchChar for IntersectChar<M1, M2>
where
    M1: MatchChar,
    M2: MatchChar,
{
    fn match_char(&self, ch: char) -> bool {
        self.matcher1.match_char(ch) && self.matcher2.match_char(ch)
    }

    fn match_start(&self, src: &str) -> bool {
        self.matcher1.match_start(src) && self.matcher2.match_start(src)
    }

    fn match_end(&self, src: &str) -> bool {
        self.matcher1.match_end(src) && self.matcher2.match_end(src)
    }

    fn fmt_char_matcher(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.matcher1.fmt_char_matcher(f)?;
        f.write_str("&")?;
        self.matcher2.fmt_char_matcher(f)?;
        Ok(())
    }
}

pub struct NotChar<M> {
    inner: M,
}

impl<M> MatchChar for NotChar<M>
where
    M: MatchChar,
{
    fn match_char(&self, ch: char) -> bool {
        !self.inner.match_char(ch)
    }

    fn match_start(&self, src: &str) -> bool {
        !self.inner.match_start(src)
    }

    fn match_end(&self, src: &str) -> bool {
        !self.inner.match_end(src)
    }

    fn fmt_char_matcher(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("!")?;
        self.inner.fmt_char_matcher(f)
    }
}

#[derive(Clone)]
pub struct CharMatcher<'m, M> {
    inner: M,
    links: (Link<'m>, Link<'m>),
}

impl<M> CharMatcher<'_, M> {
    pub fn new(inner: M) -> Self {
        Self {
            inner,
            links: default(),
        }
    }
}

impl<M: MatchChar> IntoMatchString for CharMatcher<'_, M> {
    type Matcher<'m> = CharMatcher<'m, M> where Self: 'm;

    fn into_match_string<'m>(self) -> Self::Matcher<'m>
    where
        Self: 'm,
    {
        let Self { inner, .. } = self;
        CharMatcher {
            inner,
            links: default(),
        }
    }
}

impl<'m, M: MatchChar> MatchString<'m> for CharMatcher<'m, M> {
    fn match_string(&'m self, cx: &mut super::StringMatcherContext<'m, '_>) -> Option<bool> {
        let ch = if cx.is_reversed() {
            cx.pre().chars().next_back()
        } else {
            cx.post().chars().next()
        };

        let Some(ch) = ch else {
            return Some(false);
        };

        if !self.inner.match_char(ch) {
            return Some(false);
        }

        if cx.is_reversed() {
            cx.back_by(ch.len_utf8());
        } else {
            cx.forward_by(ch.len_utf8());
        }

        cx.run_next(self)
    }

    fn as_char_matcher(&'m self) -> Option<impl MatchChar + 'm>
    where
        Self: Sized,
    {
        Some(MatchCharRef { inner: &self.inner })
    }

    fn links(&'m self) -> Links<'m> {
        (&self.links).into()
    }

    fn fmt_matcher(&self, f: &mut fmt::Formatter, _: DebugPrecedence) -> fmt::Result {
        self.inner.fmt_char_matcher(f)
    }
}

impl<'m, M1: MatchChar, M2: MatchChar> IntersectPattern<CharMatcher<'m, M2>>
    for CharMatcher<'m, M1>
{
    type Output = CharMatcher<'m, IntersectChar<M1, M2>>;

    fn intersect_pattern(self, rhs: CharMatcher<'m, M2>) -> Self::Output {
        CharMatcher::new(IntersectChar {
            matcher1: self.inner,
            matcher2: rhs.inner,
        })
    }
}

impl<'m, M: MatchChar> NegatePattern for CharMatcher<'m, M> {
    type Output = CharMatcher<'m, NotChar<M>>;

    fn negate_pattern(self) -> Self::Output {
        CharMatcher::new(NotChar { inner: self.inner })
    }
}

pub fn char<'m, M: MatchChar>(matcher: M) -> StringPattern<CharMatcher<'m, M>> {
    StringPattern::new(CharMatcher::new(matcher))
}

pub fn whitespace<'m>() -> StringPattern<CharMatcher<'m, impl MatchChar>> {
    char(|x: char| char::is_whitespace(x))
}

pub fn alphanumeric<'m>() -> StringPattern<CharMatcher<'m, impl MatchChar>> {
    char(char::is_alphanumeric)
}

pub fn alphabetic<'m>() -> StringPattern<CharMatcher<'m, impl MatchChar>> {
    char(char::is_alphabetic)
}

pub fn numeric<'m>() -> StringPattern<CharMatcher<'m, impl MatchChar>> {
    char(char::is_numeric)
}

pub fn word<'m>() -> StringPattern<CharMatcher<'m, impl MatchChar>> {
    char(|ch| matches!(ch, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9'))
}

pub(crate) mod boundary;
