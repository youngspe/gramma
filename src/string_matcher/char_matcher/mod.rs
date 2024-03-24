use core::{
    fmt,
    ops::{Range, RangeFrom, RangeInclusive, RangeTo, RangeToInclusive},
};

use fmt::Debug;

use crate::utils::{default, DebugFn};

use super::{private::DebugPrecedence, Link, Links, MatchString, StringMatcher};

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

impl<'m, M: MatchChar> MatchChar for StringMatcher<'m, CharMatcher<'m, M>> {
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

impl<'m, M: MatchChar + 'm> MatchString<'m> for CharMatcher<'m, M> {
    fn match_string(&'m self, cx: &mut super::StringMatcherContext<'m, '_>) -> bool {
        let ch = not_false!(if cx.is_reversed() {
            cx.pre().chars().next_back()
        } else {
            cx.post().chars().next()
        });

        not_false!(self.inner.match_char(ch));

        if cx.is_reversed() {
            cx.back_by(ch.len_utf8());
        } else {
            cx.forward_by(ch.len_utf8());
        }

        cx.push_next(self)
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

pub fn char<'m, M: MatchChar>(matcher: M) -> StringMatcher<'m, CharMatcher<'m, M>> {
    StringMatcher::new(CharMatcher::new(matcher))
}

pub fn whitespace<'m>() -> StringMatcher<'m, CharMatcher<'m, impl MatchChar>> {
    char(|x: char| char::is_whitespace(x))
}

pub fn alphanumeric<'m>() -> StringMatcher<'m, CharMatcher<'m, impl MatchChar>> {
    char(char::is_alphanumeric)
}

pub fn alphabetic<'m>() -> StringMatcher<'m, CharMatcher<'m, impl MatchChar>> {
    char(char::is_alphabetic)
}

pub fn numeric<'m>() -> StringMatcher<'m, CharMatcher<'m, impl MatchChar>> {
    char(char::is_numeric)
}

pub fn word<'m>() -> StringMatcher<'m, CharMatcher<'m, impl MatchChar>> {
    char(|ch| matches!(ch, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9'))
}

pub(crate) mod boundary;
