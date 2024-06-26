use core::{borrow::Borrow, fmt};

use crate::utils::default;

use super::{
    char, follows, precedes, traits::IntoMatchString, Link, Links, MatchString, Matcher,
    StringMatcherContext, StringPattern,
};

pub struct Exactly<'m, B> {
    value: B,
    links: (Link<'m>, Link<'m>),
}

impl<B: Borrow<str>> IntoMatchString for Exactly<'_, B> {
    type Matcher<'m> = Exactly<'m, B> where Self: 'm;

    fn into_match_string<'m>(self) -> Self::Matcher<'m>
    where
        Self: 'm,
    {
        Self::Matcher {
            value: self.value,
            links: default(),
        }
    }
}

impl<'m, B: Borrow<str>> Exactly<'m, B> {
    fn match_exact(&'m self, cx: &mut StringMatcherContext<'m, '_>) -> bool {
        let value = self.value.borrow();
        if cx.is_reversed() {
            if !cx.pre().ends_with(value) {
                return false;
            }
            cx.back_by(value.len());
        } else {
            if !cx.post().starts_with(value) {
                return false;
            }
            cx.forward_by(value.len());
        }

        true
    }
}

impl<'m, B: Borrow<str>> MatchString<'m> for Exactly<'m, B> {
    fn match_string(&'m self, cx: &mut StringMatcherContext<'m, '_>) -> Option<bool> {
        if !self.match_exact(cx) {
            return Some(false);
        }

        cx.run_next(self)
    }

    fn as_char_matcher(&'m self) -> Option<impl super::char_matcher::MatchChar + 'm>
    where
        Self: Sized,
    {
        let mut chars = self.value.borrow().chars();
        let ch = chars.next()?;
        // return None if there is more than 1 char in the string
        chars.next().is_none().then_some(char(ch))
    }

    fn links(&'m self) -> Option<Links<'m>> {
        Some((&self.links).into())
    }

    fn fmt_matcher(
        &self,
        f: &mut core::fmt::Formatter,
        _: super::DebugPrecedence,
    ) -> core::fmt::Result {
        fmt::Debug::fmt(self.value.borrow(), f)
    }

    fn quick_test(&'m self, cx: &mut StringMatcherContext<'m, '_>) -> Option<bool> {
        if !self.match_exact(cx) {
            return Some(false);
        }

        let Some(next) = self.next_matcher(cx.is_reversed()) else {
            return Some(true);
        };

        cx.quick_test_matcher(next, 1)
    }
}

/// Matches an exact string.
pub fn exactly<'m, B: Borrow<str>>(value: B) -> StringPattern<Exactly<'m, B>> {
    StringPattern::new(Exactly {
        value,
        links: default(),
    })
}

pub struct Empty<'m> {
    links: (Link<'m>, Link<'m>),
}

impl IntoMatchString for Empty<'_> {
    type Matcher<'m> = Empty<'m>
    where
        Self: 'm;

    fn into_match_string<'m>(self) -> Self::Matcher<'m>
    where
        Self: 'm,
    {
        Empty { links: default() }
    }
}

impl<'m> MatchString<'m> for Empty<'m> {
    fn match_string(&'m self, cx: &mut StringMatcherContext<'m, '_>) -> Option<bool> {
        cx.run_next(self)
    }

    fn links(&'m self) -> Option<Links<'m>> {
        Some((&self.links).into())
    }

    fn first(&'m self) -> Matcher<'m> {
        self.forward_matcher().unwrap_or(self.into())
    }

    fn last(&'m self) -> Matcher<'m> {
        self.backward_matcher().unwrap_or(self.into())
    }
}

/// Matches the empty string.
pub fn empty<'m>() -> StringPattern<Empty<'m>> {
    StringPattern::new(Empty { links: default() })
}

/// Matches the start of a line.
pub fn line_start<'m>() -> StringPattern<impl IntoMatchString> {
    follows(char('\n'))
}

/// Matches the end of a line.
pub fn line_end<'m>() -> StringPattern<impl IntoMatchString> {
    precedes(exactly("\r\n") | char('\n'))
}

/// Matches the start of the source string.
pub fn src_start<'m>() -> StringPattern<impl IntoMatchString> {
    !follows(char(..))
}

/// Matches the end of the source string.
pub fn src_end<'m>() -> StringPattern<impl IntoMatchString> {
    !precedes(char(..))
}

#[test]
fn src_start_at_start() {
    let out = src_start().matcher().match_string(0, "asdf");
    assert_eq!(out, Some(0..0));
}

#[test]
fn src_start_mid_src() {
    let out = src_start().matcher().match_string(2, "asdf");
    assert_eq!(out, None);
}

#[test]
fn src_start_at_end() {
    let out = src_start().matcher().match_string(4, "asdf");
    assert_eq!(out, None);
}
