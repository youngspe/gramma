use core::{borrow::Borrow, fmt};

use crate::utils::default;

use super::{char, Link, Links, MatchString, Matcher, StringMatcher, StringMatcherContext};

#[derive(Clone)]
pub struct Exactly<'m, B> {
    value: B,
    links: (Link<'m>, Link<'m>),
}

impl<'m, B: Borrow<str>> MatchString<'m> for Exactly<'m, B> {
    fn match_string(&'m self, cx: &mut StringMatcherContext<'m, '_>) -> bool {
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

        cx.push_next(self)
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

    fn links(&'m self) -> Links<'m> {
        (&self.links).into()
    }

    fn fmt_matcher(
        &self,
        f: &mut core::fmt::Formatter,
        _: super::private::DebugPrecedence,
    ) -> core::fmt::Result {
        fmt::Debug::fmt(self.value.borrow(), f)
    }
}

pub fn exactly<'m, B: Borrow<str>>(value: B) -> StringMatcher<'m, Exactly<'m, B>> {
    StringMatcher::new(Exactly {
        value,
        links: default(),
    })
}

pub struct Empty<'m> {
    links: (Link<'m>, Link<'m>),
}

impl<'m> MatchString<'m> for Empty<'m> {
    fn match_string(&'m self, cx: &mut StringMatcherContext<'m, '_>) -> bool {
        cx.push_next(self)
    }

    fn links(&'m self) -> Links<'m> {
        (&self.links).into()
    }

    fn first(&'m self) -> Matcher<'m> {
        self.next_link().get().unwrap_or(self.into())
    }

    fn last(&'m self) -> Matcher<'m> {
        self.prev_link().get().unwrap_or(self.into())
    }
}

pub fn empty<'m>() -> StringMatcher<'m, Empty<'m>> {
    StringMatcher::new(Empty { links: default() })
}
