use core::{cell::Cell, fmt};

use super::traits::{AsMatcher, DebugPrecedence, MatchString};

#[derive(Default, Clone)]
pub struct Link<'m>(Cell<Option<Matcher<'m>>>);

impl<'m> Link<'m> {
    pub(crate) fn get(&self) -> Option<Matcher<'m>> {
        self.0.get()
    }
    pub(crate) fn set(&self, matcher: impl Into<Option<Matcher<'m>>>) {
        self.0.set(matcher.into())
    }
}

#[derive(Clone, Copy)]
pub struct Links<'m>(pub &'m Link<'m>, pub &'m Link<'m>);

impl<'m> From<&'m (Link<'m>, Link<'m>)> for Links<'m> {
    fn from((a, b): &'m (Link<'m>, Link<'m>)) -> Self {
        Self(a, b)
    }
}

#[derive(Clone, Copy)]
pub struct Matcher<'m> {
    inner: &'m (dyn MatchString<'m> + 'm),
}

impl fmt::Debug for Matcher<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt_matcher(f, DebugPrecedence::Initial)
    }
}

impl<'m> core::ops::Deref for Matcher<'m> {
    type Target = &'m (dyn MatchString<'m> + 'm);

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'m, M: MatchString<'m> + ?Sized> From<&'m M> for Matcher<'m> {
    fn from(matcher: &'m M) -> Self {
        matcher.as_matcher()
    }
}

impl<'m, M: MatchString<'m>> AsMatcher<'m> for M {
    fn _as_matcher(&'m self) -> Matcher<'m> {
        Matcher { inner: self }
    }
}
