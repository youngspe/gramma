mod basic_matchers;
mod char_matcher;
mod machine;
mod macros;
mod objects;
mod operators;
mod repeat;
mod traits;

pub(crate) use machine::StringMatcherContext;
pub(crate) use objects::{Link, Links, Matcher};
pub(crate) use patterns::*;
pub(crate) use traits::DebugPrecedence;
pub use traits::{IntoMatchString, MatchString};

use crate::utils::default;

use core::{cell::Cell, fmt, ops::Range};

pub mod patterns {
    pub use super::{
        basic_matchers::{empty, exactly, line_end, line_start, src_end, src_start},
        char_matcher::{
            alphabetic, alphanumeric,
            boundary::{boundary, boundary_end, boundary_start, word_boundary},
            char, numeric, whitespace, word,
        },
        operators::{follows, precedes},
        repeat::repeat,
    };
}

#[derive(Clone)]
pub struct StringPattern<M> {
    inner: M,
}

impl<'m, M: MatchString<'m>> fmt::Debug for StringPattern<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt_matcher(f, default())
    }
}

impl<'m, M> StringPattern<M> {
    pub(crate) const fn new(inner: M) -> Self {
        Self { inner }
    }
}

impl<M: IntoMatchString> StringPattern<M> {
    pub fn matcher<'m>(self) -> StringMatcher<M::Matcher<'m>> {
        StringMatcher {
            inner: core::mem::ManuallyDrop::new(self.inner.into_match_string()),
            initialized: false.into(),
        }
    }

    #[doc(hidden)]
    pub fn _validate_string_pattern(self) -> Self {
        self
    }
}

#[test]
fn simple_match() {
    let m = follows(exactly("abcd"))
        + alphabetic().repeat(..)
        + follows(exactly("start").repeat(1..))
        + precedes(exactly("end"));

    let out = m
        .matcher()
        .match_string(4, "abcdefgendhijklstartstartendmnop");

    assert_eq!(out, Some(4..25));
}

impl<M: IntoMatchString> From<M> for StringPattern<M> {
    fn from(inner: M) -> Self {
        Self::new(inner)
    }
}

pub struct StringMatcher<M: ?Sized> {
    initialized: Cell<bool>,
    inner: core::mem::ManuallyDrop<M>,
}

pub type AnyStringMatcher<'m> = &'m StringMatcher<dyn MatchString<'m> + 'm>;

impl<'m, M: MatchString<'m> + ?Sized> StringMatcher<M> {
    pub fn match_string(&'m self, start: usize, src: &str) -> Option<Range<usize>> {
        if !self.initialized.replace(true) {
            self.inner.initialize();
        }
        let mut stack = default();
        let mut cx = StringMatcherContext::new(src, &mut stack);

        cx.move_to(start).push_matcher(&*self.inner);
        cx.execute().then(|| start..cx.position())
    }
}
