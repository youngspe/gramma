use core::{
    fmt,
    ops::{Add, BitOr},
};

use crate::utils::default;

use super::{char_matcher::MatchChar, private::DebugPrecedence, Link, MatchString, StringMatcher};

#[derive(Clone)]
pub struct MatchPlus<A, B> {
    matcher1: A,
    matcher2: B,
}

impl<'m, A, B> MatchString<'m> for MatchPlus<A, B>
where
    A: MatchString<'m>,
    B: MatchString<'m>,
{
    fn match_string(&'m self, cx: &mut super::StringMatcherContext<'m, '_>) -> bool {
        if cx.is_reversed() {
            cx.run_matcher(&self.matcher2)
        } else {
            cx.run_matcher(&self.matcher1)
        }
    }

    fn links(&'m self) -> super::Links<'m> {
        super::Links(self.prev_link(), self.next_link())
    }

    fn prev_link(&'m self) -> &'m Link<'m> {
        self.matcher1.prev_link()
    }

    fn next_link(&'m self) -> &'m Link<'m> {
        self.matcher2.next_link()
    }

    fn initialize(&'m self) {
        self.matcher1.next_link().set(self.matcher2.first());
        self.matcher1.initialize();

        self.matcher2.prev_link().set(self.matcher1.last());
        self.matcher2.initialize();
    }

    fn fmt_matcher(&self, f: &mut fmt::Formatter, prec: DebugPrecedence) -> fmt::Result {
        prec.wrap_below(DebugPrecedence::Add, f, |f| {
            self.matcher1.fmt_matcher(f, DebugPrecedence::Add)?;
            f.write_str(" + ")?;
            self.matcher2.fmt_matcher(f, DebugPrecedence::Add)?;
            Ok(())
        })
    }
}

impl<'m, A, B> Add<StringMatcher<'m, B>> for StringMatcher<'m, A>
where
    A: MatchString<'m>,
    B: MatchString<'m>,
{
    type Output = StringMatcher<'m, MatchPlus<A, B>>;

    fn add(self, rhs: StringMatcher<B>) -> Self::Output {
        StringMatcher::new(MatchPlus {
            matcher1: self.inner,
            matcher2: rhs.inner,
        })
    }
}

#[derive(Clone)]
pub struct MatchOr<'m, A, B> {
    matcher1: A,
    matcher2: B,
    links: (Link<'m>, Link<'m>),
}

impl<'m, A, B> MatchString<'m> for MatchOr<'m, A, B>
where
    A: MatchString<'m>,
    B: MatchString<'m>,
{
    fn match_string(&'m self, cx: &mut super::StringMatcherContext<'m, '_>) -> bool {
        cx.push_matcher(&self.matcher2).push_reset();
        cx.run_matcher(&self.matcher1)
    }

    fn links(&'m self) -> super::Links<'m> {
        (&self.links).into()
    }

    fn as_char_matcher(&'m self) -> Option<impl MatchChar + 'm> {
        Some((
            self.matcher1.as_char_matcher()?,
            self.matcher2.as_char_matcher()?,
        ))
    }

    fn initialize(&'m self) {
        self.matcher1.prev_link().set(self.links.0.get());
        self.matcher1.next_link().set(self.links.1.get());
        self.matcher1.initialize();

        self.matcher2.prev_link().set(self.links.0.get());
        self.matcher2.next_link().set(self.links.1.get());
        self.matcher2.initialize();
    }

    fn fmt_matcher(&self, f: &mut fmt::Formatter, prec: DebugPrecedence) -> fmt::Result {
        prec.wrap_below(DebugPrecedence::Or, f, |f| {
            self.matcher1.fmt_matcher(f, DebugPrecedence::Or)?;
            f.write_str(" | ")?;
            self.matcher2.fmt_matcher(f, DebugPrecedence::Or)?;
            Ok(())
        })
    }
}

impl<'m, A, B> BitOr<StringMatcher<'m, B>> for StringMatcher<'m, A>
where
    A: MatchString<'m>,
    B: MatchString<'m>,
{
    type Output = StringMatcher<'m, MatchOr<'m, A, B>>;

    fn bitor(self, rhs: StringMatcher<B>) -> Self::Output {
        StringMatcher::new(MatchOr {
            matcher1: self.inner,
            matcher2: rhs.inner,
            links: default(),
        })
    }
}

#[derive(Clone)]
pub struct Lookaround<'m, M, const REVERSE: bool> {
    inner: M,
    links: (Link<'m>, Link<'m>),
}

impl<'m, M, const REVERSE: bool> MatchString<'m> for Lookaround<'m, M, REVERSE>
where
    M: MatchString<'m>,
{
    fn match_string(&'m self, cx: &mut super::StringMatcherContext<'m, '_>) -> bool {
        cx.push_next_or_accept(self)
            .push_reset()
            .push_frame((), 2)
            .reverse(REVERSE);
        cx.run_matcher(&self.inner)
    }

    fn links(&'m self) -> super::Links<'m> {
        (&self.links).into()
    }

    fn initialize(&'m self) {
        self.inner.initialize()
    }

    fn fmt_matcher(&self, f: &mut fmt::Formatter, _: DebugPrecedence) -> fmt::Result {
        f.debug_tuple(if REVERSE { "follows" } else { "precedes" })
            .field(&self.inner.as_debug(default()))
            .finish()
    }
}

pub fn precedes<'m, M: MatchString<'m>>(
    matcher: StringMatcher<M>,
) -> StringMatcher<Lookaround<'m, M, false>> {
    StringMatcher::new(Lookaround {
        inner: matcher.inner,
        links: default(),
    })
}

pub fn follows<'m, M: MatchString<'m>>(
    matcher: StringMatcher<M>,
) -> StringMatcher<Lookaround<'m, M, true>> {
    StringMatcher::new(Lookaround {
        inner: matcher.inner,
        links: default(),
    })
}
