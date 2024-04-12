use core::{
    fmt,
    ops::{Add, BitAnd, BitOr, Not},
};

use crate::utils::default;

use super::{
    char_matcher::MatchChar,
    traits::{IntersectPattern, IntoMatchString, NegatePattern},
    DebugPrecedence, Link, MatchString, StringPattern,
};

pub struct MatchPlus<A, B> {
    matcher1: A,
    matcher2: B,
}

impl<A, B> IntoMatchString for MatchPlus<A, B>
where
    A: IntoMatchString,
    B: IntoMatchString,
{
    type Matcher<'m> = MatchPlus<A::Matcher<'m>, B::Matcher<'m>> where Self: 'm;

    fn into_match_string<'m>(self) -> Self::Matcher<'m>
    where
        Self: 'm,
    {
        Self::Matcher {
            matcher1: self.matcher1.into_match_string(),
            matcher2: self.matcher2.into_match_string(),
        }
    }
}

impl<'m, A, B> MatchString<'m> for MatchPlus<A, B>
where
    A: MatchString<'m>,
    B: MatchString<'m>,
{
    fn match_string(&'m self, cx: &mut super::StringMatcherContext<'m, '_>) -> Option<bool> {
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

    fn should_push(&'m self, cx: &mut super::StringMatcherContext<'m, '_>) -> bool {
        if cx.is_reversed() {
            self.matcher2.should_push(cx)
        } else {
            self.matcher1.should_push(cx)
        }
    }

    fn smart_push(&'m self, cx: &mut super::StringMatcherContext<'m, '_>) -> bool {
        if cx.is_reversed() {
            self.matcher2.smart_push(cx)
        } else {
            self.matcher1.smart_push(cx)
        }
    }
}

impl<'m, A, B> Add<StringPattern<B>> for StringPattern<A> {
    type Output = StringPattern<MatchPlus<A, B>>;

    fn add(self, rhs: StringPattern<B>) -> Self::Output {
        StringPattern::new(MatchPlus {
            matcher1: self.inner,
            matcher2: rhs.inner,
        })
    }
}

pub struct MatchOr<A, B> {
    matcher1: A,
    matcher2: B,
}

impl<A, B> IntoMatchString for MatchOr<A, B>
where
    A: IntoMatchString,
    B: IntoMatchString,
{
    type Matcher<'m> = MatchOr<A::Matcher<'m>, B::Matcher<'m>> where Self: 'm;

    fn into_match_string<'m>(self) -> Self::Matcher<'m>
    where
        Self: 'm,
    {
        Self::Matcher {
            matcher1: self.matcher1.into_match_string(),
            matcher2: self.matcher2.into_match_string(),
        }
    }
}

impl<'m, A, B> MatchString<'m> for MatchOr<A, B>
where
    A: MatchString<'m>,
    B: MatchString<'m>,
{
    fn match_string(&'m self, cx: &mut super::StringMatcherContext<'m, '_>) -> Option<bool> {
        if let Some(char_matcher) = self.as_char_matcher() {
            if !cx.match_char(&char_matcher) {
                return Some(false);
            }

            return cx.run_next(&self.matcher1);
        }

        if cx.smart_push_matcher(&self.matcher2) {
            cx.push_reset();
        }

        match cx.run_matcher(&self.matcher1) {
            Some(false) => None,
            out => out,
        }
    }

    fn should_push(&'m self, cx: &mut super::StringMatcherContext<'m, '_>) -> bool {
        self.matcher1.should_push(cx) || self.matcher2.should_push(cx)
    }

    fn smart_push(&'m self, cx: &mut super::StringMatcherContext<'m, '_>) -> bool {
        if cx.smart_push_matcher(&self.matcher2) {
            cx.push_reset();
        }
        cx.smart_push_matcher(&self.matcher1)
    }

    fn links(&'m self) -> super::Links<'m> {
        self.matcher1.links()
    }

    fn prev_link(&'m self) -> &'m Link<'m> {
        self.matcher1.prev_link()
    }

    fn next_link(&'m self) -> &'m Link<'m> {
        self.matcher1.next_link()
    }

    fn as_char_matcher(&'m self) -> Option<impl MatchChar + 'm> {
        Some((
            self.matcher1.as_char_matcher()?,
            self.matcher2.as_char_matcher()?,
        ))
    }

    fn initialize(&'m self) {
        self.matcher1.initialize();

        self.matcher2.prev_link().set(self.prev_link().get());
        self.matcher2.next_link().set(self.next_link().get());
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

impl<A, B, AB> NegatePattern for MatchOr<A, B>
where
    A: NegatePattern,
    B: NegatePattern,
    A::Output: IntersectPattern<B::Output, Output = AB>,
{
    type Output = AB;

    fn negate_pattern(self) -> Self::Output {
        self.matcher1
            .negate_pattern()
            .intersect_pattern(self.matcher2.negate_pattern())
    }
}

impl<A, B> BitOr<StringPattern<B>> for StringPattern<A> {
    type Output = StringPattern<MatchOr<A, B>>;

    fn bitor(self, rhs: StringPattern<B>) -> Self::Output {
        StringPattern::new(MatchOr {
            matcher1: self.inner,
            matcher2: rhs.inner,
        })
    }
}

pub struct Lookaround<'m, M, const REVERSE: bool, const NEGATE: bool> {
    inner: M,
    links: (Link<'m>, Link<'m>),
}

impl<'m, M, const REVERSE: bool, const NEGATE: bool> Lookaround<'m, M, REVERSE, NEGATE> {
    fn pop_pair(&self, next_len: usize) -> (u16, u16) {
        if NEGATE {
            (next_len as u16, 0)
        } else {
            (0, next_len as u16)
        }
    }
}

impl<M, const REVERSE: bool, const NEGATE: bool> IntoMatchString
    for Lookaround<'_, M, REVERSE, NEGATE>
where
    M: IntoMatchString,
{
    type Matcher<'m> = Lookaround<'m, M::Matcher<'m>, REVERSE, NEGATE> where Self: 'm;

    fn into_match_string<'m>(self) -> Self::Matcher<'m>
    where
        Self: 'm,
    {
        Self::Matcher {
            inner: self.inner.into_match_string(),
            links: default(),
        }
    }
}

impl<'m, M, const REVERSE: bool, const NEGATE: bool> MatchString<'m>
    for Lookaround<'m, M, REVERSE, NEGATE>
where
    M: MatchString<'m>,
{
    fn match_string(&'m self, cx: &mut super::StringMatcherContext<'m, '_>) -> Option<bool> {
        let pre_stack_len = cx.stack.len();

        cx.push_next_or_accept(self);
        if cx.stack.len() == pre_stack_len {
            return Some(false);
        }
        cx.push_reset();
        let (pop_ok, pop_err) = self.pop_pair(cx.stack.len() - pre_stack_len);
        cx.push_frame(pop_ok, pop_err).reverse(REVERSE);
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

    fn should_push(&'m self, cx: &mut super::StringMatcherContext<'m, '_>) -> bool {
        if !NEGATE {
            let state = cx.state();
            let should_push_inner = self.inner.should_push(cx.reverse(REVERSE));
            cx.set_state(state);
            if !should_push_inner {
                return false;
            }
        };

        let next = if cx.is_reversed() {
            self.prev_link().get()
        } else {
            self.next_link().get()
        };

        if let Some(next) = next {
            cx.should_push_matcher(next)
        } else {
            true
        }
    }
}

/// Tests whether the given matcher matches starting at the current location
/// without changing the current location.
///
/// #Example
///
/// ```
/// # use gramma::string_matcher;
///
/// string_matcher!(precedes(exactly("bar"))).match_string(3, "foobar").unwrap();
/// // `precedes` can be negated:
/// string_matcher!(!precedes(exactly("foo"))).match_string(3, "foobar").unwrap();
/// ```
pub fn precedes<'m, M: IntoMatchString>(
    matcher: StringPattern<M>,
) -> StringPattern<Lookaround<'m, M, false, false>> {
    StringPattern::new(Lookaround {
        inner: matcher.inner,
        links: default(),
    })
}

/// Tests whether the given matcher matches ending at the current location
/// without changing the current location.
///
/// #Example
///
/// ```
/// # use gramma::string_matcher;
///
/// string_matcher!(follows(exactly("foo"))).match_string(3, "foobar").unwrap();
/// // `follows` can be negated:
/// string_matcher!(!follows(exactly("bar"))).match_string(3, "foobar").unwrap();
/// ```
pub fn follows<'m, M: IntoMatchString>(
    matcher: StringPattern<M>,
) -> StringPattern<Lookaround<'m, M, true, false>> {
    StringPattern::new(Lookaround {
        inner: matcher.inner,
        links: default(),
    })
}

impl<'m, M, const REVERSE: bool> NegatePattern for Lookaround<'m, M, REVERSE, false> {
    type Output = Lookaround<'m, M, REVERSE, true>;

    fn negate_pattern(self) -> Self::Output {
        Lookaround {
            inner: self.inner,
            links: self.links,
        }
    }
}

impl<'m, M, const REVERSE: bool> NegatePattern for Lookaround<'m, M, REVERSE, true> {
    type Output = Lookaround<'m, M, REVERSE, false>;

    fn negate_pattern(self) -> Self::Output {
        Lookaround {
            inner: self.inner,
            links: self.links,
        }
    }
}

impl<M> Not for StringPattern<M>
where
    M: NegatePattern,
{
    type Output = StringPattern<M::Output>;

    fn not(self) -> Self::Output {
        StringPattern::new(self.inner.negate_pattern())
    }
}

impl<M> StringPattern<M>
where
    M: NegatePattern,
{
    pub fn not(self) -> StringPattern<M::Output> {
        !self
    }
}

impl<M1, M2> BitAnd<StringPattern<M2>> for StringPattern<M1>
where
    M1: IntersectPattern<M2>,
{
    type Output = StringPattern<M1::Output>;

    fn bitand(self, rhs: StringPattern<M2>) -> Self::Output {
        StringPattern::new(self.inner.intersect_pattern(rhs.inner))
    }
}
