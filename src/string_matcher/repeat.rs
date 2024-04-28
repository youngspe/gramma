use core::{
    fmt,
    ops::{Bound, RangeBounds},
};

use crate::utils::default;

use super::{
    machine::{RepeatState, RepeatStyle, StackItem},
    traits::IntoMatchString,
    DebugPrecedence, Link, MatchString, Matcher, StringMatcherContext, StringPattern,
};

struct RepeatContinue {}

pub struct Repeat<'m, M> {
    min: u32,
    max: u32,
    style: RepeatStyle,
    inner: M,
    links: (Link<'m>, Link<'m>),
}

impl<'m, M: MatchString<'m>> Repeat<'m, M> {
    fn get_repeat_parts(
        &'m self,
        cx: &mut StringMatcherContext<'m, '_>,
    ) -> (StackItem<'m>, StackItem<'m>) {
        let (inner, outer) = if cx.is_reversed() {
            (self.inner.last(), self.backward_matcher())
        } else {
            (self.inner.first(), self.forward_matcher())
        };

        let outer = if let Some(outer) = outer {
            StackItem::Matcher { matcher: outer }
        } else {
            StackItem::Accept
        };

        let inner = StackItem::Matcher { matcher: inner };

        (outer, inner)
    }
}

impl<M: IntoMatchString> IntoMatchString for Repeat<'_, M> {
    type Matcher<'m> = Repeat<'m, M::Matcher<'m>>
    where
        Self: 'm;

    fn into_match_string<'m>(self) -> Self::Matcher<'m>
    where
        Self: 'm,
    {
        let Self {
            min,
            max,
            style,
            inner,
            ..
        } = self;
        Self::Matcher {
            min,
            max,
            style,
            inner: inner.into_match_string(),
            links: default(),
        }
    }
}

impl<'m, M: MatchString<'m>> MatchString<'m> for Repeat<'m, M> {
    fn match_string(&'m self, cx: &mut super::StringMatcherContext<'m, '_>) -> Option<bool> {
        let (outer, inner) = self.get_repeat_parts(cx);
        cx.stack.extend([outer, inner]);

        match self.style {
            RepeatStyle::Greedy | RepeatStyle::Lazy => {
                let mut state = cx.state();
                let RepeatState {
                    repeat_index: last_repeat,
                    depth: last_depth,
                    ..
                } = state.repeat;
                let greedy = self.style == RepeatStyle::Greedy;
                let repeat_index: u16 = cx
                    .stack
                    .len()
                    .try_into()
                    .expect("string matcher stack exceeded maximum size");
                cx.stack.push(StackItem::Repeat {
                    min: self.min,
                    max: self.max,
                    greedy,
                    last_repeat,
                    last_depth,
                });

                state.repeat = RepeatState {
                    repeat_index,
                    depth: 0,
                    greedy,
                    min: self.min,
                    max: self.max,
                };
                cx.set_state(state).continue_repeat()
            }
            RepeatStyle::Simple => cx.continue_repeat_simple(self.min, self.max, 0),
        }
    }

    fn links(&'m self) -> Option<super::Links<'m>> {
        Some((&self.links).into())
    }

    fn initialize(&'m self) {
        if self.style != RepeatStyle::Simple {
            let next_matcher = Matcher::from(&RepeatContinue {});
            self.inner.set_backward(Some(next_matcher));
            self.inner.set_forward(Some(next_matcher));
        }
        self.inner.initialize()
    }

    fn fmt_matcher(&self, f: &mut fmt::Formatter, prec: DebugPrecedence) -> fmt::Result {
        prec.wrap_below(DebugPrecedence::Mul, f, |f| {
            self.inner.fmt_matcher(f, DebugPrecedence::Mul)?;
            f.write_str(" * ")?;
            match (self.min, self.max) {
                (min, max) if min == max => write!(f, "{min}")?,
                (0, u32::MAX) => f.write_str("..")?,
                (0, max) => write!(f, "..={max}")?,
                (min, u32::MAX) => write!(f, "{min}..")?,
                (min, max) => write!(f, "{min}..={max}")?,
            }
            Ok(())
        })
    }

    fn quick_test(&'m self, cx: &mut super::StringMatcherContext<'m, '_>) -> Option<bool> {
        let (outer, inner) = self.get_repeat_parts(cx);
        let &Self { min, max, .. } = self;

        let depth = if cx.is_reversed() { min } else { 0 };

        cx.quick_test_repeat(outer, inner, depth, min, max, 1)
    }
}

impl<'m> MatchString<'m> for RepeatContinue {
    fn match_string(
        &'m self,
        cx: &mut super::machine::StringMatcherContext<'m, '_>,
    ) -> Option<bool> {
        cx.continue_repeat()
    }

    fn quick_test(&'m self, cx: &mut super::StringMatcherContext<'m, '_>) -> Option<bool> {
        let RepeatState {
            min,
            max,
            depth: mut repeat_depth,
            ..
        } = cx.state().repeat;
        let (outer, inner) = cx.get_repeat_parts();

        if cx.is_reversed() {
            if repeat_depth == 0 {
                return None;
            }
            repeat_depth -= 1;
        } else {
            repeat_depth = repeat_depth.saturating_add(1);
        }

        cx.quick_test_repeat(outer, inner, repeat_depth, min, max, 1)
    }
}

pub trait RepeatCount {
    fn bounds(&self) -> (Bound<&u32>, Bound<&u32>);
}

macro_rules! repeat_count_ranges {
    ($($Name:ty),* $(,)?) => {
        $(
            impl RepeatCount for $Name {
                fn bounds(&self) -> (Bound<&u32>, Bound<&u32>) {
                    (self.start_bound(), self.end_bound())
                }
            }
        )*
        impl_mul!($($Name,)*);
    };
}

macro_rules! impl_mul {
    ($($Name:ty),* $(,)?) => { $(
        impl<M> core::ops::Mul<$Name> for StringPattern<M> {
            type Output = StringPattern<Repeat<'static, M>>;

            fn mul(self, rhs: $Name) -> Self::Output {
                self.repeat(rhs)
            }
        }
        impl<M> core::ops::Mul<StringPattern<M>> for $Name {
            type Output = StringPattern<Repeat<'static, M>>;

            fn mul(self, rhs: StringPattern<M>) -> Self::Output {
                rhs * self
            }
        }
    )* };
}

repeat_count_ranges!(
    core::ops::RangeInclusive<u32>,
    core::ops::RangeToInclusive<u32>,
    core::ops::RangeFrom<u32>,
    core::ops::RangeFull,
);

impl_mul!(u32);

impl RepeatCount for u32 {
    fn bounds(&self) -> (Bound<&u32>, Bound<&u32>) {
        (Bound::Included(self), Bound::Included(self))
    }
}

/// Equivalent to [`inner.repeat(count)`](StringPattern::repeat).
pub fn repeat<'m, M>(
    count: impl RepeatCount,
    inner: StringPattern<M>,
) -> StringPattern<Repeat<'m, M>> {
    inner.repeat(count)
}

/// Equivalent to [`inner.repeat(..=1)`](StringPattern::repeat).
pub fn optional<'m, M>(inner: StringPattern<M>) -> StringPattern<Repeat<'m, M>> {
    inner.optional()
}

impl<M> StringPattern<M> {
    pub fn repeat<'m>(self, count: impl RepeatCount) -> StringPattern<Repeat<'m, M>> {
        let (start, end) = count.bounds();
        let min = match start {
            Bound::Included(&x) => x,
            Bound::Excluded(&x) => x.saturating_add(1),
            Bound::Unbounded => 0,
        };
        let max = match end {
            Bound::Included(&x) => x,
            Bound::Excluded(&x) => x.saturating_sub(1),
            Bound::Unbounded => u32::MAX,
        };
        StringPattern::new(Repeat {
            min,
            max,
            style: default(),
            inner: self.inner,
            links: default(),
        })
    }

    pub fn optional<'m>(self) -> StringPattern<Repeat<'m, M>> {
        self.repeat(..=1)
    }
}

impl<'m, M> StringPattern<Repeat<'m, M>> {
    /// First attempts to match the most repetitions, then backtracks to fewer
    /// if the rest of the pattern fails.
    pub fn greedy(mut self) -> Self {
        self.inner.style = RepeatStyle::Greedy;
        self
    }
    /// First attempts to match the fewest repetitions, then backtracks to more
    /// if the rest of the pattern fails.
    pub fn lazy(mut self) -> Self {
        self.inner.style = RepeatStyle::Lazy;
        self
    }
    /// Repeats as many times as the repeated pattern matches and does not backtrack.
    pub fn simple(mut self) -> Self {
        self.inner.style = RepeatStyle::Simple;
        self
    }
}
