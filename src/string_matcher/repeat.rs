use core::{
    fmt,
    ops::{Bound, RangeBounds},
};

use crate::utils::default;

use super::{
    machine::{RepeatState, RepeatStyle, StackItem},
    traits::IntoMatchString,
    DebugPrecedence, Link, Links, MatchString, Matcher, StringPattern,
};

struct RepeatContinue {}

pub struct Repeat<'m, M> {
    min: u32,
    max: u32,
    style: RepeatStyle,
    inner: M,
    links: (Link<'m>, Link<'m>),
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
        let (inner, outer) = if cx.is_reversed() {
            (self.inner.last(), self.prev_link())
        } else {
            (self.inner.first(), self.next_link())
        };

        cx.stack.push(
            outer
                .get()
                .map_or(StackItem::Accept, |m| StackItem::Matcher {
                    matcher: m.into(),
                }),
        );
        cx.push_matcher(inner);

        match self.style {
            RepeatStyle::Greedy | RepeatStyle::Lazy => {
                let mut state = cx.state();
                let RepeatState {
                    repeat_index: last_repeat,
                    depth: last_depth,
                    ..
                } = state.repeat;
                let greedy = self.style == RepeatStyle::Greedy;
                let repeat_index = cx.stack.len() as u16;
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
                cx.set_state(state);
                cx.continue_repeat()
            }
            RepeatStyle::Simple => cx.continue_repeat_simple(self.min, self.max, 0),
        }
    }

    fn links(&'m self) -> super::Links<'m> {
        (&self.links).into()
    }

    fn initialize(&'m self) {
        if self.style != RepeatStyle::Simple {
            let Links(prev, next) = self.inner.links();
            let next_matcher = Matcher::from(&RepeatContinue {});
            prev.set(next_matcher);
            next.set(next_matcher);
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
}

impl<'m> MatchString<'m> for RepeatContinue {
    fn match_string(
        &'m self,
        cx: &mut super::machine::StringMatcherContext<'m, '_>,
    ) -> Option<bool> {
        cx.continue_repeat()
    }

    fn links(&'m self) -> Links<'m> {
        panic!()
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
    // core::ops::Range<u32>,
    core::ops::RangeInclusive<u32>,
    // core::ops::RangeTo<u32>,
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

pub fn repeat<'m, M>(
    count: impl RepeatCount,
    inner: StringPattern<M>,
) -> StringPattern<Repeat<'m, M>> {
    inner.repeat(count)
}

pub fn optional<'m, M>(
    inner: StringPattern<M>,
) -> StringPattern<Repeat<'m, M>> {
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
    pub fn greedy(mut self) -> Self {
        self.inner.style = RepeatStyle::Greedy;
        self
    }
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
