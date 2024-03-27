use core::{
    fmt,
    ops::{Bound, RangeBounds},
};

use crate::utils::default;

use super::{traits::IntoMatchString, DebugPrecedence, Link, MatchString, StringPattern};

pub struct Repeat<'m, M> {
    min: u32,
    max: u32,
    greedy: bool,
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
            greedy,
            inner,
            ..
        } = self;
        Self::Matcher {
            min,
            max,
            greedy,
            inner: inner.into_match_string(),
            links: default(),
        }
    }
}

impl<'m, M: MatchString<'m>> MatchString<'m> for Repeat<'m, M> {
    fn match_string(&'m self, cx: &mut super::StringMatcherContext<'m, '_>) -> bool {
        let (inner, outer) = if cx.is_reversed() {
            (self.inner.last(), self.prev_link())
        } else {
            (self.inner.first(), self.next_link())
        };

        cx.push_repeat(self.min, self.max, self.greedy, inner, outer.get());
        false
    }

    fn links(&'m self) -> super::Links<'m> {
        (&self.links).into()
    }

    fn initialize(&'m self) {
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
        impl<'m, M: MatchString<'m>> core::ops::Mul<$Name> for StringPattern<M> {
            type Output = StringPattern<Repeat<'static, M>>;

            fn mul(self, rhs: $Name) -> Self::Output {
                self.repeat(rhs)
            }
        }
        impl<M: MatchString<'static>> core::ops::Mul<StringPattern<M>> for $Name {
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
            greedy: true,
            inner: self.inner,
            links: Default::default(),
        })
    }

    pub fn optional<'m>(self) -> StringPattern<Repeat<'m, M>> {
        self.repeat(..=1)
    }
}

impl<'m, M: MatchString<'m>> StringPattern<Repeat<'m, M>> {
    pub fn lazy(mut self) -> Self {
        self.inner.greedy = false;
        self
    }
}
