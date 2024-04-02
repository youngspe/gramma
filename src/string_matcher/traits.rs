use core::{cell::Cell, fmt};

use crate::utils::DebugFn;

use super::{char_matcher::MatchChar, machine::StringMatcherState, Link, Links, Matcher};

pub trait AsMatcher<'m> {
    fn _as_matcher(&'m self) -> Matcher<'m>;
}

pub trait IntoMatchString {
    type Matcher<'m>: MatchString<'m>
    where
        Self: 'm;

    fn into_match_string<'m>(self) -> Self::Matcher<'m>
    where
        Self: 'm;
}

pub trait MatchString<'m>: AsMatcher<'m> {
    #[inline(always)]
    fn as_char_matcher(&'m self) -> Option<impl MatchChar + 'm>
    where
        Self: Sized,
    {
        None::<()>
    }

    fn match_string(
        &'m self,
        cx: &mut super::machine::StringMatcherContext<'m, '_>,
    ) -> Option<bool>;

    fn  match_repeated(
        &'m self,
        cx: &mut super::machine::StringMatcherContext<'m, '_>,
        reset_state: &mut StringMatcherState,
        max_items: &mut u32,
    ) -> Option<bool> {
        let mut last_position = reset_state.position;
        while *max_items > 0 {
            *max_items -= 1;
            match self.match_string(cx) {
                Some(true) => {
                    *reset_state = cx.state();
                    if cx.position() == last_position {
                        *max_items = 0;
                    }
                    last_position = cx.position()
                }
                out => return out,
            }
        }
        Some(true)
    }

    fn as_matcher(&'m self) -> Matcher<'m> {
        self._as_matcher()
    }

    fn links(&'m self) -> Links<'m>;
    fn prev_link(&'m self) -> &'m Link<'m> {
        self.links().0
    }
    fn next_link(&'m self) -> &'m Link<'m> {
        self.links().1
    }
    fn first(&'m self) -> Matcher<'m> {
        self.into()
    }
    fn last(&'m self) -> Matcher<'m> {
        self.into()
    }

    fn initialize(&'m self) {}

    fn fmt_matcher(&self, f: &mut fmt::Formatter, prec: DebugPrecedence) -> fmt::Result {
        let _ = prec;
        f.write_str(core::any::type_name::<Self>())
    }

    fn as_debug(&self, prec: DebugPrecedence) -> impl fmt::Debug + '_
    where
        Self: Sized,
    {
        DebugFn(move |f| self.fmt_matcher(f, prec))
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DebugPrecedence {
    Base,
    Char,
    Mul,
    Add,
    Or,
    #[default]
    Initial,
}

impl DebugPrecedence {
    pub(crate) fn wrap_below(
        self,
        prec: DebugPrecedence,
        f: &mut fmt::Formatter,
        block: impl FnOnce(&mut fmt::Formatter) -> fmt::Result,
    ) -> fmt::Result {
        if self < prec {
            let block = Cell::new(Some(block));
            f.debug_tuple("")
                .field(&DebugFn(|f| block.take().unwrap()(f)))
                .finish()
        } else {
            block(f)
        }
    }
}

pub trait NegatePattern {
    type Output;
    fn negate_pattern(self) -> Self::Output;
}

pub trait IntersectPattern<Rhs = Self> {
    type Output;
    fn intersect_pattern(self, rhs: Rhs) -> Self::Output;
}
