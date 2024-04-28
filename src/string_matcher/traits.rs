use core::{cell::Cell, convert::Infallible, fmt};

use crate::utils::DebugFn;

use super::{
    char_matcher::MatchChar, machine::StringMatcherState, Links, Matcher,
    StringMatcherContext,
};

pub trait AsMatcher<'m> {
    fn _as_matcher(&'m self) -> Matcher<'m>;
    fn _quick_test(&'m self, cx: &mut StringMatcherContext<'m, '_>) -> Option<bool>;
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
        None::<Infallible>
    }

    fn match_string(
        &'m self,
        cx: &mut super::machine::StringMatcherContext<'m, '_>,
    ) -> Option<bool>;

    fn match_repeated(
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

    fn quick_test(&'m self, cx: &mut StringMatcherContext<'m, '_>) -> Option<bool> {
        self._quick_test(cx)
    }

    fn should_push(&'m self, cx: &mut StringMatcherContext<'m, '_>) -> bool {
        self.quick_test(cx).unwrap_or(true)
    }

    fn as_matcher(&'m self) -> Matcher<'m> {
        self._as_matcher()
    }

    fn links(&'m self) -> Option<Links<'m>> {
        None
    }

    fn backward_matcher(&'m self) -> Option<Matcher<'m>> {
        self.links()?.0.get()
    }

    fn forward_matcher(&'m self) -> Option<Matcher<'m>> {
        self.links()?.1.get()
    }

    fn set_backward(&'m self, matcher: Option<Matcher<'m>>) {
        if let Some(Links(back, _)) = self.links() {
            back.set(matcher)
        }
    }

    fn set_forward(&'m self, matcher: Option<Matcher<'m>>) {
        if let Some(Links(_, fwd)) = self.links() {
            fwd.set(matcher)
        }
    }

    fn next_matcher(&'m self, is_reversed: bool) -> Option<Matcher<'m>> {
        if is_reversed {
            self.backward_matcher()
        } else {
            self.forward_matcher()
        }
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
