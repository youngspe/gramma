use smallvec::SmallVec;

use super::Link;
use super::MatchString;
use super::Matcher;

use core::{
    fmt::{self, Debug},
    ops::{Range, RangeInclusive, RangeTo, RangeToInclusive},
};

use crate::utils::DebugFn;

pub trait PopRange {
    fn pop_range(self) -> (u16, u16);
}

impl PopRange for Range<u16> {
    fn pop_range(self) -> (u16, u16) {
        (self.start, self.end)
    }
}

impl PopRange for RangeInclusive<u16> {
    fn pop_range(self) -> (u16, u16) {
        (*self.start(), self.end().saturating_add(1))
    }
}

impl PopRange for RangeTo<u16> {
    fn pop_range(self) -> (u16, u16) {
        (0, self.end)
    }
}

impl PopRange for RangeToInclusive<u16> {
    fn pop_range(self) -> (u16, u16) {
        (0, self.end.saturating_add(1))
    }
}

impl PopRange for () {
    fn pop_range(self) -> (u16, u16) {
        (0, 0)
    }
}

impl PopRange for u16 {
    fn pop_range(self) -> (u16, u16) {
        (0, self)
    }
}

pub(crate) const MAX_DEPTH: u16 = 16;

pub(crate) type MachineStack<'m> = SmallVec<[StackItem<'m>; 8]>;

pub struct StringMatcherContext<'matcher, 'data> {
    pub(crate) src: &'data str,
    pub(crate) position: usize,
    pub(crate) stack: &'data mut MachineStack<'matcher>,
    pub(crate) reverse: bool,
    pub(crate) last_frame: usize,
    pub(crate) depth: u16,
}

impl<'m, 'data> StringMatcherContext<'m, 'data> {
    pub(crate) fn new(src: &'data str, stack: &'data mut MachineStack<'m>) -> Self {
        Self {
            src,
            position: 0,
            stack,
            reverse: false,
            last_frame: usize::MAX,
            depth: 0,
        }
    }

    pub fn post(&self) -> &'data str {
        &self.src[self.position..]
    }

    pub fn pre(&self) -> &'data str {
        &self.src[..self.position]
    }

    pub fn back_by(&mut self, position: usize) -> &mut Self {
        self.position -= position;
        self
    }

    pub fn forward_by(&mut self, position: usize) -> &mut Self {
        self.position += position;
        self
    }

    pub fn move_to(&mut self, position: usize) -> &mut Self {
        self.position = position;
        self
    }

    pub fn reverse(&mut self, value: bool) -> &mut Self {
        self.reverse = value;
        self
    }

    pub fn is_reversed(&self) -> bool {
        self.reverse
    }

    pub fn position(&self) -> usize {
        self.position
    }

    pub(crate) fn push_matcher_internal(&mut self, matcher: Matcher<'m>) -> &mut Self {
        self.stack.push(StackItem::Matcher { matcher });
        self
    }

    pub fn push_matcher(&mut self, matcher: impl Into<Matcher<'m>>) -> &mut Self {
        let matcher: Matcher<'m> = matcher.into();
        self.push_matcher_internal(if self.is_reversed() {
            matcher.first()
        } else {
            matcher.last()
        })
    }

    pub fn run_matcher(&mut self, matcher: &'m (impl MatchString<'m> + ?Sized)) -> bool {
        if self.depth >= MAX_DEPTH {
            self.push_matcher(matcher);
            false
        } else {
            self.depth += 1;
            if self.is_reversed() {
                matcher.last().match_string(self)
            } else {
                matcher.first().match_string(self)
            }
        }
    }

    pub fn push_link(&mut self, link: &Link<'m>) -> bool {
        if let Some(matcher) = link.get() {
            self.push_matcher_internal(matcher);
            false
        } else {
            true
        }
    }

    pub fn push_next(&mut self, current: &'m (impl MatchString<'m> + ?Sized)) -> bool {
        self.push_link(if self.is_reversed() {
            current.prev_link()
        } else {
            current.next_link()
        })
    }

    pub fn push_next_or_accept(
        &mut self,
        current: &'m (impl MatchString<'m> + ?Sized),
    ) -> &mut Self {
        if self.push_next(current) {
            self.stack.push(StackItem::Accept);
        }
        self
    }

    pub fn push_frame(
        &mut self,
        pop_on_success: impl PopRange,
        pop_on_error: impl PopRange,
    ) -> &mut Self {
        let last_frame = self.stack.len();
        self.stack.push(StackItem::Frame {
            last_frame: self.last_frame,
            pop_on_success: pop_on_success.pop_range(),
            pop_on_error: pop_on_error.pop_range(),
        });
        self.last_frame = last_frame;
        self
    }

    pub fn push_reset(&mut self) -> &mut Self {
        self.stack.push(StackItem::Reset {
            position: self.position,
            reverse: self.reverse,
        });
        self
    }

    pub fn push_repeat(
        &mut self,
        min: u32,
        max: u32,
        greedy: bool,
        inner: Matcher<'m>,
        outer: Option<Matcher<'m>>,
    ) -> &mut Self {
        if let Some(outer) = outer {
            self.push_matcher(outer);
        } else {
            self.stack.push(StackItem::Accept);
        }
        self.push_matcher(inner);
        self.stack.push(StackItem::RepeatFixed {
            min,
            max_after_min: if max == u32::MAX { max } else { max - min },
            greedy,
        });
        self
    }
}

#[derive(Clone, Copy)]
pub(crate) enum StackItem<'m> {
    Accept,
    Frame {
        last_frame: usize,
        pop_on_success: (u16, u16),
        pop_on_error: (u16, u16),
    },
    Reset {
        position: usize,
        reverse: bool,
    },
    Matcher {
        matcher: Matcher<'m>,
    },
    RepeatFixed {
        min: u32,
        max_after_min: u32,
        greedy: bool,
    },
    RepeatGreedy {
        max: u32,
    },
    RepeatLazy {
        max: u32,
    },
    RepeatLazyContinued {
        max: u32,
    },
}

impl<'m> Debug for StackItem<'m> {
    fn fmt<'x>(&'x self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fmt_max = |max: u32| {
            DebugFn(move |f| {
                if max == u32::MAX {
                    f.write_str("N/A")
                } else {
                    max.fmt(f)
                }
            })
        };
        match self {
            Self::Accept => write!(f, "Accept"),
            Self::Frame {
                last_frame,
                pop_on_success,
                pop_on_error,
            } => f
                .debug_struct("Frame")
                .field("last_frame", &format_args!("{last_frame:X}"))
                .field("pop_on_success", pop_on_success)
                .field("pop_on_error", pop_on_error)
                .finish(),
            Self::Reset { position, reverse } => f
                .debug_struct("Reset")
                .field("position", &format_args!("{position:X}"))
                .field("reverse", reverse)
                .finish(),
            Self::Matcher { matcher } => fmt::Debug::fmt(&matcher, f),
            Self::RepeatFixed {
                min,
                max_after_min,
                greedy,
            } => f
                .debug_struct("RepeatFixed")
                .field("min", min)
                .field("max_after_min", &fmt_max(*max_after_min))
                .field("greedy", greedy)
                .finish(),
            Self::RepeatGreedy { max } => f
                .debug_struct("RepeatGreedy")
                .field("max", &fmt_max(*max))
                .finish(),
            Self::RepeatLazy { max } => f
                .debug_struct("RepeatLazy")
                .field("max", &fmt_max(*max))
                .finish(),
            Self::RepeatLazyContinued { max } => f
                .debug_struct("RepeatLazyContinued")
                .field("max", &fmt_max(*max))
                .finish(),
        }
    }
}

impl<'m> StringMatcherContext<'m, '_> {
    pub(crate) fn pop_range(stack: &mut MachineStack<'m>, (start, end): (u16, u16)) {
        let lo = stack.len().saturating_sub(end as usize);
        let hi = stack.len().saturating_sub(start as usize);
        stack.drain(lo..hi);
    }

    pub(crate) fn execute(&mut self) -> bool {
        while let Some(item) = self.stack.pop() {
            self.depth = 0;
            let success = match item {
                StackItem::Accept => true,
                StackItem::Reset { position, reverse } => {
                    self.position = position;
                    self.reverse = reverse;
                    continue;
                }
                StackItem::Frame {
                    last_frame,
                    pop_on_success: _,
                    pop_on_error,
                } => {
                    self.last_frame = last_frame;
                    Self::pop_range(&mut self.stack, pop_on_error);
                    continue;
                }
                StackItem::Matcher { matcher } => matcher.match_string(self),
                StackItem::RepeatFixed {
                    min: 0,
                    max_after_min: max,
                    greedy: true,
                } => {
                    self.stack.push(StackItem::RepeatGreedy { max });
                    continue;
                }
                StackItem::RepeatFixed {
                    min: 0,
                    max_after_min: max,
                    greedy: false,
                } => {
                    self.stack.push(StackItem::RepeatLazy { max });
                    continue;
                }
                StackItem::RepeatFixed {
                    mut min,
                    max_after_min,
                    greedy,
                } => {
                    let inner = self.stack[self.stack.len() - 1];
                    min -= 1;
                    self.stack.push(StackItem::RepeatFixed {
                        min,
                        max_after_min,
                        greedy,
                    });
                    self.push_reset();
                    self.push_frame(
                        // skip the reset
                        0..=0,
                        // skip the repeat & post-repeat
                        1..=3,
                    );
                    self.stack.push(inner);

                    continue;
                }
                StackItem::RepeatLazy { max: 0 } | StackItem::RepeatGreedy { max: 0 } => {
                    self.stack.pop();
                    continue;
                }
                StackItem::RepeatGreedy { mut max } => {
                    let outer = self.stack[self.stack.len() - 2];
                    let inner = self.stack[self.stack.len() - 1];
                    if max != u32::MAX {
                        max -= 1;
                    }
                    self.stack.insert(
                        self.stack.len() - 2,
                        StackItem::Reset {
                            position: self.position,
                            reverse: self.reverse,
                        },
                    );
                    self.stack.insert(self.stack.len() - 2, outer);
                    self.stack.push(StackItem::RepeatGreedy { max });
                    self.push_frame(
                        0,
                        // skip the repeat
                        0..=1,
                    );
                    if let StackItem::Matcher { matcher } = inner {
                        matcher.match_string(self)
                    } else {
                        self.stack.push(inner);
                        continue;
                    }
                }
                StackItem::RepeatLazy { mut max } => {
                    let outer = self.stack[self.stack.len() - 2];
                    if max != u32::MAX {
                        max -= 1;
                    }
                    self.stack.push(StackItem::RepeatLazyContinued { max });
                    self.push_reset();
                    self.stack.push(outer);
                    continue;
                }
                StackItem::RepeatLazyContinued { max } => {
                    let inner = self.stack[self.stack.len() - 1];
                    self.stack.push(StackItem::RepeatLazy { max });
                    self.push_reset();
                    self.push_frame(
                        // skip the reset
                        0..=0,
                        // skip the repeat
                        1..=2,
                    );
                    self.stack.push(inner);
                    continue;
                }
            };

            if success {
                if self.last_frame == usize::MAX {
                    return true;
                }

                self.stack.truncate(self.last_frame + 1);

                let Some(StackItem::Frame {
                    last_frame,
                    pop_on_success,
                    pop_on_error: _,
                }) = self.stack.pop()
                else {
                    panic!("expected StackItem to be Frame");
                };

                self.last_frame = last_frame;
                Self::pop_range(&mut self.stack, pop_on_success);
            }
        }

        false
    }
}
