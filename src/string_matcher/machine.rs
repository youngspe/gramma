use smallvec::SmallVec;

use super::Link;
use super::MatchString;
use super::Matcher;

use core::{
    fmt::Debug,
    ops::{Range, RangeInclusive, RangeTo, RangeToInclusive},
};

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

#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub(crate) struct RepeatState {
    pub(crate) min: u32,
    pub(crate) max: u32,
    pub(crate) depth: u32,
    pub(crate) greedy: bool,
    pub(crate) repeat_index: u16,
}

#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub struct StringMatcherState {
    pub(crate) position: usize,
    pub(crate) reverse: bool,
    pub(crate) repeat: RepeatState,
}

pub struct StringMatcherContext<'matcher, 'data> {
    pub(crate) src: &'data str,
    pub(crate) stack: &'data mut MachineStack<'matcher>,
    pub(crate) state: StringMatcherState,
    pub(crate) last_frame: usize,
    pub(crate) depth: u16,
}

impl<'m, 'data> StringMatcherContext<'m, 'data> {
    pub(crate) fn new(src: &'data str, stack: &'data mut MachineStack<'m>) -> Self {
        Self {
            src,
            state: Default::default(),
            stack,
            last_frame: usize::MAX,
            depth: 0,
        }
    }

    pub fn post(&self) -> &'data str {
        &self.src[self.position()..]
    }

    pub fn pre(&self) -> &'data str {
        &self.src[..self.position()]
    }

    pub fn back_by(&mut self, position: usize) -> &mut Self {
        self.state.position -= position;
        self
    }

    pub fn forward_by(&mut self, position: usize) -> &mut Self {
        self.state.position += position;
        self
    }

    pub fn move_to(&mut self, position: usize) -> &mut Self {
        self.state.position = position;
        self
    }

    pub fn reverse(&mut self, value: bool) -> &mut Self {
        self.state.reverse = value;
        self
    }

    pub fn is_reversed(&self) -> bool {
        self.state.reverse
    }

    pub fn position(&self) -> usize {
        self.state.position
    }

    pub fn state(&self) -> StringMatcherState {
        self.state
    }

    pub fn set_state(&mut self, state: StringMatcherState) -> &mut Self {
        self.state = state;
        self
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

    pub fn run_matcher(&mut self, matcher: &'m (impl MatchString<'m> + ?Sized)) -> Option<bool> {
        if self.depth >= MAX_DEPTH {
            self.push_matcher(matcher);
            None
        } else {
            self.depth += 1;
            if self.is_reversed() {
                matcher.last().match_string(self)
            } else {
                matcher.first().match_string(self)
            }
        }
    }

    pub fn push_link(&mut self, link: &Link<'m>) -> Option<bool> {
        if let Some(matcher) = link.get() {
            self.push_matcher_internal(matcher);
            None
        } else {
            Some(true)
        }
    }

    pub fn run_link(&mut self, link: &Link<'m>) -> Option<bool> {
        if let Some(matcher) = link.get() {
            self.run_matcher(*matcher)
        } else {
            Some(true)
        }
    }

    pub fn push_next(&mut self, current: &'m (impl MatchString<'m> + ?Sized)) -> Option<bool> {
        self.push_link(if self.is_reversed() {
            current.prev_link()
        } else {
            current.next_link()
        })
    }

    pub fn run_next(&mut self, current: &'m (impl MatchString<'m> + ?Sized)) -> Option<bool> {
        self.run_link(if self.is_reversed() {
            current.prev_link()
        } else {
            current.next_link()
        })
    }

    pub fn push_next_or_accept(
        &mut self,
        current: &'m (impl MatchString<'m> + ?Sized),
    ) -> &mut Self {
        match self.push_next(current) {
            Some(true) => self.stack.push(StackItem::Accept),
            Some(false) => self.stack.push(StackItem::Reject),
            None => {}
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

    pub(crate) fn push_state(&mut self, new_state: StringMatcherState) -> &mut Self {
        match self.stack.last_mut() {
            Some(StackItem::Reset { state }) => *state = new_state,
            _ => self.stack.push(StackItem::Reset { state: new_state }),
        }
        self
    }

    pub fn push_reset(&mut self) -> &mut Self {
        self.push_state(self.state())
    }

    pub(crate) fn exit_repeat_state(&mut self) -> StringMatcherState {
        let mut state = self.state();
        let repeat_index = state.repeat.repeat_index;

        if repeat_index == 0 {
            return state;
        }

        let StackItem::Repeat {
            last_repeat: repeat_index,
            last_depth: depth,
            ..
        } = self.stack[state.repeat.repeat_index as usize]
        else {
            panic!("StackItem::Repeat expected")
        };

        if repeat_index == 0 {
            return state;
        }

        let StackItem::Repeat {
            min, max, greedy, ..
        } = self.stack[repeat_index as usize]
        else {
            panic!("StackItem::Repeat expected")
        };
        state.repeat = RepeatState {
            min,
            max,
            depth,
            greedy,
            repeat_index,
        };

        state
    }

    pub(crate) fn continue_repeat(&mut self) -> Option<bool> {
        let repeat = self.state().repeat;

        let outer = self.stack[repeat.repeat_index as usize - 2];
        let inner = self.stack[repeat.repeat_index as usize - 1];

        let use_outer = repeat.depth >= repeat.min;
        let use_inner = repeat.depth < repeat.max;

        let outer_state = self.exit_repeat_state();
        let inner_state = StringMatcherState {
            repeat: RepeatState {
                depth: repeat.depth + 1,
                ..repeat
            },
            ..self.state()
        };

        match (use_outer, use_inner) {
            (true, true) if repeat.greedy => {
                self.stack.push(outer);
                self.push_state(outer_state);
                self.set_state(inner_state).run_stack_item(inner)
            }
            (true, true) => {
                self.stack.push(inner);
                self.push_state(inner_state);
                self.set_state(outer_state).run_stack_item(outer)
            }
            (true, false) => self.set_state(outer_state).run_stack_item(outer),
            (false, true) => self.set_state(inner_state).run_stack_item(inner),
            (false, false) => Some(true),
        }
    }

    pub(crate) fn continue_repeat_simple(
        &mut self,
        min: u32,
        max: u32,
        depth: u32,
    ) -> Option<bool> {
        let repeat_index = self.stack.len();
        let inner = self.stack[self.stack.len() - 1];
        self.stack
            .push(StackItem::Panic("RepeatSimple should still be in progress"));
        self.stack.push(StackItem::Panic(
            "RepeatSimple > Reset should still be in progress",
        ));
        self.push_frame(
            // skip the reset
            0..=0,
            // skip the repeat + inner
            1..=2,
        );

        let mut reset_state = self.state;
        let max_remaining_init = max - depth;
        let mut max_remaining = max_remaining_init;
        let initial_position = self.position();

        let output = self.repeat_stack_item(inner, &mut reset_state, &mut max_remaining);

        let new_depth = depth + (max_remaining_init - max_remaining);

        match output {
            Some(success) => {
                if !success {
                    self.state = reset_state;
                }

                if new_depth < min {
                    // pop the repeat, inner, and outer item
                    self.truncate_stack(repeat_index - 2);
                    Some(false)
                } else {
                    // pop the repeat and inner item
                    self.truncate_stack(repeat_index - 1);
                    let outer = self.stack.pop().unwrap();
                    self.run_stack_item(outer)
                }
            }
            None => {
                self.stack[repeat_index] = StackItem::RepeatSimple {
                    min,
                    max,
                    depth: new_depth,
                    last_position: initial_position,
                };
                self.stack[repeat_index + 1] = if new_depth <= min {
                    StackItem::Pop {
                        range: (0..=2).pop_range(),
                    }
                } else {
                    StackItem::Reset { state: reset_state }
                };
                None
            }
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum RepeatStyle {
    #[default]
    Greedy,
    Lazy,
    Simple,
}

#[derive(Clone, Copy)]
pub(crate) enum StackItem<'m> {
    Accept,
    Reject,
    Panic(&'m str),
    Pop {
        range: (u16, u16),
    },
    Frame {
        last_frame: usize,
        pop_on_success: (u16, u16),
        pop_on_error: (u16, u16),
    },
    Reset {
        state: StringMatcherState,
    },
    Matcher {
        matcher: Matcher<'m>,
    },
    Repeat {
        min: u32,
        max: u32,
        greedy: bool,
        last_repeat: u16,
        last_depth: u32,
    },
    RepeatSimple {
        min: u32,
        max: u32,
        depth: u32,
        last_position: usize,
    },
}

impl<'m> StringMatcherContext<'m, '_> {
    pub(crate) fn pop_range(stack: &mut MachineStack<'m>, (start, end): (u16, u16)) {
        let lo = stack.len().saturating_sub(end as usize);
        let hi = stack.len().saturating_sub(start as usize);
        stack.drain(lo..hi);
    }

    pub(crate) fn truncate_stack(&mut self, len: usize) {
        while self.last_frame >= len && self.last_frame != usize::MAX {
            match self.stack[self.last_frame] {
                StackItem::Frame { last_frame, .. } => {
                    self.last_frame = last_frame;
                }
                _ => panic!("last_frame should point to a a StackItem::Frame"),
            }
        }
        self.stack.truncate(len)
    }

    fn run_stack_item(&mut self, item: StackItem<'m>) -> Option<bool> {
        match item {
            StackItem::Accept => Some(true),
            StackItem::Reject => Some(false),
            StackItem::Panic(s) => panic!("{s}"),
            StackItem::Matcher { matcher } => self.run_matcher(*matcher),
            item => {
                self.stack.push(item);
                None
            }
        }
    }

    fn repeat_stack_item(
        &mut self,
        item: StackItem<'m>,
        reset_state: &mut StringMatcherState,
        max_times: &mut u32,
    ) -> Option<bool> {
        if *max_times == 0 {
            return Some(true);
        }

        match item {
            StackItem::Accept => {
                *max_times = 0;
                Some(true)
            }
            StackItem::Reject => Some(false),
            StackItem::Panic(s) => panic!("{s}"),
            StackItem::Matcher { matcher } if self.depth < MAX_DEPTH => {
                self.depth += 1;
                matcher.match_repeated(self, reset_state, max_times)
            }
            item => {
                *max_times -= 1;
                self.stack.push(item);
                None
            }
        }
    }

    fn handle_stack_item(&mut self, item: StackItem<'m>) -> Option<bool> {
        match item {
            StackItem::Accept => return Some(true),
            StackItem::Reject => return Some(false),
            StackItem::Panic(s) => panic!("{s}"),
            StackItem::Pop { range } => {
                Self::pop_range(&mut self.stack, range);
            }
            StackItem::Reset { state } => {
                self.state = state;
            }
            StackItem::Frame {
                last_frame,
                pop_on_success: _,
                pop_on_error,
            } => {
                self.last_frame = last_frame;
                Self::pop_range(&mut self.stack, pop_on_error);
            }
            StackItem::Matcher { matcher } => return matcher.match_string(self),
            StackItem::Repeat {
                last_repeat: repeat_index,
                last_depth: depth,
                ..
            } => {
                let mut state = self.state();
                if repeat_index != 0 {
                    let StackItem::Repeat {
                        min, max, greedy, ..
                    } = self.stack[repeat_index as usize]
                    else {
                        panic!("StackItem::Repeat expected")
                    };
                    state.repeat = RepeatState {
                        min,
                        max,
                        depth,
                        greedy,
                        repeat_index,
                    };
                } else {
                    state.repeat = RepeatState::default();
                }
                self.set_state(state);
                self.stack.truncate(self.stack.len() - 2);
            }
            StackItem::RepeatSimple { last_position, .. } if self.position() == last_position => {
                self.stack.pop();
            }
            StackItem::RepeatSimple {
                min, max, depth, ..
            } => return self.continue_repeat_simple(min, max, depth),
        };
        None
    }

    pub(crate) fn execute(&mut self) -> bool {
        while let Some(item) = self.stack.pop() {
            self.depth = 0;
            let Some(success) = self.handle_stack_item(item) else {
                continue;
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
