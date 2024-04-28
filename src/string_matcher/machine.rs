use smallvec::SmallVec;

use crate::utils::default;

use super::char_matcher::MatchChar;
use super::MatchString;
use super::Matcher;

use core::{
    fmt::Debug,
    mem,
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
pub(crate) const MAX_SMART_PUSH_DEPTH: u16 = 4;

pub(crate) type MachineStack<'m> = SmallVec<[StackItem<'m>; 8]>;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) struct RepeatState {
    pub(crate) min: u32,
    pub(crate) max: u32,
    pub(crate) depth: u32,
    pub(crate) greedy: bool,
    pub(crate) repeat_index: u16,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct StringMatcherState {
    pub(crate) position: usize,
    pub(crate) reverse: bool,
    pub(crate) repeat: RepeatState,
}

#[derive(Debug)]
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

    pub(crate) fn push(&mut self, item: StackItem<'m>) -> &mut Self {
        self.stack.push(item);
        self
    }

    pub(crate) fn push_matcher_internal(&mut self, matcher: Matcher<'m>) -> &mut Self {
        self.stack.push(StackItem::Matcher { matcher });
        self
    }

    pub fn push_matcher(&mut self, matcher: impl Into<Matcher<'m>>) -> &mut Self {
        let matcher: Matcher<'m> = matcher.into();
        self.push_matcher_internal(if self.is_reversed() {
            matcher.last()
        } else {
            matcher.first()
        })
    }

    pub fn quick_test_matcher(
        &mut self,
        matcher: impl Into<Matcher<'m>>,
        depth_incr: u16,
    ) -> Option<bool> {
        if self.depth >= MAX_DEPTH {
            return None;
        }
        let old_state = self.state();
        let old_depth = self.depth;
        self.depth = (self.depth + depth_incr).max(MAX_DEPTH - MAX_SMART_PUSH_DEPTH);
        let matcher: Matcher<'m> = matcher.into();
        let out = matcher.quick_test(self);
        self.depth = old_depth;
        self.set_state(old_state);
        out
    }

    pub fn run_matcher(&mut self, matcher: &'m (impl MatchString<'m> + ?Sized)) -> Option<bool> {
        if self.depth >= MAX_DEPTH {
            self.push_matcher(matcher);
            None
        } else {
            let stack_len = self.stack.len();
            let old_depth = self.depth;
            let old_state = self.state();
            self.depth += 1;
            let out = if self.is_reversed() {
                matcher.last().match_string(self)
            } else {
                matcher.first().match_string(self)
            };
            self.depth = old_depth;

            if out.is_some() {
                self.truncate_stack(stack_len);
            }

            if out == Some(false) {
                self.set_state(old_state);
            }
            out
        }
    }

    pub fn quick_run_matcher(
        &mut self,
        matcher: &'m (impl MatchString<'m> + ?Sized),
    ) -> Option<bool> {
        let old_depth = self.depth;
        self.depth = self.depth.max(MAX_DEPTH - MAX_SMART_PUSH_DEPTH - 1);
        let out = self.run_matcher(matcher);
        self.depth = old_depth;
        out
    }

    pub fn push_alternate(
        &mut self,
        matcher: &'m (impl MatchString<'m> + ?Sized),
    ) -> (Option<bool>, StringMatcherState) {
        let old_state = self.state();
        let res = self.quick_run_matcher(matcher);

        if res == Some(false) {
            return (res, old_state);
        }

        if res == Some(true) {
            self.stack.push(StackItem::Accept);
        }

        self.push_reset();

        let out_state = self.state();
        self.set_state(old_state);
        (res, out_state)
    }

    pub fn select_alternate(
        &mut self,
        primary: Option<bool>,
        alternate: (Option<bool>, StringMatcherState),
    ) -> Option<bool> {
        let (secondary, state) = alternate;

        if primary != Some(false) {
            return primary;
        }

        self.set_state(state);
        secondary
    }

    pub fn run_next(&mut self, current: &'m (impl MatchString<'m> + ?Sized)) -> Option<bool> {
        match current.next_matcher(self.is_reversed()) {
            Some(matcher) => self.run_matcher(*matcher),
            None => Some(true),
        }
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
            Some(StackItem::Reset { .. }) => {
                // the earlier reset would override the new one when popping off the stack
            }
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

    pub(crate) fn get_repeat_parts(&mut self) -> (StackItem<'m>, StackItem<'m>) {
        let repeat_index = self.state().repeat.repeat_index;

        debug_assert_ne!(
            repeat_index, 0,
            "invalid repeat index {repeat_index} with:\n\nstack = {:#?}\n\n state = {:#?}",
            self.stack, self.state,
        );

        let outer = self.stack[repeat_index as usize - 2];
        let inner = self.stack[repeat_index as usize - 1];

        (outer, inner)
    }

    pub(crate) fn quick_test_repeat(
        &mut self,
        outer: StackItem<'m>,
        inner: StackItem<'m>,
        depth: u32,
        min: u32,
        max: u32,
        depth_incr: u16,
    ) -> Option<bool> {
        let test1 = if depth >= min {
            self.quick_test_stack_item(inner, depth_incr)
        } else {
            Some(false)
        };

        if test1 == Some(true) {
            return Some(true);
        }

        let test2 = if depth < max {
            self.quick_test_stack_item(outer, depth_incr)
        } else {
            Some(false)
        };

        match (test1, test2) {
            (_, Some(true)) => Some(true),
            (Some(false), Some(false)) => Some(false),
            _ => None,
        }
    }
    pub(crate) fn continue_repeat(&mut self) -> Option<bool> {
        let (outer, inner) = self.get_repeat_parts();
        let repeat = self.state().repeat;

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
            (true, true) => {
                let ((option1, state1), (option2, state2)) = if repeat.greedy {
                    ((inner, inner_state), (outer, outer_state))
                } else {
                    ((outer, outer_state), (inner, inner_state))
                };

                let option2_out = self.set_state(state2).quick_test_stack_item(option2, 1);

                if option2_out != Some(false) {
                    self.push(option2).push_state(state2);
                }

                let option1_out = self.set_state(state1).run_stack_item(option1);

                match (option1_out, option2_out) {
                    (Some(true), _) => Some(true),
                    (Some(false), Some(false)) => Some(false),
                    _ => None,
                }
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

        let mut new_depth = depth + (max_remaining_init - max_remaining);

        match output {
            Some(success) => {
                if !success {
                    self.state = reset_state;
                    new_depth -= 1;
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

    pub(crate) fn match_char(&mut self, matcher: &impl MatchChar) -> bool {
        let ch = if self.is_reversed() {
            self.pre().chars().next_back()
        } else {
            self.post().chars().next()
        };

        let Some(ch) = ch else {
            return false;
        };

        if !matcher.match_char(ch) {
            return false;
        }

        if self.is_reversed() {
            self.back_by(ch.len_utf8());
        } else {
            self.forward_by(ch.len_utf8());
        }

        true
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum RepeatStyle {
    #[default]
    Greedy,
    Lazy,
    Simple,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum StackItem<'m> {
    Accept,
    Noop,
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
    pub(crate) fn pop_range(&mut self, (start, end): (u16, u16)) {
        let lo = self.stack.len().saturating_sub(end as usize);
        let hi = self.stack.len().saturating_sub(start as usize);
        for i in (lo..hi).rev() {
            let item = mem::replace(&mut self.stack[i], StackItem::Noop);
            self.discard_stack_item(i, item);
        }
        if start == 0 {
            self.stack.drain(lo..hi);
        }
    }

    pub(crate) fn truncate_stack(&mut self, len: usize) -> &mut Self {
        while self.last_frame >= len && self.last_frame != usize::MAX {
            match self.stack[self.last_frame] {
                StackItem::Frame { last_frame, .. } => {
                    self.last_frame = last_frame;
                }
                _ => panic!("last_frame should point to a StackItem::Frame"),
            }
        }
        while self.state.repeat.repeat_index as usize >= len.max(1) {
            self.state.repeat = match self.stack[self.state.repeat.repeat_index as usize] {
                StackItem::Repeat { last_repeat: 0, .. } => default(),
                StackItem::Repeat {
                    last_repeat: repeat_index,
                    last_depth: depth,
                    ..
                } => match self.stack[repeat_index as usize] {
                    StackItem::Repeat {
                        min, max, greedy, ..
                    } => RepeatState {
                        min,
                        max,
                        greedy,
                        depth,
                        repeat_index,
                    },
                    _ => panic!("last_repeat should point to a StackItem::Repeat"),
                },

                _ => panic!("repeat_index should point to a StackItem::Repeat"),
            }
        }
        self.stack.truncate(len);
        self
    }

    fn discard_stack_item(&mut self, index: usize, item: StackItem<'m>) {
        match item {
            StackItem::Frame { last_frame, .. } if self.last_frame == index => {
                self.last_frame = last_frame;
            }
            StackItem::Repeat { last_repeat: 0, .. }
                if self.state.repeat.repeat_index as usize == index =>
            {
                self.state.repeat = default();
            }
            StackItem::Repeat {
                last_repeat: repeat_index,
                last_depth: depth,
                ..
            } if self.state.repeat.repeat_index as usize == index => {
                self.state.repeat = match self.stack[repeat_index as usize] {
                    StackItem::Repeat {
                        min, max, greedy, ..
                    } => RepeatState {
                        min,
                        max,
                        depth,
                        greedy,
                        repeat_index,
                    },
                    _ => panic!("last_repeat should point to a StackItem::Repeat"),
                };
            }
            _ => {}
        }
    }

    fn run_stack_item(&mut self, item: StackItem<'m>) -> Option<bool> {
        match item {
            StackItem::Accept => Some(true),
            StackItem::Matcher { matcher } => self.run_matcher(*matcher),
            item => {
                self.stack.push(item);
                None
            }
        }
    }

    fn quick_test_stack_item(&mut self, item: StackItem<'m>, depth_incr: u16) -> Option<bool> {
        match item {
            StackItem::Accept => Some(true),
            StackItem::Matcher { matcher } => self.quick_test_matcher(matcher, depth_incr),
            _ => None,
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
            StackItem::Noop => {
                *max_times = 0;
                None
            }
            StackItem::Panic(s) => panic!("{s}"),
            StackItem::Matcher { matcher } if self.depth < MAX_DEPTH => {
                self.depth += 1;
                let out = matcher.match_repeated(self, reset_state, max_times);
                self.depth -= 1;
                out
            }
            item => {
                *max_times -= 1;
                self.stack.push(item);
                None
            }
        }
    }

    fn handle_stack_item(&mut self, item: StackItem<'m>) -> Option<bool> {
        let index = self.stack.len();
        match item {
            StackItem::Accept => return Some(true),
            StackItem::Noop => {}
            StackItem::Panic(s) => panic!("{s}"),
            StackItem::Pop { range } => self.pop_range(range),
            StackItem::Reset { state } => self.state = state,
            StackItem::Frame {
                last_frame,
                pop_on_success: _,
                pop_on_error,
            } => {
                self.last_frame = last_frame;
                self.pop_range(pop_on_error);
            }
            StackItem::Matcher { matcher } => return matcher.match_string(self),
            StackItem::Repeat {
                last_repeat: repeat_index,
                last_depth: depth,
                ..
            } if self.state.repeat.repeat_index as usize >= index => {
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
                self.truncate_stack(self.stack.len() - 2);
            }
            StackItem::Repeat { .. } => {}
            StackItem::RepeatSimple { last_position, .. } if self.position() == last_position => {
                self.stack.pop();
            }
            StackItem::RepeatSimple {
                min, max, depth, ..
            } => return self.continue_repeat_simple(min, max, depth),
        };
        None
    }

    pub(crate) fn handle_success(&mut self) -> bool {
        if self.last_frame == usize::MAX {
            return true;
        }

        let StackItem::Frame {
            pop_on_success,
            pop_on_error: _,
            last_frame,
            ..
        } = self.stack[self.last_frame]
        else {
            panic!("expected StackItem to be Frame");
        };

        self.truncate_stack(self.last_frame);
        self.last_frame = last_frame;

        self.pop_range(pop_on_success);
        false
    }

    pub(crate) fn execute(&mut self, mut success_status: Option<bool>) -> bool {
        loop {
            if let Some(true) = success_status {
                if self.handle_success() {
                    return true;
                }
            }

            success_status = match self.stack.pop().map(|item| self.handle_stack_item(item)) {
                Some(s) => s,
                None => return false,
            };
        }
    }
}
