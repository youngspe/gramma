use super::{
    super::{MatchString, StringMatcher},
    word, MatchChar,
};

use crate::{
    string_matcher::{Link, Links, StringMatcherContext},
    utils::default,
};

use core::{fmt, marker::PhantomData};

pub trait BoundaryDef {
    fn is_boundary(&self, pre: &str, post: &str) -> bool;
    fn fmt_boundary(&self, f: &mut fmt::Formatter) -> fmt::Result;
}

pub(crate) struct FnBoundary<M, F1, F2>
where
    F1: Fn(&M, &str, &str) -> bool,
{
    pub(crate) data: M,
    pub(crate) is_boundary: F1,
    pub(crate) fmt: F2,
}

impl<M, F1, F2> FnBoundary<M, F1, F2>
where
    M: MatchChar,
    F1: Fn(&M, &str, &str) -> bool,
{
    pub(crate) fn fmt(
        self,
        name_field: impl Fn() -> (&'static str, bool) + 'static,
    ) -> FnBoundary<M, F1, impl Fn(&M, &mut fmt::Formatter) -> fmt::Result> {
        let Self {
            data, is_boundary, ..
        } = self;
        FnBoundary {
            data,
            is_boundary,
            fmt: move |m: &M, f: &mut fmt::Formatter| {
                let (name, include_field) = name_field();
                let mut d = f.debug_tuple(name);
                if include_field {
                    d.field(&m.as_debug());
                }
                d.finish()
            },
        }
    }
}

pub(crate) fn boundary_def<M: MatchChar>(
    matcher: M,
) -> FnBoundary<M, impl Fn(&M, &str, &str) -> bool, ()> {
    FnBoundary {
        data: matcher,
        is_boundary: |matcher, pre, post| {
            pre.is_empty()
                || post.is_empty()
                || (matcher.match_end(pre) ^ matcher.match_start(post))
        },
        fmt: (),
    }
}

pub(crate) fn boundary_start_def<M: MatchChar>(
    matcher: M,
) -> FnBoundary<M, impl Fn(&M, &str, &str) -> bool, ()> {
    FnBoundary {
        data: matcher,
        is_boundary: |matcher, pre, post| {
            pre.is_empty() || (!matcher.match_end(pre) && matcher.match_start(post))
        },
        fmt: (),
    }
}

pub(crate) fn boundary_end_def<M: MatchChar>(
    matcher: M,
) -> FnBoundary<M, impl Fn(&M, &str, &str) -> bool, ()> {
    FnBoundary {
        data: matcher,
        is_boundary: |matcher, pre, post| {
            post.is_empty() || (matcher.match_end(pre) && !matcher.match_start(post))
        },
        fmt: (),
    }
}

impl<M, F1, F2> BoundaryDef for FnBoundary<M, F1, F2>
where
    F1: Fn(&M, &str, &str) -> bool,
    F2: Fn(&M, &mut fmt::Formatter) -> fmt::Result,
{
    fn is_boundary(&self, pre: &str, post: &str) -> bool {
        (self.is_boundary)(&self.data, pre, post)
    }

    fn fmt_boundary(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (self.fmt)(&self.data, f)
    }
}

pub struct Boundary<'m, D> {
    pub(crate) def: D,
    pub(crate) links: (Link<'m>, Link<'m>),
    pub(crate) _m: PhantomData<&'m D>,
}

impl<'m, D> Boundary<'m, D> {
    pub(crate) fn new(def: D) -> Self {
        Self {
            def,
            links: default(),
            _m: PhantomData,
        }
    }
}

impl<'m, D: BoundaryDef> MatchString<'m> for Boundary<'m, D> {
    fn match_string(&'m self, cx: &mut StringMatcherContext<'m, '_>) -> bool {
        self.def.is_boundary(cx.pre(), cx.post()) && cx.push_next(self)
    }

    fn links(&'m self) -> Links<'m> {
        (&self.links).into()
    }
}

pub fn boundary<'m>(matcher: impl MatchChar + 'm) -> StringMatcher<'m, impl MatchString<'m>> {
    Boundary::new(boundary_def(matcher).fmt(|| ("boundary", true))).into()
}

pub fn boundary_start<'m>(
    matcher: impl MatchChar + 'm,
) -> StringMatcher<'m, impl MatchString<'m>> {
    Boundary::new(boundary_start_def(matcher).fmt(|| ("boundary_start", true))).into()
}

pub fn boundary_end<'m>(
    matcher: impl MatchChar + 'm,
) -> StringMatcher<'m, impl MatchString<'m>> {
    Boundary::new(boundary_end_def(matcher).fmt(|| ("boundary_end", true))).into()
}

pub fn word_boundary<'m>() -> StringMatcher<'m, impl MatchString<'m> + 'm> {
    Boundary::new(boundary_def(word()).fmt(|| ("word_boundary", false))).into()
}
