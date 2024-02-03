#![allow(non_camel_case_types)]

use std::marker::PhantomData;

use crate::Rule;

use super::{CompoundToken, DelimitedList, DualParse, Ignore, TransformList};

pub trait TransformInto<Out> {
    type Input;
    fn transform(input: Self::Input) -> Out;
}

#[non_exhaustive]
pub struct identity {}

impl<T> TransformInto<T> for identity {
    type Input = T;

    fn transform(input: Self::Input) -> T {
        input
    }
}

pub struct compose<A, B> {
    a: A,
    b: B,
}

impl<A, B, Out> TransformInto<Out> for compose<A, B>
where
    A: TransformInto<B::Input>,
    B: TransformInto<Out>,
{
    type Input = A::Input;
    fn transform(input: Self::Input) -> Out {
        B::transform(A::transform(input))
    }
}

pub struct delimited<Delim: Rule, const TRAIL: bool = true> {
    _delim: PhantomData<Delim>,
}

impl<T: Rule, Delim: Rule, const TRAIL: bool> TransformInto<Vec<T>> for delimited<Delim, TRAIL> {
    type Input = DelimitedList<T, Delim, TRAIL>;

    fn transform(input: Self::Input) -> Vec<T> {
        input.items
    }
}

pub struct ignore_before<S> {
    _space: PhantomData<S>,
}

impl<T: Rule, S: Rule> TransformInto<T> for ignore_before<S> {
    type Input = (Ignore<S>, T);

    fn transform((_, out): Self::Input) -> T {
        out
    }
}

pub struct ignore_after<S> {
    _space: PhantomData<S>,
}

impl<T: Rule, S: Rule> TransformInto<T> for ignore_after<S> {
    type Input = (T, Ignore<S>);

    fn transform((out, _): Self::Input) -> T {
        out
    }
}

pub type ignore_around<S> = compose<ignore_before<S>, ignore_after<S>>;

pub struct for_each<X> {
    _x: PhantomData<X>,
}

impl<In: Rule, Out, X: TransformInto<Out, Input = In>> TransformInto<Vec<Out>> for for_each<X> {
    type Input = TransformList<Out, X>;

    fn transform(input: Self::Input) -> Vec<Out> {
        input.items
    }
}

#[non_exhaustive]
pub struct compound_token {}

impl<T: Rule> TransformInto<T> for compound_token {
    type Input = CompoundToken<T>;

    fn transform(input: Self::Input) -> T {
        input.value
    }
}

pub struct parse_as<Outer> {
    _outer: PhantomData<Outer>,
}

impl<Outer: Rule, Inner: Rule> TransformInto<Inner> for parse_as<Outer> {
    type Input = DualParse<Outer, Inner>;

    fn transform(input: Self::Input) -> Inner {
        input.inner
    }
}
