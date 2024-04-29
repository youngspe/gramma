//! Transformations for use in the `#[transform(...)]` attribute in invocations of the
//! (define_rule!)[crate::define_rule] macro.
#![allow(non_camel_case_types)]

use core::marker::PhantomData;

use crate::{internal_prelude::*, Rule};

use super::{
    CompoundToken, DelimitedList, Discard, DualParse, Empty, Ignore, NotParse, TransformList,
};

/// This trait defines the behavior of a transformation.
pub trait TransformInto<Out> {
    /// Actual rule that should be parsed.
    type Input;
    /// Transform the parsed rule into the target rule.
    fn transform(input: Self::Input) -> Out;
}

/// Leaves the target rule unchanged.
#[non_exhaustive]
pub struct identity {}

impl<T> TransformInto<T> for identity {
    type Input = T;

    fn transform(input: Self::Input) -> T {
        input
    }
}

/// Applies transform `B` to the target rule, then applies transform `A` to the transformed rule.
pub struct compose<A, B> {
    _a: A,
    _b: B,
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

/// Parse and discard `Delim` between each item in the target rule.
/// If `TRAIL` is true or unspecified, allow an optional instance of `Delim` after the last item.
/// Useful when matching a `Vec` of rules, for example a comma-separated list.
pub struct delimited<Delim: Rule, const TRAIL: bool = true> {
    _delim: PhantomData<Delim>,
}

impl<T: Rule, Delim: Rule, const TRAIL: bool> TransformInto<Vec<T>> for delimited<Delim, TRAIL> {
    type Input = DelimitedList<T, Delim, TRAIL>;

    fn transform(input: Self::Input) -> Vec<T> {
        input.items
    }
}

impl<T: Rule, X: TransformInto<T>, Delim: Rule, const TRAIL: bool>
    TransformInto<TransformList<T, X, Delim>> for delimited<Delim, TRAIL>
{
    type Input = TransformList<T, X, Delim, TRAIL>;

    fn transform(input: Self::Input) -> TransformList<T, X, Delim> {
        TransformList::new(input.items)
    }
}

/// Parse an _optional_ `S` before the target rule and discard the result if found.
/// Useful for ignoring optional whitespace before the target rule.
pub struct ignore_before<S> {
    _space: PhantomData<S>,
}

impl<T: Rule, S: Rule> TransformInto<T> for ignore_before<S> {
    type Input = (Ignore<S>, T);

    fn transform((_, out): Self::Input) -> T {
        out
    }
}

/// Parse an _optional_ `S` after the target rule and discard the result if found.
/// Useful for ignoring optional whitespace after the target rule.
pub struct ignore_after<S> {
    _space: PhantomData<S>,
}

impl<T: Rule, S: Rule> TransformInto<T> for ignore_after<S> {
    type Input = (T, Ignore<S>);

    fn transform((out, _): Self::Input) -> T {
        out
    }
}

/// Parse an _optional_ `S1` before and `S2` after the target rule and discard the results if found.
/// Useful for ignoring optional whitespace around the target rule.
pub type ignore_around<S1, S2 = S1> = compose<ignore_before<S1>, ignore_after<S2>>;

/// Parse a _required_ `S` before the target rule and discard the result.
/// Useful for matching punctuation before the target rule.
pub struct discard_before<S> {
    _space: PhantomData<S>,
}

impl<T: Rule, S: Rule> TransformInto<T> for discard_before<S> {
    type Input = (Discard<S>, T);

    fn transform((_, out): Self::Input) -> T {
        out
    }
}

/// Parse a _required_ `S` after the target rule and discard the result.
/// Useful for matching punctuation after the target rule.
pub struct discard_after<S> {
    _space: PhantomData<S>,
}

impl<T: Rule, S: Rule> TransformInto<T> for discard_after<S> {
    type Input = (T, Discard<S>);

    fn transform((out, _): Self::Input) -> T {
        out
    }
}

/// Parse a _required_ `S1` before and `S2` after the target rule and discard the results.
/// Useful for matching punctuation around the target rule.
pub type discard_around<S1, S2 = S1> = compose<discard_before<S1>, discard_after<S2>>;

/// Apply the transformation `X` to the inner value.
/// Useful when matching an `Option<T>` and you want the rules to apply to `T`.
pub struct map<X> {
    _x: PhantomData<X>,
}

impl<In: Rule, Out, X: TransformInto<Out, Input = In>> TransformInto<Option<Out>> for map<X> {
    type Input = Option<In>;

    fn transform(input: Self::Input) -> Option<Out> {
        input.map(X::transform)
    }
}

/// Apply the transformation `X` to each item in the target rule.
/// Useful when matching a `Vec` of rules.
pub struct for_each<X> {
    _x: PhantomData<X>,
}

impl<In: Rule, Out, X: TransformInto<Out, Input = In>> TransformInto<Vec<Out>> for for_each<X> {
    type Input = TransformList<Out, X>;

    fn transform(input: Self::Input) -> Vec<Out> {
        input.items
    }
}

impl<
        In: Rule,
        Out,
        In1: Rule,
        X: TransformInto<In1, Input = In>,
        X1: TransformInto<Out, Input = In1>,
        Delim: Rule,
        const TRAIL: bool,
        const PREFER_SHORT: bool,
    > TransformInto<TransformList<Out, X1, Delim, TRAIL, PREFER_SHORT>> for for_each<X>
{
    type Input = TransformList<Out, compose<X, X1>, Delim, TRAIL, PREFER_SHORT>;

    fn transform(input: Self::Input) -> TransformList<Out, X1, Delim, TRAIL, PREFER_SHORT> {
        TransformList::new(input.items)
    }
}

/// Prefer to end a list rather than continue parsing items.
#[non_exhaustive]
pub struct prefer_short<const PREFER_SHORT: bool = true> {}

impl<T: Rule, const PREFER_SHORT: bool> TransformInto<Vec<T>> for prefer_short<PREFER_SHORT> {
    type Input = TransformList<T, identity, Empty, false, PREFER_SHORT>;

    fn transform(input: Self::Input) -> Vec<T> {
        input.items
    }
}

impl<
        T: Rule,
        X: TransformInto<T>,
        Delim: Rule,
        const TRAIL: bool,
        const PREFER_SHORT: bool,
        const PREFER_SHORT1: bool,
    > TransformInto<TransformList<T, X, Delim, TRAIL, PREFER_SHORT1>>
    for prefer_short<PREFER_SHORT>
{
    type Input = TransformList<T, X, Delim, TRAIL, PREFER_SHORT>;

    fn transform(input: Self::Input) -> TransformList<T, X, Delim, TRAIL, PREFER_SHORT1> {
        TransformList::new(input.items)
    }
}

/// Parse as a single token for lookahead purposes.
#[non_exhaustive]
pub struct compound_token {}

impl<T: Rule> TransformInto<T> for compound_token {
    type Input = CompoundToken<T>;

    fn transform(input: Self::Input) -> T {
        input.value
    }
}

/// Initially parse as `Outer`.
/// If `Outer` successfully parses, parse the target rule over the section matched by `Outer`.
/// Discard the parsed `Outer`.
///
/// If you wish to keep both the inner and outer parses, see [DualParse].
pub struct parse_as<Outer> {
    _outer: PhantomData<Outer>,
}

impl<Outer: Rule, Inner: Rule> TransformInto<Inner> for parse_as<Outer> {
    type Input = DualParse<Outer, Inner>;

    fn transform(input: Self::Input) -> Inner {
        input.inner
    }
}

/// Rejects if the section matched by the target rule starts with `Invalid`.
pub struct not<Invalid> {
    _invalid: PhantomData<Invalid>,
}

impl<Invalid: Rule, Valid: Rule> TransformInto<Valid> for not<Invalid> {
    type Input = NotParse<Invalid, Valid>;

    fn transform(input: Self::Input) -> Valid {
        input.value
    }
}
