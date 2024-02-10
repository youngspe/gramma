///
mod macros;
pub mod print;
pub mod transform;

use core::{
    any::{Any, TypeId},
    cmp::Ordering,
    fmt::{self, Debug, Formatter},
    hash::Hash,
    marker::PhantomData,
    ops::ControlFlow::{self, Break, Continue},
};

use either::{for_both, Either};

use crate::{
    internal_prelude::*,
    parse::{
        CxType, Location, LocationRange, ParseContext, ParseContextParts, ParseContextUpdate,
        ParseError, SizedParseContext,
    },
    token::{AnyToken, Eof, Token, TokenObject},
    utils::{default, simple_name, try_run, DebugFn, MyTry},
};

use self::{
    print::{PrintContext, PrintVisibility},
    transform::{identity, TransformInto},
};

struct WithSource<'data, T: ?Sized> {
    pub src: &'data str,
    pub ast: &'data T,
}

impl<T: Rule + ?Sized> Debug for WithSource<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.ast
            .print_tree(PrintContext::new(self.src).set_debug(true), f)
    }
}

impl<T: Rule + ?Sized> fmt::Display for WithSource<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.ast.print_tree(&PrintContext::new(self.src), f)
    }
}

/// Values used to represent the current parser state in [Rule::pre_parse].
#[non_exhaustive]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PreParseState {
    pub start: Location,
    pub end: Location,
    pub dist: usize,
}

/// Represents part of a grammar.
///
/// See also [crate::define_rule].
pub trait Rule: Any + Debug {
    /// Determines whether the AST node should be printed.
    fn print_visibility(&self, cx: &PrintContext) -> PrintVisibility {
        let _ = cx;
        PrintVisibility::Always
    }

    /// Writes the contents of the parsed AST node to `f`.
    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        let _ = cx;
        Debug::fmt(self, f)
    }

    /// The name of the rule.
    fn name() -> &'static str
    where
        Self: Sized,
    {
        simple_name::<Self>()
    }

    /// Writes the name of this rule to `f`.
    fn print_name(f: &mut Formatter) -> fmt::Result
    where
        Self: Sized,
    {
        f.write_str(Self::name())
    }

    /// Begins evaluating this rule, stopping when the lookahead buffer is full.
    fn pre_parse<Cx: CxType>(
        cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleObject<Cx>,
    ) -> RuleParseResult<()>
    where
        Self: Sized;

    /// Evaluates the rule.
    ///
    /// Returns `Ok(Self)` if successful, or `Err(_)` if the source does not match this rule.
    fn parse<Cx: CxType>(cx: ParseContext<Cx>, next: &RuleObject<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized;
}

/// An object dynamically representing an implementation of [Rule].
#[derive(Clone, Copy)]
pub struct RuleObject<'lt, Cx: CxType> {
    name: fn() -> &'static str,
    print_name: fn(&mut Formatter) -> fmt::Result,
    pre_parse: fn(ParseContext<Cx>, PreParseState, &RuleObject<Cx>) -> RuleParseResult<()>,
    next: Option<&'lt Self>,
}

impl<'lt, Cx: CxType> RuleObject<'lt, Cx> {
    /// Creates a [RuleObject] for rule `T`, which may be followed by another [RuleObject] `next`.
    pub fn new<T: Rule>(next: impl Into<Option<&'lt Self>>) -> Self {
        RuleObject {
            name: T::name,
            print_name: T::print_name,
            pre_parse: T::pre_parse::<Cx>,
            next: next.into(),
        }
    }

    /// Creates an unbounded reference to a [RuleObject] for `T`.
    pub const fn of<T: Rule>() -> &'lt Self {
        &RuleObject::<Cx> {
            name: T::name,
            print_name: T::print_name,
            pre_parse: T::pre_parse,
            next: None,
        }
    }

    /// The name of the rule.
    #[inline]
    pub fn name(&self) -> &str {
        (self.name)()
    }

    /// Begins evaluating this rule, stopping when the lookahead buffer is full.
    #[inline]
    pub fn pre_parse(&self, cx: ParseContext<Cx>, state: PreParseState) -> RuleParseResult<()> {
        if state.start > state.end || state.dist >= cx.look_ahead().len() {
            return Ok(());
        }
        (self.pre_parse)(cx, state, self.next.unwrap_or_default())
    }
}

impl<Cx: CxType> Debug for RuleObject<'_, Cx> {
    fn fmt(mut self: &Self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut list = f.debug_list();

        loop {
            list.entry(&DebugFn(self.print_name));

            match self.next {
                Some(next) => self = next,
                None => break list.finish(),
            }
        }
    }
}

impl<Cx: CxType> Default for RuleObject<'_, Cx> {
    fn default() -> Self {
        Self::new::<Accept>(None)
    }
}

impl<'lt, Cx: CxType> Default for &'lt RuleObject<'lt, Cx> {
    fn default() -> Self {
        RuleObject::of::<Accept>()
    }
}

/// A rule that rejects all possible source strings.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Reject {}

impl Rule for Reject {
    fn pre_parse<Cx: CxType>(
        _: ParseContext<Cx>,
        state: PreParseState,
        _: &RuleObject<Cx>,
    ) -> RuleParseResult<()> {
        Err(RuleParseFailed {
            location: state.start,
        })
    }

    fn parse<Cx: CxType>(cx: ParseContext<Cx>, _: &RuleObject<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        Err(RuleParseFailed {
            location: cx.location(),
        })
    }
}

/// A helper trait for implementing [Rule] based on another rule.
pub trait DelegateRule: Any + Debug {
    /// The rule to parse.
    type Inner: Rule;

    /// See [Rule::print_name].
    fn print_name(f: &mut Formatter) -> fmt::Result {
        f.write_str(Self::name())
    }

    /// See [Rule::print_visibility]
    fn print_visibility(&self, cx: &PrintContext) -> PrintVisibility {
        let _ = cx;
        PrintVisibility::Always
    }

    /// Transforms `Self::Inner` into `Self` after successfully parsing `Self::Inner`.
    fn from_inner(inner: Self::Inner) -> Self;

    /// See [Rule::print_tree]
    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        let _ = cx;
        Debug::fmt(self, f)
    }

    /// See [Rule::name]
    fn name() -> &'static str {
        simple_name::<Self>()
    }

    /// Optionally performs operations on the [ParseContext] before parsing `Self::Inner`.
    fn update_context<Cx: CxType, R>(
        cx: ParseContext<Cx>,
        f: impl FnOnce(ParseContext<Cx>) -> R,
    ) -> R {
        f(cx)
    }
}

impl<This> Rule for This
where
    This: DelegateRule,
{
    fn print_name(f: &mut Formatter) -> fmt::Result {
        <This as DelegateRule>::print_name(f)
    }
    fn name() -> &'static str {
        <This as DelegateRule>::name()
    }
    fn print_visibility(&self, cx: &PrintContext) -> PrintVisibility {
        DelegateRule::print_visibility(self, cx)
    }

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        DelegateRule::print_tree(self, cx, f)
    }

    fn pre_parse<Cx: CxType>(
        cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleObject<Cx>,
    ) -> RuleParseResult<()> {
        Self::update_context(cx, |cx| This::Inner::pre_parse(cx, state, next))
    }

    fn parse<Cx: CxType>(cx: ParseContext<Cx>, next: &RuleObject<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        Self::update_context(cx, |cx| This::Inner::parse(cx, next).map(This::from_inner))
    }
}

/// Base [Rule] implementation for [Vec].
pub struct TransformList<
    T,
    X: TransformInto<T>,
    Delim = Empty,
    const TRAIL: bool = false,
    const PREFER_SHORT: bool = false,
> {
    pub items: Vec<T>,
    _x: PhantomData<X>,
    _delim: PhantomData<Delim>,
}

impl<T, X: TransformInto<T>, Delim, const TRAIL: bool, const PREFER_SHORT: bool>
    TransformList<T, X, Delim, TRAIL, PREFER_SHORT>
{
    pub fn new(items: Vec<T>) -> Self {
        Self {
            items,
            _x: PhantomData,
            _delim: PhantomData,
        }
    }
}

impl<T: Debug, X: TransformInto<T>, Delim, const TRAIL: bool, const PREFER_SHORT: bool> Debug
    for TransformList<T, X, Delim, TRAIL, PREFER_SHORT>
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.items, f)
    }
}

impl<T: Rule> DelegateRule for Vec<T> {
    type Inner = TransformList<T, identity>;
    fn print_name(f: &mut Formatter) -> fmt::Result {
        f.write_str("Vec(")?;
        T::print_name(f)?;
        f.write_str(")")
    }

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        cx.debug_list(f, self.iter().map(|item| item as _))
    }

    fn from_inner(inner: Self::Inner) -> Self {
        inner.items
    }
}

/// Rule that reads zero tokens and accepts. Equivalent to using `()`.
#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Empty;

impl Rule for Empty {
    fn pre_parse<Cx: CxType>(
        cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleObject<Cx>,
    ) -> RuleParseResult<()> {
        next.pre_parse(cx, state)
    }

    fn parse<Cx: CxType>(_: ParseContext<Cx>, _: &RuleObject<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        Ok(Self)
    }
}

impl<T: Rule> DelegateRule for Option<T> {
    fn print_name(f: &mut Formatter) -> fmt::Result {
        f.write_str("(")?;
        T::print_name(f)?;
        f.write_str(")?")
    }

    type Inner = Either<T, Empty>;

    fn from_inner(inner: Self::Inner) -> Self {
        inner.left()
    }

    fn print_visibility(&self, _: &PrintContext) -> PrintVisibility {
        match self {
            Some(_) => PrintVisibility::Always,
            None => PrintVisibility::DebugOnly,
        }
    }

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        match (self, cx.is_debug()) {
            (None, true) => f.write_str("None"),
            (None, false) => Ok(()),
            (Some(value), true) => {
                f.write_str("Some(")?;
                value.print_tree(cx, f)?;
                f.write_str(")")
            }
            (Some(value), false) => value.print_tree(cx, f),
        }
    }
}

impl<T: Rule> DelegateRule for Box<T> {
    type Inner = T;

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        (**self).print_tree(cx, f)
    }

    fn from_inner(inner: Self::Inner) -> Self {
        Box::new(inner)
    }
}

/// Simple error type indicating where parsing failed.
#[derive(Debug)]
pub struct RuleParseFailed {
    pub location: Location,
}

impl RuleParseFailed {
    pub fn combine(self, rhs: Self) -> Self {
        RuleParseFailed {
            location: self.location.max(rhs.location),
        }
    }
}

pub type RuleParseResult<T> = Result<T, RuleParseFailed>;

impl<T: Rule, U: Rule> Rule for Either<T, U> {
    fn print_name(f: &mut Formatter) -> fmt::Result {
        f.write_str("(")?;
        T::print_name(f)?;
        f.write_str(" | ")?;
        U::print_name(f)?;
        f.write_str(")")
    }
    fn print_visibility(&self, cx: &PrintContext) -> PrintVisibility {
        for_both!(self, ast => ast.print_visibility(cx))
    }
    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        for_both!(self, ast => ast.print_tree(cx, f))
    }

    fn pre_parse<Cx: CxType>(
        mut cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleObject<Cx>,
    ) -> RuleParseResult<()> {
        T::pre_parse(cx.by_ref(), state, next).or_else(|_| U::pre_parse(cx, state, next))
    }

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, next: &RuleObject<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        let Err(err1) = cx.pre_parse::<T>(next) else {
            return T::parse(cx, next).map(Either::Left);
        };
        let Err(err2) = cx.pre_parse::<U>(next) else {
            return U::parse(cx, next).map(Either::Right);
        };

        let _ = cx.record_error::<T>(next);
        let _ = cx.record_error::<U>(next);

        Err(err1.combine(err2))
    }
}

impl<T: Rule, U: Rule> Rule for (T, U) {
    fn print_name(f: &mut Formatter) -> fmt::Result {
        T::print_name(f)?;
        f.write_str(" >> ")?;
        U::print_name(f)
    }

    fn print_visibility(&self, cx: &PrintContext) -> PrintVisibility {
        self.0.print_visibility(cx).max(self.1.print_visibility(cx))
    }

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        cx.debug_tuple("", f, [&self.0 as _, &self.1 as _])
    }

    fn pre_parse<Cx: CxType>(
        cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleObject<Cx>,
    ) -> RuleParseResult<()> {
        T::pre_parse(cx, state, &RuleObject::new::<U>(next))
    }

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, next: &RuleObject<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        Ok((
            T::parse(cx.by_ref(), &RuleObject::new::<U>(next))?,
            U::parse(cx, next)?,
        ))
    }
}

impl DelegateRule for () {
    type Inner = Empty;

    fn print_name(f: &mut Formatter) -> fmt::Result {
        f.write_str("()")
    }

    fn print_visibility(&self, _: &PrintContext) -> PrintVisibility {
        PrintVisibility::DebugOnly
    }

    fn from_inner(_: Self::Inner) -> Self {
        ()
    }
}

impl<T0: Rule> DelegateRule for (T0,) {
    type Inner = T0;

    fn print_name(f: &mut Formatter) -> fmt::Result {
        T0::print_name(f)
    }

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        self.0.print_tree(cx, f)
    }

    fn print_visibility(&self, _: &PrintContext) -> PrintVisibility {
        PrintVisibility::DebugOnly
    }

    fn from_inner(inner: Self::Inner) -> Self {
        (inner,)
    }
}

impl<T0: Rule, T1: Rule, T2: Rule> DelegateRule for (T0, T1, T2) {
    type Inner = (T0, (T1, T2));

    fn print_name(f: &mut Formatter) -> fmt::Result {
        T0::print_name(f)?;
        f.write_str(" >> ")?;
        T1::print_name(f)?;
        f.write_str(" >> ")?;
        T2::print_name(f)
    }

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        self.0.print_tree(cx, f)
    }

    fn print_visibility(&self, cx: &PrintContext) -> PrintVisibility {
        self.0
            .print_visibility(cx)
            .max(self.1.print_visibility(cx))
            .max(self.2.print_visibility(cx))
    }

    fn from_inner((x0, (x1, x2)): Self::Inner) -> Self {
        (x0, x1, x2)
    }
}

impl<T0: Rule, T1: Rule, T2: Rule, T3: Rule> DelegateRule for (T0, T1, T2, T3) {
    type Inner = ((T0, T1), (T2, T3));

    fn print_name(f: &mut Formatter) -> fmt::Result {
        T0::print_name(f)?;
        f.write_str(" >> ")?;
        T1::print_name(f)?;
        f.write_str(" >> ")?;
        T2::print_name(f)?;
        f.write_str(" >> ")?;
        T3::print_name(f)
    }

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        self.0.print_tree(cx, f)
    }

    fn print_visibility(&self, cx: &PrintContext) -> PrintVisibility {
        self.0
            .print_visibility(cx)
            .max(self.1.print_visibility(cx))
            .max(self.2.print_visibility(cx))
            .max(self.3.print_visibility(cx))
    }

    fn from_inner(((x0, x1), (x2, x3)): Self::Inner) -> Self {
        (x0, x1, x2, x3)
    }
}

/// Rule that reads exactly one token of type `T`.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReadToken<T> {
    pub range: LocationRange,
    _t: PhantomData<T>,
}

impl<T> Copy for ReadToken<T> {}

impl<T> Clone for ReadToken<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Token> Debug for ReadToken<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(T::name())
    }
}

impl<T: Token> From<ReadToken<T>> for AnyToken {
    fn from(value: ReadToken<T>) -> Self {
        Self {
            token_type: TokenObject::of::<T>(),
            range: value.range,
        }
    }
}

impl<T> From<LocationRange> for ReadToken<T> {
    fn from(range: LocationRange) -> Self {
        Self {
            range,
            _t: PhantomData,
        }
    }
}

impl<T: Token> Rule for ReadToken<T> {
    fn print_name(f: &mut Formatter) -> fmt::Result
    where
        Self: Sized,
    {
        f.write_str(T::display_name())
    }

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        if cx.is_debug() {
            T::print_debug(cx.src(), self.range, f)
        } else {
            T::print_display(cx.src(), self.range, f)
        }
    }
    fn pre_parse<Cx: CxType>(
        mut cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleObject<Cx>,
    ) -> RuleParseResult<()> {
        if state.start > state.end {
            return Ok(());
        }

        let ParseContextParts {
            src, look_ahead, ..
        } = cx.as_parts();

        let end = match look_ahead.get_mut(state.dist..) {
            None | Some([]) => return Ok(()),
            Some([Some(token), ..]) if token.token_type.token_id() == TypeId::of::<T>() => {
                token.range.end
            }
            Some([token, ..]) => {
                let Some(range) = T::try_lex(src, state.start) else {
                    cx.error_mut()
                        .add_expected(state.start, TokenObject::of::<T>());
                    return Err(RuleParseFailed {
                        location: state.start,
                    });
                };
                *token = Some(AnyToken {
                    token_type: TokenObject::of::<T>(),
                    range,
                });
                range.end
            }
        };

        next.pre_parse(
            cx,
            PreParseState {
                start: end,
                dist: state.dist + 1,
                ..state
            },
        )
    }

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, _: &RuleObject<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        let location = cx.location();

        try_run(|| {
            if let [Some(token), ..] = **cx.look_ahead() {
                if token.token_type.token_id() != TypeId::of::<T>() {
                    return Err(RuleParseFailed { location });
                }
                cx.advance();
                return Ok(token.range.into());
            }

            let range = T::try_lex(cx.src(), location).ok_or(RuleParseFailed { location })?;
            cx.set_location(range.end);

            Ok(range.into())
        })
        .break_also(|err| {
            cx.error_mut()
                .add_expected(err.location, TokenObject::of::<T>())
        })
    }
}

impl<T: Rule> DelegateRule for PhantomData<T> {
    type Inner = Discard<T>;

    fn print_visibility(&self, _: &PrintContext) -> PrintVisibility {
        PrintVisibility::Never
    }

    fn from_inner(_: Self::Inner) -> Self {
        Self
    }
}

macro_rules! generic_unit {
    ($($(#$attr:tt)*$vis:vis struct $Name:ident<$($T:ident),* $(,)?>;)*) => {$(
        $(#$attr)*
        $vis struct $Name<$($T: ?Sized),*>($(PhantomData<$T>),*);


        impl<$($T: ?Sized),*> Debug for $Name<$($T),*> {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                f.debug_tuple("Discard").field(&self.0).finish()
            }
        }

        impl<$($T: ?Sized),*> Default for $Name<$($T),*> {
            fn default() -> Self {
                Self(PhantomData)
            }
        }

        impl<$($T: ?Sized),*> Clone for $Name<$($T),*> {
            fn clone(&self) -> Self {
                *self
            }
        }

        impl<$($T: ?Sized),*> Copy for $Name<$($T),*> {}

        impl<$($T: ?Sized),*> PartialEq for $Name<$($T),*> {
            fn eq(&self, _: &Self) -> bool {
                true
            }
        }
        impl<$($T: ?Sized),*> Eq for $Name<$($T),*> {}
        impl<$($T: ?Sized),*> PartialOrd for $Name<$($T),*> {
            fn partial_cmp(&self, _: &Self) -> Option<Ordering> {
                Some(Ordering::Equal)
            }
        }
        impl<$($T: ?Sized),*> Ord for $Name<$($T),*> {
            fn cmp(&self, _: &Self) -> Ordering {
                Ordering::Equal
            }
        }
        impl<$($T: ?Sized),*> Hash for $Name<$($T),*> {
            fn hash<H: core::hash::Hasher>(&self, _: &mut H) {}
        }
    )*};
}

/// Rule that consumes the remaining source string and accepts regardless of its content.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Accept;

impl Rule for Accept {
    fn name() -> &'static str {
        "Accept"
    }

    fn pre_parse<Cx: CxType>(
        _: ParseContext<Cx>,
        _: PreParseState,
        _: &RuleObject<Cx>,
    ) -> RuleParseResult<()> {
        Ok(())
    }

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, _: &RuleObject<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        cx.set_location(Location {
            position: cx.src().len(),
        });
        Ok(Self)
    }
}

/// Rule that is initially parsed as `Outer`.
/// If `Outer` parses successfully, the string that matched `Outer` will be parsed again as `Inner`.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DualParse<Outer, Inner> {
    pub outer: Outer,
    pub inner: Inner,
}

impl<Outer: Rule, Inner: Rule> Rule for DualParse<Outer, Inner> {
    fn print_name(f: &mut Formatter) -> fmt::Result
    where
        Self: Sized,
    {
        f.write_str("(")?;
        Outer::print_name(f)?;
        f.write_str(" & ")?;
        Inner::print_name(f)?;
        f.write_str(")")
    }

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        let outer_vis = self.outer.print_visibility(cx).should_print(cx);
        let inner_vis = self.inner.print_visibility(cx).should_print(cx);

        if outer_vis {
            self.outer.print_tree(cx, f)?;
        }

        if outer_vis && inner_vis {
            f.write_str(" & ")?;
        }

        if inner_vis {
            self.inner.print_tree(cx, f)?;
        }

        Ok(())
    }

    fn pre_parse<Cx: CxType>(
        mut cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleObject<Cx>,
    ) -> RuleParseResult<()>
    where
        Self: Sized,
    {
        let mut look_ahead = *cx.look_ahead();
        Outer::pre_parse(cx.by_ref(), state, next)?;
        Inner::pre_parse(
            cx.by_ref().update(ParseContextUpdate {
                look_ahead: Some(&mut look_ahead),
                ..default()
            }),
            state,
            default(),
        )
    }

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, next: &RuleObject<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        let src = cx.src();
        let start = cx.location();

        let (outer, end) = <(Outer, Location)>::parse(cx.by_ref(), next)?;

        let (inner, _) = match cx.by_ref().update(ParseContextUpdate {
            src: Some(&src[..end.position]),
            location: Some(&mut start.clone()),
            look_ahead: Some(&mut default()),
            ..default()
        }) {
            cx => <(Inner, Silent<ReadToken<Eof>>)>::parse(cx, default())?,
        };

        if end > start {
            cx.set_location(end);
        }

        Ok(Self { outer, inner })
    }
}

/// Rule that parses `T` as if it's a singular token for lookahead purposes.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CompoundToken<T> {
    pub value: T,
}

struct CompoundTokenDef<T>(PhantomData<T>);

impl<T: 'static> Token for CompoundTokenDef<T> {
    fn try_lex(_: &str, _: Location) -> Option<LocationRange> {
        None
    }
}

impl<T: Rule> Rule for CompoundToken<T> {
    fn print_name(f: &mut Formatter) -> fmt::Result {
        f.write_str("CompoundToken(")?;
        T::print_name(f)?;
        f.write_str(")")
    }

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        self.value.print_tree(cx, f)
    }

    fn print_visibility(&self, cx: &PrintContext) -> PrintVisibility {
        self.value.print_visibility(cx)
    }

    fn pre_parse<Cx: CxType>(
        mut cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleObject<Cx>,
    ) -> RuleParseResult<()>
    where
        Self: Sized,
    {
        let end = match cx.look_ahead().get(state.dist).copied() {
            Some(Some(token)) if token.token_type == TokenObject::of::<CompoundTokenDef<T>>() => {
                token.range.end
            }
            Some(_) => {
                let (_, end) = cx.isolated_parse::<Discard<T>>(state.start, next)?;
                cx.look_ahead_mut()[state.dist] = Some(AnyToken {
                    token_type: TokenObject::of::<CompoundTokenDef<T>>(),
                    range: LocationRange {
                        start: state.start,
                        end,
                    },
                });
                end
            }
            None => return Ok(()),
        };

        next.pre_parse(
            cx,
            PreParseState {
                start: end,
                dist: state.dist + 1,
                ..state
            },
        )
    }

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, next: &RuleObject<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        let location = cx.location();
        Ok(match cx.look_ahead().first().copied().flatten() {
            Some(token) if token.token_type != TokenObject::of::<CompoundTokenDef<T>>() => {
                return Err(RuleParseFailed { location });
            }
            Some(_) => {
                let value = T::parse(
                    cx.by_ref().update(ParseContextUpdate {
                        look_ahead: Some(&mut default()),
                        ..default()
                    }),
                    next,
                )?;
                cx.advance();
                Self { value }
            }
            None => Self {
                value: T::parse(cx, next)?,
            },
        })
    }
}

/// Rule that ignores the lookahead buffer altogether and just tries parsing to see if it matches the source.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Backtrack<T> {
    pub value: T,
}

impl<T: Rule> Rule for Backtrack<T> {
    fn pre_parse<Cx: CxType>(
        mut cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleObject<Cx>,
    ) -> RuleParseResult<()>
    where
        Self: Sized,
    {
        let (_, end) = cx.isolated_parse::<(Discard<T>,)>(state.start, next)?;
        next.pre_parse(
            cx,
            PreParseState {
                start: end,
                ..state
            },
        )
    }

    fn parse<Cx: CxType>(cx: ParseContext<Cx>, next: &RuleObject<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        let value = T::parse(
            cx.update(ParseContextUpdate {
                look_ahead: Some(&mut default()),
                ..default()
            }),
            next,
        )?;
        Ok(Self { value })
    }
}

/// Rule that parses as `T` but doesn't record any of its errors, and potentially ignores subsequent rules' errors.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Silent<T> {
    pub value: T,
}

impl<T: Rule> DelegateRule for Silent<T> {
    type Inner = T;

    fn print_name(f: &mut Formatter) -> fmt::Result {
        f.write_str("Silent(")?;
        T::print_name(f)?;
        f.write_str(")")
    }

    fn print_visibility(&self, cx: &PrintContext) -> PrintVisibility {
        self.value.print_visibility(cx)
    }

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        self.value.print_tree(cx, f)
    }

    fn from_inner(value: Self::Inner) -> Self {
        Self { value }
    }

    fn update_context<Cx: CxType, R>(
        cx: ParseContext<Cx>,
        f: impl FnOnce(ParseContext<Cx>) -> R,
    ) -> R {
        f(cx.update(ParseContextUpdate {
            error: Some(&mut ParseError {
                location: Location::MAX,
                ..default()
            }),
            ..default()
        }))
    }
}

impl Rule for Location {
    fn pre_parse<Cx: CxType>(
        cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleObject<Cx>,
    ) -> RuleParseResult<()>
    where
        Self: Sized,
    {
        next.pre_parse(cx, state)
    }

    fn parse<Cx: CxType>(cx: ParseContext<Cx>, _: &RuleObject<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        Ok(cx.location())
    }
}

impl Rule for LocationRange {
    fn pre_parse<Cx: CxType>(
        cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleObject<Cx>,
    ) -> RuleParseResult<()>
    where
        Self: Sized,
    {
        let end = Location {
            position: cx.src().len(),
        };
        next.pre_parse(
            cx,
            PreParseState {
                start: end,
                ..state
            },
        )
    }

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, _: &RuleObject<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        let start = cx.location();
        let end = Location {
            position: cx.src().len(),
        };
        cx.set_location(end);
        Ok(Self { start, end })
    }

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}..{} => {:?}",
            self.start.position,
            self.end.position,
            cx.src()
                .get(self.start.position..self.end.position)
                .unwrap_or_default()
        )
    }
}

generic_unit!(
    /// Rule that parses as `T` but doesn't store the result.
    pub struct Discard<T>;
    /// Rule that optionally parses as `T`, but doesn't store the result
    /// and is omitted from the lookahead buffer.
    pub struct Ignore<T>;
);

impl<T: Rule> DelegateRule for Discard<T> {
    type Inner = T;

    fn print_name(f: &mut Formatter) -> fmt::Result {
        f.write_str("Discard(")?;
        T::print_name(f)?;
        f.write_str(")")
    }

    fn from_inner(_: Self::Inner) -> Self {
        default()
    }

    fn print_tree(&self, _: &PrintContext, f: &mut Formatter) -> fmt::Result {
        write!(f, "Discard<{}>", T::name())
    }

    fn print_visibility(&self, _: &PrintContext) -> PrintVisibility {
        PrintVisibility::DebugOnly
    }

    fn update_context<Cx: CxType, R>(
        cx: ParseContext<Cx>,
        f: impl FnOnce(ParseContext<Cx>) -> R,
    ) -> R {
        f(cx.discarding())
    }
}

impl<T: Rule> DelegateRule for Ignore<T> {
    type Inner = Backtrack<Discard<Option<T>>>;
    fn from_inner(_: Self::Inner) -> Self {
        default()
    }

    // impl<T: Rule> Rule for Ignore<T> {
    fn print_name(f: &mut Formatter) -> fmt::Result {
        f.write_str("Ignore(")?;
        T::print_name(f)?;
        f.write_str(")")
    }

    fn print_tree(&self, _: &PrintContext, _: &mut Formatter) -> fmt::Result {
        Ok(())
    }

    fn print_visibility(&self, _: &PrintContext) -> PrintVisibility {
        PrintVisibility::Never
    }
}

impl<B: Rule, C: Rule> Rule for ControlFlow<B, C> {
    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        if cx.is_debug() {
            match self {
                Continue(_) => {
                    f.write_str("Continue -> ")?;
                }
                Break(_) => {
                    f.write_str("Break -> ")?;
                }
            }
        }

        match self {
            Continue(x) => x.print_tree(cx, f),
            Break(x) => x.print_tree(cx, f),
        }
    }

    fn print_visibility(&self, cx: &PrintContext) -> PrintVisibility {
        match self {
            Continue(x) => x.print_visibility(cx),
            Break(x) => x.print_visibility(cx),
        }
    }

    fn pre_parse<Cx: CxType>(
        cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleObject<Cx>,
    ) -> RuleParseResult<()>
    where
        Self: Sized,
    {
        if cx.prefer_continue() {
            Either::<C, B>::pre_parse(cx, state, next)
        } else {
            Either::<B, C>::pre_parse(cx, state, next)
        }
    }

    fn parse<Cx: CxType>(cx: ParseContext<Cx>, next: &RuleObject<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        Ok(if cx.prefer_continue() {
            match Either::<C, B>::parse(cx, next)? {
                Either::Left(x) => Continue(x),
                Either::Right(x) => Break(x),
            }
        } else {
            match Either::<B, C>::parse(cx, next)? {
                Either::Left(x) => Break(x),
                Either::Right(x) => Continue(x),
            }
        })
    }
}

/// Rule that parses `T` is if it may be followed by one or more instances of `T`.
#[derive(Debug)]
pub struct ListNode<T> {
    value: Option<T>,
}

impl<T: Rule> DelegateRule for ListNode<T> {
    type Inner = ControlFlow<(), Partial<T, ControlFlow<(), ListNode<T>>>>;

    fn print_name(f: &mut Formatter) -> fmt::Result {
        f.write_str("ListNodePlaceholder(")?;
        T::print_name(f)?;
        f.write_str(")")
    }

    fn from_inner(inner: Self::Inner) -> Self {
        Self {
            value: match inner {
                Break(()) => None,
                Continue(Partial { value, .. }) => Some(value),
            },
        }
    }
}

/// Rule that parses `T` as if it is followed by `After`.
#[derive(Debug)]
pub struct Partial<T, After> {
    pub value: T,
    _after: PhantomData<After>,
}

impl<T, After> Partial<T, After> {
    pub fn new(value: T) -> Self {
        Self {
            value,
            _after: PhantomData,
        }
    }
}

impl<T: Rule, After: Rule> Rule for Partial<T, After> {
    fn print_name(f: &mut Formatter) -> fmt::Result
    where
        Self: Sized,
    {
        f.write_str("Partial(")?;
        T::print_name(f)?;
        f.write_str(", ")?;
        After::print_name(f)?;
        f.write_str(")")
    }

    fn pre_parse<Cx: CxType>(
        cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleObject<Cx>,
    ) -> RuleParseResult<()>
    where
        Self: Sized,
    {
        <(T, After)>::pre_parse(cx, state, next)
    }

    fn parse<Cx: CxType>(cx: ParseContext<Cx>, next: &RuleObject<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        T::parse(cx, &RuleObject::new::<After>(next)).map(Self::new)
    }
}

/// Rule that parses `T` after transformation `X` has been applied.
pub struct Transformed<T, X> {
    pub value: T,
    _x: PhantomData<X>,
}

impl<T, X> Debug for Transformed<T, X> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Transformed").finish_non_exhaustive()
    }
}

impl<In: Rule, Out: 'static, X: TransformInto<Out, Input = In> + 'static> DelegateRule
    for Transformed<Out, X>
{
    type Inner = In;

    fn from_inner(input: Self::Inner) -> Self {
        Self {
            value: X::transform(input),
            _x: PhantomData,
        }
    }
}

type DelimitedListPrototypeTail<T, Delim, Trailing> = (ListNode<(Delim, T)>, Trailing);

type DelimitedListPrototype<T, Delim, Trailing> =
    Option<(T, DelimitedListPrototypeTail<T, Delim, Trailing>)>;

#[derive(Debug)]
struct DelimitedListTailTrailing<T, Delim> {
    value: Option<T>,
    _delim: PhantomData<Delim>,
}

impl<T: Rule, Delim: Rule> DelegateRule for DelimitedListTailTrailing<T, Delim> {
    type Inner = ControlFlow<(), (Discard<Delim>, ControlFlow<(), Partial<T, Self>>)>;

    fn from_inner(inner: Self::Inner) -> Self {
        let value = match inner {
            Continue((_, Continue(Partial { value, .. }))) => Some(value),
            Break(()) | Continue((_, Break(()))) => None,
        };

        Self {
            value,
            _delim: PhantomData,
        }
    }
}

type DelimitedListTail<T, Delim> = ListNode<(Discard<Delim>, T)>;

/// Parses a list of `T` separated by `Delim`.
/// When `TRAIL` is `true`, the list may end with an optional extra `Delim`
pub type DelimitedList<T, Delim, const TRAIL: bool = true> =
    TransformList<T, identity, Delim, TRAIL>;

impl<Out, In, X, Delim, const TRAIL: bool, const PREFER_SHORT: bool> Rule
    for TransformList<Out, X, Delim, TRAIL, PREFER_SHORT>
where
    Out: Rule,
    In: Rule,
    X: TransformInto<Out, Input = In> + 'static,
    Delim: Rule,
{
    fn print_name(f: &mut Formatter) -> fmt::Result {
        f.debug_struct("List")
            .field("In", &DebugFn(In::print_name))
            .field("Out", &DebugFn(Out::print_name))
            .field("Delim", &DebugFn(Delim::print_name))
            .finish()
    }

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        cx.debug_list(f, self.items.iter().map(|item| item as _))
    }

    fn pre_parse<Cx: CxType>(
        cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleObject<Cx>,
    ) -> RuleParseResult<()>
    where
        Self: Sized,
    {
        if TRAIL {
            DelimitedListPrototype::<Out, Delim, Option<Delim>>::pre_parse(cx, state, next)
        } else {
            DelimitedListPrototype::<Out, Delim, Empty>::pre_parse(cx, state, next)
        }
    }

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, next: &RuleObject<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        cx = cx.update(ParseContextUpdate {
            prefer_continue: Some(!PREFER_SHORT),
            ..default()
        });
        let mut out = Vec::new();
        let discard = cx.should_discard();

        if TRAIL {
            let Continue(Partial { value: first, .. }) = ControlFlow::<
                (),
                Partial<In, DelimitedListTailTrailing<In, Delim>>,
            >::parse(cx.by_ref(), next)?
            else {
                return Ok(Self::new(out));
            };

            if !discard {
                out.push(X::transform(first));
            }

            while let Some(item) =
                DelimitedListTailTrailing::<In, Delim>::parse(cx.by_ref(), next)?.value
            {
                if !discard {
                    out.push(X::transform(item));
                }
            }
        } else {
            let Continue(Partial { value: first, .. }) = ControlFlow::<
                (),
                Partial<In, DelimitedListTail<In, Delim>>,
            >::parse(cx.by_ref(), next)?
            else {
                return Ok(Self::new(out));
            };

            if !discard {
                out.push(X::transform(first));
            }

            while let Some((_, item)) =
                DelimitedListTail::<In, Delim>::parse(cx.by_ref(), next)?.value
            {
                if !discard {
                    out.push(X::transform(item));
                }
            }
        }

        Ok(Self::new(out))
    }
}

/// An operator-operand pair.
#[derive(Debug)]
pub struct InfixChainItem<T, Op> {
    op: Op,
    value: T,
}

impl<T: Rule, Op: Rule> DelegateRule for InfixChainItem<T, Op> {
    type Inner = (Op, T);

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        let Self { op, value } = self;
        f.write_str("{ ")?;
        op.print_tree(cx, f)?;
        f.write_str(", ")?;
        value.print_tree(cx, f)?;
        f.write_str(" }")
    }

    fn from_inner((op, value): Self::Inner) -> Self {
        Self { op, value }
    }
}

/// Parses a set of binary operations with operand `T` and operator `Op`.
#[derive(Debug)]
pub struct InfixChain<T, Op> {
    first: T,
    rest: Vec<InfixChainItem<T, Op>>,
}

impl<T: Rule, Op: Rule> DelegateRule for InfixChain<T, Op> {
    type Inner = (T, Vec<InfixChainItem<T, Op>>);

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        let Self { first, rest } = self;
        cx.debug_tuple(
            "",
            f,
            [first as _].into_iter().chain(rest.iter().map(|x| x as _)),
        )
    }

    fn from_inner((first, rest): Self::Inner) -> Self {
        Self { first, rest }
    }
}

/// Rule that parses as `Valid`, but rejects if it also parses as `Invalid`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NotParse<Invalid, Valid> {
    _invalid: PhantomData<Invalid>,
    pub value: Valid,
}

impl<Invalid: Rule, Valid: Rule> Rule for NotParse<Invalid, Valid> {
    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        self.value.print_tree(cx, f)
    }

    fn print_visibility(&self, cx: &PrintContext) -> PrintVisibility {
        self.value.print_visibility(cx)
    }

    fn pre_parse<Cx: CxType>(
        mut cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleObject<Cx>,
    ) -> RuleParseResult<()>
    where
        Self: Sized,
    {
        let Err(_) = cx.isolated_parse::<(Invalid, Accept)>(None, default()) else {
            return Err(RuleParseFailed {
                location: cx.location(),
            });
        };

        Valid::pre_parse(cx, state, next)
    }

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, next: &RuleObject<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        let Err(_) = cx.isolated_parse::<(Invalid, Accept)>(None, default()) else {
            return Err(RuleParseFailed {
                location: cx.location(),
            });
        };

        Ok(Self {
            value: Valid::parse(cx, next)?,
            _invalid: PhantomData,
        })
    }
}

fn extract_actual<'src>(src: &'src str, start: usize) -> &'src str {
    if start >= src.len() {
        return "<end-of-file>";
    }

    crate::_lazy_regex! {
        static ref PSEUDO_TOKEN => r"\A.+?\b|.";
    }

    const MAX_LEN: usize = 32;

    let len = PSEUDO_TOKEN
        .find(&src[start..])
        .map(|m| m.end().min(MAX_LEN))
        .unwrap_or(1);

    &src[start..start + len]
}

pub fn display_tree<'data>(
    src: &'data str,
    ast: &'data impl Rule,
) -> impl Debug + fmt::Display + 'data {
    WithSource { src, ast }
}

/// Parses `src` using the rule `T` and lookahead buffer size `N`.
pub fn parse_tree<'src, T: Rule, const N: usize>(src: &'src str) -> Result<T, ParseError<'src>> {
    match SizedParseContext::<N>::new_with(src, move |cx| {
        <(T, ReadToken<Eof>)>::parse(cx, &mut default())
    }) {
        (Ok((value, _)), _) => Ok(value),
        (Err(_), mut err) => {
            err.actual = extract_actual(src, err.location.position);
            Err(err)
        }
    }
}
