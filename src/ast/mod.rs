mod macros;
pub mod print;
pub mod transform;

use std::{
    any::{Any, TypeId},
    cmp::Ordering,
    fmt::{self, Debug, Formatter},
    hash::Hash,
    marker::PhantomData,
};

use either::{for_both, Either};
use regex::Regex;

use crate::{
    parse::{
        CxType, Location, LocationRange, ParseContext, ParseContextParts, ParseContextUpdate,
        ParseError, SizedParseContext,
    },
    token::{AnyToken, Eof, TokenDef, TokenType},
    utils::{default, simple_name, try_run, DebugFn, MyTry},
};

use self::{
    print::{PrintContext, PrintVisibility},
    transform::{identity, TransformInto},
};

pub struct WithSource<'src, T: ?Sized> {
    pub src: &'src str,
    pub ast: T,
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

#[non_exhaustive]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PreParseState {
    pub start: Location,
    pub end: Location,
    pub dist: usize,
}

pub trait Rule: Any + Debug {
    fn print_visibility(&self, cx: &PrintContext) -> PrintVisibility {
        let _ = cx;
        PrintVisibility::Always
    }

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        let _ = cx;
        Debug::fmt(self, f)
    }

    fn name() -> &'static str
    where
        Self: Sized,
    {
        simple_name::<Self>()
    }

    fn print_name(f: &mut Formatter) -> fmt::Result
    where
        Self: Sized,
    {
        f.write_str(Self::name())
    }

    fn pre_parse<Cx: CxType>(
        cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()>
    where
        Self: Sized;

    fn parse<Cx: CxType>(cx: ParseContext<Cx>, next: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized;

    fn matches_empty() -> bool
    where
        Self: Sized,
    {
        true
    }
}

#[derive(Clone, Copy)]
pub struct RuleType<'lt, Cx: CxType> {
    name: fn() -> &'static str,
    print_name: fn(&mut Formatter) -> fmt::Result,
    node_id: fn() -> TypeId,
    pre_parse: fn(ParseContext<Cx>, PreParseState, &RuleType<Cx>) -> RuleParseResult<()>,
    next: Option<&'lt Self>,
}

impl<'lt, Cx: CxType> RuleType<'lt, Cx> {
    pub fn new<T: Rule>(next: impl Into<Option<&'lt Self>>) -> Self {
        RuleType {
            name: T::name,
            print_name: T::print_name,
            node_id: TypeId::of::<T>,
            pre_parse: T::pre_parse::<Cx>,
            next: next.into(),
        }
    }

    pub const fn of<T: Rule>() -> &'lt Self {
        &RuleType::<Cx> {
            name: T::name,
            print_name: T::print_name,
            node_id: TypeId::of::<T>,
            pre_parse: T::pre_parse,
            next: None,
        }
    }

    pub fn new_repeating<T: Rule>(next: impl Into<Option<&'lt Self>>) -> Self {
        Self::new::<ListNodePlaceholder<T>>(next)
    }

    #[inline]
    pub fn name(&self) -> &str {
        (self.name)()
    }

    #[inline]
    pub fn node_id(&self) -> TypeId {
        (self.node_id)()
    }

    #[inline]
    pub fn pre_parse(&self, cx: ParseContext<Cx>, state: PreParseState) -> RuleParseResult<()> {
        if state.dist >= cx.look_ahead().len() {
            return Ok(());
        }
        (self.pre_parse)(cx, state, self.next.unwrap_or_default())
    }
}

impl<Cx: CxType> Debug for RuleType<'_, Cx> {
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

impl<Cx: CxType> Default for RuleType<'_, Cx> {
    fn default() -> Self {
        Self::new::<Accept>(None)
    }
}

impl<'lt, Cx: CxType> Default for &'lt RuleType<'lt, Cx> {
    fn default() -> Self {
        RuleType::of::<Accept>()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Reject {}

impl Rule for Reject {
    fn pre_parse<Cx: CxType>(
        _: ParseContext<Cx>,
        state: PreParseState,
        _: &RuleType<Cx>,
    ) -> RuleParseResult<()> {
        Err(RuleParseFailed {
            location: state.start,
        })
    }

    fn parse<Cx: CxType>(cx: ParseContext<Cx>, _: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        Err(RuleParseFailed {
            location: cx.location(),
        })
    }

    fn matches_empty() -> bool {
        false
    }
}

pub trait TransformRule: Any + Debug {
    type Inner: Rule;

    fn print_name(f: &mut Formatter) -> fmt::Result {
        f.write_str(Self::name())
    }

    fn print_visibility(&self, cx: &PrintContext) -> PrintVisibility {
        let _ = cx;
        PrintVisibility::Always
    }

    fn from_inner(inner: Self::Inner) -> Self;

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        let _ = cx;
        Debug::fmt(self, f)
    }

    fn name() -> &'static str {
        simple_name::<Self>()
    }

    fn update_context<Cx: CxType, R>(
        cx: ParseContext<Cx>,
        f: impl FnOnce(ParseContext<Cx>) -> R,
    ) -> R {
        f(cx)
    }
}

impl<This> Rule for This
where
    This: TransformRule,
{
    fn print_name(f: &mut Formatter) -> fmt::Result {
        <This as TransformRule>::print_name(f)
    }
    fn name() -> &'static str {
        <This as TransformRule>::name()
    }
    fn print_visibility(&self, cx: &PrintContext) -> PrintVisibility {
        TransformRule::print_visibility(self, cx)
    }

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        TransformRule::print_tree(self, cx, f)
    }

    fn pre_parse<Cx: CxType>(
        cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()> {
        Self::update_context(cx, |cx| This::Inner::pre_parse(cx, state, next))
    }

    fn parse<Cx: CxType>(cx: ParseContext<Cx>, next: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        Self::update_context(cx, |cx| This::Inner::parse(cx, next).map(This::from_inner))
    }

    fn matches_empty() -> bool
    where
        Self: Sized,
    {
        This::Inner::matches_empty()
    }
}

pub struct TransformList<T, X: TransformInto<T>> {
    pub items: Vec<T>,
    _x: PhantomData<X>,
}

impl<T: Debug, X: TransformInto<T>> Debug for TransformList<T, X> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.items, f)
    }
}

impl<In: Rule, Out: Rule, X: TransformInto<Out, Input = In> + 'static> Rule
    for TransformList<Out, X>
{
    fn print_name(f: &mut Formatter) -> fmt::Result {
        f.write_str("List(")?;
        In::print_name(f)?;
        f.write_str(")")
    }

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        cx.debug_list(f, self.items.iter().map(|item| item as _))
    }

    fn pre_parse<Cx: CxType>(
        cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()> {
        ListNodePlaceholder::<In>::pre_parse(cx, state, next)
    }

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, next: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        let mut items = Vec::new();
        let item_next = RuleType::new_repeating::<In>(next);
        let discard = cx.should_discard();

        while let Some(item) = Rule::parse(cx.by_ref(), &item_next)? {
            if !discard {
                items.push(X::transform(item));
            }
        }

        Ok(Self {
            items,
            _x: PhantomData,
        })
    }
}

impl<T: Rule> TransformRule for Vec<T> {
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

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Empty;

impl Rule for Empty {
    fn pre_parse<Cx: CxType>(
        cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()> {
        next.pre_parse(cx, state)
    }

    fn parse<Cx: CxType>(_: ParseContext<Cx>, _: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        Ok(Self)
    }

    fn matches_empty() -> bool {
        true
    }
}

impl<T: Rule> TransformRule for Option<T> {
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

impl<T: Rule> TransformRule for Box<T> {
    type Inner = T;

    fn from_inner(inner: Self::Inner) -> Self {
        Box::new(inner)
    }
}

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

    fn matches_empty() -> bool
    where
        Self: Sized,
    {
        T::matches_empty() || U::matches_empty()
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
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()> {
        T::pre_parse(cx.by_ref(), state, next).or_else(|_| U::pre_parse(cx, state, next))
    }

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, next: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        let Err(err1) = cx.pre_parse::<T>(next) else {
            return T::parse(cx, next).map(Either::Left);
        };
        let Err(err2) = cx.pre_parse::<U>(next) else {
            return U::parse(cx, next).map(Either::Right);
        };
        let max_location = err1.location.max(err2.location);

        if err1.location == max_location {
            let _ = cx.record_error::<T>(next);
        }

        if err2.location == max_location {
            let _ = cx.record_error::<U>(next);
        }

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
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()> {
        T::pre_parse(cx, state, &RuleType::new::<U>(next))
    }

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, next: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        Ok((
            T::parse(cx.by_ref(), &RuleType::new::<U>(next))?,
            U::parse(cx, next)?,
        ))
    }

    fn matches_empty() -> bool
    where
        Self: Sized,
    {
        T::matches_empty() && U::matches_empty()
    }
}

impl TransformRule for () {
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

impl<T0: Rule> TransformRule for (T0,) {
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

impl<T0: Rule, T1: Rule, T2: Rule> TransformRule for (T0, T1, T2) {
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

impl<T0: Rule, T1: Rule, T2: Rule, T3: Rule> TransformRule for (T0, T1, T2, T3) {
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

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token<T> {
    pub range: LocationRange,
    _t: PhantomData<T>,
}

impl<T> Copy for Token<T> {}

impl<T> Clone for Token<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: TokenDef> Debug for Token<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(T::name())
    }
}

impl<T: TokenDef> From<Token<T>> for AnyToken {
    fn from(value: Token<T>) -> Self {
        Self {
            token_type: TokenType::of::<T>(),
            range: value.range,
        }
    }
}

impl<T> From<LocationRange> for Token<T> {
    fn from(range: LocationRange) -> Self {
        Self {
            range,
            _t: PhantomData,
        }
    }
}

impl<T: TokenDef> Rule for Token<T> {
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

    fn matches_empty() -> bool {
        false
    }

    fn pre_parse<Cx: CxType>(
        mut cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()> {
        if state.start > state.end {
            return Ok(());
        }

        let ParseContextParts {
            src, look_ahead, ..
        } = cx.as_parts();

        let end = match &mut look_ahead[state.dist..] {
            [] => return Ok(()),
            [Some(token), ..] if token.token_type.token_id() == TypeId::of::<T>() => {
                token.range.end
            }
            [token, ..] => {
                let Some(range) = T::try_lex(src, state.start) else {
                    cx.error_mut()
                        .add_expected(state.start, TokenType::of::<T>());
                    return Err(RuleParseFailed {
                        location: state.start,
                    });
                };
                *token = Some(AnyToken {
                    token_type: TokenType::of::<T>(),
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

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, _: &RuleType<Cx>) -> RuleParseResult<Self>
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
                .add_expected(err.location, TokenType::of::<T>())
        })
    }
}

impl<T: Rule> TransformRule for PhantomData<T> {
    type Inner = T;

    fn print_visibility(&self, _: &PrintContext) -> PrintVisibility {
        PrintVisibility::Never
    }

    fn from_inner(_: Self::Inner) -> Self {
        Self
    }
}

macro_rules! generic_unit {
    ($($vis:vis struct $Name:ident<$($T:ident),* $(,)?>;)*) => {$(
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
            fn hash<H: std::hash::Hasher>(&self, _: &mut H) {}
        }
    )*};
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Accept;

impl Rule for Accept {
    fn name() -> &'static str {
        "Accept"
    }

    fn pre_parse<Cx: CxType>(
        _: ParseContext<Cx>,
        _: PreParseState,
        _: &RuleType<Cx>,
    ) -> RuleParseResult<()> {
        Ok(())
    }

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, _: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        cx.set_location(Location {
            position: cx.src().len(),
        });
        Ok(Self)
    }

    fn matches_empty() -> bool {
        // does match an empty string, but doesn't parse any tokens after this
        false
    }
}

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
        next: &RuleType<Cx>,
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

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, next: &RuleType<Cx>) -> RuleParseResult<Self>
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
            cx => <(Inner, Silent<Token<Eof>>)>::parse(cx, default())?,
        };

        if end > start {
            cx.set_location(end);
        }

        Ok(Self { outer, inner })
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CompoundToken<T> {
    pub value: T,
}

struct CompoundTokenDef<T>(PhantomData<T>);

impl<T: 'static> TokenDef for CompoundTokenDef<T> {
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
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()>
    where
        Self: Sized,
    {
        let end = match cx.look_ahead().get(state.dist).copied() {
            Some(Some(token)) if token.token_type == TokenType::of::<CompoundTokenDef<T>>() => {
                token.range.end
            }
            Some(_) => {
                let (_, end) = cx.isolated_parse::<Discard<T>>(state.start, next)?;
                cx.look_ahead_mut()[state.dist] = Some(AnyToken {
                    token_type: TokenType::of::<CompoundTokenDef<T>>(),
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

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, next: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        let location = cx.location();
        Ok(match cx.look_ahead().first().copied().flatten() {
            Some(token) if token.token_type != TokenType::of::<CompoundTokenDef<T>>() => {
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

/// Ignore the lookahead buffer altogether and just try parsing it to see if it matches.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Backtrack<T> {
    pub value: T,
}

impl<T: Rule> Rule for Backtrack<T> {
    fn pre_parse<Cx: CxType>(
        mut cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleType<Cx>,
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

    fn parse<Cx: CxType>(cx: ParseContext<Cx>, next: &RuleType<Cx>) -> RuleParseResult<Self>
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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Silent<T> {
    pub value: T,
}

impl<T: Rule> TransformRule for Silent<T> {
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
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()>
    where
        Self: Sized,
    {
        next.pre_parse(cx, state)
    }

    fn parse<Cx: CxType>(cx: ParseContext<Cx>, _: &RuleType<Cx>) -> RuleParseResult<Self>
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
        next: &RuleType<Cx>,
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

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, _: &RuleType<Cx>) -> RuleParseResult<Self>
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
}

generic_unit!(
    pub struct Discard<T>;
    pub struct Ignore<T>;
);

impl<T: Rule> TransformRule for Discard<T> {
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

impl<T: Rule> TransformRule for Ignore<T> {
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

#[derive(Debug)]
pub struct ListNodePlaceholder<T>(PhantomData<T>);

impl<T: Rule> TransformRule for ListNodePlaceholder<T> {
    type Inner = Option<(T, Option<ListNodePlaceholder<T>>)>;

    fn print_name(f: &mut Formatter) -> fmt::Result {
        f.write_str("ListNodePlaceholder(")?;
        T::print_name(f)?;
        f.write_str(")")
    }

    fn from_inner(_: Self::Inner) -> Self {
        Self(PhantomData)
    }
}

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
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()>
    where
        Self: Sized,
    {
        <(T, After)>::pre_parse(cx, state, next)
    }

    fn parse<Cx: CxType>(cx: ParseContext<Cx>, next: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        T::parse(cx, &RuleType::new::<After>(next)).map(Self::new)
    }
}

pub struct Transformed<T, X> {
    pub value: T,
    _x: PhantomData<X>,
}

impl<T, X> Debug for Transformed<T, X> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Transformed").finish_non_exhaustive()
    }
}

impl<In: Rule, Out: 'static, X: TransformInto<Out, Input = In> + 'static> TransformRule
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

type DelimitedListPrototypeTail<T, Delim, Trailing> = (ListNodePlaceholder<(Delim, T)>, Trailing);

type DelimitedListPrototype<T, Delim, Trailing> =
    Option<(T, DelimitedListPrototypeTail<T, Delim, Trailing>)>;

#[derive(Debug)]
struct DelimitedListTailTrailing<T, Delim> {
    value: Option<T>,
    _delim: PhantomData<Delim>,
}

impl<T: Rule, Delim: Rule> TransformRule for DelimitedListTailTrailing<T, Delim> {
    type Inner = Option<(Discard<Delim>, Option<Partial<T, Self>>)>;

    fn from_inner(inner: Self::Inner) -> Self {
        let value = match inner {
            Some((_, Some(Partial { value, .. }))) => Some(value),
            None | Some((_, None)) => None,
        };

        Self {
            value,
            _delim: PhantomData,
        }
    }
}

#[derive(Debug)]
struct DelimitedListTail<T, Delim> {
    value: Option<T>,
    _delim: PhantomData<Delim>,
}

impl<T: Rule, Delim: Rule> TransformRule for DelimitedListTail<T, Delim> {
    type Inner = Option<(Discard<Delim>, Partial<T, Self>)>;

    fn from_inner(inner: Self::Inner) -> Self {
        let value = match inner {
            Some((_, Partial { value, .. })) => Some(value),
            None => None,
        };

        Self {
            value,
            _delim: PhantomData,
        }
    }
}

#[derive(Debug)]
pub struct DelimitedList<T, Delim, const TRAIL: bool = true> {
    pub items: Vec<T>,
    _delim: PhantomData<Delim>,
}

impl<T, Delim, const TRAIL: bool> DelimitedList<T, Delim, TRAIL> {
    pub fn new(items: Vec<T>) -> Self {
        Self {
            items,
            _delim: PhantomData,
        }
    }
}

impl<T: Rule, Delim: Rule, const TRAIL: bool> Rule for DelimitedList<T, Delim, TRAIL> {
    fn print_name(f: &mut Formatter) -> fmt::Result {
        f.debug_struct("DelimitedList")
            .field("T", &DebugFn(T::print_name))
            .field("Delim", &DebugFn(Delim::print_name))
            .finish()
    }

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        cx.debug_list(f, self.items.iter().map(|item| item as _))
    }

    fn pre_parse<Cx: CxType>(
        cx: ParseContext<Cx>,
        state: PreParseState,
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()>
    where
        Self: Sized,
    {
        if TRAIL {
            DelimitedListPrototype::<T, Delim, Option<Delim>>::pre_parse(cx, state, next)
        } else {
            DelimitedListPrototype::<T, Delim, Empty>::pre_parse(cx, state, next)
        }
    }

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, next: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        let mut out = Vec::new();
        let discard = cx.should_discard();

        if TRAIL {
            let Some(Partial { value: first, .. }) = Option::<
                Partial<T, DelimitedListTailTrailing<T, Delim>>,
            >::parse(cx.by_ref(), next)?
            else {
                return Ok(Self::new(out));
            };

            if !discard {
                out.push(first);
            }

            while let Some(item) =
                DelimitedListTailTrailing::<T, Delim>::parse(cx.by_ref(), next)?.value
            {
                if !discard {
                    out.push(item);
                }
            }
        } else {
            let Some(Partial { value: first, .. }) =
                Option::<Partial<T, DelimitedListTail<T, Delim>>>::parse(cx.by_ref(), next)?
            else {
                return Ok(Self::new(out));
            };

            if !discard {
                out.push(first);
            }

            while let Some(item) = DelimitedListTail::<T, Delim>::parse(cx.by_ref(), next)?.value {
                if !discard {
                    out.push(item);
                }
            }
        }

        Ok(Self::new(out))
    }
}

#[derive(Debug)]
pub struct InfixChainItem<T, Op> {
    op: Op,
    value: T,
}

impl<T: Rule, Op: Rule> TransformRule for InfixChainItem<T, Op> {
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

#[derive(Debug)]
pub struct InfixChain<T, Op> {
    first: T,
    rest: Vec<InfixChainItem<T, Op>>,
}

impl<T: Rule, Op: Rule> TransformRule for InfixChain<T, Op> {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NotParse<Invalid, Valid> {
    _invalid: PhantomData<Invalid>,
    pub value: Valid,
}

impl<Invalid: Rule, Valid: Rule> TransformRule for NotParse<Invalid, Valid> {
    type Inner = Either<CompoundToken<(Discard<Invalid>, Reject)>, Valid>;

    fn from_inner(inner: Self::Inner) -> Self {
        Self {
            value: match inner {
                Either::Left(CompoundToken {
                    value: (_, reject), ..
                }) => match reject {},
                Either::Right(value) => value,
            },
            _invalid: PhantomData,
        }
    }
}

pub fn extract_actual<'src>(src: &'src str, start: usize) -> &'src str {
    if start >= src.len() {
        return "<end-of-file>";
    }

    lazy_static::lazy_static! {
        static ref PSEUDO_TOKEN: Regex = Regex::new(r"\A.+?\b|.").unwrap();
    }

    const MAX_LEN: usize = 32;

    let len = PSEUDO_TOKEN
        .find(&src[start..])
        .map(|m| m.end().min(MAX_LEN))
        .unwrap_or(1);

    &src[start..start + len]
}

pub fn parse_tree<'src, T: Rule, const N: usize>(src: &'src str) -> Result<T, ParseError<'src>> {
    match SizedParseContext::<N>::new_with(src, move |cx| {
        <(T, Token<Eof>)>::parse(cx, &mut default())
    }) {
        (Ok((value, _)), _) => Ok(value),
        (Err(_), mut err) => {
            err.actual = extract_actual(src, err.location.position);
            Err(err)
        }
    }
}
