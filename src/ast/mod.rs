mod macros;
pub mod print;

use std::{
    any::{Any, TypeId},
    cmp::Ordering,
    convert::Infallible,
    fmt::{self, Debug, Formatter},
    hash::Hash,
    marker::PhantomData,
};

use either::{for_both, Either};
use regex::Regex;

use crate::{
    parse::{
        CxType, Location, LocationRange, ParseContext, ParseContextParts, ParseError,
        SizedParseContext,
    },
    token::{AnyToken, Eof, TokenDef, TokenType},
    utils::{default, simple_name, DebugFn},
};

use self::print::{PrintContext, PrintVisibility};

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

pub trait Rule: Any + Debug {
    fn print_visibility(&self, _: &PrintContext) -> PrintVisibility {
        PrintVisibility::Always
    }

    fn print_tree(&self, _: &PrintContext, f: &mut Formatter) -> fmt::Result {
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
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()>
    where
        Self: Sized;

    fn record_error<Cx: CxType>(
        cx: ParseContext<Cx>,
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) where
        Self: Sized;

    fn parse<Cx: CxType>(cx: ParseContext<Cx>, next: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized;

    fn matches_empty() -> bool
    where
        Self: Sized,
    {
        false
    }
}

#[derive(Clone, Copy)]
pub struct RuleType<'lt, Cx: CxType> {
    name: fn() -> &'static str,
    print_name: fn(&mut Formatter) -> fmt::Result,
    node_id: fn() -> TypeId,
    pre_parse: fn(ParseContext<Cx>, Location, usize, &RuleType<Cx>) -> RuleParseResult<()>,
    record_error: fn(ParseContext<Cx>, Location, usize, &RuleType<Cx>),
    next: Option<&'lt Self>,
}

impl<'lt, Cx: CxType> RuleType<'lt, Cx> {
    pub fn new<T: Rule>(next: impl Into<Option<&'lt Self>>) -> Self {
        RuleType {
            name: T::name,
            print_name: T::print_name,
            node_id: TypeId::of::<T>,
            pre_parse: T::pre_parse::<Cx>,
            record_error: T::record_error,
            next: next.into(),
        }
    }

    pub const fn of<T: Rule>() -> &'lt Self {
        &RuleType::<Cx> {
            name: T::name,
            print_name: T::print_name,
            node_id: TypeId::of::<T>,
            pre_parse: T::pre_parse,
            record_error: T::record_error,
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
    pub fn pre_parse(
        &self,
        cx: ParseContext<Cx>,
        start: Location,
        dist: usize,
    ) -> RuleParseResult<()> {
        if dist >= cx.look_ahead().len() {
            return Ok(());
        }
        (self.pre_parse)(cx, start, dist, self.next.unwrap_or_default())
    }

    #[inline]
    pub fn record_error(&self, mut cx: ParseContext<Cx>, start: Location, dist: usize) {
        if dist >= cx.look_ahead().len() || cx.error_mut().location == Location::MAX {
            return;
        }
        (self.record_error)(cx, start, dist, self.next.unwrap_or_default())
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

impl Rule for Infallible {
    fn pre_parse<Cx: CxType>(
        _: ParseContext<Cx>,
        _: Location,
        _: usize,
        _: &RuleType<Cx>,
    ) -> RuleParseResult<()> {
        Err(default())
    }

    fn parse<Cx: CxType>(_: ParseContext<Cx>, _: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        Err(default())
    }

    fn record_error<Cx: CxType>(_: ParseContext<Cx>, _: Location, _: usize, _: &RuleType<Cx>) {}
}

pub trait TransformRule: Any + Debug {
    type Inner: Rule;

    fn print_name(f: &mut Formatter) -> fmt::Result {
        f.write_str(Self::name())
    }

    fn print_visibility(&self, _: &PrintContext) -> PrintVisibility {
        PrintVisibility::Always
    }

    fn from_inner(inner: Self::Inner) -> Self;

    fn print_tree(&self, _: &PrintContext, f: &mut Formatter) -> fmt::Result {
        Debug::fmt(self, f)
    }

    fn name() -> &'static str {
        simple_name::<Self>()
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
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()> {
        This::Inner::pre_parse(cx, start, dist, next)
    }

    fn parse<Cx: CxType>(cx: ParseContext<Cx>, next: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        This::Inner::parse(cx, next).map(This::from_inner)
    }

    fn matches_empty() -> bool
    where
        Self: Sized,
    {
        This::Inner::matches_empty()
    }

    fn record_error<Cx: CxType>(
        cx: ParseContext<Cx>,
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) {
        This::Inner::record_error(cx, start, dist, next)
    }
}

impl<T: Rule> Rule for Vec<T> {
    fn print_name(f: &mut Formatter) -> fmt::Result {
        f.write_str("Vec(")?;
        T::print_name(f)?;
        f.write_str(")")
    }

    fn print_tree(&self, cx: &PrintContext, f: &mut Formatter) -> fmt::Result {
        cx.debug_list(f, self.iter().map(|item| item as _))
    }

    fn pre_parse<Cx: CxType>(
        cx: ParseContext<Cx>,
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()> {
        ListNodePlaceholder::<T>::pre_parse(cx, start, dist, next)
    }

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, next: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        let mut out = Vec::new();
        let item_next = RuleType::new_repeating::<T>(next);

        // while T::parse_first(cx.src, *cx.location, &item_next, cx.tokens.slice_mut()) {
        //     out.push(T::parse(cx.by_ref(), &item_next)?);
        // }

        while let Some(item) = Rule::parse(cx.by_ref(), &item_next)? {
            out.push(item);
        }

        Ok(out)
    }

    fn record_error<Cx: CxType>(
        cx: ParseContext<Cx>,
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) {
        ListNodePlaceholder::<T>::record_error(cx, start, dist, next)
    }
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Empty;

impl Rule for Empty {
    fn pre_parse<Cx: CxType>(
        cx: ParseContext<Cx>,
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()> {
        next.pre_parse(cx, start, dist)
    }

    fn parse<Cx: CxType>(_: ParseContext<Cx>, _: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        Ok(Self)
    }

    fn record_error<Cx: CxType>(
        cx: ParseContext<Cx>,
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) {
        next.record_error(cx, start, dist)
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

#[derive(Debug, Default)]
pub struct RuleParseFailed {}

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
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()> {
        T::pre_parse(cx.by_ref(), start, dist, next)
            .or_else(|_| U::pre_parse(cx, start, dist, next))
    }

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, next: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        let start = cx.location();
        if T::pre_parse(cx.by_ref(), start, 0, next).is_ok() {
            return T::parse(cx, next).map(Either::Left);
        }
        if U::pre_parse(cx.by_ref(), start, 0, next).is_ok() {
            return U::parse(cx, next).map(Either::Right);
        }
        Self::record_error(cx, start, 0, next);
        Err(default())
    }

    fn record_error<Cx: CxType>(
        mut cx: ParseContext<Cx>,
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) {
        if cx.error_mut().location == Location::MAX {
            return;
        }
        T::record_error(cx.by_ref(), start, dist, next);
        U::record_error(cx, start, dist, next);
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
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()> {
        T::pre_parse(cx, start, dist, &RuleType::new::<U>(next))
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

    fn record_error<Cx: CxType>(
        cx: ParseContext<Cx>,
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) {
        T::record_error(cx, start, dist, next)
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token<T> {
    pub range: LocationRange,
    pub value: Option<T>,
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
        Self { range, value: None }
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

    fn pre_parse<Cx: CxType>(
        mut cx: ParseContext<Cx>,
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()> {
        let ParseContextParts {
            src, look_ahead, ..
        } = cx.as_parts();

        let (end, rest) = match &mut look_ahead[dist..] {
            [] => return Ok(()),
            [Some(token), rest @ ..] if token.token_type.token_id() == TypeId::of::<T>() => {
                (token.range.end, rest)
            }
            [token, rest @ ..] => {
                let Some(range) = T::try_lex(src, start) else {
                    return Err(default());
                };
                *token = Some(AnyToken {
                    token_type: TokenType::of::<T>(),
                    range,
                });
                (range.end, rest)
            }
        };

        match rest {
            [] => Ok(()),
            [..] => next.pre_parse(cx, end, dist + 1),
        }
    }

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, _: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        let location = cx.location();

        let out = (|| {
            if let [Some(token), ..] = **cx.look_ahead() {
                if token.token_type.token_id() != TypeId::of::<T>() {
                    return Err(default());
                }
                cx.advance();
                return Ok(Self {
                    range: token.range,
                    value: None,
                });
            }

            let range = T::try_lex(cx.src(), location).ok_or(default())?;
            cx.set_location(range.end);

            Ok(Self { range, value: None })
        })();

        if out.is_err() {
            cx.error_mut().add_expected(location, TokenType::of::<T>())
        }

        out
    }

    fn record_error<Cx: CxType>(
        mut cx: ParseContext<Cx>,
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) {
        if cx.error_mut().location == Location::MAX {
            return;
        }
        let ParseContextParts {
            src,
            error,
            look_ahead,
            ..
        } = cx.as_parts();
        let end = match &mut look_ahead[dist..] {
            [] => return,
            [Some(token), ..] if token.token_type.token_id() == TypeId::of::<T>() => {
                token.range.end
            }
            [token, ..] => match T::try_lex(src, start) {
                Some(LocationRange { end, .. }) => end,
                None => {
                    error.add_expected(
                        token.map(|t| t.range.start).unwrap_or(start),
                        TokenType::of::<T>(),
                    );
                    return;
                }
            },
        };

        next.record_error(cx, end, dist + 1)
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

#[derive(Debug)]
pub struct Accept {
    pub location: Location,
}

impl Rule for Accept {
    fn name() -> &'static str {
        "Accept"
    }

    fn pre_parse<Cx: CxType>(
        _: ParseContext<Cx>,
        _: Location,
        _: usize,
        _: &RuleType<Cx>,
    ) -> RuleParseResult<()> {
        Ok(())
    }

    fn record_error<Cx: CxType>(_: ParseContext<Cx>, _: Location, _: usize, _: &RuleType<Cx>)
    where
        Self: Sized,
    {
    }

    fn parse<Cx: CxType>(cx: ParseContext<Cx>, _: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        Ok(Self {
            location: cx.location(),
        })
    }
}

pub struct SubTree<T> {
    inner: T,
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
}

impl<T: Rule> Rule for Ignore<T> {
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

    fn pre_parse<Cx: CxType>(
        mut cx: ParseContext<Cx>,
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()>
    where
        Self: Sized,
    {
        let (_, new_start) = cx.isolated_parse::<Discard<Option<T>>>(start, next)?;
        if new_start > start {
            cx.look_ahead_mut()[dist..].fill(None);
        }

        next.pre_parse(cx, new_start, dist)
    }

    fn record_error<Cx: CxType>(
        cx: ParseContext<Cx>,
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) where
        Self: Sized,
    {
        let start = match cx.look_ahead()[dist..] {
            [Some(t), ..] => t.range.start,
            _ => start,
        };
        next.record_error(cx, start, dist)
    }

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, next: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        let (_, location) = cx.isolated_parse::<Discard<Option<T>>>(None, next)?;
        cx.set_location(location);
        Ok(default())
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
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()>
    where
        Self: Sized,
    {
        <(T, After)>::pre_parse(cx, start, dist, next)
    }

    fn record_error<Cx: CxType>(
        cx: ParseContext<Cx>,
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) where
        Self: Sized,
    {
        <(T, After)>::record_error(cx, start, dist, next)
    }

    fn parse<Cx: CxType>(cx: ParseContext<Cx>, next: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        T::parse(cx, &RuleType::new::<After>(next)).map(Self::new)
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
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) -> RuleParseResult<()>
    where
        Self: Sized,
    {
        if TRAIL {
            DelimitedListPrototype::<T, Delim, Option<Delim>>::pre_parse(cx, start, dist, next)
        } else {
            DelimitedListPrototype::<T, Delim, Empty>::pre_parse(cx, start, dist, next)
        }
    }

    fn record_error<Cx: CxType>(
        cx: ParseContext<Cx>,
        start: Location,
        dist: usize,
        next: &RuleType<Cx>,
    ) where
        Self: Sized,
    {
        if TRAIL {
            DelimitedListPrototype::<T, Delim, Option<Delim>>::record_error(cx, start, dist, next)
        } else {
            DelimitedListPrototype::<T, Delim, Empty>::record_error(cx, start, dist, next)
        }
    }

    fn parse<Cx: CxType>(mut cx: ParseContext<Cx>, next: &RuleType<Cx>) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        let mut out = Vec::new();

        if TRAIL {
            let Some(Partial { value: first, .. }) = Option::<
                Partial<T, DelimitedListTailTrailing<T, Delim>>,
            >::parse(cx.by_ref(), next)?
            else {
                return Ok(Self::new(out));
            };

            out.push(first);

            while let Some(item) =
                DelimitedListTailTrailing::<T, Delim>::parse(cx.by_ref(), next)?.value
            {
                out.push(item);
            }
        } else {
            let Some(Partial { value: first, .. }) =
                Option::<Partial<T, DelimitedListTail<T, Delim>>>::parse(cx.by_ref(), next)?
            else {
                return Ok(Self::new(out));
            };

            out.push(first);

            while let Some(item) = DelimitedListTail::<T, Delim>::parse(cx.by_ref(), next)?.value {
                out.push(item);
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
