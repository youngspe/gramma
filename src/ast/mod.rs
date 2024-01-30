mod macros;
pub mod print;

use std::{
    any::{Any, TypeId},
    cmp::Ordering,
    convert::Infallible,
    fmt::{self, Debug, Formatter},
    hash::Hash,
    marker::PhantomData,
    mem,
};

use either::{for_both, Either};
use regex::Regex;

use crate::{
    parse::{
        Location, LocationRange, LookAheadBuf, ParseContext, ParseError, SizedParseContext,
        TokenBuf,
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

    fn parse_first(
        src: &str,
        start: Location,
        next: &RuleType,
        out: &mut [Option<AnyToken>],
    ) -> bool
    where
        Self: Sized;

    fn record_error(
        error: &mut ParseError,
        tokens: &[Option<AnyToken>],
        location: Location,
        next: &RuleType,
    ) where
        Self: Sized;

    fn parse<A: TokenBuf + ?Sized>(cx: ParseContext<A>, next: &RuleType) -> RuleParseResult<Self>
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
pub struct RuleType<'lt> {
    name: fn() -> &'static str,
    print_name: fn(&mut Formatter) -> fmt::Result,
    node_id: fn() -> TypeId,
    parse_first: fn(&str, Location, &RuleType, &mut [Option<AnyToken>]) -> bool,
    record_error: fn(&mut ParseError, &[Option<AnyToken>], Location, &RuleType),
    next: Option<&'lt Self>,
}

impl<'lt> RuleType<'lt> {
    pub fn new<T: Rule>(next: impl Into<Option<&'lt Self>>) -> Self {
        RuleType {
            name: T::name,
            print_name: T::print_name,
            node_id: TypeId::of::<T>,
            parse_first: T::parse_first,
            record_error: T::record_error,
            next: next.into(),
        }
    }

    pub const fn of<T: Rule>() -> &'static Self {
        &RuleType {
            name: T::name,
            print_name: T::print_name,
            node_id: TypeId::of::<T>,
            parse_first: T::parse_first,
            record_error: T::record_error,
            next: None,
        }
    }

    pub fn name(&self) -> &str {
        (self.name)()
    }

    pub fn new_repeating<T: Rule>(next: impl Into<Option<&'lt Self>>) -> Self {
        Self::new::<ListNodePlaceholder<T>>(next)
    }

    pub fn node_id(&self) -> TypeId {
        (self.node_id)()
    }

    pub fn parse_first(&self, src: &str, start: Location, out: &mut [Option<AnyToken>]) -> bool {
        (self.parse_first)(src, start, self.next.unwrap_or_default(), out)
    }

    pub fn record_error(
        &self,
        error: &mut ParseError,
        tokens: &[Option<AnyToken>],
        location: Location,
    ) {
        (self.record_error)(error, tokens, location, self.next.unwrap_or_default())
    }
}

impl Debug for RuleType<'_> {
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

impl<'lt> Default for RuleType<'lt> {
    fn default() -> Self {
        Self::new::<Accept>(None)
    }
}

impl Default for &RuleType<'_> {
    fn default() -> Self {
        RuleType::of::<Accept>()
    }
}

impl Rule for Infallible {
    fn parse_first(_: &str, _: Location, _: &RuleType, _: &mut [Option<AnyToken>]) -> bool {
        false
    }

    fn parse<A: TokenBuf + ?Sized>(_: ParseContext<A>, _: &RuleType) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        Err(default())
    }

    fn record_error(_: &mut ParseError, _: &[Option<AnyToken>], _: Location, _: &RuleType) {}
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

    fn parse_first(
        src: &str,
        start: Location,
        next: &RuleType,
        out: &mut [Option<AnyToken>],
    ) -> bool {
        This::Inner::parse_first(src, start, next, out)
    }

    fn parse<A: TokenBuf + ?Sized>(cx: ParseContext<A>, next: &RuleType) -> RuleParseResult<Self>
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

    fn record_error(
        error: &mut ParseError,
        tokens: &[Option<AnyToken>],
        location: Location,
        next: &RuleType,
    ) {
        This::Inner::record_error(error, tokens, location, next)
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

    fn parse_first(
        src: &str,
        start: Location,
        next: &RuleType,
        out: &mut [Option<AnyToken>],
    ) -> bool {
        ListNodePlaceholder::<T>::parse_first(src, start, next, out)
    }

    fn parse<A: TokenBuf + ?Sized>(
        mut cx: ParseContext<A>,
        next: &RuleType,
    ) -> RuleParseResult<Self>
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

    fn record_error(
        error: &mut ParseError,
        tokens: &[Option<AnyToken>],
        location: Location,
        next: &RuleType,
    ) {
        ListNodePlaceholder::<T>::record_error(error, tokens, location, next)
    }
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Empty;

impl Rule for Empty {
    fn parse_first(
        src: &str,
        start: Location,
        next: &RuleType,
        out: &mut [Option<AnyToken>],
    ) -> bool {
        next.parse_first(src, start, out)
    }

    fn parse<A: TokenBuf + ?Sized>(_: ParseContext<A>, _: &RuleType) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        Ok(Self)
    }

    fn record_error(
        error: &mut ParseError,
        tokens: &[Option<AnyToken>],
        location: Location,
        next: &RuleType,
    ) {
        next.record_error(error, tokens, location)
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

    fn parse_first(
        src: &str,
        start: Location,
        next: &RuleType,
        out: &mut [Option<AnyToken>],
    ) -> bool {
        T::parse_first(src, start, next, out) || U::parse_first(src, start, next, out)
    }

    fn parse<A: TokenBuf + ?Sized>(
        mut cx: ParseContext<A>,
        next: &RuleType,
    ) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        if T::parse_first(cx.src, *cx.location, next, &mut cx.tokens.slice_mut()) {
            return T::parse(cx, next).map(Either::Left);
        }
        if U::parse_first(cx.src, *cx.location, next, &mut cx.tokens.slice_mut()) {
            return U::parse(cx, next).map(Either::Right);
        }
        Self::record_error(&mut cx.error, cx.tokens.slice(), *cx.location, next);
        Err(default())
    }

    fn record_error(
        error: &mut ParseError,
        tokens: &[Option<AnyToken>],
        location: Location,
        next: &RuleType,
    ) {
        T::record_error(error, tokens, location, next);
        U::record_error(error, tokens, location, next);
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

    fn parse_first(
        src: &str,
        start: Location,
        next: &RuleType,
        out: &mut [Option<AnyToken>],
    ) -> bool {
        T::parse_first(src, start, &RuleType::new::<U>(next), out)
    }

    fn parse<A: TokenBuf + ?Sized>(
        mut cx: ParseContext<A>,
        next: &RuleType,
    ) -> RuleParseResult<Self>
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

    fn record_error(
        error: &mut ParseError,
        tokens: &[Option<AnyToken>],
        location: Location,
        next: &RuleType,
    ) {
        T::record_error(error, tokens, location, next)
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

    fn parse_first(
        src: &str,
        start: Location,
        next: &RuleType,
        out: &mut [Option<AnyToken>],
    ) -> bool {
        let (token, rest) = match out {
            [] => return true,
            [Some(token), rest @ ..] if token.token_type.token_id() == TypeId::of::<T>() => {
                (token, rest)
            }
            [token, rest @ ..] => {
                *token = T::try_lex(src, start).map(|range| AnyToken {
                    token_type: TokenType::of::<T>(),
                    range,
                });
                let Some(token) = token else {
                    return false;
                };
                (token, rest)
            }
        };

        match rest {
            [] => true,
            rest => next.parse_first(src, token.range.end, rest),
        }
    }

    fn parse<A: TokenBuf + ?Sized>(mut cx: ParseContext<A>, _: &RuleType) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        let out = (|| {
            let range = if let [Some(token), ..] = *cx.tokens.slice() {
                if token.token_type.token_id() != TypeId::of::<T>() {
                    return Err(default());
                }
                cx.advance();
                token.range
            } else {
                let range = T::try_lex(cx.src, *cx.location).ok_or(default())?;
                cx.set_location(range.end);
                range
            };

            Ok(Self { range, value: None })
        })();

        if out.is_err() {
            cx.error.add_expected(*cx.location, TokenType::of::<T>())
        }

        out
    }

    fn record_error(
        error: &mut ParseError,
        tokens: &[Option<AnyToken>],
        location: Location,
        next: &RuleType,
    ) {
        match tokens {
            [] => {}
            [Some(token), rest @ ..] if token.token_type.token_id() == TypeId::of::<T>() => {
                next.record_error(error, rest, token.range.end)
            }
            [token, ..] => error.add_expected(
                token.map(|t| t.range.start).unwrap_or(location),
                TokenType::of::<T>(),
            ),
        }
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

    fn parse_first(_: &str, _: Location, _: &RuleType, _: &mut [Option<AnyToken>]) -> bool {
        true
    }

    fn record_error(_: &mut ParseError, _: &[Option<AnyToken>], _: Location, _: &RuleType)
    where
        Self: Sized,
    {
    }

    fn parse<A: TokenBuf + ?Sized>(cx: ParseContext<A>, _: &RuleType) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        Ok(Self {
            location: cx.location(),
        })
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
}

impl<T: Rule> Ignore<T> {
    fn isolated_parse(
        src: &str,
        mut start: Location,
        next: &RuleType,
    ) -> RuleParseResult<Location> {
        Option::<T>::parse(
            ParseContext {
                src,
                location: &mut start,
                error: &mut ParseError {
                    location: Location::MAX,
                    ..default()
                },
                tokens: &mut LookAheadBuf::new([None]),
            },
            next,
        )?;
        Ok(start)
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

    fn parse_first(
        src: &str,
        mut start: Location,
        next: &RuleType,
        out: &mut [Option<AnyToken>],
    ) -> bool
    where
        Self: Sized,
    {
        if next.parse_first(src, start, out) {
            return true;
        }

        if let Ok(new_start) = Self::isolated_parse(src, start, next) {
            if new_start > start {
                out.fill(None);
            }
            start = new_start
        }

        next.parse_first(src, start, out)
    }

    fn record_error(
        error: &mut ParseError,
        tokens: &[Option<AnyToken>],
        location: Location,
        next: &RuleType,
    ) where
        Self: Sized,
    {
        let start = match tokens {
            [Some(t), ..] => t.range.start,
            _ => location,
        };
        next.record_error(error, tokens, start)
    }

    fn parse<A: TokenBuf + ?Sized>(
        mut cx: ParseContext<A>,
        next: &RuleType,
    ) -> RuleParseResult<Self>
    where
        Self: Sized,
    {
        let location = Self::isolated_parse(cx.src, cx.location(), next).expect("foo");
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
    fn parse_first(
        src: &str,
        start: Location,
        next: &RuleType,
        out: &mut [Option<AnyToken>],
    ) -> bool
    where
        Self: Sized,
    {
        <(T, After)>::parse_first(src, start, next, out)
    }

    fn record_error(
        error: &mut ParseError,
        tokens: &[Option<AnyToken>],
        location: Location,
        next: &RuleType,
    ) where
        Self: Sized,
    {
        <(T, After)>::record_error(error, tokens, location, next)
    }

    fn parse<A: TokenBuf + ?Sized>(cx: ParseContext<A>, next: &RuleType) -> RuleParseResult<Self>
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

    fn parse_first(
        src: &str,
        start: Location,
        next: &RuleType,
        out: &mut [Option<AnyToken>],
    ) -> bool
    where
        Self: Sized,
    {
        if TRAIL {
            DelimitedListPrototype::<T, Delim, Option<Delim>>::parse_first(src, start, next, out)
        } else {
            DelimitedListPrototype::<T, Delim, Empty>::parse_first(src, start, next, out)
        }
    }

    fn record_error(
        error: &mut ParseError,
        tokens: &[Option<AnyToken>],
        location: Location,
        next: &RuleType,
    ) where
        Self: Sized,
    {
        if TRAIL {
            DelimitedListPrototype::<T, Delim, Option<Delim>>::record_error(
                error, tokens, location, next,
            )
        } else {
            DelimitedListPrototype::<T, Delim, Empty>::record_error(error, tokens, location, next)
        }
    }

    fn parse<A: TokenBuf + ?Sized>(
        mut cx: ParseContext<A>,
        next: &RuleType,
    ) -> RuleParseResult<Self>
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
    let mut error = ParseError::default();

    let mut cx = SizedParseContext {
        src,
        error: &mut error,
        location: &mut Location { position: 0 },
        tokens: &mut LookAheadBuf::new([None; N]),
    };

    let mut out = {
        <(T, Token<Eof>)>::parse(cx.by_ref(), &mut default())
            .map(|(value, _)| value)
            .map_err(|_| mem::take(cx.error))
    };

    if let Err(err) = &mut out {
        err.actual = extract_actual(src, err.location.position);
    }

    out
}
