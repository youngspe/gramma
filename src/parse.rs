use core::{
    cmp::Ordering,
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    ops::{Add, AddAssign, Deref, DerefMut, Index, IndexMut, Range, Sub, SubAssign},
    slice::SliceIndex,
};

use crate::{
    ast::{EmptyParseState, PreParseState, RuleObject, RuleParseResult},
    error::ExpectedParse,
    internal_prelude::*,
    string_matcher::{MatchString, StringMatcher},
    token::AnyToken,
    utils::default,
    Rule,
};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    pub position: usize,
}

impl Location {
    pub const MAX: Self = Location {
        position: usize::MAX,
    };
    pub const MIN: Self = Location {
        position: usize::MIN,
    };
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocationRange {
    pub start: Location,
    pub end: Location,
}

impl LocationRange {
    pub const INVALID: Self = Self {
        start: Location {
            position: usize::MAX,
        },
        end: Location {
            position: usize::MIN,
        },
    };

    pub fn combine(self, other: Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    pub fn slice<'src>(self, src: &'src str) -> &'src str {
        &src[self.start.position..self.end.position]
    }
}

impl Add<usize> for Location {
    type Output = Self;

    fn add(mut self, rhs: usize) -> Self::Output {
        self += rhs;
        self
    }
}

impl AddAssign<usize> for Location {
    fn add_assign(&mut self, rhs: usize) {
        self.position += rhs;
    }
}

impl Add<usize> for LocationRange {
    type Output = Self;

    fn add(mut self, rhs: usize) -> Self::Output {
        self += rhs;
        self
    }
}

impl AddAssign<usize> for LocationRange {
    fn add_assign(&mut self, rhs: usize) {
        self.start += rhs;
        self.end += rhs;
    }
}

impl Sub<usize> for Location {
    type Output = Self;

    fn sub(mut self, rhs: usize) -> Self::Output {
        self -= rhs;
        self
    }
}

impl SubAssign<usize> for Location {
    fn sub_assign(&mut self, rhs: usize) {
        self.position -= rhs;
    }
}

impl Sub<usize> for LocationRange {
    type Output = Self;

    fn sub(mut self, rhs: usize) -> Self::Output {
        self -= rhs;
        self
    }
}

impl SubAssign<usize> for LocationRange {
    fn sub_assign(&mut self, rhs: usize) {
        self.start -= rhs;
        self.end -= rhs;
    }
}

#[cfg(feature = "regex")]
pub fn lex_regex(
    regex: &regex::Regex,
    capture: usize,
    src: &str,
    location: Location,
) -> Option<LocationRange> {
    let src = &src[location.position..];
    let Range { start, end } = if capture == 0 {
        regex.find(src)?.range()
    } else {
        regex.captures(src)?.get(capture)?.range()
    };

    if start == end {
        return None;
    }

    Some(LocationRange {
        start: location + start,
        end: location + end,
    })
}

pub fn lex_exact(pattern: &str, src: &str, location: Location) -> Option<LocationRange> {
    src[location.position..]
        .starts_with(pattern)
        .then_some(LocationRange {
            start: location,
            end: location + pattern.len(),
        })
}

pub fn lex_matcher<'m>(
    matcher: &'m StringMatcher<impl MatchString<'m>>,
    src: &str,
    location: Location,
) -> Option<LocationRange> {
    matcher
        .match_string(location.position, src)
        .filter(|Range { start, end }| start != end)
        .map(|r| LocationRange {
            start: Location { position: r.start },
            end: Location { position: r.end },
        })
}

mod private {
    use super::*;

    pub trait ContextType: Debug {
        type LookAhead: TokenBufData;
        fn child(&self) -> Self;
    }
}

#[derive(Debug)]
pub(crate) struct CxTypeImpl<const LA: usize> {}

impl<const LA: usize> private::ContextType for CxTypeImpl<LA> {
    type LookAhead = [Option<AnyToken>; LA];

    fn child(&self) -> Self {
        Self {}
    }
}

pub trait CxType: private::ContextType {}

impl<Cx: private::ContextType> CxType for Cx {}

#[derive(Debug)]
pub struct ParseContext<'src, 'cx, Cx: CxType> {
    src: &'src str,
    error: &'cx mut ParseError<'static>,
    location: &'cx mut Location,
    look_ahead: &'cx mut TokenBuf<Cx::LookAhead>,
    discard: bool,
    prefer_continue: bool,
    cx_type: Cx,
    expected_parse: Option<ExpectedParse>,
    _cx_type: PhantomData<&'cx Cx>,
}

#[derive(Debug)]
#[non_exhaustive]
pub struct ParseContextUpdate<'src, 'cx, Cx: CxType> {
    pub src: Option<&'src str>,
    pub location: Option<&'cx mut Location>,
    pub error: Option<&'cx mut ParseError<'static>>,
    pub look_ahead: Option<&'cx mut TokenBuf<Cx::LookAhead>>,
    pub prefer_continue: Option<bool>,
    pub discard: Option<bool>,
    pub expected_parse: Option<Option<ExpectedParse>>,
}

impl<Cx: CxType> Default for ParseContextUpdate<'_, '_, Cx> {
    fn default() -> Self {
        Self {
            src: None,
            location: None,
            error: None,
            look_ahead: None,
            prefer_continue: None,
            discard: None,
            expected_parse: None,
        }
    }
}

pub(crate) type SizedParseContext<'src, 'cx, const LA: usize> =
    ParseContext<'src, 'cx, CxTypeImpl<LA>>;

impl<'src, const LA: usize> SizedParseContext<'src, 'static, LA> {
    pub fn new_with<R>(
        src: &'src str,
        f: impl FnOnce(SizedParseContext<'src, '_, LA>) -> R,
    ) -> (R, ParseError<'src>) {
        let cx_type = CxTypeImpl::<LA> {};
        let mut error = default();

        let ret = f(ParseContext {
            src,
            error: &mut error,
            location: &mut Location { position: 0 },
            discard: false,
            look_ahead: &mut default(),
            prefer_continue: true,
            expected_parse: None,
            cx_type,
            _cx_type: PhantomData,
        });

        (ret, error)
    }
}

impl<'src, 'cx, Cx: CxType> ParseContext<'src, 'cx, Cx> {
    pub fn by_ref<'this>(&'this mut self) -> ParseContext<'src, 'this, Cx> {
        let ParseContext {
            src,
            error,
            location,
            discard,
            look_ahead,
            prefer_continue,
            expected_parse,
            cx_type,
            ..
        } = self;
        ParseContext {
            src,
            error,
            location,
            discard: *discard,
            look_ahead,
            prefer_continue: *prefer_continue,
            cx_type: cx_type.child(),
            expected_parse: *expected_parse,
            _cx_type: PhantomData,
        }
    }

    pub fn should_discard(&self) -> bool {
        self.discard
    }

    pub fn prefer_continue(&self) -> bool {
        self.prefer_continue
    }

    pub fn update<'src2, 'cx2>(
        self,
        update: ParseContextUpdate<'src2, 'cx2, Cx>,
    ) -> ParseContext<'src2, 'cx2, Cx>
    where
        'src: 'src2,
        'cx: 'cx2,
    {
        let ParseContextUpdate {
            src,
            location,
            error,
            look_ahead,
            discard,
            prefer_continue,
            expected_parse,
        } = update;

        ParseContext {
            src: src.unwrap_or(self.src),
            error: error.unwrap_or(self.error),
            location: location.unwrap_or(self.location),
            look_ahead: look_ahead.unwrap_or(self.look_ahead),
            discard: discard.unwrap_or(self.discard),
            prefer_continue: prefer_continue.unwrap_or(self.prefer_continue),
            expected_parse: expected_parse.unwrap_or(self.expected_parse),
            ..self
        }
    }

    pub fn discarding(self) -> Self {
        self.update(ParseContextUpdate {
            discard: Some(true),
            ..default()
        })
    }

    pub fn expecting(mut self, expected_parse: impl Into<Option<ExpectedParse>>) -> Self {
        self.expected_parse = match (self.expected_parse, expected_parse.into()) {
            (None, expected_parse) | (expected_parse @ Some(_), Some(_)) => expected_parse,
            (_, None) => None,
        };
        self
    }

    pub fn src(&self) -> &'src str {
        self.src
    }

    pub fn error_mut(&mut self) -> &mut ParseError<'static> {
        &mut self.error
    }

    pub fn location(&self) -> Location {
        *self.location
    }

    pub fn set_location(&mut self, location: Location) {
        (*self.location) = location;
    }
    pub fn advance(&mut self) {
        if let Some(token) = self.look_ahead.shift() {
            self.set_location(token.range.end)
        }
    }

    pub fn look_ahead(&self) -> &TokenBuf<Cx::LookAhead> {
        &self.look_ahead
    }

    pub fn look_ahead_mut(&mut self) -> &mut TokenBuf<Cx::LookAhead> {
        &mut self.look_ahead
    }

    pub fn into_parts(self) -> ParseContextParts<'src, 'cx> {
        let Self {
            src,
            location,
            error,
            look_ahead,
            expected_parse,
            ..
        } = self;
        ParseContextParts {
            src,
            location: *location,
            error,
            look_ahead,
            expected_parse,
        }
    }

    pub fn as_parts(&mut self) -> ParseContextParts<'src, '_> {
        self.by_ref().into_parts()
    }

    fn pre_parse_inner<'next, T: Rule>(self, next: Option<&RuleObject<Cx>>) -> RuleParseResult<()>
    where
        Cx: 'next,
    {
        let start = self.location();
        let end = Location {
            position: self.src.len(),
        };
        T::pre_parse(
            self.update(ParseContextUpdate {
                discard: Some(true),
                ..default()
            }),
            PreParseState {
                start,
                end,
                dist: 0,
                empty_state: EmptyParseState {
                    location: start,
                    dist: 0,
                    count: 0,
                },
            },
            next.unwrap_or_default(),
        )
    }

    pub fn pre_parse<'next, T: Rule>(
        &mut self,
        next: impl Into<Option<&'next RuleObject<'next, Cx>>> + 'next,
    ) -> RuleParseResult<()>
    where
        Cx: 'next,
    {
        self.by_ref()
            .update(ParseContextUpdate {
                error: Some(&mut ParseError {
                    location: Location::MAX,
                    ..default()
                }),
                ..default()
            })
            .pre_parse_inner::<T>(next.into())
    }

    pub fn record_error<'next, T: Rule>(
        &mut self,
        next: impl Into<Option<&'next RuleObject<'next, Cx>>> + 'next,
    ) -> RuleParseResult<()>
    where
        Cx: 'next,
    {
        self.by_ref().pre_parse_inner::<T>(next.into())
    }

    pub(crate) fn isolated_parse<T: Rule>(
        &mut self,
        start: impl Into<Option<Location>>,
        next: &RuleObject<Cx>,
    ) -> RuleParseResult<(T, Location)> {
        let mut start = start.into().unwrap_or(*self.location);

        let mut look_ahead = if start == *self.location {
            self.look_ahead.clone()
        } else {
            default()
        };

        let out = T::parse(
            self.by_ref().update(ParseContextUpdate {
                look_ahead: Some(&mut look_ahead),
                location: Some(&mut start),
                ..default()
            }),
            next,
        )?;

        Ok((out, start))
    }

    pub fn error_at(&mut self, location: Location) {
        match self.expected_parse {
            Some(ex) => self.error.add_expected(location, ex),
            None => self.error.set_location(location),
        }
    }
}

#[non_exhaustive]
pub struct ParseContextParts<'src, 'cx> {
    pub src: &'src str,
    pub location: Location,
    pub error: &'cx mut ParseError<'static>,
    pub look_ahead: &'cx mut [Option<AnyToken>],
    pub expected_parse: Option<ExpectedParse>,
}

pub trait TokenBufData:
    Debug + Copy + AsRef<[Option<AnyToken>]> + AsMut<[Option<AnyToken>]> + Ord + Hash + 'static
{
    fn init_data() -> Self;
}

impl<const LEN: usize> TokenBufData for [Option<AnyToken>; LEN] {
    fn init_data() -> Self {
        [None; LEN]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TokenBuf<A: TokenBufData> {
    data: A,
}

impl<A: TokenBufData> TokenBuf<A> {
    pub fn shift(&mut self) -> Option<AnyToken> {
        let token = self.first_mut()?.take();
        self.rotate_left(1);
        token
    }
}
impl<A: TokenBufData> Default for TokenBuf<A> {
    fn default() -> Self {
        Self {
            data: A::init_data(),
        }
    }
}

impl<A: TokenBufData> Deref for TokenBuf<A> {
    type Target = [Option<AnyToken>];

    fn deref(&self) -> &Self::Target {
        self.data.as_ref()
    }
}

impl<A: TokenBufData> DerefMut for TokenBuf<A> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.data.as_mut()
    }
}

impl<A: TokenBufData, S: SliceIndex<[Option<AnyToken>]>> Index<S> for TokenBuf<A> {
    type Output = S::Output;

    fn index(&self, index: S) -> &Self::Output {
        self.data.as_ref().index(index)
    }
}

impl<A: TokenBufData, S: SliceIndex<[Option<AnyToken>]>> IndexMut<S> for TokenBuf<A> {
    fn index_mut(&mut self, index: S) -> &mut Self::Output {
        self.data.as_mut().index_mut(index)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ParseError<'src> {
    pub location: Location,
    pub actual: &'src str,
    pub expected: Vec<ExpectedParse>,
    pub error_rule_location: Option<Location>,
}

impl ParseError<'_> {
    pub fn add_expected(&mut self, location: Location, expected_parse: ExpectedParse) {
        match location.cmp(&self.location) {
            Ordering::Less => return,
            Ordering::Equal => {}
            Ordering::Greater => {
                self.location = location;
                self.expected.clear();
            }
        }

        if let Err(index) = self.expected.binary_search(&expected_parse) {
            self.expected.insert(index, expected_parse)
        }
    }

    pub fn set_location(&mut self, location: Location) {
        match location.cmp(&self.location) {
            Ordering::Less => return,
            Ordering::Equal => {}
            Ordering::Greater => {
                self.location = location;
                self.expected.clear();
            }
        }
    }

    pub fn clear(&mut self) {
        self.expected.clear();
    }

    pub fn expected(&self) -> impl Iterator<Item = ExpectedParse> + '_ {
        self.expected.iter().copied()
    }
}
