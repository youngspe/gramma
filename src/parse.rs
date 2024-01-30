use std::{
    cmp::Ordering,
    fmt::Debug,
    ops::{Add, AddAssign, Range},
};

use regex::Regex;

use crate::{
    token::{AnyToken, TokenType},
    utils::default,
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

pub fn lex_regex(
    regex: &Regex,
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

#[derive(Debug)]
pub struct ParseContext<'src, 'cx, A: ?Sized = [Option<AnyToken>]> {
    pub src: &'src str,
    pub error: &'cx mut ParseError<'src>,
    pub location: &'cx mut Location,
    pub tokens: &'cx mut LookAheadBuf<A>,
}

pub type SizedParseContext<'src, 'cx, const LA: usize> =
    ParseContext<'src, 'cx, [Option<AnyToken>; LA]>;

impl<'src, const LA: usize> SizedParseContext<'src, 'static, LA> {
    pub fn new_with<R>(src: &'src str, f: impl FnOnce(SizedParseContext<'src, '_, LA>) -> R) -> R {
        f(SizedParseContext::<LA> {
            src,
            error: &mut default(),
            location: &mut Location { position: 0 },
            tokens: &mut LookAheadBuf { tokens: [None; LA] },
        })
    }
}

impl<'src, 'cx, const LA: usize> SizedParseContext<'src, 'cx, LA> {
    pub fn unsize(self) -> ParseContext<'src, 'cx> {
        let ParseContext {
            src,
            error,
            location,
            tokens,
        } = self;
        ParseContext {
            src,
            error,
            location,
            tokens: tokens as _,
        }
    }
}

impl<'src, 'cx, A: ?Sized> ParseContext<'src, 'cx, A> {
    pub fn by_ref<'this>(&'this mut self) -> ParseContext<'src, 'this, A> {
        let ParseContext {
            src,
            error,
            location,
            tokens,
        } = self;
        ParseContext {
            src,
            error,
            location,
            tokens,
        }
    }

    pub fn error_mut(&mut self) -> &mut ParseError<'src> {
        &mut self.error
    }

    pub fn location(&self) -> Location {
        *self.location
    }
}
impl<'src, 'cx, A: TokenBuf + ?Sized> ParseContext<'src, 'cx, A> {
    pub fn set_location(&mut self, location: Location) {
        (*self.location) = location;
        if *self.location > self.error.location {
            self.error.location = *self.location;
            self.error.clear();
        }
    }
    pub fn advance(&mut self) {
        if let Some(token) = self.tokens.shift() {
            self.set_location(token.range.end)
        }
    }
}

pub trait TokenBuf:
    Debug + AsRef<[Option<AnyToken>]> + AsMut<[Option<AnyToken>]> + 'static
{
}

impl<This: ?Sized + Debug + AsRef<[Option<AnyToken>]> + AsMut<[Option<AnyToken>]> + 'static>
    TokenBuf for This
{
}

#[derive(Debug)]
pub struct LookAheadBuf<A: ?Sized = [Option<AnyToken>]> {
    tokens: A,
}

impl<A: TokenBuf + ?Sized> LookAheadBuf<A> {
    pub(crate) fn new(tokens: A) -> Self
    where
        A: Sized,
    {
        Self { tokens }
    }

    pub fn shift(&mut self) -> Option<AnyToken> {
        let mut out = None;
        let slice = self.tokens.as_mut();
        if let Some(t) = slice.first_mut() {
            out = t.take();
            slice.rotate_left(1);
        }
        out
    }

    pub fn slice(&self) -> &[Option<AnyToken>] {
        self.tokens.as_ref()
    }

    pub fn slice_mut(&mut self) -> &mut [Option<AnyToken>] {
        self.tokens.as_mut()
    }
}

#[derive(Debug, Default, Clone)]
pub struct ParseError<'src> {
    pub location: Location,
    pub actual: &'src str,
    pub expected: Vec<&'static TokenType>,
}

impl ParseError<'_> {
    pub fn add_expected(&mut self, location: Location, token_type: &'static TokenType) {
        match location.cmp(&self.location) {
            Ordering::Less => return,
            Ordering::Equal => {}
            Ordering::Greater => {
                self.location = location;
                self.expected.clear();
            }
        }
        if !self.expected.contains(&token_type) {
            self.expected.push(token_type);
        }
    }

    pub fn clear(&mut self) {
        self.expected.clear();
    }

    pub fn expected(&self) -> impl Iterator<Item = &'static TokenType> + '_ {
        self.expected.iter().copied()
    }
}
