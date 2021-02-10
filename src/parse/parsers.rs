use crate::lex::BasicToken;

use super::Ast;
use super::{ParseError, ParseResult as PResult};

fn map_parser<X, Y>(f: impl FnOnce(X) -> Y, x: PResult<(X, usize)>) -> PResult<(Y, usize)> {
    let (inner, end) = x?;
    Ok((f(inner), end))
}

/// Object representing one possible abstract syntax tree of type `Out`.
///
/// If accepted, the object will be consumed and return `Out`.
/// If rejected, it will keep trying to parse the AST until there are no remaining ways to parse it.
pub trait Parse<Out: Ast>: Sized {
    /// Consumes the parser and returns the parsing result.
    fn accept(self) -> Out;
    /// Attempts to find another way to parse `Out`.
    ///
    /// `reason` is the error that caused the current result not to be accepted.
    ///
    /// If there is another way to parse `Out`, this returns a tuple of (`Self`, usize)
    /// respectively containing the new parser and the index to continue parsing after.
    /// If `Out` cannot be parsed, this will return `Err(ParseError)` which may or may not be based
    /// on `reason`.
    fn reject(self, reason: ParseError) -> PResult<(Self, usize)>;
}

/// An object used in the definition of a grammar that can be used to parse an AST.
/// Sometimes, but not always, this will parse itself.
pub trait BeginParse<'a, T> {
    type Parser: Parse<Self::Output>;
    type Output: Ast + Sized;
    fn begin_parse(tokens: &'a [T], index: usize) -> PResult<(Self::Parser, usize)>;
}

/// An AST that can be parsed from a list of tokens.
pub trait Parseable<T>: Ast + Sized {
    fn parse(tokens: &[T]) -> PResult<Self>;
}

/// Parser that places the result of parser `P` in a Box.
pub struct BoxParser<P>(pub Box<P>);
// Parser that can parse any value that implements `From` for a type that can be parsed by `P`.
pub struct ConvertParser<'a, T, P: BeginParse<'a, T> + ?Sized>(pub P::Parser);
/// Parser that can be accepted exactly once, reads no tokens, and yields an empty `Vec`.
pub struct EmptyListParser;
/// Parser that can be accepted exactly once, reads no tokens, and yields an `Empty`.
pub struct EmptyParser;
/// Parser that can parse one of two types and places the result in an `Enum2`.
pub enum Enum2Parser<'a, T, A: BeginParse<'a, T> + ?Sized, B: BeginParse<'a, T> + ?Sized> {
    A {
        parser: A::Parser,
        tokens: &'a [T],
        index: usize,
    },
    B {
        parser: B::Parser,
        err: ParseError,
    },
}

/// Parser that accepts `Item` followed by either `Sep` or `Trail`.
type ListItemParser<'a, T, Item, Sep, Trail> =
    <super::Seq2Def<Item, super::Enum2Def<Sep, Trail>> as BeginParse<'a, T>>::Parser;

/// Parser that accepts any list of one or more `Item`s, separated by `Sep`, ending with `Trail`.
pub struct NonEmptyListParser<
    'a,
    T,
    Item: BeginParse<'a, T> + ?Sized + 'a,
    Sep: BeginParse<'a, T> + ?Sized + 'a,
    Trail: BeginParse<'a, T> + ?Sized + 'a,
> {
    parsers: Vec<ListItemParser<'a, T, Item, Sep, Trail>>,
    tokens: &'a [T],
}

/// Parser that accepts either `O` or `Empty`, preferring `O`.
pub struct OptionParser<'a, T, O: BeginParse<'a, T> + ?Sized>(Enum2Parser<'a, T, O, super::Empty>);

/// Parser that accepts `A` followed by `B`.
pub struct Seq2Parser<'a, T, A: BeginParse<'a, T> + ?Sized, B: BeginParse<'a, T> + ?Sized> {
    tokens: &'a [T],
    a_parser: A::Parser,
    b_parser: B::Parser,
}

/// Parser that accepts exactly once if the given token can be converted to the one we're trying to parse.
pub struct TokenParser<T> {
    pub(crate) index: usize,
    pub(crate) token: T,
}

/// Parser that matches either `P1` or `P2`, converting the result into the desired AST.
pub struct Union2Parser<'a, T, P1: BeginParse<'a, T> + ?Sized, P2: BeginParse<'a, T> + ?Sized>(
    Enum2Parser<'a, T, P1, P2>,
);

impl<P> BoxParser<P> {
    pub fn new(inner: PResult<(P, usize)>) -> PResult<(Self, usize)> {
        map_parser(Self::from_inner, inner)
    }

    fn from_inner(inner: P) -> Self {
        Self(Box::new(inner))
    }
}

impl<'a, O: Ast, P: Parse<O>> Parse<Box<O>> for BoxParser<P> {
    fn accept(self) -> Box<O> {
        Box::new(self.0.accept())
    }

    fn reject(self, reason: ParseError) -> PResult<(Self, usize)> {
        map_parser(Self::from_inner, self.0.reject(reason))
    }
}

impl<'a, T, P: BeginParse<'a, T> + ?Sized> ConvertParser<'a, T, P> {
    pub fn new(tokens: &'a [T], index: usize) -> PResult<(Self, usize)> {
        map_parser(Self, P::begin_parse(tokens, index))
    }
}

impl<'a, T, P: BeginParse<'a, T> + ?Sized, Out: Ast> Parse<Out> for ConvertParser<'a, T, P>
where
    P::Output: Into<Out>,
{
    fn accept(self) -> Out {
        self.0.accept().into()
    }

    fn reject(self, reason: ParseError) -> PResult<(Self, usize)> {
        map_parser(Self, self.0.reject(reason))
    }
}

impl<Out: Ast> Parse<Vec<Out>> for EmptyListParser {
    fn accept(self) -> Vec<Out> {
        Vec::new()
    }

    fn reject(self, reason: ParseError) -> PResult<(Self, usize)> {
        Err(reason)
    }
}

impl Parse<super::Empty> for EmptyParser {
    fn accept(self) -> super::Empty {
        super::Empty
    }

    fn reject(self, reason: ParseError) -> PResult<(Self, usize)> {
        Err(reason)
    }
}

impl<'a, T, A: BeginParse<'a, T> + ?Sized, B: BeginParse<'a, T> + ?Sized> Enum2Parser<'a, T, A, B> {
    pub fn new(tokens: &'a [T], index: usize) -> PResult<(Self, usize)> {
        Self::parse_a(tokens, index, A::begin_parse(tokens, index))
    }

    fn parse_a(
        tokens: &'a [T],
        index: usize,
        a_parser: PResult<(A::Parser, usize)>,
    ) -> PResult<(Self, usize)> {
        match a_parser {
            Ok((parser, end)) => Ok((
                Self::A {
                    parser,
                    tokens,
                    index,
                },
                end,
            )),
            Err(err) => Self::parse_b(B::begin_parse(tokens, index), err),
        }
    }

    fn parse_b(
        b_parser: PResult<(B::Parser, usize)>,
        old_err: ParseError,
    ) -> PResult<(Self, usize)> {
        match b_parser {
            Ok((parser, end)) => Ok((
                Self::B {
                    parser,
                    err: old_err,
                },
                end,
            )),
            Err(err) => Err(old_err.combine(err)),
        }
    }
}

impl<'a, T, A: BeginParse<'a, T> + ?Sized, B: BeginParse<'a, T> + ?Sized>
    Parse<super::Enum2<A::Output, B::Output>> for Enum2Parser<'a, T, A, B>
{
    fn accept(self) -> super::Enum2<A::Output, B::Output> {
        match self {
            Self::A { parser, .. } => super::Enum2::A(parser.accept()),
            Self::B { parser, .. } => super::Enum2::B(parser.accept()),
        }
    }

    fn reject(self, reason: ParseError) -> PResult<(Self, usize)> {
        match self {
            Self::A {
                parser,
                tokens,
                index,
            } => Self::parse_a(tokens, index, parser.reject(reason)),
            Self::B { parser, err } => Self::parse_b(parser.reject(reason), err),
        }
    }
}

impl<
        'a,
        T,
        Item: BeginParse<'a, T> + ?Sized,
        Sep: BeginParse<'a, T> + ?Sized,
        Trail: BeginParse<'a, T> + ?Sized,
    > NonEmptyListParser<'a, T, Item, Sep, Trail>
{
    pub fn new(tokens: &'a [T], index: usize) -> PResult<(Self, usize)> {
        Self {
            tokens,
            parsers: Vec::new(),
        }
        .expand(super::Seq2::begin_parse(tokens, index))
    }

    fn expand(
        mut self,
        new_parser: PResult<(ListItemParser<'a, T, Item, Sep, Trail>, usize)>,
    ) -> PResult<(Self, usize)> {
        match new_parser {
            Ok((parser, end)) => {
                let recurse = matches!(parser.b_parser, Enum2Parser::A { .. });
                self.parsers.push(parser);

                if recurse {
                    let tokens = self.tokens;
                    self.expand(super::Seq2::begin_parse(tokens, end))
                } else {
                    Ok((self, end))
                }
            }
            Err(err) => self.reject(err),
        }
    }
}

impl<
        'a,
        T,
        Item: BeginParse<'a, T> + ?Sized,
        Sep: BeginParse<'a, T> + ?Sized,
        Trail: BeginParse<'a, T> + ?Sized,
    > Parse<Vec<Item::Output>> for NonEmptyListParser<'a, T, Item, Sep, Trail>
{
    fn accept(self) -> Vec<Item::Output> {
        self.parsers
            .into_iter()
            .map(|x| x.a_parser.accept())
            .collect()
    }

    fn reject(mut self, reason: ParseError) -> PResult<(Self, usize)> {
        match self.parsers.pop() {
            Some(p) => self.expand(p.reject(reason)),
            None => Err(reason),
        }
    }
}

impl<'a, T, O: BeginParse<'a, T> + 'a> OptionParser<'a, T, O> {
    pub fn new(tokens: &'a [T], index: usize) -> PResult<(Self, usize)> {
        map_parser(
            Self,
            super::Enum2::<O, super::Empty>::begin_parse(tokens, index),
        )
    }
}

impl<'a, T, O: BeginParse<'a, T> + ?Sized> Parse<Option<O::Output>> for OptionParser<'a, T, O> {
    fn accept(self) -> Option<O::Output> {
        match self.0.accept() {
            super::Enum2::A(x) => Some(x),
            super::Enum2::B(_) => None,
        }
    }

    fn reject(self, reason: ParseError) -> PResult<(Self, usize)> {
        map_parser(Self, self.0.reject(reason))
    }
}

impl<'a, T, A: BeginParse<'a, T> + ?Sized, B: BeginParse<'a, T> + ?Sized> Seq2Parser<'a, T, A, B> {
    pub fn new(tokens: &'a [T], index: usize) -> PResult<(Self, usize)> {
        Self::with_a_parser(tokens, A::begin_parse(tokens, index))
    }

    fn with_a_parser(
        tokens: &'a [T],
        a_parser: PResult<(A::Parser, usize)>,
    ) -> PResult<(Self, usize)> {
        // parse A if possible:
        let (a_parser, a_end) = a_parser?;
        // starting from the end of this parsing of A, try to parse B as well.
        Self::with_b_parser(tokens, a_parser, B::begin_parse(tokens, a_end))
    }

    fn with_b_parser(
        tokens: &'a [T],
        a_parser: A::Parser,
        b_parser: PResult<(B::Parser, usize)>,
    ) -> PResult<(Self, usize)> {
        match b_parser {
            // We succeeded in finding another way to parse B.
            // The returned instance now represents the existing parsing of A followed by the new
            // parsing of B.
            Ok((b_parser, end)) => Ok((
                Self {
                    tokens,
                    a_parser,
                    b_parser,
                },
                end,
            )),
            // There was no other way to parse B.
            // Pass the error from `b_parser` along as the reason for rejecting the result of `a_parser`.
            Err(err) => Self::with_a_parser(tokens, a_parser.reject(err)),
        }
    }
}

impl<'a, T, A: BeginParse<'a, T> + ?Sized, B: BeginParse<'a, T> + ?Sized>
    Parse<super::Seq2<A::Output, B::Output>> for Seq2Parser<'a, T, A, B>
{
    fn accept(self) -> super::Seq2<A::Output, B::Output> {
        // accept both of our inner parsers and return the results.
        super::Seq2(self.a_parser.accept(), self.b_parser.accept())
    }

    fn reject(self, reason: ParseError) -> PResult<(Self, usize)> {
        // pass the rejection to b_parser and see what happens
        Self::with_b_parser(self.tokens, self.a_parser, self.b_parser.reject(reason))
    }
}

impl<T: std::fmt::Debug + BasicToken> Parse<super::TokenAst<T>> for TokenParser<T> {
    fn accept(self) -> super::TokenAst<T> {
        // if this exists, then we've already found the right token
        super::TokenAst::new(self.index, self.token)
    }

    fn reject(self, reason: ParseError) -> PResult<(Self, usize)> {
        // there's really only one way to read a token
        Err(reason)
    }
}

impl<'a, T, P1: BeginParse<'a, T>, P2: BeginParse<'a, T>> Union2Parser<'a, T, P1, P2> {
    pub fn new(tokens: &'a [T], index: usize) -> PResult<(Self, usize)> {
        map_parser(Self, Enum2Parser::new(tokens, index))
    }
}

impl<'a, T, P1: BeginParse<'a, T> + ?Sized, P2: BeginParse<'a, T> + ?Sized, O: Ast> Parse<O>
    for Union2Parser<'a, T, P1, P2>
where
    P1::Output: Into<O>,
    P2::Output: Into<O>,
{
    fn accept(self) -> O {
        match self.0.accept() {
            super::Enum2::A(x) => x.into(),
            super::Enum2::B(x) => x.into(),
        }
    }

    fn reject(self, reason: ParseError) -> PResult<(Self, usize)> {
        map_parser(Self, self.0.reject(reason))
    }
}
