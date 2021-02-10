/// Types that represent or can be used to parse abstract syntax trees.
use std::convert::TryInto;
use std::marker::PhantomData;

use super::BeginParse;
use super::{ParseError, ParseResult as PResult};
use crate::lex::TokenIndex;
use crate::lex::{Token, TokenValue};

/// Represents an abstract syntax tree -- what we will eventually parse from a defined grammar.
pub trait Ast: std::fmt::Debug {
    /// Returns the range of tokens spanned by this part of the tree, if available.
    fn range(&self) -> Option<(usize, usize)>;
}

/// Parse with `A` and convert its value to AST `B`.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct Convert<A: ?Sized, B>(PhantomData<A>, PhantomData<B>);

/// AST parsed from one of two types.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Enum2<A, B> {
    A(A),
    B(B),
}

/// Used to parse Enum2 from possibly-unsized BeginParse impls.
pub type Enum2Def<A, B> = Enum2<WrapAst<A>, WrapAst<B>>;

/// Always matches immediately without reading any tokens.
#[derive(Copy, Clone, PartialEq, Eq, Default, Debug)]
pub struct Empty;

/// Parse A followed by B.
#[derive(Clone, PartialEq, Eq, Default, Debug)]
pub struct Seq2<A, B>(pub A, pub B);

/// Used to parse Seq2 from possibly-unsized BeginParse impls.
pub type Seq2Def<A, B> = Seq2<WrapAst<A>, WrapAst<B>>;

/// A "terminal" in the grammar.
/// Matches the token in the type argument.
/// All rules will eventually end in either tokens or the empty rule.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TokenAst<T: TokenValue> {
    pub index: TokenIndex<T>,
    _t: PhantomData<T>,
}

/// Used to parse one of two ASTs that can both be converted to `Out`.
#[derive(Copy, Clone, PartialEq, Eq, Default, Debug)]
pub struct Union2<A: ?Sized, B: ?Sized, Out>(PhantomData<A>, PhantomData<B>, PhantomData<Out>);

/// Wrap a BeginParse implementation that may not be `Sized`.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct WrapAst<A: ?Sized>(PhantomData<A>);

impl<T: TokenValue> TokenAst<T> {
    pub fn new(index: usize) -> Self {
        Self {
            index: TokenIndex::new(index),
            _t: PhantomData,
        }
    }

    pub fn get_str<'a, 'b, U: TokenValue>(&self, src: &'a str, tokens: &'b [Token<U>]) -> &'a str
    where
        &'b U: TryInto<&'b T>,
    {
        &src[self.index.lookup(tokens).range]
    }
}

impl<T: TokenValue> Ast for TokenAst<T> {
    fn range(&self) -> Option<(usize, usize)> {
        Some((self.index.0, self.index.0 + 1))
    }
}

impl<'a, T: TokenValue, U: TokenValue> BeginParse<'a, T> for TokenAst<U>
where
    // for<'b> &'b U: TryFrom<&'b T>,
    &'a T: TryInto<&'a U>,
{
    type Parser = super::TokenParser;
    type Output = Self;

    fn begin_parse(tokens: &'a [Token<T>], mut index: usize) -> PResult<(Self::Parser, usize)> {
        for token in &tokens[index..] {
            if <&T as TryInto<&U>>::try_into(&token.value).is_ok() {
                // if <&U>::try_from(&token.value).is_ok() {
                return Ok((super::TokenParser { index }, index + 1));
            } else if !token.value.should_ignore() {
                break;
            }
            index += 1
        }
        return Err(ParseError {
            index,
            expected: smallvec::smallvec![U::token_type_repr as fn() -> String],
        });
    }
}

impl<T: Ast + ?Sized> Ast for Box<T> {
    fn range(&self) -> Option<(usize, usize)> {
        T::range(self)
    }
}

impl<'a, T: TokenValue, O: BeginParse<'a, T> + ?Sized> BeginParse<'a, T> for Box<O> {
    type Parser = super::BoxParser<O::Parser>;
    type Output = Box<O::Output>;

    fn begin_parse(tokens: &'a [Token<T>], index: usize) -> PResult<(Self::Parser, usize)> {
        Self::Parser::new(O::begin_parse(tokens, index))
    }
}

impl<'a, T: TokenValue, A: BeginParse<'a, T> + ?Sized + 'a, B: Ast> BeginParse<'a, T>
    for Convert<A, B>
where
    A::Output: Into<B>,
{
    type Parser = super::ConvertParser<'a, T, A>;
    type Output = B;

    fn begin_parse(tokens: &'a [Token<T>], index: usize) -> PResult<(Self::Parser, usize)> {
        Self::Parser::new(tokens, index)
    }
}

impl Ast for Empty {
    fn range(&self) -> Option<(usize, usize)> {
        None
    }
}

impl<'a, T: TokenValue> BeginParse<'a, T> for Empty {
    type Parser = super::EmptyParser;
    type Output = Self;

    fn begin_parse(_tokens: &'a [Token<T>], index: usize) -> PResult<(Self::Parser, usize)> {
        Ok((super::EmptyParser, index))
    }
}

impl<A: Ast, B: Ast> Ast for Enum2<A, B> {
    fn range(&self) -> Option<(usize, usize)> {
        match self {
            Self::A(x) => x.range(),
            Self::B(x) => x.range(),
        }
    }
}

impl<'a, T: TokenValue, A: BeginParse<'a, T> + 'a, B: BeginParse<'a, T> + 'a> BeginParse<'a, T>
    for Enum2<A, B>
{
    type Parser = super::Enum2Parser<'a, T, A, B>;
    type Output = Enum2<A::Output, B::Output>;

    fn begin_parse(tokens: &'a [Token<T>], index: usize) -> PResult<(Self::Parser, usize)> {
        Self::Parser::new(tokens, index)
    }
}

impl<T: Ast> Ast for Option<T> {
    fn range(&self) -> Option<(usize, usize)> {
        self.as_ref()?.range()
    }
}

impl<'a, T: TokenValue, O: BeginParse<'a, T> + 'a> BeginParse<'a, T> for Option<O> {
    type Parser = super::OptionParser<'a, T, O>;
    type Output = Option<O::Output>;

    fn begin_parse(tokens: &'a [Token<T>], index: usize) -> PResult<(Self::Parser, usize)> {
        Self::Parser::new(tokens, index)
    }
}

impl<A: Ast, B: Ast> Ast for Seq2<A, B> {
    fn range(&self) -> Option<(usize, usize)> {
        Some((self.0.range()?.0, self.1.range()?.1))
    }
}

impl<'a, T: TokenValue, A: BeginParse<'a, T> + 'a, B: BeginParse<'a, T> + 'a> BeginParse<'a, T>
    for Seq2<A, B>
{
    type Parser = super::Seq2Parser<'a, T, A, B>;
    type Output = Seq2<A::Output, B::Output>;

    fn begin_parse(tokens: &'a [Token<T>], index: usize) -> PResult<(Self::Parser, usize)> {
        Self::Parser::new(tokens, index)
    }
}

impl<'a, T: TokenValue, A: BeginParse<'a, T> + 'a, B: BeginParse<'a, T> + 'a, Out: Ast>
    BeginParse<'a, T> for Union2<A, B, Out>
where
    A::Output: Into<Out>,
    B::Output: Into<Out>,
{
    type Parser = super::Union2Parser<'a, T, A, B>;
    type Output = Out;

    fn begin_parse(tokens: &'a [Token<T>], index: usize) -> PResult<(Self::Parser, usize)> {
        Self::Parser::new(tokens, index)
    }
}

pub struct NonEmptyList<Item: ?Sized, Sep: ?Sized, Trail: ?Sized>(
    PhantomData<Item>,
    PhantomData<Sep>,
    PhantomData<Trail>,
);

pub struct EmptyList<Item: ?Sized>(PhantomData<Item>);

pub struct List<Item: ?Sized, Sep: ?Sized, Trail: ?Sized>(
    PhantomData<Item>,
    PhantomData<Sep>,
    PhantomData<Trail>,
);

impl<T: Ast> Ast for [T] {
    fn range(&self) -> Option<(usize, usize)> {
        Some((self.first()?.range()?.0, self.last()?.range()?.1))
    }
}

impl<T: Ast> Ast for Vec<T> {
    fn range(&self) -> Option<(usize, usize)> {
        <[T]>::range(self)
    }
}

impl<
        'a,
        T: TokenValue,
        Item: BeginParse<'a, T> + ?Sized + 'a,
        Sep: BeginParse<'a, T> + ?Sized + 'a,
        Trail: BeginParse<'a, T> + ?Sized + 'a,
    > BeginParse<'a, T> for NonEmptyList<Item, Sep, Trail>
{
    type Parser = super::NonEmptyListParser<'a, T, Item, Sep, Trail>;
    type Output = Vec<Item::Output>;

    fn begin_parse(tokens: &'a [Token<T>], index: usize) -> PResult<(Self::Parser, usize)> {
        Self::Parser::new(tokens, index)
    }
}

impl<'a, T: TokenValue, Item: BeginParse<'a, T> + ?Sized> BeginParse<'a, T> for EmptyList<Item> {
    type Parser = super::EmptyListParser;
    type Output = Vec<Item::Output>;

    fn begin_parse(_tokens: &'a [Token<T>], index: usize) -> PResult<(Self::Parser, usize)> {
        Ok((super::EmptyListParser, index))
    }
}

impl<
        'a,
        T: TokenValue,
        Item: BeginParse<'a, T> + ?Sized + 'a,
        Sep: BeginParse<'a, T> + ?Sized + 'a,
        Trail: BeginParse<'a, T> + Sized + 'a,
    > BeginParse<'a, T> for List<Item, Sep, Trail>
{
    type Parser =
        <Union2<EmptyList<Item>, NonEmptyList<Item, Sep, Trail>, Vec<Item::Output>> as BeginParse<
            'a,
            T,
        >>::Parser;
    type Output = Vec<Item::Output>;

    fn begin_parse(tokens: &'a [Token<T>], index: usize) -> PResult<(Self::Parser, usize)> {
        Self::Parser::new(tokens, index)
    }
}

impl<'a, T: TokenValue, A: BeginParse<'a, T> + ?Sized> BeginParse<'a, T> for WrapAst<A> {
    type Parser = A::Parser;
    type Output = A::Output;

    fn begin_parse(tokens: &'a [Token<T>], index: usize) -> PResult<(Self::Parser, usize)> {
        A::begin_parse(tokens, index)
    }
}
