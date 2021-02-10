use crate::lex::{Eof, RealDecimal, StringToken, Whitespace};
use crate::match_assert;
use crate::test::token_slice;
use crate::test::unwrap_display;
use crate::test::Token;
use crate::{grammar, tokens};

tokens! {
    pub struct LParen => symbol("(");
    pub struct RParen => symbol(")");
    pub struct LBracket => symbol("[");
    pub struct RBracket => symbol("]");
    pub struct LBrace => symbol("{");
    pub struct RBrace => symbol("}");
    pub struct True => symbol("true");
    pub struct False => symbol("false");
    pub struct Null => symbol("null");
    pub struct Id => regex("identifier", r"^[a-zA-Z_!@#\$%\^&\*\-<>+=][a-zA-Z_!@#\$%\^&\*\-<>0-9+=]*");
    /// Token type used to parse Lispish:
    pub enum LispishToken => {
        | LParen | RParen
        | LBracket | RBracket
        | LBrace | RBrace
        | StringToken
        | RealDecimal
        | True
        | False
        | Null
        | Id
        | Whitespace
        | Eof
    };
}

grammar! { LispishToken;
    /// Parses a list of expressions
    type CallArgs => { [(Expr)+] };
    /// Parses a list of expressions in prens, brackets, or braces
    pub struct Call => {
        | LParen, vals: CallArgs, RParen,
        | LBracket, vals: CallArgs, RBracket,
        | LBrace, vals: CallArgs, RBrace,
    };
    pub enum Bool => { True | False };
    pub enum Expr => {
        | Call
        | Bool
        | String(StringToken)
        | Number(RealDecimal)
        | Id
        | Null
    };
    /// Root rule for this grammar
    pub struct Lispish => { value: Expr, Eof };
}

fn parse(src: &str) -> (Vec<Token<LispishToken>>, Lispish) {
    unwrap_display(crate::parse(src))
}

#[test]
fn parse_lispish() {
    use Expr as E;

    let src = "{ foo-123+4 (bar 1 [baz 2.0 true \"hi\"]) qux }";
    let (tokens, actual) = parse(src);

    match_assert!(E::Call(v) = &actual.value =>
        match_assert!([E::Id(a), E::Call(b), E::Id(c)] = &v.vals[..] => {
            assert_eq!("foo-123+4", token_slice(src, &tokens, &a));
            assert_eq!("qux", token_slice(src, &tokens, &c));

            match_assert!([E::Id(a), E::Number(b) ,E::Call(c)] = &b.vals[..] => {
                assert_eq!("bar", token_slice(src, &tokens, &a));
                assert_eq!("1", token_slice(src, &tokens, &b));

                match_assert!([E::Id(a), E::Number(b), E::Bool(Bool::True(..)), E::String(c)] = &c.vals[..] => {
                    assert_eq!("baz", token_slice(src, &tokens, &a));
                    assert_eq!("2.0", token_slice(src, &tokens, &b));
                    assert_eq!("\"hi\"", token_slice(src, &tokens, &c));
                });
            });
        });
    );
}
