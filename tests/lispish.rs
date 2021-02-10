#[macro_use]
pub mod common;

use common::unwrap_display;
use gramma::lex::{Eof, RealDecimal, StringToken, Whitespace};
use gramma::{grammar, tokens};

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

#[test]
fn parse_lispish() {
    use Expr as E;

    let src = "{ foo-123+4 (bar 1 [baz 2.0 true \"hi\"]) qux }";
    let (_, actual) = unwrap_display(gramma::parse::<LispishToken, Lispish>(src));

    match_assert!(E::Call(v) = &actual.value =>
        match_assert!([E::Id(a), E::Call(b), E::Id(c)] = &v.vals[..] => {
            assert_eq!("foo-123+4", a.get_str(src));
            assert_eq!("qux", c.get_str(src));

            match_assert!([E::Id(a), E::Number(b) ,E::Call(c)] = &b.vals[..] => {
                assert_eq!("bar", a.get_str(src));
                assert_eq!("1", b.get_str(src));

                match_assert!([E::Id(a), E::Number(b), E::Bool(Bool::True(..)), E::String(c)] = &c.vals[..] => {
                    assert_eq!("baz", a.get_str(src));
                    assert_eq!("2.0", b.get_str(src));
                    assert_eq!("\"hi\"", c.get_str(src));
                });
            });
        });
    );
}
