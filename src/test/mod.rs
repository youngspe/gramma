mod json;
mod lispish;

use crate::lex::Token;
use crate::parse::TokenAst;
use std::convert::TryInto;

#[macro_export]
macro_rules! match_assert {
    ($($pat:pat = $val:expr $(,$cond:expr)*$(,)? => $then:expr);*$(;)?) => {
        $(if let $pat = $val {
            $(assert!($cond);)*
            $then
        } else {
            panic!("Expected {}; got {:?}", stringify!($pat), $val)
        })*
    };
}

fn unwrap_display<T, E: std::fmt::Display>(r: Result<T, E>) -> T {
    match r {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    }
}

fn token_slice<'a, T: crate::lex::TokenValue, U: crate::lex::TokenValue>(
    src: &'a str,
    tokens: &[Token<T>],
    token: &TokenAst<U>,
) -> &'a str
where
    for<'b> &'b T: TryInto<&'b U>,
{
    &src[token.index.lookup(tokens).range]
}
