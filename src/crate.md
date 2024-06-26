This library provides macros for defining context-free grammar-ish parsers in terms
of Rust types.

# Abstract Syntax Tree

A grammar is defined by the abstract syntax tree (AST) that it produces.

ASTs are made up of two kinds of components:

- Tokens
- Rule

## Token

A _token_ (in some contexts called a _terminal_ or _symbol_) is the basic unit of a grammar.
It's defined by either an exact string match or a regular language.

## Rule

# Example

```rust
use gramma::{
    ast::InfixChain,
    define_rule,
    define_token,
    display_tree,
    parse_tree,
};

define_token!(
    #[pattern(exact = "(")]
    pub struct LeftParen;
    #[pattern(exact = ")")]
    pub struct RightParen;
    #[pattern(exact = "[")]
    pub struct LeftBracket;
    #[pattern(exact = "]")]
    pub struct RightBracket;
    #[pattern(exact = "+")]
    pub struct Plus;
    #[pattern(exact = "-")]
    pub struct Minus;
    #[pattern(exact = ",")]
    pub struct Comma;
    #[pattern(regex = r"[a-zA-Z_]\w*")]
    pub struct Ident;
    #[pattern(matcher = ascii_digit().repeat(1..))]
    pub struct Number;
    #[pattern(regex = r"\s*")]
    pub struct Whitespace;
);

define_rule!(
    #[transform(ignore_around<Whitespace>)]
    pub enum BaseExpr {
        Parens {
            l_paren: LeftParen,
            expr: Box<Expr>,
            r_paren: RightParen,
        },
        List {
            l_bracket: LeftBracket,
            #[transform(delimited<Comma>)]
            exprs: Vec<Expr>,
            #[transform(ignore_before<Whitespace>)]
            r_bracket: RightBracket,
        },
        Ident { ident: Ident },
        Number { number: Number },
    }

    #[transform(ignore_around<Whitespace>)]
    pub enum Op {
        Plus { plus: Plus },
        Minus { minus: Minus },
    }

    pub struct Expr {
        binary_ops: InfixChain<BaseExpr, Op>,
    }
);

let src = "
    a - (b + c) + [1, 2]
";

let expr = parse_tree::<Expr, 1>(src).unwrap();
let display = format!("{:#}", display_tree(src, &expr));

assert_eq!(display, r#"Expr -> (
    <Ident "a">,
    { "-", {
        "(",
        Expr -> (
            <Ident "b">,
            { "+", <Ident "c"> },
        ),
        ")",
    } },
    { "+", {
        "[",
        [
            Expr -> <Number "1">,
            Expr -> <Number "2">,
        ],
        "]",
    } },
)"#);
```
