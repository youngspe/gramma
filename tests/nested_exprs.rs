use rs_typed_parser::ast::{
    CompoundToken, DelimitedList, Discard, DualParse, Ignore, InfixChain, Token, WithSource,
};

type Blank = Ignore<Token<Space>>;

rs_typed_parser::define_rule!(
    #[transform(ignore_before<Token<Space>>)]
    pub struct Braces {
        l_brace: Discard<Token<LBrace>>,
        inner: DelimitedList<Expr, (Blank, Token<Comma>)>,
        #[transform(ignore_before<Token<Space>>)]
        r_brace: Discard<Token<RBrace>>,
    }
    pub struct Expr {
        value: InfixChain<BaseExpr, InfixOp>,
    }
    pub enum BaseExpr {
        Ident {
            ident: Ident,
        },
        Braces {
            braces: Braces,
        },
        #[transform(ignore_before<Token<Space>>)]
        BracketedIdent {
            bracketed: CompoundToken<BracketedIdent>,
        },
        #[transform(ignore_before<Token<Space>>)]
        BracketedNumber {
            bracketed: CompoundToken<BracketedNumber>,
        },
    }

    #[transform(ignore_before<Token<Space>>)]
    pub enum InfixOp {
        Plus { value: Token<Plus> },
        Minus { value: Token<Minus> },
    }

    pub struct IdentParts {
        parts: DelimitedList<Token<IdentPart>, Token<Underscore>>,
    }
    #[transform(ignore_before<Token<Space>>)]
    pub struct Ident {
        inner: DualParse<Token<IdentString>, IdentParts>,
    }
    pub struct BracketedIdent {
        l_bracket: Discard<Token<LBracket>>,
        ident: Ident,
        #[transform(ignore_before<Token<Space>>)]
        r_bracket: Discard<Token<RBracket>>,
    }
    pub struct BracketedNumber {
        l_bracket: Discard<Token<LBracket>>,
        #[transform(ignore_around<Token<Space>>)]
        ident: Token<Digits>,
        r_bracket: Discard<Token<RBracket>>,
    }
);

rs_typed_parser::define_token!(
    #[pattern(exact = "[")]
    struct LBracket;
    #[pattern(exact = "]")]
    struct RBracket;
    #[pattern(exact = "{")]
    struct LBrace;
    #[pattern(exact = "}")]
    struct RBrace;
    #[pattern(exact = ",")]
    struct Comma;
    #[pattern(exact = "+")]
    struct Plus;
    #[pattern(exact = "-")]
    struct Minus;
    #[pattern(exact = "_")]
    struct Underscore;
    #[pattern(regex = r"[a-zA-Z][a-zA-Z0-9_]*")]
    struct IdentString;
    #[pattern(regex = r"[a-zA-Z0-9]+")]
    struct IdentPart;
    #[pattern(regex = r"[0-9]+")]
    struct Digits;
    #[pattern(regex = r"\s+")]
    struct Space;
);

#[test]
pub fn parse_test1() {
    let src = "{a,{a, {{{ a +foo_bar+ {{{a,b} }}}-{}}},b,}, b +b }";
    let ast = rs_typed_parser::parse_tree::<Braces, 1>(src).unwrap();
    println!("{:#}", WithSource { src, ast });
    let x: <Braces as rs_typed_parser::ast::TransformRule>::Inner;
}

#[test]
pub fn parse_test2() {
    let src = "abc_def";
    let ast = rs_typed_parser::parse_tree::<Ident, 1>(src).unwrap();
    println!("{:#}", WithSource { src, ast });
}

#[test]
pub fn parse_test3() {
    let src = r#"
    {
        a,
        {
            a,
            {
                {
                    {
                        a + foo_bar + {
                            {{a,b}},
                        }
                    }-{}
                }
            },
            b,
        },
        b
    }"#;
    let ast = rs_typed_parser::parse_tree::<Expr, 1>(src).unwrap();
    println!("{:#}", WithSource { src, ast });
}
