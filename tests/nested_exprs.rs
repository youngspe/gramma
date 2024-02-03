use rs_typed_parser::ast::{
    CompoundToken, DelimitedList, Discard, DualParse, Ignore, InfixChain, WithSource,
};

type Blank = Ignore<Space>;

rs_typed_parser::define_rule!(
    #[transform(ignore_before<Space>)]
    pub struct Braces {
        l_brace: Discard<LBrace>,
        inner: DelimitedList<Expr, (Blank, Comma)>,
        #[transform(ignore_before<Space>)]
        r_brace: Discard<RBrace>,
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
        #[transform(ignore_before<Space>)]
        BracketedIdent {
            bracketed: CompoundToken<BracketedIdent>,
        },
        #[transform(ignore_before<Space>)]
        BracketedNumber {
            bracketed: CompoundToken<BracketedNumber>,
        },
    }

    #[transform(ignore_before<Space>)]
    pub enum InfixOp {
        Plus { value: Plus },
        Minus { value: Minus },
    }

    pub struct IdentParts {
        parts: DelimitedList<IdentPart, Underscore>,
    }
    #[transform(ignore_before<Space>)]
    pub struct Ident {
        inner: DualParse<IdentString, IdentParts>,
    }
    #[transform(
        ignore_after<Space>,
        discard_before<LBracket>,
        discard_after<RBracket>,
    )]
    pub struct BracketedIdent {
        ident: Ident,
    }
    pub struct BracketedNumber {
        l_bracket: Discard<LBracket>,
        #[transform(ignore_around<Space>)]
        ident: Digits,
        r_bracket: Discard<RBracket>,
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
                            {{a,[b] + [1]}},
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
