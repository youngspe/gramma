use gramma::ast::{CompoundToken, DelimitedList, Discard, DualParse, Ignore, InfixChain};

type Blank = Ignore<Space>;

gramma::define_rule!(
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
    pub enum Empty {}
);

gramma::define_token!(
    #[pattern(exact = "[")]
    pub struct LBracket;
    #[pattern(exact = "]")]
    pub struct RBracket;
    #[pattern(exact = "{")]
    pub struct LBrace;
    #[pattern(exact = "}")]
    pub struct RBrace;
    #[pattern(exact = ",")]
    pub struct Comma;
    #[pattern(exact = "+")]
    pub struct Plus;
    #[pattern(exact = "-")]
    pub struct Minus;
    #[pattern(exact = "_")]
    pub struct Underscore;
    #[pattern(regex = r"[a-zA-Z][a-zA-Z0-9_]*")]
    pub struct IdentString;
    #[pattern(regex = r"[a-zA-Z0-9]+")]
    pub struct IdentPart;
    #[pattern(regex = r"[0-9]+")]
    pub struct Digits;
    #[pattern(regex = r"\s+")]
    pub struct Space;
);

#[test]
pub fn parse_test1() {
    let src = "{a,{a, {{{ a +foo_bar+ {{{a,b} }}}-{}}},b,}, b +b }";
    let ast = gramma::parse_tree::<Braces, 2>(src).unwrap();
    println!("{:#}", gramma::display_tree(src, &ast));
}

#[test]
pub fn parse_test2() {
    let src = "abc_def";
    let ast = gramma::parse_tree::<Ident, 1>(src).unwrap();
    println!("{:#}", gramma::display_tree(src, &ast));
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
    let ast = gramma::parse_tree::<Expr, 1>(src).unwrap();
    println!("{:#}", gramma::display_tree(src, &ast));
}
