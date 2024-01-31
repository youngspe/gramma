use rs_typed_parser::ast::{DelimitedList, Discard, Ignore, InfixChain, Token, WithSource};

#[test]
pub fn parse_test() {
    type Blank = Ignore<Token<Space>>;

    rs_typed_parser::define_rule!(
        pub struct Braces {
            l_brace: Discard<Token<LBrace>>,
            inner: DelimitedList<Expr, (Blank, Token<Comma>)>,
            _space1: Blank,
            r_brace: Discard<Token<RBrace>>,
        }
        pub struct Expr {
            value: InfixChain<BaseExpr, (Blank, InfixOp)>,
        }
        pub enum BaseExpr {
            Ident { _space1: Blank, ident: Token<Ident> },
            Braces { _space1: Blank, braces: Braces },
        }
        pub enum InfixOp {
            Plus { _space1: Blank, value: Token<Plus> },
            Minus { value: Token<Minus> },
        }
    );

    rs_typed_parser::define_token!(
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
        #[pattern(regex = r"[a-zA-Z]\w*")]
        struct Ident;
        #[pattern(regex = r"\s+")]
        struct Space;
    );

    let src = "{a,{a, {{{ a +b+ {{{a,b}}}}-{}}},b,}, b  }";
    let ast = rs_typed_parser::parse_tree::<Braces, 1>(src).unwrap();
    println!("{:#}", WithSource { src, ast });
}
