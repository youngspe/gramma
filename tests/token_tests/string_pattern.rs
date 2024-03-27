gramma::define_token!(
    #[pattern(matcher = char('0'..='9').repeat(1..) + word_boundary())]
    pub struct Number;
    #[pattern(matcher = (word() & !numeric()) + word().repeat(..))]
    pub struct Ident;
    #[pattern(matcher = {
        char('"') + char(..).repeat(..).lazy() + !follows(char('\\')) + char('"')
    })]
    pub struct String;
    #[pattern(matcher = whitespace().repeat(1..))]
    pub struct Whitespace;
    #[pattern(matcher = char('('))]
    pub struct LParen;
    #[pattern(matcher = char(')'))]
    pub struct RParen;
);

gramma::define_rule!(
    #[transform(ignore_around<Whitespace>)]
    pub enum Expr {
        Parens {
            #[transform(discard_around<LParen, RParen>)]
            exprs: Vec<Expr>,
        },
        Number {
            value: Number,
        },
        Ident {
            value: Ident,
        },
        String {
            value: String,
        },
    }
);

#[test]
pub fn parse_expr1() {
    let src = r#"
        (foo 1 (
            "bar" () 2)3 )
    "#;
    let ast = gramma::parse_tree::<Expr, 1>(src).unwrap();

    let Expr::Parens { exprs, .. } = &ast else {
        panic!()
    };

    let [Expr::Ident { value: val1 }, Expr::Number { value: val2 }, Expr::Parens { exprs }, Expr::Number { value: val3 }] =
        &**exprs
    else {
        panic!()
    };

    assert_eq!(val1.range.slice(src), "foo");
    assert_eq!(val2.range.slice(src), "1");
    assert_eq!(val3.range.slice(src), "3");

    let [Expr::String { value: val1 }, Expr::Parens { exprs }, Expr::Number { value: val2 }] =
        &**exprs
    else {
        panic!()
    };

    assert_eq!(val1.range.slice(src), r#""bar""#);
    assert_eq!(val2.range.slice(src), "2");

    assert_eq!(exprs.len(), 0);
}
