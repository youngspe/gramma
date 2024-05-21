mod utils;

use gramma::{
    ast::{Ignore, NotEmpty},
    define_rule, define_token, parse_tree,
};

define_token!(
    #[pattern(exact = "(")]
    struct LParen;
    #[pattern(exact = ")")]
    struct RParen;
    #[pattern(exact = ";")]
    struct Semicolon;
    #[pattern(matcher = !precedes(ascii_digit()) + word().repeat(1..).simple())]
    struct Ident(String);
    #[pattern(matcher = ascii_digit().repeat(1..).simple())]
    struct Digits(String);
    #[pattern(matcher = whitespace().repeat(1..).simple())]
    struct Whitespace;
);

define_rule!(
    enum Expr {
        Group {
            #[transform(discard_around<LParen, RParen>)]
            #[transform(for_each<ignore_around<Whitespace>>)]
            exprs: Vec<Expr>,
        },
        Ident {
            ident: Ident,
        },
        Num {
            num: Digits,
        },
    }

    // List of exprs with an optional ident in front
    #[transform(ignore_around<Whitespace>)]
    struct PrefixedList {
        prefix: Option<Ident>,
        #[transform(for_each<ignore_before<Whitespace>>)]
        exprs: Vec<Expr>,
        _delimiter: Option<Semicolon>,
    }
    // List of exprs with an optional ident in front that must not be empty
    #[transform(ignore_around<Whitespace>, not_empty)]
    struct NonEmptyPrefixedList {
        prefix: Option<Ident>,
        #[transform(for_each<ignore_before<Whitespace>>)]
        exprs: Vec<Expr>,
    }
);

#[test]
fn non_empty_prefix_list_name_args_success() {
    let out = parse_tree::<NonEmptyPrefixedList, 1>(
        "
        foo 1 (2 three) four
    ",
    )
    .unwrap();

    assert_eq!(out.prefix.unwrap().0, "foo");
    assert_let!(
        [
            Expr::Num { num: num1 },
            Expr::Group { exprs: group2 },
            Expr::Ident { ident: ident4 },
        ] = &out.exprs[..]
    );

    assert_eq!(num1.0, "1");
    assert_let!([Expr::Num { num: num2 }, Expr::Ident { ident: ident3 }] = &group2[..]);
    assert_eq!(num2.0, "2");
    assert_eq!(ident3.0, "three");
    assert_eq!(ident4.0, "four");
}

#[test]
fn non_empty_prefix_list_no_name_args_success() {
    let out = parse_tree::<NonEmptyPrefixedList, 1>(
        "
        1 (2 three) four
    ",
    )
    .unwrap();

    assert_let!(None = out.prefix);
    assert_let!(
        [
            Expr::Num { num: num1 },
            Expr::Group { exprs: group2 },
            Expr::Ident { ident: ident4 },
        ] = &out.exprs[..]
    );

    assert_eq!(num1.0, "1");
    assert_let!([Expr::Num { num: num2 }, Expr::Ident { ident: ident3 }] = &group2[..]);
    assert_eq!(num2.0, "2");
    assert_eq!(ident3.0, "three");
    assert_eq!(ident4.0, "four");
}

#[test]
fn non_empty_prefix_list_no_name_no_args_failure() {
    const SRC: &str = "
    ";
    const LEN: usize = SRC.len();

    assert_let!(
        Err(gramma::ParseError {
            location: gramma::parse::Location { position: LEN },
            ..
        }) = parse_tree::<NonEmptyPrefixedList, 1>(SRC)
    );
}

#[test]
fn optional_list_empty_success() {
    let out = parse_tree::<Vec<Option<(Ignore<Whitespace>, Ident)>>, 1>("").unwrap();
    assert_eq!(out.len(), 0);
}

#[test]
fn optional_list_success() {
    let out = parse_tree::<Vec<Option<(Ignore<Whitespace>, Ident)>>, 1>("a b c").unwrap();
    assert_eq!(out[0].as_ref().unwrap().1 .0, "a");
    assert_eq!(out[1].as_ref().unwrap().1 .0, "b");
    assert_eq!(out[2].as_ref().unwrap().1 .0, "c");
}

#[test]
fn non_empty_optional_list_success() {
    let out = parse_tree::<Vec<NotEmpty<Option<(Ignore<Whitespace>, Ident)>>>, 1>("a b c").unwrap();
    assert_eq!(out[0].value.as_ref().unwrap().1 .0, "a");
    assert_eq!(out[1].value.as_ref().unwrap().1 .0, "b");
    assert_eq!(out[2].value.as_ref().unwrap().1 .0, "c");
}

parameterize!(
    mod delimited {
        macro delimited($trailing:expr)  {
        define_rule!(struct IdentList {
            #[transform(for_each<ignore_before<Whitespace>>, delimited<Option<Semicolon>, $trailing>)]
            items: Vec<Option<Ident>>,
        });

        #[test]
        fn optional_list_delimited_success() {
            let out = parse_tree::<IdentList, 1>("a;;; b c").unwrap().items;
            assert_let!([Some(item0), None, None, Some(item1), Some(item2)] = &*out);
            assert_eq!(item0.0, "a");
            assert_eq!(item1.0, "b");
            assert_eq!(item2.0, "c");
        }

        #[test]
        fn optional_list_delimited_delimiters_only_success() {
            let out = parse_tree::<IdentList, 1>(";;;").unwrap().items;
            assert_eq!(out.len(), 3 + !$trailing as usize);
        }

        #[test]
        fn optional_list_delimited_empty_success() {
            let out = parse_tree::<IdentList, 1>("").unwrap().items;
            assert_eq!(out.len(), 0);
        }
    }

        #[params = delimited(false)]
        mod delimited_standard;

        #[params = delimited(true)]
        mod delimited_trailing;
    }
);

#[test]
fn nested_prefix_list_success() {
    let out = parse_tree::<Vec<PrefixedList>, 1>(
        "
        foo 1 (2 three) four;
        6;
        seven
    ",
    )
    .unwrap();

    {
        assert_eq!(out[0].prefix.as_ref().unwrap().0, "foo");
        assert_let!(
            [
                Expr::Num { num: num1 },
                Expr::Group { exprs: group2 },
                Expr::Ident { ident: ident4 },
            ] = &out[0].exprs[..]
        );

        assert_eq!(num1.0, "1");
        assert_let!([Expr::Num { num: num2 }, Expr::Ident { ident: ident3 }] = &group2[..]);
        assert_eq!(num2.0, "2");
        assert_eq!(ident3.0, "three");
        assert_eq!(ident4.0, "four");
    }

    {
        assert_let!(None = &out[1].prefix);
        assert_let!([Expr::Num { num },] = &out[1].exprs[..]);
        assert_eq!(num.0, "6");
    }

    {
        assert_eq!(out[2].prefix.as_ref().unwrap().0, "seven");
        assert_let!([] = &out[2].exprs[..]);
    }
}
