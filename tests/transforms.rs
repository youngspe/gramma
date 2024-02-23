use std::ops::ControlFlow;

use either::Either;
use gramma::{ast::Ignore, parse_tree};

mod not_parse {
    use gramma::{
        ast::{transform, NotParse},
        parse::LocationRange,
    };

    gramma::define_token!(
        #[pattern(regex = r"\w+")]
        pub struct Ident;
        #[pattern(exact = "foo")]
        pub struct Foo;

        #[pattern(regex = r#"[\w"]+"#)]
        pub struct TextSegment;

        #[pattern(regex = r#"""""#)]
        pub struct InvalidTextSegment;

        #[pattern(regex = r"[ \t]+")]
        pub struct Space;

        #[pattern(regex = r"\s+")]
        pub struct Whitespace;

        #[pattern(regex = r"[ \t]*\n[ \t]*|[ \t]*$")]
        pub struct NewLine;

        #[pattern(regex = r#"(?s)""".*?""""#)]
        pub struct TextBlock;

        #[pattern(exact = "!")]
        pub struct Bang;
    );
    gramma::define_rule!(
        pub struct NonFooIdent {
            #[transform(not<Foo>)]
            ident: Ident,
        }

        pub struct ValidTextSegment {
            #[transform(not<InvalidTextSegment>)]
            text: TextSegment,
        }

        #[transform(ignore_before<Space>)]
        pub struct TextLine {
            segment1: ValidTextSegment,
            segments: Vec<(Option<Space>, ValidTextSegment)>,
        }

        pub struct Paragraph {
            line1: TextLine,
            #[transform(for_each<discard_before<NewLine>>)]
            lines: Vec<TextLine>,
        }

        #[transform(ignore_before<Whitespace>)]
        pub enum Node {
            TextBlock { text_block: TextBlock },
            Paragraph { paragraph: Paragraph },
            Bang { bang: Bang },
        }

        #[transform(ignore_around<Whitespace>)]
        pub struct Document {
            #[transform(delimited<NewLine, false>)]
            nodes: Vec<Node>,
        }
    );
}

#[test]
fn not_parse_simple_valid_1() {
    let src = "bar";

    let ast = parse_tree::<not_parse::NonFooIdent, 1>(src).unwrap();
    println!("{:#}", gramma::display_tree(src, &ast));
}

#[test]
fn not_parse_simple_invalid_1() {
    let src = "foobar";

    parse_tree::<not_parse::NonFooIdent, 1>(src).unwrap_err();
}

#[test]
fn not_parse_text_segment_valid_1() {
    let src = r#"
    foo bar baz
    qux
    "#;

    let ast = parse_tree::<not_parse::Document, 2>(src).unwrap();
    println!("{:#}", gramma::display_tree(src, &ast));
}

#[test]
fn not_parse_text_segment_invalid_1() {
    let src = r#"
    foo """bar baz
    qux
    "#;

    let err = parse_tree::<not_parse::Document, 2>(src).unwrap_err();
    assert_eq!(err.location.position, 9);
}

#[test]
fn not_parse_text_block_valid_1() {
    let src = r#"foo bar baz
    """
    foo
    """"#;

    parse_tree::<
        (
            not_parse::TextLine,
            Either<
                (not_parse::NewLine, not_parse::TextLine),
                (not_parse::NewLine, not_parse::TextBlock),
            >,
        ),
        2,
    >(src)
    .unwrap();
}
