use gramma::ast::{parse_tree, Discard};

gramma::define_token!(
    #[pattern(exact = "[")]
    pub struct LeftBracket;
    #[pattern(exact = "]")]
    pub struct RightBracket;

    #[pattern(exact = "<(")]
    pub struct OpenInline;
    #[pattern(exact = ")>")]
    pub struct CloseInline;

    #[pattern(regex = r"(?xm) (
        [\  \t]* (
            [^ < > \) \\ \{ \} \s ]
            | \\ ( . | u\{[^ \} \s ]*\} )
            | < [^ \( ! \n ]
            | \) [^ > \n ]
            | [ < \) ] $
        )+
    )+")]
    pub struct TextSegment;

    #[pattern(exact = "\n")]
    pub struct NewLine;
    #[pattern(regex = r"[ \t]+")]
    pub struct Space;
    #[pattern(regex = r"\s+")]
    pub struct Whitespace;

    #[pattern(regex = r"(?xm) (
        [^ < ! ]
        | < [^ ! \n\ ]
        | ! [^ > \n ]
        | [ < ! ] $
    )+")]
    pub struct CommentText;

    #[pattern(exact = "<!")]
    pub struct OpenComment;
    #[pattern(exact = "!>")]
    pub struct CloseComment;

    #[pattern(exact = ">")]
    pub struct RightAngle;
    #[pattern(regex = r"\w+")]
    pub struct Selector;
);

gramma::define_rule!(
    #[transform(ignore_before<Space>)]
    pub struct TextLine {
        pub part1: TextLinePart,
        pub parts: Vec<(Option<Discard<Space>>, TextLinePart)>,
    }

    pub enum TextLinePart {
        TextSegment { text: TextSegment },
        Inline { inline: Inline },
        Comment { comment: Comment },
    }

    pub struct Paragraph {
        pub line1: TextLine,
        #[transform(for_each<discard_before<NewLine>>)]
        pub lines: Vec<TextLine>,
    }

    pub struct Comment {
        pub open: Discard<OpenComment>,
        pub inner: CommentText,
        pub close: Discard<CloseComment>,
    }

    pub struct Inline {
        pub open: Discard<OpenInline>,
        #[transform(ignore_around<Whitespace>)]
        pub inner: Box<TextLine>,
        pub close: Discard<CloseInline>,
    }

    pub struct Element {
        selector: Option<Selector>,
        _angle: RightAngle,
        body: Option<Box<Node>>,
    }

    #[transform(ignore_before<Whitespace>)]
    pub enum Node {
        Element { element: Element },
        Paragraph { paragraph: Paragraph },
    }

    #[transform(ignore_after<Whitespace>)]
    pub struct Nodes {
        #[transform(for_each<ignore_before<Whitespace>>, delimited<NewLine, false>)]
        pub nodes: Vec<Node>,
    }
);

#[test]
fn long1() {
    let src = r#"A <(b )> c<!d!> e"#;
    let _ast = parse_tree::<TextLine, 4>(src).unwrap();
    println!("{:#}", gramma::display_tree(src, &_ast));
}

#[test]
fn long2() {
    let src = r#"<(emphasized)> foo
    <(strong)>"#;
    let _ast = parse_tree::<Paragraph, 4>(src).unwrap();
    println!("{:#}", gramma::display_tree(src, &_ast));
}

#[test]
fn long3() {
    let src = r#"a

    b"#;
    let err2 = parse_tree::<Paragraph, 2>(src).unwrap_err();
    let err3 = parse_tree::<Paragraph, 3>(src).unwrap_err();

    assert_eq!(err2, err3);
    assert_eq!(err3.location.position, 2);
}

#[test]
fn long4() {
    let src = r#"a

    b"#;
    let ast3 = parse_tree::<Nodes, 3>(src).unwrap();
    println!("{:#}", gramma::display_tree(src, &ast3));

    assert!(matches!(
        *ast3.nodes,
        [Node::Paragraph { .. }, Node::Paragraph { .. }]
    ));
}
