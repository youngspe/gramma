use gramma::define_rule;
use gramma::{ast::Discard, parse::LocationRange};

use gramma::ast::{parse_tree, Ignore, ListNode};
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
);

gramma::define_rule!(
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
        #[transform(for_each<discard_before<(Ignore<Space>, NewLine, Ignore<Space>)>>)]
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
