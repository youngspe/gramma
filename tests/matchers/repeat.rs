use gramma::string_matcher;

use super::test_matches;

#[test]
fn fixed_repeat_zero() {
    let src = "foo bar baz";
    test_matches(src, string_matcher!(exactly("foo").repeat(0)), 4, 4);
    test_matches(src, string_matcher!(exactly("foo").repeat(0).lazy()), 4, 4);
    test_matches(
        src,
        string_matcher!(exactly("foo").repeat(0).simple()),
        4,
        4,
    );
}

#[test]
fn fixed_repeat_zero_matches_empty_when_repetition_found() {
    let src = "foo bar baz";
    test_matches(src, string_matcher!(exactly("bar").repeat(0)), 4, 4);
    test_matches(src, string_matcher!(exactly("bar").repeat(0).lazy()), 4, 4);
    test_matches(
        src,
        string_matcher!(exactly("bar").repeat(0).simple()),
        4,
        4,
    );
}

#[test]
fn fixed_repeat_match() {
    let src = "foo barbarbar baz";
    test_matches(src, string_matcher!(exactly("bar").repeat(3)), 4, 13);
    test_matches(src, string_matcher!(exactly("bar").repeat(3).lazy()), 4, 13);
    test_matches(
        src,
        string_matcher!(exactly("bar").repeat(3).simple()),
        4,
        13,
    );
}

#[test]
fn fixed_repeat_too_many_match() {
    let src = "foo barbarbarbar baz";
    test_matches(src, string_matcher!(exactly("bar").repeat(3)), 4, 13);
    test_matches(src, string_matcher!(exactly("bar").repeat(3).lazy()), 4, 13);
    test_matches(
        src,
        string_matcher!(exactly("bar").repeat(3).simple()),
        4,
        13,
    );
}

#[test]
fn fixed_repeat_not_enough_miss() {
    let src = "foo barbar baz";
    test_matches(src, string_matcher!(exactly("bar").repeat(3)), 4, None);
    test_matches(
        src,
        string_matcher!(exactly("bar").repeat(3).lazy()),
        4,
        None,
    );
    test_matches(
        src,
        string_matcher!(exactly("bar").repeat(3).simple()),
        4,
        None,
    );
}

#[test]
fn lower_bounded_repeat_match_greedy() {
    let src = "foo barbarbarbar baz";
    test_matches(src, string_matcher!(exactly("bar").repeat(2..)), 4, 16);
}

#[test]
fn lower_bounded_repeat_match_lazy() {
    let src = "foo barbarbarbar baz";
    test_matches(
        src,
        string_matcher!(exactly("bar").repeat(2..).lazy()),
        4,
        10,
    );
}

#[test]
fn lower_bounded_repeat_match_simple() {
    let src = "foo barbarbarbar baz";
    test_matches(
        src,
        string_matcher!(exactly("bar").repeat(2..).simple()),
        4,
        16,
    );
}

#[test]
fn lower_bounded_repeat_miss() {
    let src = "foo barbarbarbar baz";
    test_matches(src, string_matcher!(exactly("bar").repeat(5..)), 4, None);
    test_matches(
        src,
        string_matcher!(exactly("bar").repeat(5..).lazy()),
        4,
        None,
    );
    test_matches(
        src,
        string_matcher!(exactly("bar").repeat(5..).simple()),
        4,
        None,
    );
}

#[test]
fn upper_bounded_repeat_match_greedy() {
    let src = "foo barbarbarbar baz";
    test_matches(src, string_matcher!(exactly("bar").repeat(2..=3)), 4, 13);
}

#[test]
fn upper_bounded_repeat_match_lazy() {
    let src = "foo barbarbarbar baz";
    test_matches(
        src,
        string_matcher!(exactly("bar").repeat(2..=3).lazy()),
        4,
        10,
    );
}

#[test]
fn upper_bounded_repeat_match_simple() {
    let src = "foo barbarbarbar baz";
    test_matches(
        src,
        string_matcher!(exactly("bar").repeat(2..=3).simple()),
        4,
        13,
    );
}

#[test]
fn empty_match_greedy() {
    let src = "foobar";
    test_matches(
        src,
        string_matcher!(exactly("foo").repeat(..).greedy() + exactly("bar")),
        3,
        6,
    );
}

#[test]
fn empty_match_lazy() {
    let src = "foobar";
    test_matches(
        src,
        string_matcher!(exactly("foo").repeat(..).lazy() + exactly("bar")),
        3,
        6,
    );
}

#[test]
fn empty_match_simple() {
    let src = "foobar";
    test_matches(
        src,
        string_matcher!(exactly("foo").repeat(..).simple() + exactly("bar")),
        3,
        6,
    );
}

#[test]
fn backtrack_empty_match_greedy() {
    let src = "foobar";
    test_matches(
        src,
        string_matcher!(exactly("bar").repeat(..).greedy() + exactly("bar")),
        3,
        6,
    );
}

#[test]
fn backtrack_empty_match_lazy() {
    let src = "foobar";
    test_matches(
        src,
        string_matcher!(exactly("bar").repeat(..).lazy() + exactly("bar")),
        3,
        6,
    );
}

#[test]
fn backtrack_empty_miss_simple() {
    let src = "foobar";
    test_matches(
        src,
        string_matcher!(exactly("bar").repeat(..).simple() + exactly("bar")),
        3,
        None,
    );
}

#[test]
fn partial_backtrack_empty_match_greedy() {
    let src = "foobarbazbarbazbarbazqux";
    test_matches(
        src,
        string_matcher!((exactly("barbaz") | exactly("bar")).repeat(1..).greedy() + exactly("bar")),
        3,
        18,
    );
}

#[test]
fn partial_backtrack_empty_match_lazy() {
    let src = "foobarbazbarbazbarbazqux";
    test_matches(
        src,
        string_matcher!((exactly("barbaz") | exactly("bar")).repeat(1..).lazy() + exactly("bar")),
        3,
        12,
    );
}

#[test]
fn partial_backtrack_empty_miss_simple() {
    let src = "foobarbazbarbazbarbazqux";
    test_matches(
        src,
        string_matcher!((exactly("barbaz") | exactly("bar")).repeat(1..).simple() + exactly("bar")),
        3,
        None,
    );
}

#[test]
fn complex_repeat_match_greedy() {
    let src = "foo bar foo foo bar foo foo bar foo bar foo";
    test_matches(
        src,
        string_matcher!(
            exactly("foo ") + (exactly("foo ") | exactly("bar ")).repeat(1..) + exactly("bar")
        ),
        0,
        39,
    );
}

#[test]
fn complex_repeat_match_lazy() {
    let src = "foo bar foo foo bar foo foo bar foo bar foo";
    // let src = "fbffbffbfbf";
    test_matches(
        src,
        string_matcher!(
            exactly("foo ")
                + (exactly("foo ") | exactly("bar ")).repeat(1..).lazy()
                + exactly("bar")
        ),
        0,
        19,
    );
}

#[test]
fn complex_repeat_miss_simple() {
    // .simple() matches as far as possible without backtracking.
    // Because the repitition will match the remainder of the string,
    // it won't find "bar" at the end.
    let src = "foo bar foo foo bar foo foo bar foo bar foo";
    test_matches(
        src,
        string_matcher!(
            exactly("foo ")
                + (exactly("foo ") | exactly("bar ")).repeat(1..).simple()
                + exactly("bar")
        ),
        0,
        None,
    );
}

#[test]
fn nested_repeat_greedy() {
    let src = "foo bar baz qux? foo";
    test_matches(
        src,
        string_matcher!((whitespace().repeat(..) + alphabetic().repeat(1..)).repeat(2..)),
        0,
        15,
    );
}

#[test]
fn nested_repeat_lazy() {
    let src = "foo bar baz qux? foo";
    test_matches(
        src,
        string_matcher!((whitespace().repeat(..) + alphabetic().repeat(1..))
            .repeat(2..)
            .lazy()),
        0,
        7,
    );
}

#[test]
fn nested_repeat_simple() {
    let src = "foo bar baz qux? foo";
    test_matches(
        src,
        string_matcher!(
            (whitespace().repeat(..).simple() + alphabetic().repeat(1..).simple())
                .repeat(..)
                .simple()
        ),
        0,
        15,
    );
}

#[test]
fn nested_repeat_with_terminator_greedy() {
    let src = "foo bar baz qux? foo";
    test_matches(
        src,
        string_matcher!(
            (whitespace().repeat(..) + alphabetic().repeat(1..)).repeat(2..) + precedes(char('?'))
        ),
        0,
        15,
    );
}

#[test]
fn nested_repeat_with_terminator_lazy() {
    let src = "foo bar baz qux? foo";
    test_matches(
        src,
        string_matcher!(
            (whitespace().repeat(..) + alphabetic().repeat(1..))
                .repeat(2..)
                .lazy()
                + precedes(char('?'))
        ),
        0,
        15,
    );
}
