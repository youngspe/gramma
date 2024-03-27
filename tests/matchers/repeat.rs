use gramma::string_pattern;

use super::test_matches;

#[test]
fn fixed_repeat_zero() {
    let src = "foo bar baz";
    test_matches(src, string_pattern!(exactly("foo").repeat(0)), 4, 4);
    test_matches(src, string_pattern!(exactly("foo").repeat(0).lazy()), 4, 4);
}

#[test]
fn fixed_repeat_zero_matches_empty_when_repetition_found() {
    let src = "foo bar baz";
    test_matches(src, string_pattern!(exactly("bar").repeat(0)), 4, 4);
    test_matches(src, string_pattern!(exactly("bar").repeat(0).lazy()), 4, 4);
}

#[test]
fn fixed_repeat_match() {
    let src = "foo barbarbar baz";
    test_matches(src, string_pattern!(exactly("bar").repeat(3)), 4, 13);
    test_matches(src, string_pattern!(exactly("bar").repeat(3).lazy()), 4, 13);
}

#[test]
fn fixed_repeat_too_many_match() {
    let src = "foo barbarbarbar baz";
    test_matches(src, string_pattern!(exactly("bar").repeat(3)), 4, 13);
    test_matches(src, string_pattern!(exactly("bar").repeat(3).lazy()), 4, 13);
}

#[test]
fn fixed_repeat_not_enough_miss() {
    let src = "foo barbar baz";
    test_matches(src, string_pattern!(exactly("bar").repeat(3)), 4, None);
    test_matches(
        src,
        string_pattern!(exactly("bar").repeat(3).lazy()),
        4,
        None,
    );
}

#[test]
fn lower_bounded_repeat_match_greedy() {
    let src = "foo barbarbarbar baz";
    test_matches(src, string_pattern!(exactly("bar").repeat(2..)), 4, 16);
}

#[test]
fn lower_bounded_repeat_match_lazy() {
    let src = "foo barbarbarbar baz";
    test_matches(
        src,
        string_pattern!(exactly("bar").repeat(2..).lazy()),
        4,
        10,
    );
}

#[test]
fn upper_bounded_repeat_match_greedy() {
    let src = "foo barbarbarbar baz";
    test_matches(src, string_pattern!(exactly("bar").repeat(2..=3)), 4, 13);
}

#[test]
fn upper_bounded_repeat_match_lazy() {
    let src = "foo barbarbarbar baz";
    test_matches(
        src,
        string_pattern!(exactly("bar").repeat(2..=3).lazy()),
        4,
        10,
    );
}

#[test]
fn complex_repeat_match_greedy() {
    let src = "foo bar foo foo bar foo foo bar foo bar foo";
    test_matches(
        src,
        string_pattern!(
            exactly("foo ") + (exactly("foo ") | exactly("bar ")).repeat(1..) + exactly("bar")
        ),
        0,
        39,
    );
}

#[test]
fn complex_repeat_match_lazy() {
    let src = "foo bar foo foo bar foo foo bar foo bar foo";
    test_matches(
        src,
        string_pattern!(
            exactly("foo ")
                + (exactly("foo ") | exactly("bar ")).repeat(1..).lazy()
                + exactly("bar")
        ),
        0,
        19,
    );
}

#[test]
fn nested_repeat_greedy() {
    let src = "foo bar baz qux? foo";
    test_matches(
        src,
        string_pattern!((whitespace().repeat(..) + alphabetic().repeat(1..)).repeat(2..)),
        0,
        15,
    );
}

#[test]
fn nested_repeat_lazy() {
    let src = "foo bar baz qux? foo";
    test_matches(
        src,
        string_pattern!((whitespace().repeat(..) + alphabetic().repeat(1..))
            .repeat(2..)
            .lazy()),
        0,
        7,
    );
}

#[test]
fn nested_repeat_with_terminator_greedy() {
    let src = "foo bar baz qux? foo";
    test_matches(
        src,
        string_pattern!(
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
        string_pattern!(
            (whitespace().repeat(..) + alphabetic().repeat(1..))
                .repeat(2..)
                .lazy()
                + precedes(char('?'))
        ),
        0,
        15,
    );
}
