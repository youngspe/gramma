use gramma::string_matcher;

use super::test_matches;

#[test]
fn simple_precedes_match() {
    test_matches(
        "foobarbaz",
        &string_matcher!(exactly("bar") + precedes(exactly("baz"))),
        3,
        6,
    );
}

#[test]
fn simple_precedes_miss() {
    test_matches(
        "foobarbat",
        &string_matcher!(exactly("bar") + precedes(exactly("baz"))),
        3,
        None,
    );
}

#[test]
fn consistent_precedes_match() {
    test_matches(
        "foobarbaz",
        &string_matcher!(precedes(exactly("bar")) + exactly("bar")),
        3,
        6,
    );
}

#[test]
fn inconsistent_precedes_miss() {
    test_matches(
        "foobarbaz",
        &string_matcher!(precedes(exactly("baz")) + exactly("bar")),
        3,
        None,
    );
}

#[test]
fn simple_follows_match() {
    test_matches(
        "foobarbaz",
        string_matcher!(follows(exactly("foo")) + exactly("bar")),
        3,
        6,
    );
}

#[test]
fn simple_follows_miss() {
    test_matches(
        "fotbarbaz",
        string_matcher!(follows(exactly("foo")) + exactly("bar")),
        3,
        None,
    );
}

#[test]
fn consistent_follows_match() {
    test_matches(
        "foobarbaz",
        string_matcher!(exactly("bar") + follows(exactly("bar"))),
        3,
        6,
    );
}

#[test]
fn inconsistent_follows_miss() {
    test_matches(
        "foobarbaz",
        string_matcher!(exactly("bar") + follows(exactly("baz"))),
        3,
        None,
    );
}
