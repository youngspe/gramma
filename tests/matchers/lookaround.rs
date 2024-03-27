use gramma::string_pattern;

use super::test_matches;

#[test]
fn simple_precedes_match() {
    test_matches(
        "foobarbaz",
        &string_pattern!(exactly("bar") + precedes(exactly("baz"))),
        3,
        6,
    );
}

#[test]
fn simple_precedes_miss() {
    test_matches(
        "foobarbat",
        &string_pattern!(exactly("bar") + precedes(exactly("baz"))),
        3,
        None,
    );
}

#[test]
fn consistent_precedes_match() {
    test_matches(
        "foobarbaz",
        &string_pattern!(precedes(exactly("bar")) + exactly("bar")),
        3,
        6,
    );
}

#[test]
fn inconsistent_precedes_miss() {
    test_matches(
        "foobarbaz",
        &string_pattern!(precedes(exactly("baz")) + exactly("bar")),
        3,
        None,
    );
}

#[test]
fn simple_follows_match() {
    test_matches(
        "foobarbaz",
        string_pattern!(follows(exactly("foo")) + exactly("bar")),
        3,
        6,
    );
}

#[test]
fn simple_follows_miss() {
    test_matches(
        "fotbarbaz",
        string_pattern!(follows(exactly("foo")) + exactly("bar")),
        3,
        None,
    );
}

#[test]
fn consistent_follows_match() {
    test_matches(
        "foobarbaz",
        string_pattern!(exactly("bar") + follows(exactly("bar"))),
        3,
        6,
    );
}

#[test]
fn inconsistent_follows_miss() {
    test_matches(
        "foobarbaz",
        string_pattern!(exactly("bar") + follows(exactly("baz"))),
        3,
        None,
    );
}
