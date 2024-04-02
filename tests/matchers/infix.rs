use gramma::string_matcher;

use super::test_matches;

#[test]
pub fn simple_add_match() {
    test_matches(
        "foobarbaz",
        string_matcher!(exactly("foo") + exactly("bar")),
        0,
        6,
    );
}

#[test]
pub fn simple_add_miss() {
    let matcher = string_matcher!(exactly("foo") + exactly("bar"));
    test_matches("foobatbaz", matcher, 0, None);
    test_matches("fodbarbaz", matcher, 0, None);
    test_matches("fodba", matcher, 0, None);
    test_matches("foo", matcher, 0, None);
}

#[test]
pub fn simple_or_match() {
    let matcher = string_matcher!(exactly("foo") | exactly("bar"));
    test_matches("bar", matcher, 0, 3);
    test_matches("foobar", matcher, 3, 6);
}

#[test]
pub fn simple_or_miss() {
    let matcher = string_matcher!(exactly("foo") | exactly("bar"));
    test_matches("foobarbaz", matcher, 2, None);
    test_matches("foobarbaz", matcher, 6, None);
    test_matches("foobarbaz", matcher, 9, None);
}
