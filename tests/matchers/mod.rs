use gramma::string_matcher::AnyStringMatcher;

pub mod infix;
pub mod lookaround;
pub mod repeat;

#[track_caller]
fn test_matches(src: &str, matcher: AnyStringMatcher, start: usize, end: impl Into<Option<usize>>) {
    assert_eq!(
        matcher.match_string(start, src),
        Into::<Option<usize>>::into(end).map(|end| start..end)
    )
}
