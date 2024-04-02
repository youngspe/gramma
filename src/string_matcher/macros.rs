#[macro_export]
macro_rules! string_matcher {
    ($expr:expr $(,)?) => {
        &$crate::string_matcher::StringPattern::matcher({
            #[allow(unused)]
            use $crate::string_matcher::patterns::*;
            $expr
        }) as &$crate::string_matcher::StringMatcher<_>
    };
}
#[macro_export]
macro_rules! string_pattern {
    ($expr:expr $(,)?) => {
        $crate::string_matcher::StringPattern::_validate_string_pattern({
            #[allow(unused)]
            use $crate::string_matcher::patterns::*;
            $expr
        })
    };
}
