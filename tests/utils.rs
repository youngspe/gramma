#[macro_export]
macro_rules! assert_let {
    ($pat:pat = $value:expr, $msg:literal $(, $($args:tt)+)?) => {
        let $pat = (match $value {
            #[allow(unused)]
            actual if matches!(actual, $pat) => actual,
            actual => panic!(
                "Assertion failed: matches!({}, {});\n{}",
                stringify!($value),
                stringify!($pat),
                format_args!($msg, actual = actual, $($args)*),
            ),
        }) else {
            unreachable!()
        };
    };
    ($pat:pat = $value:expr) => {
        $crate::assert_let!($pat = $value, " Actual: {actual:?}")
    };
}
