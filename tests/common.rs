#[allow(unused_macros)]
macro_rules! match_assert {
    ($($pat:pat = $val:expr $(,$cond:expr)*$(,)? => $then:expr);*$(;)?) => {
        $(if let $pat = $val {
            $(assert!($cond);)*
            $then
        } else {
            panic!("Expected {}; got {:?}", stringify!($pat), $val)
        })*
    };
}

pub fn unwrap_display<T, E: std::fmt::Display>(r: Result<T, E>) -> T {
    match r {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    }
}
