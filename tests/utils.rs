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

#[macro_export]
macro_rules! parameterize {
    (@mod attrs = (); $(#$attrs:tt)* $vis:vis mod $Mod:ident ;) => {
        $crate::parameterize! { @mod
            attrs = ();
            $(#$attrs)*
            $vis mod $Mod {}
        }
    };
    (@mod attrs = $attrs0:tt; #[params = $Macro:ident $params:tt] $(#$attrs1:tt)* $vis:vis mod $Mod:ident { $($body:tt)* }) => {
        $crate::parameterize! { @mod
            attrs = $attrs0;
            $(#$attrs1)*
            $vis mod $Mod {
                $Macro! $params;
                $($body)*
            }
        }
    };
    (@mod attrs = ($($attrs0:tt)*); #$attr:tt $(#$attrs1:tt)* $vis:vis mod $Mod:ident $body:tt) => {
        $crate::parameterize! { @mod
            attrs = ($($attrs0)* #$attr);
            $(#$attrs1)*
            $vis mod $Mod $body
        }
    };
    (@mod attrs = ($($attrs0:tt)*); $vis:vis mod $Mod:ident { $($body:tt)* }) => {
        $(#$attrs0)*
        $vis mod $Mod {
            #[allow(unused_imports)]
            use super::*;
            $crate::parameterize! { $($body)* }
        }
    };
    () => {};
    ($(macro $Macro:ident ($($arg:tt)*) { $($body:tt)* })+ $($rest:tt)*) => {
        $(macro_rules! $Macro {
            ($($arg)*) => { $crate::parameterize! { $($body)* } };
        })+
        $crate::parameterize! { $($rest)* }
    };
    ($($(#$attrs:tt)* pub mod $Mod:ident $body:tt)+ $($rest:tt)*) => {
        $($crate::parameterize! { @mod
            attrs = ();
            $(#$attrs)*
            pub mod $Mod $body
        })+
        $crate::parameterize! { $($rest)* }
    };
    ($(#$attrs:tt)* $vis:vis mod $Mod:ident $body:tt $($rest:tt)*) => {
        $crate::parameterize! { @mod
            attrs = ();
            $(#$attrs)*
            $vis mod $Mod $body
        }
        $crate::parameterize! { $($rest)* }
    };
    ($item:item $($rest:tt)*) => {
        $item
        $crate::parameterize! { $($rest)* }
    };
    ($item:stmt; $($rest:tt)*) => {
        $item;
        $crate::parameterize! { $($rest)* }
    };
}
