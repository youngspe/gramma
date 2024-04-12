#[allow(unused)]
use crate::ast::transform::*;

#[doc(hidden)]
#[macro_export]
macro_rules! _into_pairs {
    () => { () };
    ($x:tt $($y:tt)+ ) => { ($x,  $crate::_into_pairs! { $($y)+ }) };
    ($x:tt) => { $x };
}

#[doc(hidden)]
#[macro_export]
macro_rules! _into_either_ty {
    ($(,)?) => { $crate::ast::Reject };
    ($x:ty, $($y:ty),+ $(,)? ) => { $crate::Either<$x,  $crate::_into_either_ty! { $($y),+ }> };
    ($x:ty $(,)?) => { $x };
}

#[doc(hidden)]
#[macro_export]
macro_rules! _enum_from_inner {
    { $inner:expr => {
        $Var0:ident { $($field0:ident),* $(,)? },
        $( $Var:ident { $($field:ident),* $(,)? } ),+ $(,)?
    }} => {
        match $inner {
            $crate::Either::Left($crate::ast::Transformed {
                value: $crate::_into_pairs!( $($field0)* ), ..
            }) => Self::$Var0 { $($field0: $field0.value),* },
            $crate::Either::Right(_inner) => $crate::_enum_from_inner! {
                _inner => { $($Var { $($field),* }),+ }
            }
        }
    };
    { $inner:expr => {
        $Var0:ident { $($field0:ident),* $(,)? } $(,)?
    }} => {
        match $inner {
            $crate::ast::Transformed {
                value: $crate::_into_pairs!( $($field0)* ), ..
            } => Self::$Var0 { $($field0: $field0.value),* },
        }
    };
    ($inner:expr => { $(,)? }) => {
        match $inner {}
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! _rule_field_input_types {
    ($(,)?) => { () };
    (
        $(#$attr1:tt)* $Field1:ty,
        $($(#$attr:tt)* $Field:ty),+ $(,)?
    ) => { (
            $crate::_rule_field_input_types! { $(#$attr1)* $Field1 },
            $crate::_rule_field_input_types! { $($(#$attr)* $Field),+ },
    ) };

    (
        #[transform($($x1:ty),* $(,)?)]
        #[transform($($x2:ty),* $(,)?)]
        $(#$attr:tt)* $Field:ty $(,)?) => {
        $crate::_rule_field_input_types! {
            #[transform($($x1),* $(,$x2)*)]
            $(#$attr)*
            $Field
        }
    };
    (#[transform()] $(#$attr:tt)* $Field:ty $(,)?) => {
        $crate::_rule_field_input_types! {
            $(#$attr)*
            $Field
        }
    };
    (#[transform $x:tt] #$attr1:tt $(#$attr:tt)* $Field:ty $(,)?) => {
        $crate::_rule_field_input_types! {
            #[transform $x]
            $(#$attr)*
            $Field
        }
    };

    (#[transform($x1:ty $(,)?)] $Field:ty $(,)?) => {
        $crate::ast::Transformed<$Field, $x1>
    };

    (#[transform($x1:ty, $x2:ty $(,$x:ty)* $(,)?)] $Field:ty $(,)?) => {
        $crate::_rule_field_input_types! {
            #[transform($crate::ast::transform::compose<$x1, $x2>, $($x),*)]
            $Field
        }
    };

    (#[transform $($x:tt)*] $($rest:tt)*) => {
        ::core::compile_error!(::core::concat!("Invalid transform value ", ::core::stringify!($($x)*)))
    };

    (#$attr1:tt $(#$attr:tt)? $Field:ty $(,)?) => {
        $crate::_rule_field_input_types! {
            $(#$attr)* $Field
        }
    };

    ($Field:ty $(,)?) => {
        $crate::ast::Transformed<$Field, $crate::ast::transform::identity>
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! _define_rule_struct {
    (#[transform $($x:tt)*] $($rest:tt)*) => {
        $crate::_define_rule_struct! { $($rest)* }
    };
    (
        $(#$attr:tt)*
        $vis:vis struct $Name:ident {$(,)?} [$($out:tt)*]
    ) => {
        $(#$attr)*
        $vis struct $Name { $($out)* }
    };
    (
        $(#$attr:tt)*
        $vis:vis struct $Name:ident {
            #[transform $($x:tt)*] $($rest:tt)*
        } $out:tt
    ) => {
        $crate::_define_rule_struct! {
            $(#$attr)*
            $vis struct $Name { $($rest)* } $out
        }
    };
    (
        $(#$attr:tt)*
        $vis:vis struct $Name:ident {
            $(#$field_attr:tt)*
            _: $Field:ty

            $(, $($rest:tt)*)?
        } $out:tt
    ) => {
        $crate::_define_rule_struct! {
            $(#$attr)*
            $vis struct $Name { $($($rest)*)? } $out
        }
    };
    (
        $(#$attr:tt)*
        $vis:vis struct $Name:ident {
            $(#$field_attr:tt)*
            $field_vis:vis $field:ident
            : $Field:ty

            $(, $($rest:tt)*)?
        } [$($out:tt)*]
    ) => {
        $crate::_define_rule_struct! {
            $(#$attr)*
            $vis struct $Name { $($($rest)*)? } [
                $($out)*
                $(#$field_attr)*
                $field_vis $field: $Field,
            ]
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! _define_rule_enum {
    (#[transform $($x:tt)*] $($rest:tt)*) => {
        $crate::_define_rule_enum! { $($rest)* }
    };
    (
        $(#$attr:tt)*
        $vis:vis enum $Name:ident {$(,)?} [$($out:tt)*] []
    ) => {
        $(#$attr)*
        $vis enum $Name { $($out)* }
    };
    (
        $(#$attr:tt)*
        $vis:vis enum $Name:ident {
            #[transform $($x:tt)*] $($rest:tt)*

        } $out:tt $var_out:tt
    ) => {
        $crate::_define_rule_enum! {
            $(#$attr)*
            $vis enum $Name { $($rest)* } $out $var_out
        }
    };
    (
        $(#$attr:tt)*
        $vis:vis enum $Name:ident {
            $(#$var_attr:tt)*
            $Var:ident { $(,)? }
            $(, $($rest:tt)*)?
        } [$($out:tt)*] [$($var_out:tt)*]
    ) => {
        $crate::_define_rule_enum! {
            $(#$attr)*
            $vis enum $Name { $($($rest)*)? } [$($out)* $Var {$($var_out)*},] []
        }
    };
    (
        $(#$attr:tt)*
        $vis:vis enum $Name:ident {
            $(#$var_attr:tt)*
            $Var:ident {
                #[transform $($x:tt)*] $($var_rest:tt)*
            }
            $(, $($rest:tt)*)?
        } $out:tt $var_out:tt
    ) => {
        $crate::_define_rule_enum! {
            $(#$attr)*
            $vis enum $Name { $Var {$($var_rest)*} $(, $($rest)*)? }
            $out $var_out
        }
    };
    (
        $(#$attr:tt)*
        $vis:vis enum $Name:ident {
            $(#$var_attr:tt)*
            $Var:ident {
                $(#$field_attr:tt)*
                $field:ident: $Field:ty
                $(, $($var_rest:tt)*)?
            }
            $(, $($rest:tt)*)?
        } $out:tt [$($var_out:tt)*]
    ) => {
        $crate::_define_rule_enum! {
            $(#$attr)*
            $vis enum $Name { $(#$var_attr)* $Var { $($($var_rest)*)? } $(,$($rest)*)* }
            $out [$($var_out)* $(#$field_attr)* $field: $Field,]
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! _define_rule {
    (
        $(#$attr:tt)*
        $vis:vis struct $Name:ident { $(
            $(#$field_attr:tt)*
            $($field_vis:vis $field:ident)?
            $(_ $(@!$under:tt!@)?)?
            : $Field:ty
        ),* $(,)? }
    ) => {

        $crate::_define_rule_struct! {
            $(#$attr)*
            $vis struct $Name {
                $(
                    $($field_vis $field)?
                    $(_ $(@!$under!@)?)?
                    : $Field,
                )*
            } []
        }

        const _: () = {
            use $crate::ast::transform::*;

            impl $crate::ast::DelegateRule for $Name {
                type Inner = $crate::_rule_field_input_types!(
                    $(#$attr)*
                    $crate::_rule_field_input_types!($( $(#$field_attr)* $Field,)*)
                );

                fn from_inner(_inner: Self::Inner) -> Self {
                    let $crate::_into_pairs!($(
                        $($field)?
                        $(_ $(@!$under!@)?)?
                    )*) = _inner.value;
                    Self { $($($field: $field.value)?),* }
                }

                fn print_tree(
                    &self,
                    cx: &$crate::ast::print::PrintContext,
                    f: &mut ::core::fmt::Formatter,
                ) -> ::core::fmt::Result {
                    let Self { $($($field)?),* } = self;
                    f.write_str(::core::stringify!($Name))?;
                    f.write_str(" -> ")?;
                    cx.debug_rule(f, [$($($field as &dyn $crate::ast::Rule,)*)*])
                }

                fn name() -> &'static str {
                    ::core::stringify!($Name)
                }

                fn update_context<Cx: $crate::parse::CxType, R>(
                    cx: $crate::parse::ParseContext<Cx>,
                    f: impl FnOnce($crate::parse::ParseContext<Cx>) -> R,
                ) -> R {
                    f(cx.expecting($crate::error::ExpectedParseObject::from_rule::<Self>()))
                }
            }
        };

        impl ::core::fmt::Debug for $Name {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                let Self { $($($field)?), * } = self;
                f.write_str(::core::stringify!($Name))?;
                f.write_str(" -> ")?;
                f.debug_set()$($(.entry($field))?)*.finish()
            }
        }
    };

    (
        $(#$attr:tt)*
            $vis:vis enum $Name:ident { $(
                $(#$var_attr:tt)*
                $Var:ident { $(
                    $(#$field_attr:tt)*
                    $field:ident : $Field:ty
                ),* $(,)? }
        ),* $(,)? }
    ) => {

        $crate::_define_rule_enum! {
            $(#$attr)*
            $vis enum $Name {
                $($(#$var_attr)* $Var { $($(#$field_attr)* $field: $Field,)* }),*
            } [] []
        }

        const _: () = {
            use $crate::ast::transform::*;

            impl $crate::ast::DelegateRule for $Name {
                type Inner = $crate::_rule_field_input_types!(
                    $(#$attr)*
                    $crate::_into_either_ty!($(
                        $crate::_rule_field_input_types!($(#$var_attr)* $crate::_rule_field_input_types!(
                            $($(#$field_attr)* $Field),*
                        ))
                    ),* )
                );

                fn from_inner(inner: Self::Inner) -> Self {
                    let _inner = inner.value;
                    $crate::_enum_from_inner! { _inner => {
                        $($Var { $($field),* } ),*
                    } }
                }

                fn print_tree(
                    &self,
                    _cx: &$crate::ast::print::PrintContext,
                    _f: &mut ::core::fmt::Formatter,
                ) -> ::core::fmt::Result {
                    match *self {$(
                        Self::$Var{ $(ref $field),* } => {
                            if _cx.is_debug() {
                                _f.write_str(::core::concat!(
                                    ::core::stringify!($Name),
                                    "::",
                                    ::core::stringify!($Var),
                                    " -> ",
                                ))?;
                            }
                            _cx.debug_rule(_f, [$($field as &dyn $crate::ast::Rule),*])
                        }
                    )*}
                }

                fn name() -> &'static str {
                    ::core::stringify!($Name)
                }
            }
        };

        impl ::core::fmt::Debug for $Name {
            fn fmt(&self, _f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                match *self {$(
                    Self::$Var{ $(ref $field),* } => {
                        _f.write_str(::core::concat!(
                            ::core::stringify!($Name),
                            "::",
                            ::core::stringify!($Var),
                            " -> ",
                        ))?;
                        _f.debug_set()$(.entry($field))*.finish()
                    }
                )*}
            }
        }
    };
}

#[doc = include_str!("define_rule.md")]
#[macro_export]
macro_rules! define_rule {
    ($(
        $(#$attr:tt)*
        $vis:vis struct $Name:ident {
            $(
                $(#$field_attr:tt)*
                $field_vis:vis $field:ident
            ),* $(,)?
        }
    )*) => {$(
        $crate::_define_rule! {
            $(#$attr)*
            $vis struct $Name { $(
                $(#$field_attr)*
                $field_vis $field,
            )* }
        }
    )*};

    ($(
        $(#$attr:tt)*
        $vis:vis enum $Name:ident {
            $(
                $(#$variant_attr:tt)*
                $Variant:ident {
                    $(
                        $(#$field_attr:tt)*
                        $field_vis:vis $field:ident
                    ),* $(,)?
                }
            ),* $(,)?
        }
    )*) => {$(
        $crate::_define_rule! {
            $(#$attr)*
            $vis enum $Name { $(
                $($variant_attr)*
                $Variant { $(
                    $(#$field_attr)*
                    $field_vis $field,
                )* },
            )* }
        }
    )*};

    // Can accept either struct or enum; the above forms are mostly for illustrative purposes
    ($(
        $(#$attr:tt)*
        $vis:vis $kind:ident $Name:ident { $($x:tt)* }
    )*) => {$(
        $crate::_define_rule! {
            $(#$attr)*
            $vis $kind $Name {$($x)*}
        }
    )*};
}
