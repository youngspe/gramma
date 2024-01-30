#[doc(hidden)]
#[macro_export]
macro_rules! _into_pairs {
    ($x:tt $($y:tt)+ ) => { ($x,  $crate::_into_pairs! { $($y)+ }) };
    ($x:tt) => { $x };
}

#[doc(hidden)]
#[macro_export]
macro_rules! _into_either_ty {
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
            $crate::Either::Left($crate::_into_pairs!($($field0)*)) => Self::$Var0 { $($field0),* },
            $crate::Either::Right(inner) => $crate::_enum_from_inner! {
                inner => { $($Var { $($field),* }),+ }
            }
        }
    };
    { $inner:expr => {
        $Var0:ident { $($field0:ident),* $(,)? } $(,)?
    }} => {
        match $inner {
            $crate::_into_pairs!($($field0)*) => Self::$Var0 { $($field0),* },
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! _define_node {
    (
        $(#$attr:tt)*
        $vis:vis struct $Name:ident {
            $($field_vis:vis $field:ident: $Field:ty),* $(,)?
        }
    ) => {
        $(#$attr)*
        $vis struct $Name {
            $($field_vis $field: $Field,)*
        }

        impl $crate::ast::TransformRule for $Name {
            type Inner = $crate::_into_pairs!($(($Field))*);

            fn from_inner($crate::_into_pairs!($($field)*): Self::Inner) -> Self {
                Self { $($field),* }
            }

            fn print_tree(
                &self,
                cx: &$crate::ast::print::PrintContext,
                f: &mut ::core::fmt::Formatter,
            ) -> ::core::fmt::Result {
                let Self { $($field), * } = self;
                f.write_str(::core::stringify!($Name))?;
                f.write_str(" -> ")?;
                cx.debug_rule(f, [$($field as &dyn $crate::ast::Rule),*])
            }

            fn name() -> &'static str {
                ::core::stringify!($Name)
            }
        }

        impl ::core::fmt::Debug for $Name {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                let Self { $($field), * } = self;
                f.write_str(::core::stringify!($Name))?;
                f.write_str(" -> ")?;
                f.debug_set()$(.entry($field))*.finish()
            }
        }
    };

    (
        $(#$attr:tt)*
        $vis:vis enum $Name:ident {
            $($Var:ident { $($field:ident : $Field:ty),* $(,)? }),* $(,)?
        }
    ) => {
        $(#$attr)*
        $vis enum $Name {
            $($Var { $($field: $Field,)* }),*
        }

        impl $crate::ast::TransformRule for $Name {
            type Inner = $crate::_into_either_ty!(
                $( $crate::_into_pairs!($(($Field))*) ),*
            );

            fn from_inner(inner: Self::Inner) -> Self {
                $crate::_enum_from_inner! { inner => {
                    $($Var { $($field),* } ),*
                } }
            }

            fn print_tree(
                &self,
                cx: &$crate::ast::print::PrintContext,
                f: &mut ::core::fmt::Formatter,
            ) -> ::core::fmt::Result {
                match self {$(
                    Self::$Var{ $($field),* } => {
                        if cx.is_debug() {
                            f.write_str(::core::concat!(
                                ::core::stringify!($Name),
                                "::",
                                ::core::stringify!($Var),
                                " -> ",
                            ))?;
                        }
                        cx.debug_rule(f, [$($field as &dyn $crate::ast::Rule),*])
                    }
                )*}
            }

            fn name() -> &'static str {
                ::core::stringify!($Name)
            }
        }

        impl ::core::fmt::Debug for $Name {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                match self {$(
                    Self::$Var{ $($field),* } => {
                        f.write_str(::core::concat!(
                            ::core::stringify!($Name),
                            "::",
                            ::core::stringify!($Var),
                            " -> ",
                        ))?;
                        f.debug_set()$(.entry($field))*.finish()
                    }
                )*}
            }
        }
    };
}

#[macro_export]
macro_rules! define_node {
    ($(
        $(#$attr:tt)*
        $vis:vis $kind:ident $Name:ident {$($x:tt)*}
    )*) => {$(
        $crate::_define_node! {
            $(#$attr)*
            $vis $kind $Name {$($x)*}
        }
    )*};
}
