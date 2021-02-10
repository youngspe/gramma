/// Used to convert nested Seq2's to a struct
#[macro_export]
#[doc(hidden)]
macro_rules! from_seq2 {
    ($type:ident { _range: $($range:expr)?; $($prop:tt),*$(,)? } = $value:expr ) => {{
        let $crate::from_seq2!(@nest_pairs $($prop),*) = $value;
        $crate::from_seq2!(@eval $type { $(_range: $range)?; [$(($prop))*] []})
    }};
    (@eval $type:ident { _range: $($range:expr)?; [] [$(($prop:tt))*] }) => {
        $type { _range: $($range)?, $($prop,)* }
    };
    (@eval $type:ident { _range: $($range:expr)?; [(_) $($rest:tt)*] [$($done:tt)*] }) => {
        $crate::from_seq2!(@eval $type { _range: $($range)?; [$($rest)*] [$($done)*]})
    };
    (@eval $type:ident { _range: $($range:expr)?; [($prop:tt) $($rest:tt)*] [$($done:tt)*] }) => {
        $crate::from_seq2!(@eval $type { _range: $($range)?; [$($rest)*] [$($done)* ($prop)]})
    };
    (@nest_pairs $prop:tt) => { $prop };
    (@nest_pairs $prop:tt, $($rest:tt),+) => {
        $crate::parse::Seq2($prop, $crate::from_seq2!(@nest_pairs $($rest),*))
    };
    (@skip_underscore _) => {};
    (@skip_underscore $prop:ident) => {$prop,};
}

#[doc(hidden)]
#[macro_export]
macro_rules! _grammar {
    // Do nothing if there are no rules left
    (@rule $m:ident $token:ty;) => {};
    // Match the syntax for union rules:
    (@rule $m:ident $token:ty; $(#[$meta:meta])* $vis:vis enum $name:ident =>
        {$(|)? $($v:ident $(($($ty:tt)+))?)|*}; $($rest:tt)*) => {
        $crate::_grammar!(
            @m $m $token; enum ($vis $name ($(#[$meta])*) [$(
                ($v : $crate::_grammar!(@vartype $token; ($v) ($($($ty)+)?)))
            )*]));
        $crate::_grammar!(@rule $m $token; $($rest)*);
    };
    // Match the syntax for sequence rules:
    (@rule $m:ident $token:ty; $(#[$meta:meta])* $vis:vis struct $name:ident =>
        {$($inner:tt)*}; $($rest:tt)*) => {
        $crate::_grammar!(@props_outer $token; (@m $m $token; struct ($vis $name ($(#[$meta])*))) {$($inner)*});
        $crate::_grammar!(@rule $m $token; $($rest)*);
    };
    // Match the syntax for rule aliases
    (@rule $m:ident $token:ty; $(#[$meta:meta])* $vis:vis type $name:ident =>
        {$($inner:tt)*}; $($rest:tt)*) => {
        $crate::_grammar!(@m $m $token; type ($vis $name ($(#[$meta])*) ($($inner)*)));
        $crate::_grammar!(@rule $m $token; $($rest)*);
    };
    // Handle invalid rules:
    (@rule $($rest:tt)*) => {compile_error!(stringify!(invalid rule: $($rest)*))};

    // implementation for union rules
    (@m ast $token:ty; enum ($vis:vis $name:ident ($(#[$meta:meta])*) [ $(($v:ident : $ty:ty))* ])) => {
        $(#[$meta])*
        #[derive(Debug)]
        $vis enum $name {
            $($v(<$ty as $crate::parse::BeginParse<'static, $token>>::Output),)*
        }
        $crate::_impl_parseable!($token, $name);
        impl $crate::parse::Ast for $name {
            fn range(&self) -> Option<(usize, usize)> {
                match self {
                    $(Self::$v(x) => x.range(),)*
                }
            }
        }
        impl<'a> $crate::parse::BeginParse<'a, $token> for $name {
            type Parser =
                <$crate::_grammar!(@union2_type Self; $($ty),*) as $crate::parse::BeginParse<'a, $token>>
                ::Parser;
            type Output = Self;
            fn begin_parse(
                tokens: &'a [$token],
                index: usize,
            ) -> $crate::parse::ParseResult<(Self::Parser, usize)>{
                Self::Parser::new(tokens, index)
            }
        }
        $(
            impl<'a> From<<$ty as $crate::parse::BeginParse<'a, $token>>::Output> for $name {
                fn from(x: <$ty as $crate::parse::BeginParse<'a, $token>>::Output) -> Self {
                    Self::$v(x)
                }
            }
            impl<'a> std::convert::TryFrom<$name> for <$ty as $crate::parse::BeginParse<'a, $token>>::Output {
                type Error = ();
                fn try_from(x: $name) -> Result<Self, ()> {
                    match x {
                        $name::$v(x) => Ok(x),
                        #[allow(unreachable_patterns)]
                        _ => Err(()),
                    }
                }
            }
            impl<'a> std::convert::TryFrom<&'a $name> for &'a <$ty as $crate::parse::BeginParse<'a, $token>>::Output {
                type Error = ();
                fn try_from(x: &'a $name) -> Result<Self, ()> {
                    match x {
                        $name::$v(x) => Ok(x),
                        #[allow(unreachable_patterns)]
                        _ => Err(()),
                    }
                }
            }
        )*
    };

    // implementation for sequence rules
    (@m ast $token:ty; struct (
        $vis:vis $name:ident
        ($(#[$meta:meta])*))
        {
            [$( (($($prop:ident)?) ($ty:ty)) )*]
            $(
                [$( (($($prop2:ident)?) ($ty2:ty)) )*]
            )*
        }
    ) => {
        $(#[$meta])*
        #[derive(Debug)]
        $vis struct $name {
            _range: Option<(usize, usize)>,
            $($($prop: <$ty as $crate::parse::BeginParse<'static, $token>>::Output,)?)*
        }
        $crate::_impl_parseable!($token, $name);
        impl $crate::parse::Ast for $name {
            fn range(&self) -> Option<(usize, usize)> { self._range }
        }
        impl<'a> $crate::parse::BeginParse<'a, $token> for $name {
            type Parser = <$crate::_grammar!(
                @union2_type Self;
                $crate::parse::Convert<$crate::_grammar!(@seq2_type $($ty,)*), $name>
                $(,$crate::parse::Convert<$crate::_grammar!(@seq2_type $($ty2,)*), $name>)*
            ) as $crate::parse::BeginParse<'a, $token>>::Parser;
            type Output = Self;

            fn begin_parse(
                tokens: &'a [$token],
                index: usize,
            ) -> $crate::parse::ParseResult<(Self::Parser, usize)> {
                Self::Parser::new(tokens, index)
            }
        }
        impl From<
            <$crate::_grammar!(@seq2_type $($ty,)*) as $crate::parse::BeginParse<'static, $token>>::Output,
        > for $name {
            fn from(
                other: <$crate::_grammar!(@seq2_type $($ty,)*) as $crate::parse::BeginParse<'static, $token>>::Output,
            ) -> Self {{
                use $crate::parse::Ast;
                let range = other.range();
                $crate::_grammar!(@call_from_seq2 $name {_range: range; [$(($($prop)?))*] []} = other)
            }}
        }
        $(impl From<
            <$crate::_grammar!(@seq2_type $($ty2,)*) as $crate::parse::BeginParse<'static, $token>>::Output,
        > for $name {
            fn from(
                other: <$crate::_grammar!(@seq2_type $($ty2,)*) as $crate::parse::BeginParse<'static, $token>>::Output,
            ) -> Self {{
                use $crate::parse::Ast;
                let range = other.range();
                $crate::_grammar!(@call_from_seq2 $name {_range: range; [$(($($prop2)?))*] []} = other)
            }}
        })*
    };

    // implementation for rule aliases
    (@m ast $token:ty; type ($vis:vis $name:ident ($(#[$meta:meta])*) ($($inner:tt)*))) => {
        $(#[$meta])*
        $vis type $name = $crate::_grammar!(@ptype $token; $($inner)*);
    };

    // handle invalid methods
    (@m $($rest:tt)*) => {compile_error!(stringify!(@m $($rest)*));};

    // @vartype: types to match enum variants of union rules:
    // automatically assign type with the name of the enum variant
    (@vartype $token:ty; ($v:ident) ()) => { $v };
    // use the type specified in the enum variant
    (@vartype $token:ty; ($v:ident) ($($ty:tt)+)) => { $crate::_grammar!(@ptype $token; $($ty)+) };

    // @ptype: allows extra syntax to specify AST types:
    // unwrap tokens wrapped in parentheses:
    (@ptype $token:ty; ($($x:tt)*)) => {$crate::_grammar!(@ptype $token; $($x)*)};
    // Convert Foo(X) to Foo<t!(X)>
    // Useful for combining generics and provided types
    // Example: Option(Box([(Foo)*])) => Option<Box<Vec<Foo>>>
    (@ptype $token:ty; $t:ident($($x:tt)*)) => {$t<$crate::_grammar!(@ptype $token; $($x)*)>};
    // Special syntax to define a List AST:
    // Example: [(Foo)(Bar)*(Baz)] => List<Foo, Bar, Baz>
    // Which creates Vec<Foo> when Foo is separated by Bar and ends with Baz, or empty when there is no Foo.
    // Use [(Foo)(Comma)*(Option(Comma))] to parse a comma-separated list of Foo with optional trailing comma
    // The separator and trailing rules are optional and default to Empty.
    // [(Foo)*] will simply parse a list of consecutive Foo's
    (@ptype $token:ty; [($($item:tt)+) $(($($delim:tt)+))? * $(($($trail:tt)+))?]) => {
        $crate::parse::List<
            $crate::_grammar!(@ptype $token; $($item)*),
            $crate::_grammar!(@default ($($crate::_grammar!(@ptype $token; $($delim)*))?) ($crate::parse::Empty)),
            $crate::_grammar!(@default ($($crate::_grammar!(@ptype $token; $($trail)*))?) ($crate::parse::Empty)),
        >
    };
    // Same as above, but there must be at least one item in the list.
    // Example: [(Foo)(Bar)+(Baz)] => NonEmptyList<Foo, Bar, Baz>, creating a Vec<Foo>
    (@ptype $token:ty; [($($item:tt)+) $(($($delim:tt)+))? + $(($($trail:tt)+))?]) => {
        $crate::parse::NonEmptyList<
            $crate::_grammar!(@ptype $token; $($item)*),
            $crate::_grammar!(@default ($($crate::_grammar!(@ptype $token; $($delim)*))?) ($crate::parse::Empty)),
            $crate::_grammar!(@default ($($crate::_grammar!(@ptype $token; $($trail)*))?) ($crate::parse::Empty)),
        >
    };
    // Default to simply parsing the given tokens as a type
    (@ptype $token:ty; $ty:ty) => {$ty};

    // @props_outer: parse the pipe delimited sequences into a list of (name?)(type)
    // Will call this macro will the given prefix followed by the sequences of names and types for each variant
    // of the rule
    // ignore leading commas and format for the real @props implementation:
    {@props_outer $token:ty; $pre:tt {| $($x:tt)*}} => {
        $crate::_grammar!(@props $token; $pre {$($x)*} [] {});
    };
    // ignore format for the real @props implementation:
    {@props_outer $token:ty; $pre:tt {$($x:tt)*}} => {
        $crate::_grammar!(@props $token; $pre {$($x)*} [] {});
    };

    // @props: perform a real work for @props_outer
    // format: (
    //      @props $token; (prefix)
    //      { contents of rule }
    //      [partially-constructed list for the current option]
    //      { all previously-constructed lists, one for each variant in the union }
    //      optional property name
    //  )

    // we found a pipe; add the current list to the list of parsed variants and start again with an empty list
    {@props $token:ty; $pre:tt { $(,)? | $($rest:tt)+} $out:tt {$($done:tt)*}} => {
        $crate::_grammar!(@props $token; $pre {$($rest)*} [] {$($done)* $out});
    };
    // we found the end; call the macro with the prefix and the parsed variants
    {@props $token:ty; ($($pre:tt)*) { $(,)? } $out:tt {$($done:tt)*}} => {
        $crate::_grammar!($($pre)* {$($done)* $out});
    };
    // this item in the sequence has a name $k. continue parsing but record that the current name is $k
    {@props $token:ty; $pre:tt {$(,)? $k:ident: $($rest:tt)*} $out:tt $done:tt} => {
        $crate::_grammar!(@props $token; $pre {$($rest)*} $out $done $k);
    };
    // match the @ptype syntax for Foo(Bar) => Foo<Bar>
    {@props $token:ty; $pre:tt {$(,)? $op:ident($($ty:tt)*) $($rest:tt)*} [$($out:tt)*] $done:tt $($k:ident)?} => {
        $crate::_grammar!(@props $token; $pre {$($rest)*} [
            $($out)*
            (($($k)?) ($crate::_grammar!(@ptype $token; $op($($ty)*))))
        ] $done);
    };
    // match the @ptype syntax for lists
    {@props $token:ty; $pre:tt {$(,)? [$($x:tt)*] $($rest:tt)*} [$($out:tt)*] $done:tt $($k:ident)?} => {
        $crate::_grammar!(@props $token; $pre {$($rest)*} [
            $($out)*
            (($($k)?) ($crate::_grammar!(@ptype $token; [$($x)*])))
        ] $done);
    };
    // match the @ptype syntax for types wrapped in parentheses
    {@props $token:ty; $pre:tt {$(,)? ($($x:tt)*) $($rest:tt)*} [$($out:tt)*] $done:tt $($k:ident)?} => {
        $crate::_grammar!(@props $token; $pre {$($rest)*} [
            $($out)*
            (($($k)?) ($crate::_grammar!(@ptype $token; ($($x)*))))
        ] $done);
    };
    // match a bare type followed by a comma
    {@props $token:ty; $pre:tt {$(,)? $ty:ty ,$($rest:tt)*} [$($out:tt)*] $done:tt $($k:ident)?} => {
        $crate::_grammar!(@props $token; $pre {,$($rest)*} [
            $($out)*
            (($($k)?) ($crate::_grammar!(@ptype $token; $ty)))
        ] $done);
    };
    // match a bare type not followed by a comma but possibly a pipe
    {@props $token:ty; $pre:tt {$(,)? $ty:ty $(|$($rest:tt)*)?} [$($out:tt)*] $done:tt $($k:ident)?} => {
        $crate::_grammar!(@props $token; $pre {$(|$($rest)*)?} [
            $($out)*
            (($($k)?) ($crate::_grammar!(@ptype $token; $ty)))
        ] $done);
    };
    // match a single token that somehow didn't resolve to a type
    // TODO: will this ever happen?
    {@props $token:ty; $pre:tt {$(,)? $ty:tt $($rest:tt)*} [$($out:tt)*] $done:tt $($k:ident)?} => {
        $crate::_grammar!(@props $token; $pre {$($rest)*} [
            $($out)*
            (($($k)?) ($crate::_grammar!(@ptype $token; $tt)))
        ] $done);
    };

    // @call_from_seq2 invoke the from_seq2! macro with relevant information specific to this macro
    // from_seq! is used in the From impls that convert nested Seq2's to user-defined rules

    // base case:
    (@call_from_seq2 $type:ident {_range: $range:expr; [] [$(($done:tt))*]} = $value:expr) => {
        $crate::from_seq2!($type {_range: $range; $($done,)*} = $value)
    };
    // case where the next item in the sequence is not named -- this has been parsed but the value will be discarded
    (@call_from_seq2 $type:ident {_range: $range:expr; [() $($rest:tt)*] [$($done:tt)*]} = $value:expr) => {
        $crate::_grammar!(@call_from_seq2 $type {_range: $range; [$($rest)*] [$($done)* (_)]} = $value)
    };
    // case where the next item has a named -- this will be added to the struct
    (@call_from_seq2 $type:ident {_range: $range:expr; [($prop:tt) $($rest:tt)*] [$($done:tt)*]} = $value:expr) => {
        $crate::_grammar!(@call_from_seq2 $type {_range: $range; [$($rest)*] [$($done)* ($prop)]} = $value)
    };

    // @seq2_type: expands to the underlying nested Seq2 type that will parse a sequence of rules
    // base case: if only one rule, use it directly instead of a Seq2
    (@seq2_type $ty:ty $(,)?) => {$ty};
    (@seq2_type $ty:ty, $($rest:ty),* $(,)?) => {
        $crate::parse::Seq2Def<$ty, $crate::_grammar!(@seq2_type $($rest,)*)>
    };

    // @union2_type: expands to the underlying nested Union2 type that will parse a union of rules
    // base case: if only one rule, use it directly instead of a Union2
    (@union2_type $out:ty; $ty:ty $(,)?) => {$ty};
    (@union2_type $out:ty; $ty:ty, $($rest:ty),* $(,)?) => {
        $crate::parse::Union2<$ty, $crate::_grammar!(@union2_type $out; $($rest,)*), $out>
    };

    // @default: helper that replaces an empty token list with the given tokens
    (@default () ($($def:tt)*)) => {$($def)*};
    (@default ($($val:tt)+) ($($def:tt)*)) => {$($val)*};

    (@define_t $token:ty; $d:tt) => {
        #[allow(unused_macros)]
        macro_rules! t {
            ($d($d x:tt)*) => {$crate::_grammar!(@ptype $token; $d( $d x )*)};
        }
    };

    // the main rule for this macro:
    // should begin with the type of token list to parse, followed by a semicolon
    // followed by any number of rule definitions
    ($token:ty; $($rest:tt)*) => {
        $crate::_grammar!(@define_t $token; $);
        $crate::_grammar!(@rule ast $token; $($rest)*);
    };
    // generate an error for any invalid invocation of this macro
    (@$($any:tt)+) => {
        compile_error!(stringify!(@$($any)*))
    }
}

/// Used to define rules for a context-free grammar
///
/// ## Usage
///
/// The contents of this block must begin with the type of token we wish to parse
/// followed by a semicolon.
/// Types representing rules in the grammar may be defined afterward.
///
/// ### Type aliases
///
/// Type aliases in this context use a special syntax.
/// Definitions of the form `[(Item)(Sep)*(Trail)]` alias to a rule that parses a list of `Item`
/// separated by `Sep` and ending with `Trail` into a `Vec<Item>`.
/// If `Sep` or `Trail` are not provided, they default to the `Empty` rule.
///
/// The item, separator, and trailing types must be in parentheses and will be parsed by the same rules
/// as a rule type alias.
///
/// Using `+` in place of `*` requires that there be at least one `Item` in the list.
///
/// Definitions of the form `Foo(Bar)` will be read as `Foo<Bar>`, where `Bar` is interpeted by the
/// same rules as a rule type alias.
///
/// Definitions that are just a plain type will alias to that type directly.
///
/// ```
/// # use gramma::{grammar, parse};
/// # gramma::tokens! { struct Comma => symbol(","); enum MyToken => {Comma}; }
/// grammar! { MyToken;
///     # struct Foo => {Comma};
///     /// Comma-separated list of `Foo` with optional trailing comma.
///     type MyList => { [(Foo)(Comma)*(Option(Comma))] };
///     /// Parse into a boxed `Foo` if possible, otherwise parse into `None`.
///     type MyBoxedOption => { Option(Box(Foo)) };
///     /// Alias directly to the type `MyList`.
///     type MyAlias => { MyList };
/// }
/// ```
///
/// ### Sequences
///
/// Types defined as `struct`s are interpreted as sequences of sub-rules.
/// Each sub-rule may be given a name, and the result of each named rule will be stored in the
/// absract syntax tree representing the sequence.
/// Types given here will be interpreted by the same rules as in a rule type alias.
///
/// Structs may even be defined as a union of sequences separated by a `|` character.
/// Each variant in the union must have the same set of named members, corresponding to the same type.
///
/// ```
/// use gramma::{grammar, parse, tokens};
/// use gramma::lex::{Eof, Identifier, Whitespace};
///
/// tokens! {
///     struct LParen => symbol("(");
///     struct RParen => symbol(")");
///     struct LBrace => symbol("{");
///     struct RBrace => symbol("}");
///     enum MyToken => {
///         | LParen | RParen
///         | LBrace | RBrace
///         | Identifier
///         | Whitespace
///         | Eof
///     };
/// }
///
/// grammar! { MyToken;
///     struct Parens1 => { LParen, id: Identifier, RParen };
///     struct Parens2 => {
///         | LParen, id: Identifier, RParen,
///         | LBrace, id: Identifier, RBrace,
///     };
/// }
///
/// assert!(parse::<MyToken, Parens1>("( a )").is_ok());
/// assert!(parse::<MyToken, Parens1>("{ b }").is_err());
/// assert!(parse::<MyToken, Parens2>("( c )").is_ok());
/// assert!(parse::<MyToken, Parens2>("{ d }").is_ok());
/// ```
#[macro_export]
macro_rules! grammar {
    ($token:ty; $($rest:tt)*) => {
        $crate::_grammar!($token; $($rest)*);
    }
}

/// Make the given token type implement `BeginParse` so we can use it in a rule.
/// Automatically called when a token is defined in _tokens!
#[doc(hidden)]
#[macro_export]
macro_rules! _impl_parse_for_token {
    ($ty:ty) => {
        impl<'a, T: $crate::lex::BasicToken + Clone> $crate::parse::BeginParse<'a, T> for $ty
        where
            T: std::convert::TryInto<$ty>,
        {
            type Parser =
                <$crate::parse::TokenAst<Self> as $crate::parse::BeginParse<'a, T>>::Parser;
            type Output = $crate::parse::TokenAst<Self>;

            fn begin_parse(
                tokens: &'a [T],
                index: usize,
            ) -> $crate::parse::ParseResult<(Self::Parser, usize)> {
                $crate::parse::TokenAst::<Self>::begin_parse(tokens, index)
            }
        }
    };
}

/// Implement Parseable<$token> for the given AST.
#[doc(hidden)]
#[macro_export]
macro_rules! _impl_parseable {
    ($token:ty, $ty:ty) => {
        impl $crate::parse::Parseable<$token> for $ty {
            fn parse(tokens: &[$token]) -> $crate::parse::ParseResult<Self> {
                let parser = <Self as $crate::parse::BeginParse<$token>>::begin_parse(tokens, 0)?.0;
                Ok($crate::parse::Parse::accept(parser))
            }
        }
    };
}
