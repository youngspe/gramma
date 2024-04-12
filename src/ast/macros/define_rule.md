Define AST rules by declaring `struct`s and `enum`s.

# Basic Example
```rust
// Define Ident, LParen, RParen, Whitespace here:
gramma::define_token! {
    /* ... */
#    #[pattern(matcher = word_boundary() + !precedes(ascii_digit()) + word().repeat(1..).simple() )]
#    struct Ident;
#    #[pattern(exact = "(")]
#    struct LParen;
#    #[pattern(exact = ")")]
#    struct RParen;
#    #[pattern(matcher = whitespace().repeat(..).simple())]
#    struct Whitespace;
}
gramma::define_rule! {
 struct Group {
     l_paren: LParen,
     exprs: Vec<Expr>,
     r_paren: RParen,
 }
 // Enum rules may only have struct-like variants:
 #[transform(ignore_around<Whitespace>)]
 enum Expr {
     Group { group: Group },
     Ident { ident: Ident },
 }
}

let src = "
(list
    (add a b)
    (mul (sub c d) e)
)
";

let expr: Expr = gramma::parse_tree::<_, 1>(src).unwrap();

let display = format!("{:#}", gramma::display_tree(src, &expr));
assert_eq!(display, r#"Group -> {
    "(",
    [
        <Ident "list">,
        Group -> {
            "(",
            [
                <Ident "add">,
                <Ident "a">,
                <Ident "b">,
            ],
            ")",
        },
        Group -> {
            "(",
            [
                <Ident "mul">,
                Group -> {
                    "(",
                    [
                        <Ident "sub">,
                        <Ident "c">,
                        <Ident "d">,
                    ],
                    ")",
                },
                <Ident "e">,
            ],
            ")",
        },
    ],
    ")",
}"#);
```
# Transformations
In the example above, `Expr` was given the attribute
`#[transform(ignore_around<Whitespace>)]`.
Transformations, denoted by the `#[transform(...)]` attribute,
modify the way a rule is parsed.
All built-in transformations are declared as types in the
[ast::transform](crate::ast::transform) module.
Some of the more common transformations are:
<table>
<thead><tr><th>Transformation</th><th>Description</th></tr></thead>
<tr><td>

[`ignore_before<S>`](ignore_before)
</td><td>

Parse an _optional_ `S` before the target rule and discard the result if found.
Useful for ignoring optional whitespace before the target rule.
</td></tr><tr><td>

[`ignore_after<S>`](ignore_after)
</td><td>


Parse an _optional_ `S` after the target rule and discard the result if found.
Useful for ignoring optional whitespace after the target rule.
</td></tr><tr><td>

[`ignore_around<S1, S2 = S1>`](ignore_around)
</td><td>


Parse an _optional_ `S1` before and `S2` after the target rule and discard the results if found.
Useful for ignoring optional whitespace around the target rule.
</td></tr><tr><td>

[`discard_before<S>`](discard_before)
</td><td>

Parse a _required_ `S` before the target rule and discard the result.
Useful for matching punctuation before the target rule.
</td></tr><tr><td>

[`discard_after<S>`](discard_after)
</td><td>

Parse a _required_ `S` after the target rule and discard the result.
Useful for matching punctuation after the target rule.
</td></tr><tr><td>

[`discard_around<S1, S2 = S1>`](discard_around)
</td><td>

Parse a _required_ `S1` before and `S2` after the target rule and discard the results.
Useful for matching punctuation around the target rule.
</td></tr><tr><td>

[`not<Invalid>`](not)
</td><td>

Rejects if the section matched by the target rule starts with `Invalid`.
</td></tr><tr><td>

[`for_each<X>`](for_each)
</td><td>

Apply the transformation `X` to each item in the target rule.
Useful when matching a `Vec` of rules.
</td></tr>
</table>

A `#[transform(...)]` can be applied to the entirety of a `struct` or `enum` rule,
an `enum` variant, or an individual field.