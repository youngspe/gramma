This module provides a domain-specific language for defining regular grammars.
These can be composed to form token patterns.

The resulting string matchers are unlikely to be as fast as a regex,
but you might just save some memory and decrease your binary size by using
these matchers instead of regex.

# Token patterns

```rust
gramma::define_token! {
    // Match any characters surrounded by quotes, as long as the end quote isn't
    // preceded by a backslash:
    #[pattern(matcher = {
        char('"') + char(..).repeat(..).lazy() + !follows(char('\\')) + char('"')
    })]
    struct Quotes;

    // Match a run of consecutive whitespace characters:
    #[pattern(matcher = whitespace().repeat(1..).simple())]
    struct Whitespace;

    // Idents must start at a word boundary and not start with a digit:
    #[pattern(matcher = {
        word_boundary() + !precedes(ascii_digit()) + word().repeat(1..).simple()
    })]
    struct Ident;

    #[pattern(matcher = exactly("=") | exactly("+="))]
    struct Assign;
}

gramma::define_rule! {
    struct Assignment {
        ident: Ident,
        #[transform(ignore_around<Whitespace>)]
        assign: Assign,
        quotes: Quotes,
    }
}

gramma::parse_tree::<Assignment, 1>(
    r#"s = "Hello, \"world\"!""#
).unwrap();
```

# Pattern components

All predefined pattern functions are located in the [patterns] module.
The most basic one is [`exactly()`](patterns::exactly); it simply tests whether
the source contains a specific string at the matcher's current location.

## `char` patterns

`char` patterns match exactly one `char`.
They are defined with the [`char()`](patterns::char) function,
which accepts a single parameter of one of the following types:

- `char`
- `Range<char>`
- `str` (meaning the pattern accepts any of the characters in the string)
- `fn(char) -> bool` (return `true` to accept the `char`)
- A nested tuple of any of the above

Predefined `char` patterns include:
- [`whitespace`](patterns::alphabetic)
- [`alphabetic`](patterns::alphabetic)
- [`numeric`](patterns::alphabetic)
- [`alphanumeric`](patterns::alphabetic)
- [`ascii_whitespace`](patterns::alphabetic)
- [`ascii_alphabetic`](patterns::alphabetic)
- [`ascii_alphanumeric`](patterns::alphabetic)
- [`ascii_digit`](patterns::alphabetic)
- [`ascii_hexdigit`](patterns::alphabetic)
- [`word`](patterns::alphabetic)

## Lookarounds

Lookaround patterns ([`follows()`](patterns::follows) and [`precedes()`](patterns::precedes)) test whether a pattern matches before or after the current location
without consuming any characters.

**TODO**
