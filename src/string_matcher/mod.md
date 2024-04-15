This module provides a domain-specific language for defining regular grammars.
These can be composed to form token patterns.

The resulting string matchers are unlikely to be as fast as a regex,
but you _might_ save some memory and will likely decrease your binary size by using
these matchers instead of regex.
In fact, you can remove the dependency on [`regex`](https://crates.io/crates/regex)
entirely by disabling the `regex` feature.

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
- Another `char` pattern
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
    (equivalent to a regex `\w`; matches `'a'..='z' | 'A'..='Z' | '0'..='9' | '_'`)

## Lookaround patterns

Lookaround patterns ([`follows()`](patterns::follows) and [`precedes()`](patterns::precedes)) test whether a pattern matches before or after the current location
without consuming any characters.

This can be used to make assertions on the boundaries of a pattern:

```rust
// A word is surrounded by whitespace:
let word_matcher = gramma::string_matcher!(
    follows(whitespace()) + alphabetic().repeat(1..) + precedes(whitespace())
);
assert!(word_matcher.match_string(2, "  foo ").is_some());
assert!(word_matcher.match_string(2, "  foo? ").is_none());
assert!(word_matcher.match_string(2, " .foo ").is_none());
```

It can also be used to make additional assertions on part of the matched section:

```rust
// Use a negated lookaround to ensure the number doesn't start with zero:
let decimal_matcher = gramma::string_matcher!(
    char('-').optional() + !precedes(char('0')) + ascii_digit().repeat(1..)
);
assert!(decimal_matcher.match_string(0, "12").is_some());
assert!(decimal_matcher.match_string(0, "-03").is_none());
```

# Operators

## Concatenate

The `+` operator concatenates two patterns.
Given patterns <var>a</var> and <var>b</var>,
<var>a</var> + <var>b</var> matches a sequence matching <var>a</var>
following a sequence matching <var>b</var>.

## Repeat

A pattern can be repeated using the infix `*` operator.
The right operand should take one of the following forms,
where <var>n</var> and <var>m</var> are integers of type `u32`:

| Operand | Meaning                                                      |
| ------- | ------------------------------------------------------------ |
| `n`     | Repeat exactly <var>n</var> times.                           |
| `n..=m` | Repeat at least <var>n</var> and at most <var>m</var> times. |
| `n..`   | Repeat at least <var>n</var> times.                          |
| `..=m`  | Repeat at most <var>m</var> times. Equivalent to `0..=m`.    |
| `..`    | Repeat any number of times. Equivalent to `0..`.             |

In addition to the `*` operator, repetitions can be declared with the
[`StringPattern::repeat()`] method or the [`repeat()`](patterns::repeat) function:

```rust
// These three forms are equivalent.
// Choose whichever is most readable in context:
let matchers = [
    gramma::string_matcher!(exactly("foo") * (2..=3) + exactly("bar")),
    gramma::string_matcher!(exactly("foo").repeat(2..=3) + exactly("bar")),
    gramma::string_matcher!(repeat(2..=3, exactly("foo")) + exactly("bar")),
];

// Verify these three matchers behave correctly in all these cases:
for (source, expected) in [
    ("foobar", None),
    ("foofoobar", Some(0..9)),
    ("foofoofoobar", Some(0..12)),
    ("foofoofoofoobar", None),
] {
    for matcher in matchers {
        assert_eq!(matcher.match_string(0, source), expected);
    }
}
```

If the range includes zero, repeated patterns may be skipped:

```rust
let matcher = gramma::string_matcher!(alphabetic().repeat(..) + char('.'));

assert_eq!(matcher.match_string(0, "abcd."), Some(0..5));
assert_eq!(matcher.match_string(0, "."), Some(0..1));
```

### Repetition strategies

There are three approaches to matching repeated patterns:

- Greedy (default)
- Lazy
- Simple

#### Greedy

A <dfn>greedy</dfn> repeat pattern attempts to match the most repititions
and will backtrack by attempting fewer reptitions if the rest of the pattern fails.

```rust
let matcher = gramma::string_matcher!(
    char('"') + char(..).repeat(..) + char('"')
    // The following is equivalent but not required because greedy is the default:
    // char('"') + char(..).repeat(..).greedy() + char('"')
);
let src = r#" "Hello", "world!" "#;
// Most likely, we only wanted to match the first pair of quotes, so greedy is
// not the right strategy here.
assert_eq!(&src[matcher.match_string(1, src).unwrap()], r#""Hello", "world!""#);
```

#### Lazy

A <dfn>lazy</dfn> repeat pattern attempts to match the fewest repititions
and will backtrack by attempting more reptitions if the rest of the pattern fails.
In general, this uses less memory than greedy.

```rust
let matcher = gramma::string_matcher!(
    char('"') + char(..).repeat(..).lazy() + char('"')
);
let src = r#" "Hello", "world!" "#;
// Lazy matches the first pair of quotes perfectly and is the ideal pattern for
// this source string.
assert_eq!(&src[matcher.match_string(1, src).unwrap()], r#""Hello""#);
```

#### Simple

A <dfn>simple</dfn> repeat pattern is similar to a greedy one.
The difference is that, if the rest of the pattern fails, it will reject immediately
rather than backtracking internally.
This is faster and uses less memory but may not work in every situation.

```rust
let matcher = gramma::string_matcher!(
    char('"') + char(..).repeat(..).simple() + char('"')
);
let src = r#" "Hello", "world!" "#;
// char(..).repeat(..).simple() matches any character until the end, after which
// there is no quote, so this is not a match.
assert_eq!(matcher.match_string(1, src), None);


// However, with a small change we can get the desired behavior:
let matcher = gramma::string_matcher!(
    // Match only non-quote characters within the repeat:
    char('"') + repeat(.., !char('"')).simple() + char('"')
);

assert_eq!(&src[matcher.match_string(1, src).unwrap()], r#""Hello""#);
```

## Optional

[`string_pattern.optional()`](StringPattern::optional) and
[`optional(string_pattern)`](patterns::optional) are short for
`string_pattern * ..=1`, meaning the pattern can either match once or be skipped altogether.
Since it's technically a repeat, you can call
[`.greedy()`](StringPattern::greedy),
[`.lazy()`](StringPattern::lazy), or
[`.simple()`](StringPattern::simple) on the output to modify its behavior.

```rust
let matcher = gramma::string_matcher!(
    exactly("Hello, world") + optional(char('!'))
);
assert_eq!(matcher.match_string(0, "Hello, world"), Some(0..12));
assert_eq!(matcher.match_string(0, "Hello, world!"), Some(0..13));
```

## Union

Patterns separated by a `|` make a union.
At least one sub-pattern of a union must match at the current source location
for the union to match.

If multiple sub-patterns would work, the first of those will be the one that matches.

```rust
let matcher = gramma::string_matcher!(
    (exactly("Hello") | exactly("Goodbye") | exactly("Howdy"))
        + exactly(", world!")
);

assert!(matcher.match_string(0, "Hello, world!").is_some());
assert!(matcher.match_string(0, "Goodbye, world!").is_some());
assert!(matcher.match_string(0, "Howdy, world!").is_some());
```

## Intersect

`char` matchers can be intersected with the `&` operator.
All sub-patterns must match at the current source location for the intersection
to match.

```rust
// Accept any letter but 'x':
let matcher = gramma::string_matcher!(alphabetic() & !char('x'));

// Matches both alphabetic() and !char('x'):
assert!(matcher.match_string(0, "a").is_some());
// Matches alphabetic() but not !char('x'):
assert!(matcher.match_string(0, "x").is_none());
// Matches !char('x') but not alphabetic():
assert!(matcher.match_string(0, "?").is_none());
```

## Negate

The `!` operator <dfn>negates</dfn> the following pattern,
meaning it matches when the inner pattern doesn't and vice versa.
Two kinds of patterns can be negated:

- `char`
- Lookaround

No other patterns can be negated as it would be ambiguous where the current position
in the string should be after the inner pattern fails to match.

### Negated `char` patterns

Given `char` pattern <code><var>A</var></code>, <code>!<var>A</var></code>
is a `char` pattern that accepts every character <code><var>A</var></code>
rejects and rejects every character <code><var>A</var></code> accepts.

For example:

- `!whitespace()` matches every non-whitespace character
- `!char('x')` matches any character except for 'x'
- `!char((whitespace(), alphabetic()))` matches any character that is neither
    whitespace nor a letter. It could also be represented as `!whitespace() & !alphabetic()`



### Negated `lookaround` patterns

Given pattern <code><var>A</var></code>:
- <code>!precedes(<var>A</var>)</code>
    accepts the beginning of any string <code><var>A</var></code> rejects,
    and rejects the beginning of any string <code><var>A</var></code> accepts.
- <code>!follows(<var>A</var>)</code>
    accepts the end of any string <code><var>A</var></code> rejects,
    and rejects the end of any string <code><var>A</var></code> accepts.
