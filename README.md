# Sentience tokenizer

Tiny **zero‑dependency** tokenizer for simple DSLs and config/query languages in Rust.  
Generic: drop it into parsers, rule engines, interpreters, or build tooling.  
Supports identifiers, numbers, strings, operators, and a small set of keywords.  
Clear spans, a streaming iterator, and a **zero‑copy** mode when you want pure speed.
Designed for speed, clarity, and easy embedding.

[![Crates.io](https://img.shields.io/crates/v/sentience-tokenize.svg)](https://crates.io/crates/sentience-tokenize)
[![Docs.rs](https://img.shields.io/docsrs/sentience-tokenize)](https://docs.rs/sentience-tokenize)
[![Build Status](https://img.shields.io/github/actions/workflow/status/nbursa/sentience-tokenize/ci.yml?branch=main)](https://github.com/nbursa/sentience-tokenize/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/nbursa/sentience-tokenize/blob/main/LICENSE)
[![codecov](https://codecov.io/gh/nbursa/sentience-tokenize/branch/main/graph/badge.svg)](https://codecov.io/gh/nbursa/sentience-tokenize)

---

## Quick start

Install:

```toml
[dependencies]
sentience-tokenize = "0.2.0"
```

Basic usage:

```rust
use sentience_tokenize::tokenize;

fn main() {
    let toks = tokenize("let x = 1").unwrap();
    for t in toks { println!("{:?} @{}..{}", t.kind, t.span.start, t.span.end); }
}
```

Streaming iterator (no allocation of full token vec):

```rust
use sentience_tokenize::tokenize_iter;

for item in tokenize_iter("let x = 1") {
    let t = item.unwrap();
    println!("{:?} @{}..{}", t.kind, t.span.start, t.span.end);
}
```

Zero-copy tokens (borrow `&str` slices from the source):

```rust
use sentience_tokenize::{tokenize_borrowed, BorrowedTokenKind};

let toks = tokenize_borrowed("\"hi\" 123").unwrap();
assert!(matches!(toks[0].kind, BorrowedTokenKind::String("hi")));
assert!(matches!(toks[1].kind, BorrowedTokenKind::Number("123")));
```

---

## Features

- **Zero dependencies** (only std).
- **Token kinds**: identifiers, numbers, strings, parens/brackets/braces, `= + - * / ->`.
- **Keywords**: `true false if then else let rule and or`.
- **Spans** included for each token.
- **Iterator API**: `tokenize_iter` yields `Result<Token, LexError>`.
- **Zero-copy API**: `tokenize_borrowed` returns `BorrowedToken<'_>`/`BorrowedTokenKind<'_>` with `&str` slices.
- **Whitespace & // comments** skipped.

---

## Optional features

- `serde`: derive `Serialize`/`Deserialize` for tokens and errors
- zero-copy API: `tokenize_borrowed` returns `BorrowedTokenKind<'a>`/`BorrowedToken<'a>` with `&str` slices (strings keep raw escapes)

---

## Spec

| Aspect       | Rules |
|--------------|------|
| Identifiers  | ASCII: `[A-Za-z_][A-Za-z0-9_]*` |
| Numbers      | Decimal integers/decimals; optional exponent `e\|E[+\-]d+`. Single dot allowed once; `..` is **not** consumed by numbers. |
| Strings      | Double-quoted. Escapes: `\n`, `\t`, `\r`, `\"`, `\\`. Unknown escapes = error. Raw newlines are accepted. |
| Comments     | `//` to end-of-line. |
| Delimiters   | `(` `)` `{` `}` `[` `]` `,` `:` `;` |
| Operators    | `=`, `+`, `-`, `*`, `/`, `->` |
| Keywords     | `true`, `false`, `if`, `then`, `else`, `let`, `rule`, `and`, `or` |

The enum `TokenKind`, types `Token`/`Span`, functions `tokenize`/`tokenize_iter`, `LineMap`, and error types `LexError{Kind}` are part of the **stable API**.

Note: new `TokenKind` variants may be added in minor releases; avoid exhaustive `match` without a `_` catch-all.

## Error Reporting

Lexing errors return a `LexError` with kind and span. Example with `LineMap`:

```rust
use sentience_tokenize::{tokenize, LineMap};

let src = "\"abc\\x\"";
let map = LineMap::new(src);
let err = tokenize(src).unwrap_err();
let (line, col) = map.to_line_col(err.span.start);
println!("{}:{}: {}", line, col, err.kind.as_str());
```

### Output

```sh
1:5: invalid escape sequence
```

## Stable API surface

- Types: `TokenKind`, `Token`, `Span`, `BorrowedTokenKind<'a>`, `BorrowedToken<'a>`
- Functions: `tokenize(&str) -> Result<Vec<Token>, LexError>`, `tokenize_iter(&str)`, `tokenize_borrowed(&str) -> Result<Vec<BorrowedToken<'_>>, LexError>`
- Utilities: `LineMap` for byte→(line, col)
- Errors: `LexError`, `LexErrorKind`

### Iterator API example

```rust
use sentience_tokenize::{tokenize_iter, TokenKind};

fn main() {
    for tok in tokenize_iter("let x = 1.2e-3") {
        let t = tok.unwrap();
        if let TokenKind::Ident(name) = &t.kind {
            println!("ident: {} @{}..{}", name, t.span.start, t.span.end);
        }
    }
}
```

### Zero-copy example

```rust
use sentience_tokenize::{tokenize_borrowed, BorrowedTokenKind};

fn main() {
    let toks = tokenize_borrowed("let x = \"hi\" 123").unwrap();
    match toks[3].kind {
        BorrowedTokenKind::String(s) => assert_eq!(s, "hi"),
        _ => unreachable!(),
    }
}
```

## Install

Add to `Cargo.toml`:

```toml
[dependencies]
sentience-tokenize = "0.2.0"
```

## Example

```rust
use sentience_tokenize::tokenize;

fn main() {
    let code = r#"
        // sample
        let rule greet(name) = "hi, " + name
        if true and false then x = 1 else x = 2;
    "#;

    let toks = tokenize(code).unwrap();
    for t in toks {
        println!("{:?} @{}..{}", t.kind, t.span.start, t.span.end);
    }
}
```

### Output (truncated)

```text
Let @18..21
Rule @22..26
Ident("greet") @27..32
LParen @32..33
Ident("name") @33..37
RParen @37..38
Eq @39..40
String("hi, ") @41..47
Plus @48..49
Ident("name") @50..54
...
```

## Run tests

```sh
cargo test
```

### Example binary

```sh
cargo run --example basic
cargo run --example pretty
echo 'let rule greet(n) = "hi, " + n' | cargo run --example stream
```

## Dev

### Benchmark

```sh
cargo bench
```

### Fuzzing

Fuzzing is supported via
`cargo-fuzz` (optional).

```sh
cargo bench
cargo test
cargo install cargo-fuzz
cargo fuzz run tokenize -- -runs=1000
```

## Why?

- Small, standalone lexer - no macros, no regexes.
- Useful as a foundation for parsers, DSLs, or interpreters.
- Explicit spans for better error reporting.

## License

[MIT](./LICENSE) © 2025 Nenad Bursać
