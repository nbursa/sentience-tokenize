# Sentience-tokenize

Tiny zero-dependency tokenizer for simple DSLs and config/query languages in Rust.  
Generic: drop it into parsers, rule engines, interpreters, or build tooling.  
Supports identifiers, numbers, strings, operators, and a small set of keywords.  
Designed for speed, clarity, and easy embedding.

[![Crates.io](https://img.shields.io/crates/v/sentience-tokenize.svg)](https://crates.io/crates/sentience-tokenize)
[![Docs](https://docs.rs/sentience-tokenize/badge.svg)](https://docs.rs/sentience-tokenize)
[![CI](https://github.com/nbursa/sentience-tokenize/actions/workflows/ci.yml/badge.svg)](https://github.com/nbursa/sentience-tokenize/actions)

---

## Features

- **Zero dependencies** (only std).
- **Token kinds**: identifiers, numbers, strings, parens/brackets/braces, `= + - * / ->`.
- **Keywords**: `true false if then else let rule and or`.
- **Spans** included for each token.
- **Whitespace & // comments** skipped.

---

## Spec

| Aspect       | Rules |
|--------------|------|
| Identifiers  | ASCII: `[A-Za-z_][A-Za-z0-9_]*` |
| Numbers      | Decimal integers/decimals; optional exponent `e\|E[+\-]d+`. Single dot allowed once; `..` is **not** consumed by numbers. |
| Strings      | Double-quoted. Escapes: `\n`, `\t`, `\r`, `\"`, `\\`. Unknown escapes = error. |
| Comments     | `//` to end-of-line. |
| Delimiters   | `(` `)` `{` `}` `[` `]` `,` `:` `;` |
| Operators    | `=`, `+`, `-`, `*`, `/`, `->` |
| Keywords     | `true`, `false`, `if`, `then`, `else`, `let`, `rule`, `and`, `or` |

The enum `TokenKind`, types `Token`/`Span`, functions `tokenize`/`tokenize_iter`, `LineMap`, and error types `LexError{Kind}` are part of the **stable API**.

## Stable API surface

- Types: `TokenKind`, `Token`, `Span`
- Functions: `tokenize(&str) -> Result<Vec<Token>, LexError>`, `tokenize_iter(&str)`
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

## Install

Add to `Cargo.toml`:

```toml
[dependencies]
sentience-tokenize = "0.1"
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
```

## Why?

- Small, standalone lexer - no macros, no regexes.
- Useful as a foundation for parsers, DSLs, or interpreters.
- Explicit spans for better error reporting.

## License

[MIT](./LICENSE) © 2025 Nenad Bursać
