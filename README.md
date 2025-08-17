# Sentience tokenizer

Tiny zero-dependency tokenizer for simple DSLs and config/query languages in Rust.  
Generic: drop it into parsers, rule engines, interpreters, or build tooling.  
Supports identifiers, numbers, strings, operators, and a small set of keywords.  
Designed for speed, clarity, and easy embedding.

[![Crates.io](https://img.shields.io/crates/v/sentience-tokenize.svg)](https://crates.io/crates/sentience-tokenize)
[![Docs.rs](https://img.shields.io/docsrs/sentience-tokenize)](https://docs.rs/sentience-tokenize)
[![Build Status](https://img.shields.io/github/actions/workflow/status/nbursa/sentience-tokenize/ci.yml?branch=main)](https://github.com/nbursa/sentience-tokenize/actions)
[![License](https://img.shields.io/github/license/nbursa/sentience-tokenize)](./LICENSE)
[![Coverage Status](https://img.shields.io/codecov/c/github/nbursa/sentience-tokenize/main.svg)](https://codecov.io/gh/nbursa/sentience-tokenize)

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
cargo run --example pretty
echo 'let rule greet(n) = "hi, " + n' | cargo run --example stream
```

## Dev

### Benchmark

```sh
cargo bench
```

### Fuzzing

Includes a cargo-fuzz setup.

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
