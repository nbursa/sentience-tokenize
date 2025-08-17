//! sentience-tokenize — tiny zero-dep tokenizer for a simple DSL.
//!
//! ## Stable API surface (guaranteed across compatible releases)
//! - `TokenKind`, `Token`, `Span`
//! - `tokenize(&str) -> Result<Vec<Token>, LexError>`
//! - `tokenize_iter(&str)` returning an iterator of `Result<Token, LexError>`
//! - `LineMap` for byte→(line,col) mapping
//! - `LexError` and `LexErrorKind`
//!
//! ## Versioning
//! - Minor releases (`0.x.y` → `0.(x+1).0`) may add new token kinds behind minor bumps but will not break existing enum variants or fields.
//! - Patch releases only fix bugs and do not change public types or behavior except to correct spec-conformant errors.
//! - Any breaking change to the above surface will be accompanied by a semver-visible minor bump and noted in the changelog.
//!
//! ## Spec (summary)
//! - **Identifiers**: `[A-Za-z_][A-Za-z0-9_]*`, ASCII only.
//! - **Numbers**: decimal integers/decimals with optional exponent (`e|E[+|-]d+`). A single dot is allowed once; `..` is not consumed by numbers.
//! - **Strings**: double-quoted with escapes `\n \t \r \" \\`. Unknown escapes are errors.
//! - **Comments**: `//` to end-of-line.
//! - **Delimiters**: `() { } [ ] , : ;`.
//! - **Operators**: `= + - * / ->`.
//! - **Keywords**: `true false if then else let rule and or`.

use std::iter::Peekable;
use std::str::CharIndices;

mod error;
/// Error type and categories returned by the lexer; stable across minor versions.
pub use error::{LexError, LexErrorKind};
mod span;
/// Utility for mapping byte offsets to `(line, column)`; stable part of the public API.
pub use span::LineMap;
mod iter;
/// Iterator-based API over tokens. Yields `Result<Token, LexError>`.
pub use iter::{tokenize_iter, Tokens};

/// Token kind for the DSL. Variant set is stable across minor releases; new variants may be added in minor versions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Ident(String),
    Number(String),
    String(String),
    True,
    False,
    If,
    Then,
    Else,
    Let,
    Rule,
    And,
    Or,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Colon,
    Semicolon,
    Arrow,
    Eq,
    Plus,
    Minus,
    Star,
    Slash,
}

/// Byte span `[start, end)` into the original source.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

/// A token with its [`TokenKind`] and [`Span`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

/// Streaming lexer. Prefer [`tokenize`] / [`tokenize_iter`] unless you need manual control.
pub struct Lexer<'a> {
    src: &'a str,
    it: Peekable<CharIndices<'a>>,
    cur: Option<(usize, char)>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        let mut it = src.char_indices().peekable();
        let cur = it.next();
        Self { src, it, cur }
    }

    fn bump(&mut self) -> Option<(usize, char)> {
        let out = self.cur;
        self.cur = self.it.next();
        out
    }

    fn peek(&self) -> Option<(usize, char)> {
        self.cur
    }

    fn skip_ws_and_comments(&mut self) {
        loop {
            let mut progressed = false;
            while let Some((_, c)) = self.peek() {
                if c.is_whitespace() {
                    self.bump();
                    progressed = true;
                } else {
                    break;
                }
            }
            if let Some((_, '/')) = self.peek() {
                let mut clone = self.it.clone();
                if let Some((_, '/')) = clone.next() {
                    self.bump();
                    self.bump();
                    while let Some((_, c)) = self.peek() {
                        if c == '\n' {
                            break;
                        }
                        self.bump();
                    }
                    continue;
                }
            }
            if !progressed {
                break;
            }
        }
    }

    fn kw_or_ident(s: &str) -> TokenKind {
        match s {
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "if" => TokenKind::If,
            "then" => TokenKind::Then,
            "else" => TokenKind::Else,
            "let" => TokenKind::Let,
            "rule" => TokenKind::Rule,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            _ => TokenKind::Ident(s.to_string()),
        }
    }

    fn lex_number(&mut self, start: usize) -> Result<Token, LexError> {
        let mut seen_dot = false;
        let mut seen_exp = false;
        let mut last_was_dot = false;
        self.bump(); // consume first digit

        while let Some((idx, ch)) = self.peek() {
            if ch.is_ascii_digit() {
                self.bump();
                last_was_dot = false;
            } else if ch == '.' {
                if seen_dot {
                    // If we just consumed a dot and see another dot, it's a range `..` -> stop number here
                    if last_was_dot {
                        break;
                    }
                    // Otherwise, this is a second dot in the same numeric literal -> invalid number
                    return Err(LexError::new(
                        LexErrorKind::InvalidNumber,
                        Span {
                            start,
                            end: idx + ch.len_utf8(),
                        },
                    ));
                }
                // lookahead: do not consume if `..` or if no digit follows ("0.")
                let mut clone = self.it.clone();
                if let Some((_, next)) = clone.next() {
                    if next == '.' {
                        break;
                    }
                    if !next.is_ascii_digit() {
                        break;
                    }
                } else {
                    break;
                }
                seen_dot = true;
                last_was_dot = true;
                self.bump();
            } else if (ch == 'e' || ch == 'E') && !seen_exp {
                seen_exp = true;
                last_was_dot = false;
                self.bump();
                if let Some((_, sign)) = self.peek() {
                    if sign == '+' || sign == '-' {
                        self.bump();
                    }
                }
                match self.peek() {
                    Some((_, d)) if d.is_ascii_digit() => {}
                    _ => {
                        return Err(LexError::new(
                            LexErrorKind::InvalidNumber,
                            Span {
                                start,
                                end: idx + ch.len_utf8(),
                            },
                        ));
                    }
                }
            } else {
                break;
            }
        }

        let end = self.peek().map(|(j, _)| j).unwrap_or(self.src.len());
        Ok(Token {
            kind: TokenKind::Number(self.src[start..end].to_string()),
            span: Span { start, end },
        })
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        let mut out = Vec::new();
        loop {
            self.skip_ws_and_comments();
            let Some((i, c)) = self.peek() else {
                break;
            };

            if c == '"' {
                let start = i;
                self.bump();
                let mut s = String::new();
                loop {
                    let Some((j, ch)) = self.bump() else {
                        return Err(LexError::new(
                            LexErrorKind::UnterminatedString,
                            Span {
                                start,
                                end: self.src.len(),
                            },
                        ));
                    };
                    match ch {
                        '\\' => {
                            let Some((_, esc)) = self.bump() else {
                                return Err(LexError::new(
                                    LexErrorKind::UnterminatedEscape,
                                    Span { start, end: j + 1 },
                                ));
                            };
                            let ch = match esc {
                                'n' => '\n',
                                't' => '\t',
                                'r' => '\r',
                                '"' => '"',
                                '\\' => '\\',
                                _ => {
                                    // invalid escape
                                    return Err(LexError::new(
                                        LexErrorKind::InvalidEscape,
                                        Span {
                                            start,
                                            end: self.src.len(),
                                        },
                                    ));
                                }
                            };
                            s.push(ch);
                        }
                        '"' => {
                            out.push(Token {
                                kind: TokenKind::String(s),
                                span: Span { start, end: j + 1 },
                            });
                            break;
                        }
                        _ => s.push(ch),
                    }
                }
                continue;
            }

            if c.is_ascii_digit() {
                match self.lex_number(i) {
                    Ok(tok) => out.push(tok),
                    Err(e) => return Err(e),
                }
                continue;
            }

            if c.is_ascii_alphabetic() || c == '_' {
                let start = i;
                self.bump();
                while let Some((_, p)) = self.peek() {
                    if p.is_ascii_alphanumeric() || p == '_' {
                        self.bump();
                    } else {
                        break;
                    }
                }
                let end = self.peek().map(|(j, _)| j).unwrap_or(self.src.len());
                let kind = Self::kw_or_ident(&self.src[start..end]);
                out.push(Token {
                    kind,
                    span: Span { start, end },
                });
                continue;
            }

            if c == '-' {
                let start = i;
                self.bump();
                if let Some((j, '>')) = self.peek() {
                    self.bump();
                    out.push(Token {
                        kind: TokenKind::Arrow,
                        span: Span { start, end: j + 1 },
                    });
                } else {
                    out.push(Token {
                        kind: TokenKind::Minus,
                        span: Span {
                            start,
                            end: start + 1,
                        },
                    });
                }
                continue;
            }

            let start = i;
            self.bump();
            let tk = match c {
                '(' => TokenKind::LParen,
                ')' => TokenKind::RParen,
                '{' => TokenKind::LBrace,
                '}' => TokenKind::RBrace,
                '[' => TokenKind::LBracket,
                ']' => TokenKind::RBracket,
                ',' => TokenKind::Comma,
                ':' => TokenKind::Colon,
                ';' => TokenKind::Semicolon,
                '=' => TokenKind::Eq,
                '+' => TokenKind::Plus,
                '*' => TokenKind::Star,
                '/' => TokenKind::Slash,
                other => {
                    return Err(LexError::new(
                        LexErrorKind::UnexpectedChar,
                        Span {
                            start,
                            end: start + other.len_utf8(),
                        },
                    ))
                }
            };
            out.push(Token {
                kind: tk,
                span: Span {
                    start,
                    end: start + 1,
                },
            });
        }
        Ok(out)
    }
}

/// Tokenize the entire input and return a vector of tokens.
/// Errors include unterminated strings/escapes, invalid escapes, invalid numbers, and unexpected characters.
pub fn tokenize(src: &str) -> Result<Vec<Token>, LexError> {
    Lexer::new(src).tokenize()
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn numbers_second_dot_invalid_unless_range() {
        // second dot with digits on both sides -> invalid number
        let err = tokenize("123.45.6").expect_err("second dot should be invalid unless range");
        assert!(matches!(err.kind, LexErrorKind::InvalidNumber));

        // but range `1..2` must remain split (we already check UnexpectedChar for the dot itself)
        let err = tokenize("1..2").expect_err("range dot should not be consumed by number");
        assert!(matches!(err.kind, LexErrorKind::UnexpectedChar));
    }

    #[test]
    fn numbers_exponent_rules() {
        // valid exponent forms
        let toks = tokenize("1e10 1E+10 1.23e-4").unwrap();
        assert!(toks
            .iter()
            .any(|t| matches!(t.kind, TokenKind::Number(ref s) if s == "1e10")));
        assert!(toks
            .iter()
            .any(|t| matches!(t.kind, TokenKind::Number(ref s) if s == "1E+10")));
        assert!(toks
            .iter()
            .any(|t| matches!(t.kind, TokenKind::Number(ref s) if s == "1.23e-4")));

        // missing exponent digits is invalid
        let err = tokenize("1e+").expect_err("missing exponent digits");
        assert!(matches!(err.kind, LexErrorKind::InvalidNumber));

        let err = tokenize("2E-").expect_err("missing exponent digits");
        assert!(matches!(err.kind, LexErrorKind::InvalidNumber));
    }
    #[test]
    fn basic() {
        let code = r#"
            // sample
            let rule greet(name) = "hi, " + name
            if true and false then x = 1 else x = 2;
        "#;
        let toks = tokenize(code).unwrap();
        assert!(toks.iter().any(|t| matches!(t.kind, TokenKind::Let)));
        assert!(toks.iter().any(|t| matches!(t.kind, TokenKind::Rule)));
        assert!(toks.iter().any(|t| matches!(t.kind, TokenKind::String(_))));
    }

    #[test]
    fn numbers_and_ranges() {
        // valid decimals and exponents
        let toks = tokenize("1 1.0 1.2e-3").unwrap();
        assert!(toks
            .iter()
            .any(|t| matches!(t.kind, TokenKind::Number(ref s) if s == "1")));
        assert!(toks
            .iter()
            .any(|t| matches!(t.kind, TokenKind::Number(ref s) if s == "1.0")));
        assert!(toks
            .iter()
            .any(|t| matches!(t.kind, TokenKind::Number(ref s) if s == "1.2e-3")));

        // ensure don't swallow `..` as part of a number
        let err = tokenize("1..2").expect_err("should error on unexpected '.'");
        assert!(matches!(err.kind, LexErrorKind::UnexpectedChar));
    }

    #[test]
    fn string_escapes() {
        // valid escapes
        let toks = tokenize("\"a\\n\\t\\r\\\\\\\"\"").unwrap();
        assert!(matches!(toks[0].kind, TokenKind::String(_)));

        // invalid escape
        let err = tokenize("\"\\x\"").unwrap_err();
        assert!(matches!(err.kind, LexErrorKind::InvalidEscape));
    }

    #[test]
    fn numbers_trailing_dot_is_error() {
        let err = tokenize("0.").expect_err("trailing dot should error");
        assert!(matches!(err.kind, LexErrorKind::UnexpectedChar));
    }

    #[test]
    fn strings_empty_and_raw_newline_and_escapes() {
        // empty string
        let toks = tokenize("\"\"").unwrap();
        assert!(matches!(toks[0].kind, TokenKind::String(ref s) if s.is_empty()));

        // raw newline inside string is allowed by this lexer
        let toks = tokenize("\"a\nb\"").unwrap();
        assert!(matches!(toks[0].kind, TokenKind::String(ref s) if s == "a\nb"));

        // complex escapes: quote, backslash, tab -> resulting string is "\t
        let toks = tokenize("\"\\\"\\\\\t\"").unwrap();
        assert!(matches!(toks[0].kind, TokenKind::String(ref s) if s == "\"\\\t"));
    }

    #[test]
    fn strings_unterminated_and_unterminated_escape() {
        // unterminated string
        let err = tokenize("\"abc").expect_err("unterminated string");
        assert!(matches!(err.kind, LexErrorKind::UnterminatedString));

        // unterminated escape
        let err = tokenize("\"abc\\").expect_err("unterminated escape");
        assert!(matches!(err.kind, LexErrorKind::UnterminatedEscape));
    }

    #[test]
    fn idents_and_keywords() {
        let toks = tokenize("let letx _x1").unwrap();
        assert!(matches!(toks[0].kind, TokenKind::Let));
        assert!(matches!(toks[1].kind, TokenKind::Ident(ref s) if s == "letx"));
        assert!(matches!(toks[2].kind, TokenKind::Ident(ref s) if s == "_x1"));
    }

    #[test]
    fn comments_do_not_leak() {
        let toks = tokenize("foo // comment\nbar").unwrap();
        assert!(matches!(toks[0].kind, TokenKind::Ident(ref s) if s == "foo"));
        assert!(matches!(toks[1].kind, TokenKind::Ident(ref s) if s == "bar"));
        assert_eq!(toks.len(), 2);
    }

    #[test]
    fn unknown_char_errors_with_span() {
        let err = tokenize("a @ b").expect_err("unknown char '@'");
        assert!(matches!(err.kind, LexErrorKind::UnexpectedChar));
        assert!(err.span.start < err.span.end);
    }

    #[test]
    fn golden_small_input() {
        let src = "let rule f(x) = \"hi\" + x";
        let toks = tokenize(src).unwrap();
        use TokenKind::*;
        let kinds: Vec<&'static str> = toks
            .iter()
            .map(|t| match &t.kind {
                Let => "Let",
                Rule => "Rule",
                Ident(s) if s == "f" => "Ident(f)",
                LParen => "LParen",
                Ident(s) if s == "x" => "Ident(x)",
                RParen => "RParen",
                Eq => "Eq",
                String(s) if s == "hi" => "String(hi)",
                Plus => "Plus",
                Ident(s) if s == "x" => "Ident(x)",
                other => panic!("unexpected token in golden: {:?}", other),
            })
            .collect();
        assert_eq!(
            kinds,
            vec![
                "Let",
                "Rule",
                "Ident(f)",
                "LParen",
                "Ident(x)",
                "RParen",
                "Eq",
                "String(hi)",
                "Plus",
                "Ident(x)"
            ]
        );
    }
}
