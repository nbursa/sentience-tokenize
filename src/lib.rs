#![forbid(unsafe_code)]
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
//! - Patch releases fix bugs only; no public API changes.
//! - Minor releases (`0.x.y` → `0.(x+1).0`) may add new `TokenKind` variants or utilities without removing existing ones.
//!   Downstream code should avoid exhaustive `match` over `TokenKind`; prefer a `_` catch-all to remain forward-compatible.
//! - Any removal or change of existing public types/fields will be treated as a breaking change and called out explicitly.
//!
//! ## Spec (summary)
//! - **Identifiers**: `[A-Za-z_][A-Za-z0-9_]*`, ASCII only.
//! - **Numbers**: decimal integers/decimals with optional exponent (`e|E[+|-]d+`). A single dot is allowed once; `..` is not consumed by numbers.
//! - **Strings**: double-quoted with escapes `\n \t \r \" \\`. Raw newlines are accepted. Unknown escapes are errors.
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

/// Zero-copy token kind borrowing slices from the source.
/// Note: `String(&str)` contains the *literal contents between quotes* without unquoting; escapes (e.g. `\n`) are left as two characters.
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BorrowedTokenKind<'a> {
    Ident(&'a str),
    Number(&'a str),
    String(&'a str),
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

/// A zero-copy token with its [`BorrowedTokenKind`] and [`Span`].
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BorrowedToken<'a> {
    pub kind: BorrowedTokenKind<'a>,
    pub span: Span,
}

/// Token kind for the DSL. Variant set is stable across minor releases; new variants may be added in minor versions.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(tag = "type", content = "value"))]
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
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

/// A token with its [`TokenKind`] and [`Span`].
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

/// Streaming lexer. Prefer [`tokenize`] / [`tokenize_iter`] unless you need manual control.
#[derive(Debug)]
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

    fn lex_number_borrowed(&mut self, start: usize) -> Result<BorrowedToken<'a>, LexError> {
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
                    if last_was_dot {
                        break;
                    }
                    return Err(LexError::new(
                        LexErrorKind::InvalidNumber,
                        Span {
                            start,
                            end: idx + ch.len_utf8(),
                        },
                    ));
                }
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
                        ))
                    }
                }
            } else {
                break;
            }
        }

        let end = self.peek().map(|(j, _)| j).unwrap_or(self.src.len());
        Ok(BorrowedToken {
            kind: BorrowedTokenKind::Number(&self.src[start..end]),
            span: Span { start, end },
        })
    }

    /// Return next borrowed token or error without allocations. Strings validate escapes but keep them as-is in the slice.
    fn next_token_borrowed(&mut self) -> Option<Result<BorrowedToken<'a>, LexError>> {
        self.skip_ws_and_comments();
        let (i, c) = self.peek()?;

        // Strings: validate and borrow the raw contents between quotes
        if c == '"' {
            let start = i; // points at opening quote
            self.bump();
            let content_start = start + 1;
            loop {
                let Some((j, ch)) = self.bump() else {
                    return Some(Err(LexError::new(
                        LexErrorKind::UnterminatedString,
                        Span {
                            start,
                            end: self.src.len(),
                        },
                    )));
                };
                match ch {
                    '\\' => {
                        // require a following valid escape char, but do not build the string
                        let Some((k, esc)) = self.bump() else {
                            return Some(Err(LexError::new(
                                LexErrorKind::UnterminatedEscape,
                                Span {
                                    start: j,
                                    end: j + 1,
                                },
                            )));
                        };
                        match esc {
                            'n' | 't' | 'r' | '"' | '\\' => {
                                let _ = k;
                            }
                            _ => {
                                let escape_end = k + esc.len_utf8();
                                return Some(Err(LexError::new(
                                    LexErrorKind::InvalidEscape,
                                    Span {
                                        start: j,
                                        end: escape_end,
                                    },
                                )));
                            }
                        }
                    }
                    '"' => {
                        let end = j + 1; // closing quote included in token span
                        return Some(Ok(BorrowedToken {
                            kind: BorrowedTokenKind::String(&self.src[content_start..j]),
                            span: Span { start, end },
                        }));
                    }
                    _ => {}
                }
            }
        }

        // Numbers
        if c.is_ascii_digit() {
            match self.lex_number_borrowed(i) {
                Ok(tok) => return Some(Ok(tok)),
                Err(e) => return Some(Err(e)),
            }
        }

        // Idents / keywords
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
            let kind = match &self.src[start..end] {
                "true" => BorrowedTokenKind::True,
                "false" => BorrowedTokenKind::False,
                "if" => BorrowedTokenKind::If,
                "then" => BorrowedTokenKind::Then,
                "else" => BorrowedTokenKind::Else,
                "let" => BorrowedTokenKind::Let,
                "rule" => BorrowedTokenKind::Rule,
                "and" => BorrowedTokenKind::And,
                "or" => BorrowedTokenKind::Or,
                s => BorrowedTokenKind::Ident(s),
            };
            return Some(Ok(BorrowedToken {
                kind,
                span: Span { start, end },
            }));
        }

        // Arrow / minus
        if c == '-' {
            let start = i;
            self.bump();
            if let Some((j, '>')) = self.peek() {
                self.bump();
                return Some(Ok(BorrowedToken {
                    kind: BorrowedTokenKind::Arrow,
                    span: Span { start, end: j + 1 },
                }));
            } else {
                return Some(Ok(BorrowedToken {
                    kind: BorrowedTokenKind::Minus,
                    span: Span {
                        start,
                        end: start + 1,
                    },
                }));
            }
        }

        // Singles / error
        let start = i;
        self.bump();
        let tk = match c {
            '(' => BorrowedTokenKind::LParen,
            ')' => BorrowedTokenKind::RParen,
            '{' => BorrowedTokenKind::LBrace,
            '}' => BorrowedTokenKind::RBrace,
            '[' => BorrowedTokenKind::LBracket,
            ']' => BorrowedTokenKind::RBracket,
            ',' => BorrowedTokenKind::Comma,
            ':' => BorrowedTokenKind::Colon,
            ';' => BorrowedTokenKind::Semicolon,
            '=' => BorrowedTokenKind::Eq,
            '+' => BorrowedTokenKind::Plus,
            '*' => BorrowedTokenKind::Star,
            '/' => BorrowedTokenKind::Slash,
            other => {
                return Some(Err(LexError::new(
                    LexErrorKind::UnexpectedChar,
                    Span {
                        start,
                        end: start + other.len_utf8(),
                    },
                )));
            }
        };
        Some(Ok(BorrowedToken {
            kind: tk,
            span: Span {
                start,
                end: start + 1,
            },
        }))
    }

    /// Return next token or error without buffering the entire input.
    /// `None` means end.
    #[inline]
    pub(crate) fn next_token(&mut self) -> Option<Result<Token, LexError>> {
        self.skip_ws_and_comments();
        let (i, c) = self.peek()?;

        // Strings
        if c == '"' {
            let start = i;
            self.bump();
            let mut s = String::new();
            loop {
                let Some((j, ch)) = self.bump() else {
                    return Some(Err(LexError::new(
                        LexErrorKind::UnterminatedString,
                        Span {
                            start,
                            end: self.src.len(),
                        },
                    )));
                };
                match ch {
                    '\\' => {
                        // precizan span za escape sekvence
                        let Some((k, esc)) = self.bump() else {
                            return Some(Err(LexError::new(
                                LexErrorKind::UnterminatedEscape,
                                Span {
                                    start: j,
                                    end: j + 1,
                                },
                            )));
                        };
                        let ch = match esc {
                            'n' => '\n',
                            't' => '\t',
                            'r' => '\r',
                            '"' => '"',
                            '\\' => '\\',
                            _ => {
                                let escape_end = k + esc.len_utf8();
                                return Some(Err(LexError::new(
                                    LexErrorKind::InvalidEscape,
                                    Span {
                                        start: j,
                                        end: escape_end,
                                    },
                                )));
                            }
                        };
                        s.push(ch);
                    }
                    '"' => {
                        return Some(Ok(Token {
                            kind: TokenKind::String(s),
                            span: Span { start, end: j + 1 },
                        }));
                    }
                    _ => s.push(ch),
                }
            }
        }

        // Numbers
        if c.is_ascii_digit() {
            match self.lex_number(i) {
                Ok(tok) => return Some(Ok(tok)),
                Err(e) => return Some(Err(e)),
            }
        }

        // Idents / keywords
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
            return Some(Ok(Token {
                kind,
                span: Span { start, end },
            }));
        }

        // Arrow / minus
        if c == '-' {
            let start = i;
            self.bump();
            if let Some((j, '>')) = self.peek() {
                self.bump();
                return Some(Ok(Token {
                    kind: TokenKind::Arrow,
                    span: Span { start, end: j + 1 },
                }));
            } else {
                return Some(Ok(Token {
                    kind: TokenKind::Minus,
                    span: Span {
                        start,
                        end: start + 1,
                    },
                }));
            }
        }

        // Singles / error
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
                return Some(Err(LexError::new(
                    LexErrorKind::UnexpectedChar,
                    Span {
                        start,
                        end: start + other.len_utf8(),
                    },
                )));
            }
        };
        Some(Ok(Token {
            kind: tk,
            span: Span {
                start,
                end: start + 1,
            },
        }))
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        let mut out = Vec::new();
        while let Some(res) = self.next_token() {
            match res {
                Ok(tok) => out.push(tok),
                Err(e) => return Err(e),
            }
        }
        Ok(out)
    }
}

/// Tokenize the entire input and return a vector of tokens.
/// Errors include unterminated strings/escapes, invalid escapes, invalid numbers, and unexpected characters.
pub fn tokenize(src: &str) -> Result<Vec<Token>, LexError> {
    Lexer::new(src).tokenize()
}

/// Tokenize the entire input returning zero-copy tokens that borrow from `src`.
/// Strings are validated (including escapes) but their contents are *not* unescaped; the returned `&str` is the raw slice between quotes.
pub fn tokenize_borrowed(src: &str) -> Result<Vec<BorrowedToken<'_>>, LexError> {
    let mut lx = Lexer::new(src);
    let mut out = Vec::new();
    while let Some(res) = lx.next_token_borrowed() {
        match res {
            Ok(t) => out.push(t),
            Err(e) => return Err(e),
        }
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn error_kind_as_str_and_display_messages() {
        use super::{LexError, LexErrorKind, Span};
        let span = Span { start: 1, end: 3 };
        let cases: &[(LexErrorKind, &str, &str)] = &[
            (
                LexErrorKind::UnexpectedChar,
                "unexpected character",
                "unexpected char",
            ),
            (
                LexErrorKind::UnterminatedString,
                "unterminated string",
                "unterminated string",
            ),
            (
                LexErrorKind::UnterminatedEscape,
                "unterminated escape",
                "unterminated escape",
            ),
            (
                LexErrorKind::InvalidNumber,
                "invalid number",
                "invalid number",
            ),
            (
                LexErrorKind::InvalidEscape,
                "invalid escape sequence",
                "invalid escape",
            ),
        ];

        for (kind, as_str_msg, display_msg) in cases.iter().cloned() {
            assert_eq!(kind.as_str(), as_str_msg);
            let err = LexError::new(kind, span);
            let rendered = format!("{}", err);
            assert_eq!(
                rendered,
                format!("{} at {}..{}", display_msg, span.start, span.end)
            );
            let _e: &dyn std::error::Error = &err;
            let _dbg = format!("{:?}", err.clone());
            assert!(!_dbg.is_empty());
        }
    }
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
    fn streaming_iterator_matches_tokenize_and_propagates_error() {
        // identičan izlaz kao tokenize()
        let src = "let x = 1 + 2\nrule r() = \"ok\"";
        let vec_tokens = tokenize(src).unwrap();
        let iter_tokens: Result<Vec<_>, _> = tokenize_iter(src).collect();
        let iter_tokens = iter_tokens.unwrap();
        assert_eq!(vec_tokens, iter_tokens);

        // greška: invalid escape — prvi element je Err, posle toga iteracija se završava
        let src_err = "\"abc\\x\" rest";
        let mut it = tokenize_iter(src_err);
        match it.next() {
            Some(Err(e)) => assert!(matches!(e.kind, LexErrorKind::InvalidEscape)),
            other => panic!("expected first item to be Err, got {:?}", other),
        }
        assert!(it.next().is_none(), "iterator should end after error");
    }

    #[test]
    fn invalid_escape_span_is_precise() {
        // src contents: \"abc\\x\"
        let src = "\"abc\\x\"";
        let err = tokenize(src).unwrap_err();
        assert!(matches!(err.kind, LexErrorKind::InvalidEscape));
        // backslash at idx 4, 'x' at idx 5 -> span 4..6
        assert_eq!(err.span, Span { start: 4, end: 6 });
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

    #[cfg(feature = "serde")]
    #[test]
    fn serde_round_trip_token() {
        let toks = tokenize("let x = 1").unwrap();
        let json = serde_json::to_string(&toks).unwrap();
        let back: Vec<Token> = serde_json::from_str(&json).unwrap();
        assert_eq!(toks, back);
    }

    #[test]
    fn borrowed_basic_no_escapes() {
        let toks = tokenize_borrowed("let x = \"hi\" 123").unwrap();
        use BorrowedTokenKind as K;
        assert!(matches!(toks[0].kind, K::Let));
        assert!(matches!(toks[1].kind, K::Ident("x")));
        assert!(matches!(toks[3].kind, K::String("hi")));
        assert!(matches!(toks[4].kind, K::Number("123")));
    }

    #[test]
    fn borrowed_string_keeps_escapes() {
        let toks = tokenize_borrowed("\"a\\n\"").unwrap();
        use BorrowedTokenKind as K;
        assert!(matches!(toks[0].kind, K::String("a\\n")));
    }

    // --- Extra coverage for borrowed API ---
    #[test]
    fn borrowed_operators_and_singles() {
        use BorrowedTokenKind as K;
        // covers: Arrow vs Minus, and all singles
        let src = "()->{}[],:;=+ - * / ->";
        let toks = tokenize_borrowed(src).unwrap();
        let kinds: Vec<&'static str> = toks
            .iter()
            .map(|t| match t.kind {
                K::LParen => "LParen",
                K::RParen => "RParen",
                K::Arrow => "Arrow",
                K::LBrace => "LBrace",
                K::RBrace => "RBrace",
                K::LBracket => "LBracket",
                K::RBracket => "RBracket",
                K::Comma => "Comma",
                K::Colon => "Colon",
                K::Semicolon => "Semicolon",
                K::Eq => "Eq",
                K::Plus => "Plus",
                K::Minus => "Minus",
                K::Star => "Star",
                K::Slash => "Slash",
                _ => "Other",
            })
            .collect();
        assert_eq!(
            kinds,
            vec![
                "LParen",
                "RParen",
                "Arrow",
                "LBrace",
                "RBrace",
                "LBracket",
                "RBracket",
                "Comma",
                "Colon",
                "Semicolon",
                "Eq",
                "Plus",
                "Minus",
                "Star",
                "Slash",
                "Arrow"
            ]
        );
    }

    #[test]
    fn borrowed_keywords_and_idents() {
        use BorrowedTokenKind as K;
        let toks =
            tokenize_borrowed("true false if then else let rule and or foo _bar a1").unwrap();
        // Pick some spot checks
        assert!(matches!(toks[0].kind, K::True));
        assert!(matches!(toks[1].kind, K::False));
        assert!(matches!(toks[2].kind, K::If));
        assert!(matches!(toks[3].kind, K::Then));
        assert!(matches!(toks[4].kind, K::Else));
        assert!(matches!(toks[5].kind, K::Let));
        assert!(matches!(toks[6].kind, K::Rule));
        assert!(matches!(toks[7].kind, K::And));
        assert!(matches!(toks[8].kind, K::Or));
        assert!(matches!(toks[9].kind, K::Ident("foo")));
        assert!(matches!(toks[10].kind, K::Ident("_bar")));
        assert!(matches!(toks[11].kind, K::Ident("a1")));
    }

    #[test]
    fn borrowed_comments_skipped() {
        use BorrowedTokenKind as K;
        let toks = tokenize_borrowed("foo // comment\nbar").unwrap();
        assert!(matches!(toks[0].kind, K::Ident("foo")));
        assert!(matches!(toks[1].kind, K::Ident("bar")));
        assert_eq!(toks.len(), 2);
    }

    #[test]
    fn borrowed_numbers_errors_and_valid() {
        use BorrowedTokenKind as K;
        // valid
        let toks = tokenize_borrowed("1 1.0 1.2e-3").unwrap();
        assert!(matches!(toks[0].kind, K::Number("1")));
        assert!(matches!(toks[1].kind, K::Number("1.0")));
        assert!(matches!(toks[2].kind, K::Number("1.2e-3")));
        // invalid: second dot that's not a range
        let err = tokenize_borrowed("123.45.6").expect_err("second dot invalid");
        assert!(matches!(err.kind, LexErrorKind::InvalidNumber));
        // invalid: exponent without digits
        let err = tokenize_borrowed("1e+").expect_err("missing exponent digits");
        assert!(matches!(err.kind, LexErrorKind::InvalidNumber));
    }

    #[test]
    fn borrowed_string_errors() {
        // invalid escape
        let err = tokenize_borrowed("\"\\x\"").unwrap_err();
        assert!(matches!(err.kind, LexErrorKind::InvalidEscape));
        // unterminated string
        let err = tokenize_borrowed("\"abc").unwrap_err();
        assert!(matches!(err.kind, LexErrorKind::UnterminatedString));
        // unterminated escape
        let err = tokenize_borrowed("\"abc\\").unwrap_err();
        assert!(matches!(err.kind, LexErrorKind::UnterminatedEscape));
    }

    #[test]
    fn borrowed_unexpected_char_error() {
        let err = tokenize_borrowed("a @ b").expect_err("unexpected '@'");
        assert!(matches!(err.kind, LexErrorKind::UnexpectedChar));
        assert!(err.span.start < err.span.end);
    }
}
