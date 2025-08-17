use std::iter::Peekable;
use std::str::CharIndices;

mod error;
pub use error::{LexError, LexErrorKind};
mod span;
pub use span::LineMap;
mod iter;
pub use iter::{tokenize_iter, Tokens};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

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
        self.bump();
        while let Some((idx, ch)) = self.peek() {
            if ch.is_ascii_digit() {
                self.bump();
            } else if ch == '.' && !seen_dot {
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
                self.bump();
            } else if (ch == 'e' || ch == 'E') && !seen_exp {
                seen_exp = true;
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

pub fn tokenize(src: &str) -> Result<Vec<Token>, LexError> {
    Lexer::new(src).tokenize()
}

#[cfg(test)]
mod tests {
    use super::*;
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
}
