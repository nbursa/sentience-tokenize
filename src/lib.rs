use std::iter::Peekable;
use std::str::CharIndices;

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
    pub message: &'static str,
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
                    progressed = true;
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
                        return Err(LexError {
                            message: "unterminated string",
                            span: Span {
                                start,
                                end: self.src.len(),
                            },
                        });
                    };
                    match ch {
                        '\\' => {
                            let Some((_, esc)) = self.bump() else {
                                return Err(LexError {
                                    message: "unterminated escape",
                                    span: Span { start, end: j + 1 },
                                });
                            };
                            s.push(match esc {
                                'n' => '\n',
                                't' => '\t',
                                'r' => '\r',
                                '"' => '"',
                                '\\' => '\\',
                                other => other,
                            });
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
                let start = i;
                self.bump();
                while let Some((_, p)) = self.peek() {
                    if p.is_ascii_digit() || p == '.' {
                        self.bump();
                    } else {
                        break;
                    }
                }
                let end = self.peek().map(|(j, _)| j).unwrap_or(self.src.len());
                out.push(Token {
                    kind: TokenKind::Number(self.src[start..end].to_string()),
                    span: Span { start, end },
                });
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
                    return Err(LexError {
                        message: "unexpected char",
                        span: Span {
                            start,
                            end: start + other.len_utf8(),
                        },
                    })
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
}
