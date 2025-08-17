use crate::Span;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexErrorKind {
    UnexpectedChar,
    UnterminatedString,
    UnterminatedEscape,
    InvalidNumber,
    InvalidEscape,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub span: Span,
}

impl LexError {
    pub fn new(kind: LexErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl LexErrorKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            LexErrorKind::UnexpectedChar => "unexpected character",
            LexErrorKind::UnterminatedString => "unterminated string",
            LexErrorKind::UnterminatedEscape => "unterminated escape",
            LexErrorKind::InvalidNumber => "invalid number",
            LexErrorKind::InvalidEscape => "invalid escape sequence",
        }
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let msg = match self.kind {
            LexErrorKind::UnexpectedChar => "unexpected char",
            LexErrorKind::UnterminatedString => "unterminated string",
            LexErrorKind::UnterminatedEscape => "unterminated escape",
            LexErrorKind::InvalidNumber => "invalid number",
            LexErrorKind::InvalidEscape => "invalid escape",
        };
        write!(f, "{} at {}..{}", msg, self.span.start, self.span.end)
    }
}

impl std::error::Error for LexError {}
