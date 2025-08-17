use crate::error::LexError;
use crate::{Lexer, Token};

pub struct Tokens {
    inner: TokensInner,
}

enum TokensInner {
    Items(std::vec::IntoIter<Token>),
    Error(Option<LexError>),
}

pub fn tokenize_iter(src: &str) -> Tokens {
    match Lexer::new(src).tokenize() {
        Ok(v) => Tokens {
            inner: TokensInner::Items(v.into_iter()),
        },
        Err(e) => Tokens {
            inner: TokensInner::Error(Some(e)),
        },
    }
}

impl Iterator for Tokens {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.inner {
            TokensInner::Items(iter) => iter.next().map(Ok),
            TokensInner::Error(e) => e.take().map(Err),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match &self.inner {
            TokensInner::Items(iter) => iter.size_hint(),
            TokensInner::Error(Some(_)) => (1, Some(1)),
            TokensInner::Error(None) => (0, Some(0)),
        }
    }
}
