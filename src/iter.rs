use crate::error::LexError;
use crate::{Lexer, Token};

#[must_use]
#[derive(Debug)]
pub struct Tokens<'a> {
    inner: TokensInner<'a>,
}

#[derive(Debug)]
enum TokensInner<'a> {
    Lex(Lexer<'a>),
    Done,
}

#[must_use]
pub fn tokenize_iter(src: &str) -> Tokens<'_> {
    Tokens {
        inner: TokensInner::Lex(Lexer::new(src)),
    }
}

impl<'a> Iterator for Tokens<'a> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.inner {
            TokensInner::Lex(lex) => match lex.next_token() {
                Some(Ok(tok)) => Some(Ok(tok)),
                Some(Err(e)) => {
                    // After an error, terminate the stream.
                    self.inner = TokensInner::Done;
                    Some(Err(e))
                }
                None => {
                    self.inner = TokensInner::Done;
                    None
                }
            },
            TokensInner::Done => None,
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        // Streaming: we don't know remaining size.
        (0, None)
    }
}
