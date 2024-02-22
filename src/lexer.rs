use logos::{Logos, SpannedIter};

use crate::tokens::{Token, TokeniserError};

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Debug, Clone, PartialEq)]
pub enum LexicalError {
	InvalidToken(TokeniserError),
}

pub struct Lexer<'input> {
	// instead of an iterator over characters, we have a token iterator
	token_stream: SpannedIter<'input, Token<'input>>,
}

impl<'input> Lexer<'input> {
	pub fn new(input: &'input str) -> Self {
		// the Token::lexer() method is provided by the Logos trait
		Self { token_stream: Token::lexer(input).spanned() }
	}
}

impl<'input> Iterator for Lexer<'input> {
	type Item = Spanned<Token<'input>, usize, LexicalError>;

	fn next(&mut self) -> Option<Self::Item> {
		self.token_stream.next().map(|(token, span)| {
			match token {
				Ok(token) => Ok((span.start, token, span.end)),
				Err(error) => Err(LexicalError::InvalidToken(error))
			}
		})
	}
}