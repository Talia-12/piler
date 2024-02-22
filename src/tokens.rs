use core::fmt;
use std::num::ParseIntError;

use logos::Logos;

#[derive(Default, Debug, Clone, PartialEq)]
pub enum TokeniserError {
	InvalidInteger(String),
	#[default]
	NonAsciiCharacter
}

/// Error type returned by calling `lex.slice().parse()` to u8.
impl From<ParseIntError> for TokeniserError {
	fn from(err: ParseIntError) -> Self {
		use std::num::IntErrorKind::*;
		match err.kind() {
			PosOverflow | NegOverflow => TokeniserError::InvalidInteger("overflow error".to_owned()),
			_ => TokeniserError::InvalidInteger("other error".to_owned()),
		}
	}
}

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
#[logos(error = TokeniserError)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token<'a> {
	#[token("+")]
	Add,

	#[token("-")]
	Sub,

	#[token("*")]
	Mul,

	#[token("/")]
	Div,

	#[token("(")]
	LBrace,

	#[token(")")]
	RBrace,

	// Or regular expressions.
	#[regex("[a-zA-Z]+", |lex| lex.slice())]
	Identifier(&'a str),

	#[regex("[0-9]+", |lex| lex.slice().parse())]
	Number(i64)
}

impl<'a> fmt::Display for Token<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{:?}", self)
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_simple_lex() {
		let mut lex = Token::lexer("1 + (2 - a) - 12*4 + 22 / 13");
	
		assert_eq!(lex.next(), Some(Ok(Token::Number(1))));
		assert_eq!(lex.next(), Some(Ok(Token::Add)));
		assert_eq!(lex.next(), Some(Ok(Token::LBrace)));
		assert_eq!(lex.next(), Some(Ok(Token::Number(2))));
		assert_eq!(lex.next(), Some(Ok(Token::Sub)));
		assert_eq!(lex.next(), Some(Ok(Token::Identifier("a"))));
		assert_eq!(lex.next(), Some(Ok(Token::RBrace)));
		assert_eq!(lex.next(), Some(Ok(Token::Sub)));
		assert_eq!(lex.next(), Some(Ok(Token::Number(12))));
		assert_eq!(lex.next(), Some(Ok(Token::Mul)));
		assert_eq!(lex.next(), Some(Ok(Token::Number(4))));
		assert_eq!(lex.next(), Some(Ok(Token::Add)));
		assert_eq!(lex.next(), Some(Ok(Token::Number(22))));
		assert_eq!(lex.next(), Some(Ok(Token::Div)));
		assert_eq!(lex.next(), Some(Ok(Token::Number(13))));
	}
}