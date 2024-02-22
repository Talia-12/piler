use std::num::ParseIntError;

use logos::Logos;

#[derive(Default, Debug, Clone, PartialEq)]
enum LexingError {
	InvalidInteger(String),
	#[default]
	NonAsciiCharacter,
}

/// Error type returned by calling `lex.slice().parse()` to u8.
impl From<ParseIntError> for LexingError {
	fn from(err: ParseIntError) -> Self {
		use std::num::IntErrorKind::*;
		match err.kind() {
			PosOverflow | NegOverflow => LexingError::InvalidInteger("overflow error".to_owned()),
			_ => LexingError::InvalidInteger("other error".to_owned()),
		}
	}
}

#[derive(Logos, Debug, PartialEq)]
#[logos(error = LexingError)]
enum Token<'a> {
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
    Text(&'a str),

    #[regex("[0-9]+", |lex| lex.slice().parse())]
		Number(u64),

    // We can also use this variant to define whitespace,
    // or any other matches we wish to skip.
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Whitespace,
}

fn main() {

}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_simple_lex() {
		let mut lex = Token::lexer("1 + 2 + a - 12*4 + 22 / 13");
	
		assert_eq!(lex.next(), Some(Ok(Token::Number(1))));
		assert_eq!(lex.next(), Some(Ok(Token::Add)));
		assert_eq!(lex.next(), Some(Ok(Token::Number(2))));
		assert_eq!(lex.next(), Some(Ok(Token::Add)));
		assert_eq!(lex.next(), Some(Ok(Token::Text("a"))));
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