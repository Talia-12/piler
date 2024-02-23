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
			TokeniserError::InvalidInteger(err.to_string())
	}
}

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
#[logos(error = TokeniserError)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token<'a> {
	#[token(":=")]
	Assign,

	#[token(":")]
	Colon,

	#[token(";")]
	Semicolon,

	#[token("..")]
	Range,

	#[token("(")]
	LParen,

	#[token(")")]
	RParen,

	#[token("[")]
	LBracket,

	#[token("]")]
	RBracket,

	#[token("=")]
	Equals,

	#[token("!=")]
	NEquals,

	#[token(">=")]
	GEquals,

	#[token(">")]
	Greater,

	#[token("<=")]
	LEquals,

	#[token("<")]
	Less,

	#[token("+")]
	Plus,
	
	#[token("-")]
	Minus,
	
	#[token("*")]
	Times,
	
	#[token("/")]
	Divide,
	
	#[token("begin")]
	Begin,
	
	#[token("call")]
	Call,
	
	#[token("const")]
	Const,
	
	#[token("do")]
	Do,
	
	#[token("else")]
	Else,
	
	#[token("end")]
	End,
	
	#[token("if")]
	If,
	
	#[token("procedure")]
	Procedure,
	
	#[token("read")]
	Read,
	
	#[token("then")]
	Then,
	
	#[token("type")]
	Type,
	
	#[token("var")]
	Var,
	
	#[token("while")]
	While,
	
	#[token("write")]
	Write,

	// Or regular expressions.
	#[regex("[a-zA-Z][a-zA-Z0-9]*", |lex| lex.slice())]
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
	use logos::Source;

use super::*;
	use super::Token::*;

	#[test]
	fn test_simple_lex() {
		let lex = Token::lexer("1 + (2 - a) - 12*4 + 22 / 13");

		let expected = vec![
			Number(1),
			Plus,
			LParen,
			Number(2),
			Minus,
			Identifier("a"),
			RParen,
			Minus,
			Number(12),
			Times,
			Number(4),
			Plus,
			Number(22),
			Divide,
			Number(13)
		];

		for (lexed, expected) in lex.into_iter().zip(expected.into_iter()) {
			assert!(lexed.is_ok());
			assert_eq!(lexed.unwrap(), expected);
		}
	}

	#[test]
	fn test_complex_lex() {
		// TODO: Preprocess remove comments
		let input_with_comments = { "
		
		const C = 42;
		type
			S = [0..C]; // subrange 0 to 42
		var
			b: boolean; // boolean variable
			r: int; // integer variable
			x: S; // subrange variable
			res: int;
		procedure sum() =
			var
				i: S; // local variable to sum
			begin // sum
				i := 0; res := 0;
				while i != x do
				begin
					res := res + i;
					i := i + 1
				end
			end; // sum
		begin // main
			read r;
			b := (r <= C); // b is boolean
			if b then
			x := r
			else
			x := 0;
			call sum();
			write res
		end

		" };

		let input = { "
		
		const C = 42;
		type
			S = [0..C];
		var
			b: boolean;
			r: int;
			x: S;
			res: int;
		procedure sum() =
			var
				i: S;
			begin
				i := 0; res := 0;
				while i != x do
				begin
					res := res + i;
					i := i + 1
				end
			end;
		begin
			read r;
			b := (r <= C);
			if b then
			x := r
			else
			x := 0;
			call sum();
			write res
		end

		" };

		let lex = Token::lexer(input).spanned();

		let expected = vec![
			Const, Identifier("C"), Equals, Number(42), Semicolon,
			Type,
				Identifier("S"), Equals, LBracket, Number(0), Range, Identifier("C"), RBracket, Semicolon,
			Var,
				Identifier("b"), Colon, Identifier("boolean"), Semicolon,
				Identifier("r"), Colon, Identifier("int"), Semicolon,
				Identifier("x"), Colon, Identifier("S"), Semicolon,
				Identifier("res"), Colon, Identifier("int"), Semicolon,
			Procedure, Identifier("sum"), LParen, RParen, Equals,
					Var,
						Identifier("i"), Colon, Identifier("S"), Semicolon,
					Begin, // sum
						Identifier("i"), Assign, Number(0), Semicolon, Identifier("res"), Assign, Number(0), Semicolon,
						While, Identifier("i"), NEquals, Identifier("x"), Do,
						Begin,
							Identifier("res"), Assign, Identifier("res"), Plus, Identifier("i"), Semicolon,
							Identifier("i"), Assign, Identifier("i"), Plus, Number(1),
						End,
					End, Semicolon,
			Begin, // main
				Read, Identifier("r"), Semicolon,
				Identifier("b"), Assign, LParen, Identifier("r"), LEquals, Identifier("C"), RParen, Semicolon,
				If, Identifier("b"), Then,
					Identifier("x"), Assign, Identifier("r"),
				Else,
					Identifier("x"), Assign, Number(0), Semicolon,
				Call, Identifier("sum"), LParen, RParen, Semicolon,
				Write, Identifier("res"),
			End
		];

		for ((lexed, span), expected) in lex.into_iter().zip(expected.into_iter()) {
			assert!(lexed.is_ok(), r#"Error lexing on span {:?}, "{}", error was: {:?}"#, span.clone(), input.slice(span.clone()).unwrap(), lexed.err().unwrap());
			assert_eq!(lexed.clone().unwrap(), expected, r#"Lexed span {:?} ({}) from input incorrectly as {:?}, should be {:?}."#, span.clone(), input.slice(span.clone()).unwrap(), lexed.unwrap(), expected);
		}
	}
}