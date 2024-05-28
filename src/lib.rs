pub mod ast;
pub mod lexer;
pub mod tokens;

pub mod parser {
	#![allow(clippy::all)]
	use lalrpop_util::lalrpop_mod;
	lalrpop_mod!(pub grammer);
	use self::lexer::LexicalError;

use super::*;
	pub use grammer::*;

	pub type ParseError<'a> = lalrpop_util::ParseError<usize, tokens::Token<'a>, LexicalError>;
	pub type ParseResult<'a> = Result<ast::Block<'a>, ParseError<'a>>;
}