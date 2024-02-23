use lalrpop_util::lalrpop_mod;
use piler::{lexer::Lexer, parser};

fn main() {
	let mut errors = Vec::new();

	println!("{:?}",
		parser::ExpParser::new().parse(
			&mut errors,
			Lexer::new("9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999")
		)
	);
}