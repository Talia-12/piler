use piler::{lexer::Lexer, parser};

fn main() {
	let mut errors = Vec::new();

	println!("{:?}",
		parser::ExpParser::new().parse(
			&mut errors,
			Lexer::new("12*(999999999999+999999)")
		)
	);
}
