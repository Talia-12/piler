use lalrpop_util::lalrpop_mod;
use piler::lexer::Lexer;

lalrpop_mod!(pub calculator1); // synthesized by LALRPOP

fn main() {
	let mut errors = Vec::new();

	println!("{:?}",
		calculator1::ExprParser::new().parse(
			&mut errors,
			Lexer::new("9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999")
		)
	);
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn calculator1() {
		let mut errors = Vec::new();
	
		assert!(calculator1::ExprParser::new().parse(&mut errors, Lexer::new("22")).is_ok());
		assert!(calculator1::ExprParser::new().parse(&mut errors, Lexer::new("(22)")).is_ok());
		assert!(calculator1::ExprParser::new().parse(&mut errors, Lexer::new("((((22))))")).is_ok());
		assert!(calculator1::ExprParser::new().parse(&mut errors, Lexer::new("((22)")).is_err());

		assert_eq!(calculator1::ExprParser::new().parse(&mut errors, Lexer::new("6*(12 + 3) / 3 - 11")), Ok(19));
	}
}