use std::{fs, env};

use piler::{lexer::Lexer, parser};

fn main() {
	let mut args = env::args();

	let file_path = args.nth(1).unwrap_or_else(|| panic!("piler PATH"));
	
	let mut errors = Vec::new();

	let contents = fs::read_to_string(&file_path).unwrap_or_else(|err| panic!("Couldn't read file: {}, err: {:?}", file_path, err));

	let lexer = Lexer::new(&contents);
	let parser = parser::ProgramParser::new();

	let result = parser.parse(&mut errors, lexer);

	println!("{:?}", result);
}
