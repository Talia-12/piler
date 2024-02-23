use variantly::Variantly;

#[derive(Debug)]
pub enum Decl<'a> {
	List(Vec<Symbol<'a>>),
	Procedure(ProcedureEntry<'a>, Block<'a>)
}

impl<'a> Decl<'a> {
	fn list(&'a self) -> Option<&Vec<Symbol<'a>>> {
		match self {
			Self::List(vec) => Some(vec),
			_ => None
		}
	}

	fn procedure(&'a self) -> Option<(&ProcedureEntry, &Block)> {
		match self {
			Self::Procedure(entry, block) => Some((entry, block)),
			_ => None
		}
	}
}

#[derive(Debug)]
pub enum Symbol<'a> {
	ConstDef { id: &'a str, c: Exp<'a> },
	TypeDef { id: &'a str, t: Type<'a> },
	VarDef(VarDef<'a>)
}

impl<'a> Symbol<'a> {
	fn const_def(&'a self) -> Option<(&'a str, &'a Exp<'a>)> {
		match self {
			Self::ConstDef { id, c } => Some((*id, c)),
			_ => None
		}
	}

	fn type_def(&'a self) -> Option<(&'a str, &'a Type<'a>)> {
		match self {
			Self::TypeDef { id, t } => Some((id, t)),
			_ => None
		}
	}

	fn var_def(&'a self) -> Option<&'a VarDef<'a>> {
		match self {
			Self::VarDef(v) => Some(v),
			_ => None
		}
	}
}

#[derive(Debug)]
pub enum Type<'a> {
	Identifier(&'a str),
	Subrange { min: Exp<'a>, max: Exp<'a> }
}

impl<'a> Type<'a> {
	fn identifier(&'a self) -> Option<&'a str> {
		match self {
			Self::Identifier(id) => Some(id),
			_ => None
		}
	}

	fn subrange(&'a self) -> Option<(&'a Exp<'a>, &'a Exp<'a>)> {
		match self {
			Self::Subrange { min, max } => Some((min, max)),
			_ => None
		}
	}
}

#[derive(Debug)]
pub struct VarDef<'a> { pub var_id: &'a str, pub type_id: &'a str }

#[derive(Debug)]
pub struct ProcedureEntry<'a> {
	pub id: ProcId<'a>,
	pub params: () // TODO
}

#[derive(Debug)]
pub struct Block<'a> {
	pub decls: Vec<Decl<'a>>,
	pub body: Statement<'a>,
	pub locals: Scope
}

#[derive(Debug, Default)]
pub struct Scope {

}

#[derive(Debug)]
pub enum Statement<'a> {
	Error,
	Assignment { lvalue: Exp<'a>, e: Exp<'a> },
	Read(Exp<'a>),
	Write(Exp<'a>),
	Call { id: ProcId<'a>, params: () },
	List(Vec<Statement<'a>>),
	If { cond: Exp<'a>, t: Box<Statement<'a>>, f: Box<Statement<'a>> },
	While { cond: Exp<'a>, s: Box<Statement<'a>> }
}

#[derive(Debug)]
pub enum Exp<'a> {
	Error,
	Const(i64),
	Identifier(&'a str),
	// TODO: Variable,
	Binary { op: BinaryOperator, left: Box<Exp<'a>>, right: Box<Exp<'a>> },
	Unary { op: UnaryOperator, arg: Box<Exp<'a>> },
	Dereference(),
	NarrowSubrange(),
	WidenSubrange()
}

#[derive(Debug)]
pub enum BinaryOperator {
	Add,
	Sub,
	Mul,
	Div,
	Equals,
	NEquals,
	GEquals,
	Greater,
	LEquals,
	Less
}

#[derive(Debug)]
pub enum UnaryOperator {
	Positive,
	Negative
}

type ProcId<'a> = &'a str;

#[cfg(test)]
mod test {
	use super::*;
	use Statement::*;
	use Exp::*;
	use BinaryOperator::*;
	use crate::{lexer::Lexer, parser::{self, ParseError}};
	use lalrpop_util::lalrpop_mod;

	#[test]
	fn test_complex_parse() {
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

		let expected = Block {
			decls: vec![
				Decl::List(vec![ Symbol::ConstDef { id: "C", c: Const(42) } ]),
				Decl::List(vec![ Symbol::TypeDef { id: "S", t: Type::Subrange { min: Const(0), max: Identifier("C") } } ]),
				Decl::List(vec![
					Symbol::VarDef(VarDef { var_id: "b", type_id: "boolean" }),
					Symbol::VarDef(VarDef { var_id: "r", type_id: "int" }),
					Symbol::VarDef(VarDef { var_id: "x", type_id: "S" }),
					Symbol::VarDef(VarDef { var_id: "res", type_id: "int" }),
				]),
				Decl::Procedure(ProcedureEntry { id: "sum", params: () }, Block {
					decls: vec![
						Decl::List(vec![ Symbol::VarDef(VarDef { var_id: "i", type_id: "S" }) ]),
					],
					body: List(vec![
						Assignment { lvalue: Identifier("i"), e: Const(0) },
						Assignment { lvalue: Identifier("res"), e: Const(0) },
						While {
							cond: Binary { op: NEquals, left: Box::new(Identifier("i")), right: Box::new(Identifier("x")) },
							s: Box::new(List(vec![
								Assignment { lvalue: Identifier("res"), e: Binary { op: Add, left: Box::new(Identifier("res")), right: Box::new(Identifier("i")) } },
								Assignment { lvalue: Identifier("i"), e: Binary { op: Add, left: Box::new(Identifier("i")), right: Box::new(Const(1)) } },
							]))
						}
					]),
					locals: Scope { }
				})
			],
			body: Statement::List(vec![
				Read(Identifier("r")),
				Assignment { lvalue: Identifier("b"), e: Binary { op: LEquals, left: Box::new(Identifier("r")), right: Box::new(Identifier("C")) } },
				If {
					cond: Identifier("b"),
					t: Box::new(Assignment { lvalue: Identifier("x"), e: Identifier("r") }),
					f: Box::new(Assignment { lvalue: Identifier("x"), e: Const(0) })
				},
				Call { id: "sum", params: () },
				Write(Identifier("res"))
			]),
			locals: Scope { }
		};

		let mut errors = Vec::new();

		let program: Result<Block, ParseError> = parser::ProgramParser::new().parse(
			&mut errors,
			Lexer::new(input)
		);

		let parsed = match program {
			Ok(p) => p,
			Err(err) => panic!("error while parsing, {:?}", err),
		};

		assert_blocks_match(&expected, &parsed)
	}

	fn assert_blocks_match(expected_block: &Block, parsed_block: &Block) {
		assert_eq!(expected_block.decls.len(), parsed_block.decls.len(), "Expected {} decls, found {} instead. The decls found were: {:?}", expected_block.decls.len(), parsed_block.decls.len(), parsed_block.decls);

		expected_block.decls.iter().zip(parsed_block.decls.iter()).for_each(|(p, e)| assert_decls_match(p, e));

		todo!()
	}

	fn assert_decls_match(expected_decl: &Decl, parsed_decl: &Decl) {
		match expected_decl {
			Decl::List(expected_list) => {
				let parsed_list = parsed_decl.list().unwrap_or_else(|| panic!("expected a Decl::List, found {:?}", parsed_decl));

				assert_eq!(expected_list.len(), parsed_list.len(), "Expected {} entries in decl list, found {} instead. The decls found were: {:?}", expected_list.len(), parsed_list.len(), parsed_list);

				expected_list.iter().zip(parsed_list.iter()).for_each(|(e, p)| assert_symbols_match(e, p));
			},
			Decl::Procedure(expected_entry, expected_block) => {
				let (parsed_entry, parsed_block) = parsed_decl.procedure().unwrap_or_else(|| panic!("expected a Decl::Procedure, found {:?}", parsed_decl));
				
				assert_eq!(expected_entry.id, parsed_entry.id);
				assert_eq!(expected_entry.params, parsed_entry.params); // Doesn't do much yet

				assert_blocks_match(expected_block, parsed_block)
			},
		}
	}

	fn assert_symbols_match(expected_symbol: &Symbol, parsed_symbol: &Symbol) {
		match expected_symbol {
			Symbol::ConstDef { id: expected_id, c: expected_c } => {
				let (parsed_id, parsed_c) = parsed_symbol.const_def().unwrap_or_else(|| panic!("expected a Symbol::ConstDef, found {:?}", parsed_symbol));
				assert_eq!(*expected_id, parsed_id);
				assert_exps_match(expected_c, parsed_c)
			},
			Symbol::TypeDef { id: expected_id, t: expected_t } => {
				let (parsed_id, parsed_t) = parsed_symbol.type_def().unwrap_or_else(|| panic!("expected a Symbol::TypeDef, found {:?}", parsed_symbol));
				assert_eq!(*expected_id, parsed_id);
				assert_types_match(expected_t, parsed_t)
			},
			Symbol::VarDef(expected_v) => {
				let parsed_v = parsed_symbol.var_def().unwrap_or_else(|| panic!("expected a Symbol::VarDef, found {:?}", parsed_symbol));

				assert_eq!(expected_v.var_id, parsed_v.var_id);
				assert_eq!(expected_v.type_id, parsed_v.type_id);
			},
		}
	}

	fn assert_exps_match(expected_exp: &Exp, parsed_exp: &Exp) {
		match expected_exp {
			Exp::Error => if let Exp::Error = parsed_exp { } else {
				panic!("expected the expression to parse as an error, instead got {:?}", parsed_exp)
			},
			Const(expected_const) => {
				
				todo!()
			},
			Identifier(_) => todo!(),
			Binary { op, left, right } => todo!(),
			Unary { op, arg } => todo!(),
			Dereference() => todo!(),
			NarrowSubrange() => todo!(),
			WidenSubrange() => todo!(),
		}
	}

	fn assert_types_match(expected_type: &Type, parsed_type: &Type) {
		match expected_type {
			Type::Identifier(expected_id) => {
				let parsed_id = parsed_type.identifier().unwrap_or_else(|| panic!("expected a Type::Identifier, found {:?}", parsed_type));
				assert_eq!(*expected_id, parsed_id)
			},
			Type::Subrange { min: expected_min, max: expected_max } => {
				let (parsed_min, parsed_max) = parsed_type.subrange().unwrap_or_else(|| panic!("expected a Type::Subrange, found {:?}", parsed_type));

				assert_exps_match(expected_min, parsed_min);
				assert_exps_match(expected_max, parsed_max);
			},
		}
	}
}