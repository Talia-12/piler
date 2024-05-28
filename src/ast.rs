use variantly::Variantly;

#[derive(Debug, Variantly)]
pub enum Decl<'a> {
	List(Vec<Symbol<'a>>),
	Procedure(ProcedureEntry<'a>, Block<'a>)
}

#[derive(Debug, Variantly)]
pub enum Symbol<'a> {
	ConstDef { id: &'a str, c: Exp<'a> },
	TypeDef { id: &'a str, t: Type<'a> },
	VarDef(VarDef<'a>)
}

impl<'a> Symbol<'a> {
	pub fn const_def_ref(&'a self) -> Option<(&'a str, &'a Exp<'a>)> {
		match self {
			Self::ConstDef { id, c } => Some((*id, c)),
			_ => None
		}
	}

	pub fn type_def_ref(&'a self) -> Option<(&'a str, &'a Type<'a>)> {
		match self {
			Self::TypeDef { id, t } => Some((id, t)),
			_ => None
		}
	}
}

#[derive(Debug, Variantly)]
pub enum Type<'a> {
	Identifier(&'a str),
	Subrange { min: Exp<'a>, max: Exp<'a> }
}

impl<'a> Type<'a> {
	fn subrange_ref(&'a self) -> Option<(&'a Exp<'a>, &'a Exp<'a>)> {
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

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Scope {

}

#[derive(Debug, Variantly)]
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

impl<'a> Statement<'a> {
	pub fn assignment_ref(&'a self) -> Option<(&'a Exp<'a>, &'a Exp<'a>)> {
		match self {
			Self::Assignment { lvalue, e } => Some((lvalue, e)),
			_ => None
		}
	}

	pub fn call_ref(&'a self) -> Option<(ProcId<'a>, &())> {
		match self {
			Self::Call { id, params } => Some((id, params)),
			_ => None
		}
	}

	pub fn if_ref(&'a self) -> Option<(&Exp<'a>, &Box<Statement<'a>>, &Box<Statement<'a>>)> {
		match self {
			Self::If { cond, t, f } => Some((cond, t, f)),
			_ => None
		}
	}

	pub fn while_ref(&'a self) -> Option<(&Exp<'a>, &Box<Statement<'a>>)> {
		match self {
			Self::While { cond, s } => Some((cond, s)),
			_ => None
		}
	}
}


#[derive(Debug, Variantly)]
pub enum Exp<'a> {
	Error,
	Number(i64),
	Identifier(&'a str),
	// TODO: Variable,
	Binary { op: BinaryOperator, left: Box<Exp<'a>>, right: Box<Exp<'a>> },
	Unary { op: UnaryOperator, arg: Box<Exp<'a>> },
	Dereference(),    // TODO
	NarrowSubrange(), // TODO
	WidenSubrange()   // TODO
}

impl<'a> Exp<'a> {
	pub fn binary_ref(&'a self) -> Option<(&'a BinaryOperator, &'a Box<Exp<'a>>, &'a Box<Exp<'a>>)> {
		match self {
			Self::Binary { op, left, right } => Some((op, left, right)),
			_ => None
		}
	}

	pub fn unary_ref(&'a self) -> Option<(&'a UnaryOperator, &'a Box<Exp<'a>>)> {
		match self {
			Self::Unary { op, arg } => Some((op, arg)),
			_ => None
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Eq)]
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

	/*
	#[test]
	fn test_procedure_head() {
		let input = "procedure sum()";

		let expected = ProcedureEntry { id: "sum", params: (), };

		let mut errors = Vec::new();

		let parsed: Result<ProcedureEntry, ParseError> = parser::ProcedureHeadParser::new().parse(
			&mut errors,
			Lexer::new(input)
		);

		let parsed = match parsed {
			Ok(p) => p,
			Err(err) => panic!("error while parsing, {:?}", err),
		};

		assert_eq!(expected.id, parsed.id);
		assert_eq!(expected.params, parsed.params);
	}
	*/

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
				Decl::List(vec![ Symbol::ConstDef { id: "C", c: Number(42) } ]),
				Decl::List(vec![ Symbol::TypeDef { id: "S", t: Type::Subrange { min: Number(0), max: Identifier("C") } } ]),
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
						Assignment { lvalue: Identifier("i"), e: Number(0) },
						Assignment { lvalue: Identifier("res"), e: Number(0) },
						While {
							cond: Binary { op: NEquals, left: Box::new(Identifier("i")), right: Box::new(Identifier("x")) },
							s: Box::new(List(vec![
								Assignment { lvalue: Identifier("res"), e: Binary { op: Add, left: Box::new(Identifier("res")), right: Box::new(Identifier("i")) } },
								Assignment { lvalue: Identifier("i"), e: Binary { op: Add, left: Box::new(Identifier("i")), right: Box::new(Number(1)) } },
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
					f: Box::new(Assignment { lvalue: Identifier("x"), e: Number(0) })
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

		assert_eq!(expected_block.locals, parsed_block.locals); // TODO: This will get more complicated

		assert_statements_match(&expected_block.body, &parsed_block.body)
	}

	fn assert_decls_match(expected_decl: &Decl, parsed_decl: &Decl) {
		match expected_decl {
			Decl::List(expected_list) => {
				let parsed_list = parsed_decl.list_ref().unwrap_or_else(|| panic!("expected a Decl::List, found {:?}", parsed_decl));

				assert_eq!(expected_list.len(), parsed_list.len(), "Expected {} entries in decl list, found {} instead. The decls found were: {:?}", expected_list.len(), parsed_list.len(), parsed_list);

				expected_list.iter().zip(parsed_list.iter()).for_each(|(e, p)| assert_symbols_match(e, p));
			},
			Decl::Procedure(expected_entry, expected_block) => {
				let (parsed_entry, parsed_block) = parsed_decl.procedure_ref().unwrap_or_else(|| panic!("expected a Decl::Procedure, found {:?}", parsed_decl));
				
				assert_eq!(expected_entry.id, parsed_entry.id);
				assert_eq!(expected_entry.params, parsed_entry.params); // Doesn't do much yet

				assert_blocks_match(expected_block, parsed_block)
			},
		}
	}

	fn assert_symbols_match(expected_symbol: &Symbol, parsed_symbol: &Symbol) {
		match expected_symbol {
			Symbol::ConstDef { id: expected_id, c: expected_c } => {
				let (parsed_id, parsed_c) = parsed_symbol.const_def_ref().unwrap_or_else(|| panic!("expected a Symbol::ConstDef, found {:?}", parsed_symbol));
				assert_eq!(*expected_id, parsed_id);
				assert_exps_match(expected_c, parsed_c)
			},
			Symbol::TypeDef { id: expected_id, t: expected_t } => {
				let (parsed_id, parsed_t) = parsed_symbol.type_def_ref().unwrap_or_else(|| panic!("expected a Symbol::TypeDef, found {:?}", parsed_symbol));
				assert_eq!(*expected_id, parsed_id);
				assert_types_match(expected_t, parsed_t)
			},
			Symbol::VarDef(expected_v) => {
				let parsed_v = parsed_symbol.var_def_ref().unwrap_or_else(|| panic!("expected a Symbol::VarDef, found {:?}", parsed_symbol));

				assert_eq!(expected_v.var_id, parsed_v.var_id);
				assert_eq!(expected_v.type_id, parsed_v.type_id);
			},
		}
	}

	fn assert_statements_match(expected_statement: &Statement, parsed_statement: &Statement) {
		match expected_statement {
			Statement::Error => if let Statement::Error = parsed_statement { } else {
				panic!("expected the statement to parse as an error, instead got {:?}", parsed_statement)
			},
			Assignment { lvalue: expected_lvalue, e: expected_e } => {
				let (parsed_lvalue, parsed_e) = parsed_statement.assignment_ref().unwrap_or_else(|| panic!("expected a Statement::Assignment, found {:?}", parsed_statement));

				assert_exps_match(expected_lvalue, parsed_lvalue);
				assert_exps_match(expected_e, parsed_e);
			},
			Read(expected_exp) => {
				let parsed_exp = parsed_statement.read_ref().unwrap_or_else(|| panic!("expected a Statement::Read, found {:?}", parsed_statement));
				assert_exps_match(expected_exp, parsed_exp);
			},
			Write(expected_exp) => {
				let parsed_exp = parsed_statement.write_ref().unwrap_or_else(|| panic!("expected a Statement::Write, found {:?}", parsed_statement));
				assert_exps_match(expected_exp, parsed_exp);
			},
			Call { id: expected_id, params: expected_params } => {
				let (parsed_id, parsed_params) = parsed_statement.call_ref().unwrap_or_else(|| panic!("expected a Statement::Call, found {:?}", parsed_statement));
				assert_eq!(*expected_id, parsed_id);
				assert_eq!(*expected_params, *parsed_params);
			},
			List(expected_list) => {
				let parsed_list = parsed_statement.list_ref().unwrap_or_else(|| panic!("expected a Statement::List, found {:?}", parsed_statement));

				assert_eq!(expected_list.len(), parsed_list.len(), "Expected {} entries in statement list, found {} instead. The stateents found were: {:?}", expected_list.len(), parsed_list.len(), parsed_list);

				expected_list.iter().zip(parsed_list.iter()).for_each(|(p, e)| assert_statements_match(p, e));
			},
			If { cond: expected_cond, t: expected_t, f: expected_f } => {
				let (parsed_cond, parsed_t, parsed_f) = parsed_statement.if_ref().unwrap_or_else(|| panic!("expected a Statement::If, found {:?}", parsed_statement));

				assert_exps_match(expected_cond, parsed_cond);
				assert_statements_match(expected_t, parsed_t);
				assert_statements_match(expected_f, parsed_f);
			},
			While { cond: expected_cond, s: expected_s } => {
				let (parsed_cond, parsed_s) = parsed_statement.while_ref().unwrap_or_else(|| panic!("expected a Statement::While, found {:?}", parsed_statement));
				
				assert_exps_match(expected_cond, parsed_cond);
				assert_statements_match(expected_s, parsed_s);
			},
		}
	}

	fn assert_exps_match(expected_exp: &Exp, parsed_exp: &Exp) {
		match expected_exp {
			Exp::Error => if let Exp::Error = parsed_exp { } else {
				panic!("expected the expression to parse as an error, instead got {:?}", parsed_exp)
			},
			Number(expected_number) => {
				let parsed_number = parsed_exp.number_ref().unwrap_or_else(|| panic!("expected a Exp::Number, found {:?}", parsed_exp));
				assert_eq!(*expected_number, *parsed_number);
			},
			Identifier(expected_id) => {
				let parsed_id = parsed_exp.identifier_ref().unwrap_or_else(|| panic!("expected a Exp::Identifier, found {:?}", parsed_exp));
				assert_eq!(*expected_id, *parsed_id);
			},
			Binary { op: expected_op, left: expected_left, right: expected_right } => {
				let (parsed_op, parsed_left, parsed_right) = parsed_exp.binary_ref().unwrap_or_else(|| panic!("expected a Exp::Binary, found {:?}", parsed_exp));

				assert_eq!(*expected_op, *parsed_op);
				assert_exps_match(expected_left, parsed_left);
				assert_exps_match(expected_right, parsed_right);
			},
			Unary { op: expected_op, arg: expected_arg } => {
				let (parsed_op, parsed_arg) = parsed_exp.unary_ref().unwrap_or_else(|| panic!("expected a Exp::Unary, found {:?}", parsed_exp));

				assert_eq!(*expected_op, *parsed_op);
				assert_exps_match(expected_arg, parsed_arg);
			},
			Dereference() => todo!(),
			NarrowSubrange() => todo!(),
			WidenSubrange() => todo!(),
		}
	}

	fn assert_types_match(expected_type: &Type, parsed_type: &Type) {
		match expected_type {
			Type::Identifier(expected_id) => {
				let parsed_id = parsed_type.identifier_ref().unwrap_or_else(|| panic!("expected a Type::Identifier, found {:?}", parsed_type));
				assert_eq!(*expected_id, *parsed_id)
			},
			Type::Subrange { min: expected_min, max: expected_max } => {
				let (parsed_min, parsed_max) = parsed_type.subrange_ref().unwrap_or_else(|| panic!("expected a Type::Subrange, found {:?}", parsed_type));

				assert_exps_match(expected_min, parsed_min);
				assert_exps_match(expected_max, parsed_max);
			},
		}
	}
}