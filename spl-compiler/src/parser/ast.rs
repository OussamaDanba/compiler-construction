use std::fmt;

pub type Ident = String;

#[derive(Debug)]
pub struct SPL {
	pub vars: Vec<Variable>,
	pub funs: Vec<Function>
}

#[derive(Debug)]
pub struct Variable {
	pub name: Ident,
	pub vtype: Option<Type>,
	pub value: Expression
}

#[derive(Debug)]
pub struct Function {
	pub name: Ident,
	pub args: Vec<Ident>,
	pub ftype: Option<Type>,
	pub vars: Vec<Variable>,
	pub stmts: Vec<Statement>
}

#[derive(Debug)]
pub enum Type {
	TInt,
	TBool,
	TChar,
	TVoid,
	TTuple(Box<Type>, Box<Type>),
	TList(Box<Type>),
	TArrow(Vec<Type>, Box<Type>),
	TIdent(Ident)
}

#[derive(Debug)]
pub enum Statement {
	If(Expression, Vec<Statement>, Vec<Statement>),
	While(Expression, Vec<Statement>),
	Assignment(Ident, Vec<Field>, Expression),
	FunCall(Expression),
	Return(Option<Expression>)
}

#[derive(Debug)]
pub enum Expression {
	Ident(Ident, Vec<Field>),
	Op2(Box<Expression>, Op2, Box<Expression>),
	Op1(Op1, Box<Expression>),
	Lit(Literal),
	FunCall(Ident, Vec<Expression>),
	Tuple(Box<Expression>, Box<Expression>)
}

#[derive(Debug)]
pub enum Field {
	Head,
	Tail,
	First,
	Second
}

#[derive(Debug)]
pub enum Op2 {
	Addition,
	Subtraction,
	Multiplication,
	Division,
	Modulo,
	Equals,
	LessThan,
	GreaterThan,
	LessEquals,
	GreaterEquals,
	NotEquals,
	And,
	Or,
	Cons
}

#[derive(Debug)]
pub enum Op1 {
	Not,
	Negation
}

#[derive(Debug)]
pub enum Literal {
	Int(i64),
	Char(char),
	Bool(bool),
	EmptyList
}

fn indent(input: String) -> String {
	input.lines().map(|x| format!("\t{}\n", x)).collect()
}

impl fmt::Display for Statement {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", match *self {
			Statement::If(ref exp, ref if_stmts, ref else_stmts) =>
				format!("if({}) {{\n{}}}{}", exp, indent(if_stmts.iter()
					.map(|x| format!("{}\n", x)).collect::<Vec<String>>().concat()),
						if else_stmts.is_empty() {
							String::new()
						} else {
							format!(" else {{\n{}}}", indent(else_stmts.iter().map(|x| format!("{}\n", x)).collect::<Vec<String>>().concat()))
						}
				),
			Statement::While(ref exp, ref stmts) => format!("while({}) {{\n{}}}", exp,
				indent(stmts.iter().map(|x| format!("{}\n", x)).collect::<Vec<String>>().concat())),
			Statement::Assignment(ref ident, ref fields, ref exp) => format!("{}{} = {}", ident,
				fields.iter().map(|x| format!("{}", x)).collect::<Vec<String>>().concat(), exp),
			Statement::FunCall(ref exp) => format!("{};", exp),
			Statement::Return(ref exp) => match *exp {
				Some(ref x) => format!("return {};", x),
				_ => String::from("return;")
			}
		})
	}
}

impl fmt::Display for Expression {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", match *self {
			Expression::Ident(ref ident, ref fields) =>	format!("{}{}", ident,
				fields.iter().map(|x| format!("{}", x)).collect::<Vec<String>>().concat()),
			Expression::Op2(_, _, _) => String::new(), //TODO: Implement with correct parenthesis
			Expression::Op1(ref op1, ref exp) => format!("{}{}", op1, exp),
			Expression::Lit(ref lit) => format!("{}", lit),
			Expression::FunCall(ref ident, ref exps) => format!("{}({})", ident,
				exps.iter().map(|x| format!("{}", x)).collect::<Vec<String>>().join(",")),
			Expression::Tuple(ref l, ref r) => format!("({}, {})", l, r)
		})
	}
}

impl fmt::Display for Field {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", match *self {
			Field::Head => String::from(".hd"),
			Field::Tail => String::from(".tl"),
			Field::First => String::from(".fst"),
			Field::Second => String::from(".snd")
		})
	}
}

impl fmt::Display for Op2 {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", match *self {
			Op2::Addition => String::from("+"),
			Op2::Subtraction => String::from("-"),
			Op2::Multiplication => String::from("*"),
			Op2::Division => String::from("/"),
			Op2::Modulo => String::from("%"),
			Op2::Equals => String::from("=="),
			Op2::LessThan => String::from("<"),
			Op2::GreaterThan => String::from(">"),
			Op2::LessEquals => String::from("<="),
			Op2::GreaterEquals => String::from(">="),
			Op2::NotEquals => String::from("!="),
			Op2::And => String::from("&&"),
			Op2::Or => String::from("||"),
			Op2::Cons => String::from(":")
		})
	}
}

impl fmt::Display for Op1 {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", match *self {
			Op1::Not => String::from("!"),
			Op1::Negation => String::from("-")
		})
	}
}

impl fmt::Display for Literal {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", match *self {
			Literal::Int(i) => i.to_string(),
			Literal::Char(c) => format!("'{}'", c.to_string()),
			Literal::Bool(b) => if b {
				String::from("True")
			} else {
				String::from("False")
			},
			Literal::EmptyList => String::from("[]")
		})
	}
}