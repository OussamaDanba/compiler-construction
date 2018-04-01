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
	pub vtype: Type,
	pub value: Expression
}

#[derive(Debug)]
pub struct Function {
	pub name: Ident,
	pub args: Vec<Ident>,
	pub ftype: Type,
	pub vars: Vec<Variable>,
	pub stmts: Vec<Statement>
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
	TInt,
	TBool,
	TChar,
	TVoid,
	TTuple(Box<Type>, Box<Type>),
	TList(Box<Type>),
	TArrow(Vec<Type>, Box<Type>)
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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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

// We need to encode the priorities somewhere in order to prevent
// superfluous parenthesis during pretty printing.
fn priority(op: &Op2) -> usize {
	match *op {
		Op2::Addition | Op2::Subtraction => 5,
		Op2::Multiplication | Op2::Division => 6,
		Op2::Modulo => 7,
		Op2::Equals | Op2::LessThan
			| Op2::GreaterThan | Op2::LessEquals
			| Op2::GreaterEquals | Op2::NotEquals=> 3,
		Op2::And => 2,
		Op2::Or => 1,
		Op2::Cons => 4
	}
}

fn is_left_associative(op: &Op2) -> bool {
	match *op {
		Op2::Addition | Op2::Subtraction
			| Op2::Multiplication | Op2::Division
			| Op2::Modulo => true,
		_ => false
	}
}
fn indent(input: &str) -> String {
	input.lines().map(|x| format!("\t{}\n", x)).collect()
}

impl fmt::Display for SPL {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}",
			format!("{}{}",
				self.vars.iter().map(|x| format!("{}\n", x)).collect::<Vec<String>>().concat(),
				self.funs.iter().map(|x| format!("{}\n\n", x)).collect::<Vec<String>>().concat()
			).trim() // Remove superfluous newlines
		)
	}
}

impl fmt::Display for Variable {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{} {} = {};",
			self.vtype,
			self.name,
			self.value
		)
	}
}

impl fmt::Display for Function {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}({}){} {{\n{}{}}}",
			self.name,
			self.args.iter().map(|x| format!("{}", x)).collect::<Vec<String>>().join(", "),
			format!(" :: {}", self.ftype),
			indent(&self.vars.iter().map(|x| format!("{}\n", x)).collect::<Vec<String>>().concat()),
			indent(&self.stmts.iter().map(|x| format!("{}\n", x)).collect::<Vec<String>>().concat())
		)
	}
}

impl fmt::Display for Type {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", match *self {
			Type::TInt => String::from("Int"),
			Type::TBool => String::from("Bool"),
			Type::TChar => String::from("Char"),
			Type::TVoid => String::from("Void"),
			Type::TTuple(ref l, ref r) => format!("({}, {})", l, r),
			Type::TList(ref t) => format!("[{}]", t),
			Type::TArrow(ref ftypes, ref rtype) => format!("{}-> {}",
				ftypes.iter().map(|x| format!("{} ", x)).collect::<Vec<String>>().concat(), rtype)
		})
	}
}

impl fmt::Display for Statement {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", match *self {
			Statement::If(ref exp, ref if_stmts, ref else_stmts) =>
				format!("if({}) {{\n{}}}{}", exp, indent(&if_stmts.iter()
					.map(|x| format!("{}\n", x)).collect::<Vec<String>>().concat()),
						if else_stmts.is_empty() {
							String::new()
						} else {
							format!(" else {{\n{}}}", indent(&else_stmts.iter().map(|x| format!("{}\n", x)).collect::<Vec<String>>().concat()))
						}
				),
			Statement::While(ref exp, ref stmts) => format!("while({}) {{\n{}}}", exp,
				indent(&stmts.iter().map(|x| format!("{}\n", x)).collect::<Vec<String>>().concat())),
			Statement::Assignment(ref ident, ref fields, ref exp) => format!("{}{} = {};", ident,
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
			Expression::Op2(ref exp1, ref op, ref exp2) => {
				format!("{} {} {}", match **exp1 {
					Expression::Op2(_, ref left_op, _) =>
						if priority(left_op) > priority(op) {
								format!("({})", exp1)
						} else {
							format!("{}", exp1)
						},
					_ => format!("{}", exp1)
				},
				op,
				match **exp2 {
					Expression::Op2(_, ref right_op, _) =>
						if priority(right_op) > priority(op)
							|| (priority(right_op) == priority(op) && is_left_associative(op)) {
								format!("({})", exp2)
						} else {
							format!("{}", exp2)
						},
					_ => format!("{}", exp2)
				})
			},
			Expression::Op1(ref op1, ref exp) => format!("{}", match **exp {
				// We need this otherwise we end up with "!a && b" instead of "!(a && b)".
				Expression::Op2(_, _, _) => format!("{}({})", op1, exp),
				_ => format!("{}{}", op1, exp)
			}),
			Expression::Lit(ref lit) => format!("{}", lit),
			Expression::FunCall(ref ident, ref exps) => format!("{}({})", ident,
				exps.iter().map(|x| format!("{}", x)).collect::<Vec<String>>().join(", ")),
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