pub type Ident = String;

#[derive(Debug)]
pub struct SPL {
	vars: Vec<Variable>,
	funs: Vec<Function>
}

#[derive(Debug)]
pub struct Variable {
	name: Ident,
	vtype: Option<Type>,
	value: Expression
}

#[derive(Debug)]
pub struct Function {
	name: Ident,
	args: Vec<Ident>,
	ftype: Option<Type>,
	vars: Vec<Variable>,
	stmts: Vec<Statement>
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
	FunCall(Expression), // To prevent code duplication we simply treat this as an expression
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
