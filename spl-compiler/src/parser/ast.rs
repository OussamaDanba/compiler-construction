#[derive(Debug)]
pub enum Type {
	TInt,
	TBool,
	TChar,
	TTuple(Box<Type>, Box<Type>),
	TList(Box<Type>),
	TIdent(String)
}
