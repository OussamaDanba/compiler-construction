use parser::{Ident, SPL, Function, Type, Statement, Expression, Field, Op2, Op1, Literal};
use std::collections::HashMap;

pub fn code_generator(ast: &SPL, expr_type: HashMap<*const Expression, Type>) -> String {
	// Temporary
	String::new()
}
