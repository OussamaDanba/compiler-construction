use parser::{Ident, SPL, Variable, Function, Type, Statement, Expression, Field, Op2, Op1, Literal};
use std::collections::HashMap;

pub fn semantic_analysis(ast: &SPL) -> Option<HashMap<&Expression, Type>> {
	// Every expression has a type. During semantic analysis we end up knowing what type this is
	// so we store it since we need it during code generation
	let mut expr_type = HashMap::new();

	// Type environment. Used to check if an identifier has already been defined and whether it has the right type for the usage
	let mut type_env = HashMap::new();
	for var in &ast.vars {
		// Type check the variable declaration
		let expression_type = analyse_expression(&var.value);
		if var.vtype != expression_type {
			println!("Global variable type mismatch in:\n{}", var);
			println!("Given type {}, found type {}.", var.vtype, expression_type);
			return None;
		}
		// Store the type of the expression
		expr_type.insert(&var.value, expression_type);

		// Check if the variable declaration is already defined
		if type_env.contains_key(&var.name) {
			println!("Global variable {} multiply defined.", var.name);
			return None;
		}

		// Add variable declaration to type environment
		type_env.insert(&var.name, &var.vtype);
	}

	Some(expr_type)
}

fn analyse_expression(expr: &Expression) -> Type {
	match *expr {
		Expression::Lit(ref literal) => match *literal {
			Literal::Int(_) => Type::TInt,
			Literal::Char(_) => Type::TChar,
			Literal::Bool(_) => Type::TBool,
			Literal::EmptyList => Type::TList(Box::new(Type::TVoid))
		},
		_ => Type::TVoid
	}
}