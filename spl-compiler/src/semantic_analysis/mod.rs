use parser::{Ident, SPL, Variable, Function, Type, Statement, Expression, Field, Op2, Op1, Literal};
use std::collections::HashMap;

pub fn semantic_analysis(ast: &SPL) -> Option<HashMap<*const Expression, Type>> {
	// Every expression has a type. During semantic analysis we end up knowing what type this is,
	// so we store it since we need it during code generation.
	// Note that we use a pointer to uniquely identify every expression even though they might be syntactically the same.
	// This is needed since an expression may use an identifier which can have a different type (and we want to keep the previous expression type intact)
	let mut expr_type: HashMap<*const Expression, Type> = HashMap::new();

	// Type environment. Used to check if an identifier has already been defined and whether it has the right type for the usage.
	let mut type_env = HashMap::new();

	// First we collect all function declarations because it is allowed to use them before the declaration
	for fun in &ast.funs {
		if type_env.contains_key(&(&fun.name, true)) {
			println!("Function {} multiply defined.", fun.name);
			return None;
		}
		type_env.insert((&fun.name, true), &fun.ftype);

		let ftype = match fun.ftype {
			Type::TArrow(ref arg_types, ref ret_type) => (arg_types, ret_type),
			_ => unreachable!()
		};

		if fun.args.len() != ftype.0.len() {
			println!("Amount of specified types do not match arguments for function {}. Expected {} but was given {}.",
				fun.name, fun.args.len(), ftype.0.len());
			return None;
		}
	}

	// Collect all global variables and type check them
	for var in &ast.vars {
		// Type check the variable declaration
		let expression_type = analyse_expression(&var.value)?;
		if var.vtype != expression_type {
			println!("Global variable type mismatch in:\n{}", var);
			println!("Given type {}, found type {}.", var.vtype, expression_type);
			return None;
		}
		// Store the type of the expression
		expr_type.insert(&var.value as *const Expression, expression_type);

		// Check if the variable declaration is already defined
		if type_env.contains_key(&(&var.name, false)) {
			println!("Global variable {} multiply defined.", var.name);
			return None;
		}
		// Add variable declaration to type environment
		type_env.insert((&var.name, false), &var.vtype);
	}

	println!("{:?}", type_env);
	Some(expr_type)
}

fn analyse_expression(expr: &Expression) -> Option<Type> {
	match *expr {
		Expression::Lit(ref literal) => match *literal {
			Literal::Int(_) => Some(Type::TInt),
			Literal::Char(_) => Some(Type::TChar),
			Literal::Bool(_) => Some(Type::TBool),
			Literal::EmptyList => Some(Type::TList(Box::new(Type::TVoid)))
		},
		_ => None
	}
}