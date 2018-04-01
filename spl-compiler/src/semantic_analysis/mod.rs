use parser::{Ident, SPL, Variable, Function, Type, Statement, Expression, Field, Op2, Op1, Literal};
use std::collections::HashMap;
use std::borrow::Borrow;

pub fn semantic_analysis(ast: &SPL) -> Option<HashMap<*const Expression, Type>> {
	// Every expression has a type. During semantic analysis we end up knowing what type this is,
	// so we store it since we need it during code generation.
	// Note that we use a pointer to uniquely identify every expression even though they might be syntactically the same.
	// This is needed since an expression may use an identifier which can have a different type (and we want to keep the previous expression type intact)
	let mut expr_type: HashMap<*const Expression, Type> = HashMap::new();

	// Type environment. Used to check if an identifier has already been defined and whether it has the right type for the usage.
	// We use a bool to distinguish between variable and function
	let mut type_env: HashMap<(&Ident, bool), &Type> = HashMap::new();

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
		let expression_type = analyse_expression(&var.value, &type_env, &mut expr_type)?;
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

fn analyse_expression(expr: &Expression, type_env: &HashMap<(&Ident, bool), &Type>, expr_type: &mut HashMap<*const Expression, Type>) -> Option<Type> {
	match *expr {
		Expression::Ident(ref ident, ref fields) => {
			// TODO
			None
		},
		Expression::Op2(ref left_exp, ref op2, ref right_exp) => {
			// TODO
			None
		},
		Expression::Op1(ref op1, ref exp) => {
			let expression_type = analyse_expression(exp, type_env, expr_type)?;
			if (*op1 == Op1::Not && expression_type != Type::TBool)
				|| (*op1 == Op1::Negation && expression_type != Type::TInt) {
					println!("Attempting to use binary operator {} on expression {} of type {}.", op1, exp, expression_type);
					return None;
			}

			expr_type.insert(exp.borrow() as *const Expression, expression_type);

			Some(
				match *op1 {
					Op1::Not => Type::TBool,
					Op1::Negation => Type::TInt
			})
		},
		Expression::Lit(ref literal) => Some (
			match *literal {
				Literal::Int(_) => Type::TInt,
				Literal::Char(_) => Type::TChar,
				Literal::Bool(_) => Type::TBool,
				Literal::EmptyList => Type::TList(Box::new(Type::TVoid))
			}
		),
		Expression::FunCall(ref ident, ref exps) => {
			match type_env.get(&(ident, true)) {
				None => {
					println!("Function call to undefined function {}.", ident);
					None
				},
				Some(fun) => match **fun {
					Type::TArrow(ref arg_types, ref ret_type) => {
						if exps.len() != arg_types.len() {
							println!("Attempting to call function {} with {} arguments while it has {}.",
								ident, exps.len(), arg_types.len());
							return None;
						}

						for (exp_index, exp) in exps.iter().enumerate() {
							let expression_type = analyse_expression(exp, type_env, expr_type)?;
							expr_type.insert(exp as *const Expression, expression_type.clone());

							if expression_type != arg_types[exp_index] && ident != "print" {
								println!("Function call to {}: argument expected type {} but got type {} from expression {}.",
									ident, arg_types[exp_index], expression_type, exp);
								return None;
							}
						}

						Some((**ret_type).clone())
					},
					_ => unreachable!()
				}
			}
		},
		Expression::Tuple(ref left_exp, ref right_exp) => {
			let left_expression_type = analyse_expression(left_exp, type_env, expr_type)?;
			let right_expression_type = analyse_expression(right_exp, type_env, expr_type)?;

			expr_type.insert(left_exp.borrow() as *const Expression, left_expression_type.clone());
			expr_type.insert(right_exp.borrow() as *const Expression, right_expression_type.clone());

			Some(Type::TTuple(Box::new(left_expression_type), Box::new(right_expression_type)))
		}
	}
}