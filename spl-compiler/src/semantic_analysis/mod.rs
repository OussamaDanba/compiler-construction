use parser::{Ident, SPL, Function, Type, Statement, Expression, Field, Op2, Op1, Literal};
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
			println!("Function '{}' multiply defined.", fun.name);
			return None;
		}
		type_env.insert((&fun.name, true), &fun.ftype);

		let ftype_arguments = match fun.ftype {
			Type::TArrow(ref arg_types, _) => arg_types,
			_ => unreachable!()
		};

		if fun.args.len() != ftype_arguments.len() {
			println!("Amount of specified types do not match arguments for function '{}'. Expected '{}' but was given '{}'.",
				fun.name, fun.args.len(), ftype_arguments.len());
			return None;
		}
	}

	// Ensure the program has an entrypoint
	if !type_env.contains_key(&(&String::from("main"), true)) {
		println!("Given program has no function main.");
		return None;
	}

	// Collect all global variables and type check them
	for var in &ast.vars {
		// Type check the variable declaration
		let expression_type = analyse_expression(&var.value, &type_env, &mut expr_type)?;
		if var.vtype != expression_type {
			println!("Global variable type mismatch in:\n{}", var);
			println!("Given type '{}', found type '{}'.", var.vtype, expression_type);
			return None;
		}
		// Store the type of the expression
		expr_type.insert(&var.value as *const Expression, expression_type);

		// Check if the variable declaration is already defined
		if type_env.contains_key(&(&var.name, false)) {
			println!("Global variable '{}' multiply defined.", var.name);
			return None;
		}
		// Add variable declaration to type environment
		type_env.insert((&var.name, false), &var.vtype);
	}

	for fun in &ast.funs {
		// There is no point in checking print and isEmpty since they are built-in
		if fun.name == "print" || fun.name == "isEmpty" {
			continue;
		}
		// We don't care about the return value we just need to know whether an error occured or not
		// so we can do an early return
		if analyse_function(fun, &type_env, &mut expr_type).is_err() { return None }
	}

	Some(expr_type)
}

fn analyse_function(fun: &Function, type_env: &HashMap<(&Ident, bool), &Type>, expr_type: &mut HashMap<*const Expression, Type>) -> Result<(), ()> {
	// Function type as tuple to make working with it easier
	let ftype_as_tuple = match fun.ftype {
		Type::TArrow(ref arg_types, ref ret_type) => (arg_types, ret_type),
		_ => unreachable!()
	};

	// To emulate shadowing we use a separate type environment for this function and allow variables to override the entries
	let mut temp_type_env = type_env.clone();

	// Enter function arguments into the type environment
	for (i, arg_name) in (&fun.args).iter().enumerate() {
		temp_type_env.insert((arg_name, false), &(ftype_as_tuple.0)[i]);
	}

	// Enter variable declarations into the type_environment
	for var in &fun.vars {
		// Type check the variable declaration
		let expression_type = match analyse_expression(&var.value, &temp_type_env, expr_type) {
			Some(x) => x,
			None => return Err(())
		};
		if var.vtype != expression_type {
			println!("Variable type mismatch in:\n{}", var);
			println!("Given type '{}', found type '{}'.", var.vtype, expression_type);
			return Err(());
		}
		// Store the type of the expression
		expr_type.insert(&var.value as *const Expression, expression_type);

		// Add variable declaration to type environment potentially shadowing previous ones
		temp_type_env.insert((&var.name, false), &var.vtype);
	}

	// Collect the first returning statement and its type (if there is one).
	let mut stmt_returning = None;
	for stmt in &fun.stmts {
		match analyse_statement(stmt, &temp_type_env, expr_type, ftype_as_tuple.1) {
			Ok(x) => if let Some(x) = x { stmt_returning = Some((stmt, x)); break },
			Err(_) => return Err(())
		}
	}

	// If there was a returning statement all paths resulted in a return
	match stmt_returning {
		Some(x) => {
			// Sanity check (not strictly necessary)
			if x.1 == **ftype_as_tuple.1 {
				Ok(())
			} else {
				println!("Statement '{}' does not result in type '{}' in function '{}'.",
					x.0, ftype_as_tuple.1, fun.name);
				Err(())
			}
		}
		None => {
			// Functions with return type void don't necessarily need to have a return statement somewhere
			if **ftype_as_tuple.1 != Type::TVoid {
				println!("Not all paths of function '{}' will result in a return value.", fun.name);
				Err(())
			} else {
				Ok(())
			}
		}
	}
}

// A return type of Err encodes failure whereas Option<Type> encodes success and whether the statement contained a return in every branch or not
fn analyse_statement(stmt: &Statement, type_env: &HashMap<(&Ident, bool), &Type>, expr_type: &mut HashMap<*const Expression, Type>, ret_type: &Type) -> Result<Option<Type>, ()> {
	match *stmt {
		Statement::If(ref expr, ref if_stmts, ref else_stmts) => match analyse_expression(expr, type_env, expr_type) {
			Some(x) => {
				if x != Type::TBool {
					println!("Expression in if-statement is not of type {} (it is {}).", Type::TBool, x);
					return Err(())
				}
				expr_type.insert(expr as *const Expression, x);

				let mut if_stmt_returning = None;
				for sub_stmt in if_stmts {
					match analyse_statement(sub_stmt, type_env, expr_type, ret_type) {
						Ok(x) => if let Some(x) = x { if_stmt_returning = Some(x); break },
						Err(_) => return Err(())
					}
				}

				let mut else_stmt_returning = None;
				for sub_stmt in else_stmts {
					match analyse_statement(sub_stmt, type_env, expr_type, ret_type) {
						Ok(x) => if let Some(x) = x { else_stmt_returning = Some(x); break },
						Err(_) => return Err(())
					}
				}

				if if_stmt_returning.is_none() || else_stmt_returning.is_none() {
					Ok(None)
				}
				// Sanity check (not strictly necessary)
				else if if_stmt_returning.clone().unwrap() != else_stmt_returning.unwrap() {
					println!("If and else branch type mismatch for if-statement:\n{}", stmt);
					Err(())
				} else {
					Ok(Some(if_stmt_returning.unwrap()))
				}
			},
			None => Err(())
		},
		Statement::While(ref expr, ref while_stmts) => match analyse_expression(expr, type_env, expr_type) {
			Some(x) => {
				if x != Type::TBool {
					println!("Expression in while-statement is not of type {} (it is {}).", Type::TBool, x);
					return Err(())
				}
				expr_type.insert(expr as *const Expression, x);

				for sub_stmt in while_stmts {
					match analyse_statement(sub_stmt, type_env, expr_type, ret_type) {
						Ok(_) => (),
						Err(_) => return Err(())
					}
				}

				Ok(None)
			},
			None => Err(())
		},
		Statement::Assignment(ref ident, ref fields, ref expr) => {
			match type_env.get(&(ident, false)) {
				Some(x) => {
					match (type_from_ident(x, fields), analyse_expression(expr, type_env, expr_type)) {
						(Some(x), Some(y)) =>
							if x == y {
								expr_type.insert(expr as *const Expression, y.clone());
								Ok(None)
							} else {
								println!("Statement '{}' has mismatching types. LHS is of type '{}' while RHS is of type '{}'.", stmt, x, y);
								Err(())
							},
						_ => Err(())
					}
				},
				None => {
					println!("Use of undefined variable '{}' in statement '{}'", ident, stmt);
					Err(())
				}
			}
		},
		Statement::FunCall(ref expr) =>	match analyse_expression(expr, type_env, expr_type) {
			Some(x) => { expr_type.insert(expr as *const Expression, x.clone()); Ok(None) },
			None => Err(())
		},
		Statement::Return(ref opt_expr) => match *opt_expr {
			Some(ref expr) => match analyse_expression(expr, type_env, expr_type) {
				Some(x) => {
					if x != *ret_type {
						println!("Return statement '{}' has type '{}' while function expects type '{}'.", stmt, x, ret_type);
						return Err(());
					}
					expr_type.insert(expr as *const Expression, x.clone());
					Ok(Some(x))
				},
				None => Err(())
			},
			None => {
				if *ret_type != Type::TVoid {
					println!("Return statement '{}' has type '{}' while function expects type '{}'.", stmt, Type::TVoid, ret_type);
					Err(())
				} else {
					Ok(Some(Type::TVoid))
				}
			}
		}
	}
}

fn type_from_ident(ident_type: &Type, fields: &[Field]) -> Option<Type> {
	if fields.is_empty() {
		Some(ident_type.clone())
	} else {
		let first_field = fields.first()?;
		let next_field = fields.split_at(1).1;
		match *first_field {
			Field::Head => match *ident_type {
				Type::TList(ref list) => type_from_ident(list, next_field),
				_ => {
					println!("Cannot take head of list from type '{}'", ident_type);
					None
				}
			},
			Field::Tail => match *ident_type {
				Type::TList(_) => type_from_ident(ident_type, next_field),
				_ => {
					println!("Cannot take tail of list from type '{}'", ident_type);
					None
				}
			},
			Field::First =>	match *ident_type {
				Type::TTuple(ref first, _) => type_from_ident(first, next_field),
				_ => {
					println!("Cannot take first element from type '{}'", ident_type);
					None
				}
			}
			Field::Second => match *ident_type {
				Type::TTuple(_, ref second) => type_from_ident(second, next_field),
				_ => {
					println!("Cannot take second element from type '{}'", ident_type);
					None
				}
			}
		}
	}
}

fn analyse_expression(expr: &Expression, type_env: &HashMap<(&Ident, bool), &Type>, expr_type: &mut HashMap<*const Expression, Type>) -> Option<Type> {
	match *expr {
		Expression::Ident(ref ident, ref fields) =>	match type_env.get(&(ident, false)) {
			None => {
				println!("Use of undefined variable '{}'.", ident);
				None
			},
			Some(var) => {
				type_from_ident(*var, fields)
			}
		},
		Expression::Op2(ref left_exp, ref op2, ref right_exp) => {
			let left_expression_type = analyse_expression(left_exp, type_env, expr_type)?;
			let right_expression_type = analyse_expression(right_exp, type_env, expr_type)?;

			expr_type.insert(left_exp.borrow() as *const Expression, left_expression_type.clone());
			expr_type.insert(right_exp.borrow() as *const Expression, right_expression_type.clone());

			match *op2 {
				| Op2::Addition
				| Op2::Subtraction => {
					if (left_expression_type == Type::TInt || left_expression_type == Type::TChar)
						&& (right_expression_type == Type::TInt || right_expression_type == Type::TChar) {
							Some(left_expression_type)
					} else {
						println!("Operator '{}' not defined for types '{}' and '{}'.",
							op2, left_expression_type, right_expression_type);
						None
					}
				},
				| Op2::Multiplication
				| Op2::Division
				| Op2::Modulo => {
					if left_expression_type == Type::TInt && left_expression_type == right_expression_type {
							Some(left_expression_type)
					} else {
						println!("Operator '{}' not defined for types '{}' and '{}'.",
							op2, left_expression_type, right_expression_type);
						None
					}
				},
				| Op2::Equals
				| Op2::LessThan
				| Op2::GreaterThan
				| Op2::LessEquals
				| Op2::GreaterEquals
				| Op2::NotEquals => {
					if left_expression_type == right_expression_type {
						Some(Type::TBool)
					} else {
						println!("Operator '{}' not defined for types '{}' and '{}'.",
							op2, left_expression_type, right_expression_type);
						None
					}
				},
				| Op2::And
				| Op2::Or => {
					if left_expression_type == Type::TBool && left_expression_type == right_expression_type {
						Some(Type::TBool)
					} else {
						println!("Operator '{}' not defined for types '{}' and '{}'.",
							op2, left_expression_type, right_expression_type);
						None
					}
				},
				Op2::Cons => {
					let left_to_list = Type::TList(Box::new(left_expression_type.clone()));
					if left_to_list == right_expression_type {
						Some(left_to_list)
					} else {
						println!("Operator '{}' not defined for types '{}' and '{}'.",
							op2, left_expression_type, right_expression_type);
						None
					}
				}
			}
		},
		Expression::Op1(ref op1, ref exp) => {
			let expression_type = analyse_expression(exp, type_env, expr_type)?;
			if (*op1 == Op1::Not && expression_type != Type::TBool)
				|| (*op1 == Op1::Negation && expression_type != Type::TInt) {
					println!("Attempting to use binary operator '{}' on expression '{}' of type '{}'.", op1, exp, expression_type);
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
					println!("Function call to undefined function '{}'.", ident);
					None
				},
				Some(fun) => match **fun {
					Type::TArrow(ref arg_types, ref ret_type) => {
						if exps.len() != arg_types.len() {
							println!("Attempting to call function '{}' with {} arguments while it has {}.",
								ident, exps.len(), arg_types.len());
							return None;
						}

						for (exp_index, exp) in exps.iter().enumerate() {
							let expression_type = analyse_expression(exp, type_env, expr_type)?;
							expr_type.insert(exp as *const Expression, expression_type.clone());

							if expression_type != arg_types[exp_index] && ident != "print" {
								println!("Function call to '{}': argument expected type '{}' but got type '{}' from expression '{}'.",
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
