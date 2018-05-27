use parser::{Ident, SPL, Function, Variable, Type, Statement, Expression, Field, Op2, Op1, Literal};
use std::collections::HashMap;
use rand::{self, Rng};

pub fn code_generator(ast: &SPL, expr_type: &HashMap<*const Expression, Type>) -> String {
	// Some standard things we almost always need so just add them.
	let includes = String::from("#include <stdbool.h>\n#include <stdio.h>\n#include <stdint.h>\n#include <stdlib.h>\n");
	let tuple = String::from("typedef struct {\n\tuintptr_t fst;\n\tuintptr_t snd;\n} tuple;\n");
	let list = String::from("typedef struct {\n\tuintptr_t hd;\n\tuintptr_t tl;\n} list;\n");

	let (global_vars, global_decls) = generate_globals_decls(&ast.vars);

	format!("{}\n{}\n{}\n{}{}{}\n", includes, tuple, list, global_decls,
		generate_functions(ast, &global_vars, expr_type), generate_globals_inits(&ast.vars, &global_vars, expr_type))
}

fn generate_globals_decls(vars: &[Variable]) -> (Vec<(Ident, Type)>, String) {
	// We need to store all the global variables we have somewhere so that we can find them later when evaluating expressions
	let mut global_vars = Vec::new();
	for var in vars {
		global_vars.push((var.name.clone(), var.vtype.clone()));
	}

	// Initialization is done separately inside the main function since some globals
	// that are allowed in SPL need run-time initialization (any global that needs a malloc).
	let mut gen_code = String::new();
	for var in vars {
		gen_code.push_str(&format!("{} {};\n", generate_type_name(&var.vtype), var.name));
	}

	(global_vars, gen_code)
}

fn generate_globals_inits(vars: &[Variable], global_vars: &[(Ident, Type)],
	expr_type: &HashMap<*const Expression, Type>) -> String {

	let mut gen_code = String::new();
	// Global variables initialization
	gen_code.push_str("int main() {\n");
	for var in vars {
		gen_code.push_str(&format!("{} = {};\n",
			var.name,
			generate_expression(&var.name, &var.value, &global_vars, &Vec::new(), &Vec::new(), expr_type)));
	}
	gen_code.push_str("spl_main();\n}\n");

	gen_code
}

fn generate_functions(ast: &SPL, global_vars: &[(Ident, Type)], expr_type: &HashMap<*const Expression, Type>) -> String {
	let mut gen_code = String::new();

	// isEmpty only has a list as argument and no locals and returns a bool
	// isEmpty is easy to implement since we only have to check whether the second pointer points to 0x0 or not
	gen_code.push_str("bool isEmpty(list* x) {\n\treturn x->tl == 0;\n}\n\n");

	// We have a default RuntimeErr function which handles runtime errors. A user can define this himself if they
	// wanted to.
	if ast.funs.iter().find(|x| x.name == "RuntimeErr").is_none() {
		gen_code.push_str("int RuntimeErr() {\n\texit(-1);\n}\n\n");
	}

	for fun in &ast.funs {
		// print and isEmpty require special handling
		if fun.name != "print" && fun.name != "isEmpty" {
			if let Type::TArrow(ref fun_args_types, ref fun_res_type) = fun.ftype {

				// Generate code for parameters and enter them into the environment
				let mut param_vars: Vec<(Ident, Type)> = Vec::new();
				let mut arguments = String::new();
				for (arg_ident, arg_type) in fun.args.iter().zip(fun_args_types) {
					param_vars.push((arg_ident.clone(), arg_type.clone()));
					arguments.push_str(&format!("{} {},", generate_type_name(arg_type), arg_ident));
				}
				arguments.pop();

				// Generate function header. main is renamed to spl_main as the C main is reserved for globals.
				if fun.name == "main" {
					gen_code.push_str(&format!("{} {}({}) {{\n", generate_type_name(fun_res_type), "spl_main", arguments));
				} else {
					gen_code.push_str(&format!("{} {}({}) {{\n", generate_type_name(fun_res_type), fun.name, arguments));
				}

				let mut local_vars: Vec<(Ident, Type, Ident)> = Vec::new();
				for var in &fun.vars {
					// Make a random mapping for the variable and enter it into the type environment
					let mut random_label = rand::thread_rng().gen_ascii_chars().take(9).collect::<String>(); random_label.insert(0, '_');
					local_vars.push((var.name.clone(), var.vtype.clone(), random_label.clone()));

					gen_code.push_str(&format!("{} {} = {};\n",
						generate_type_name(&var.vtype),
						random_label,
						generate_expression(&random_label, &var.value, global_vars, &param_vars, &local_vars, expr_type)));
				}

				for stmt in &fun.stmts {
					let statement_code = generate_statements(stmt, fun, global_vars, &param_vars, &local_vars, expr_type);
					gen_code.push_str(&statement_code);
				}

				gen_code.push_str("}\n\n");
			}

		}
	}

	// TODO: implement print

	gen_code
}

fn generate_statements(stmt: &Statement, fun: &Function, global_vars: &[(Ident, Type)], param_vars: &[(Ident, Type)],
	local_vars: &[(Ident, Type, Ident)], expr_type: &HashMap<*const Expression, Type>) -> String {

	let mut gen_code = String::new();
	match *stmt {
		Statement::If(ref expr, ref if_stmts, ref else_stmts) => {
			// Evaluate the expression
			let mut random_label = rand::thread_rng().gen_ascii_chars().take(9).collect::<String>(); random_label.insert(0, '_');
			gen_code.push_str(&format!("{} {} = {};\n",
				generate_type_name(expr_type.get(&(&(*expr) as *const Expression)).unwrap()),
				random_label,
				generate_expression(&random_label, &expr, global_vars, param_vars, local_vars, expr_type))
			);

			gen_code.push_str(&format!("if({}) {{\n", random_label));
			// Generate if-statements
			for if_stmt in if_stmts {
				gen_code.push_str(&generate_statements(if_stmt, fun, global_vars, param_vars, local_vars, expr_type));
			}
			gen_code.push_str("} else {\n");

			// Generate else-statements
			for else_stmt in else_stmts {
				gen_code.push_str(&generate_statements(else_stmt, fun, global_vars, param_vars, local_vars, expr_type));
			}
			gen_code.push_str("}\n");
		},
		Statement::While(ref expr, ref stmts) => {
			// Evaluate the expression once
			let mut random_label = rand::thread_rng().gen_ascii_chars().take(9).collect::<String>(); random_label.insert(0, '_');
			gen_code.push_str(&format!("{} {} = {};\n",
				generate_type_name(expr_type.get(&(&(*expr) as *const Expression)).unwrap()),
				random_label,
				generate_expression(&random_label, &expr, global_vars, param_vars, local_vars, expr_type))
			);

			gen_code.push_str(&format!("while({}) {{\n", random_label));
			// Generate while-statements
			for stmt in stmts {
				gen_code.push_str(&generate_statements(stmt, fun, global_vars, param_vars, local_vars, expr_type));
			}

			// Re-evaluate the expression so that we possibly loop
			gen_code.push_str(&format!("{} = {};\n",
				random_label,
				generate_expression(&random_label, &expr, global_vars, param_vars, local_vars, expr_type))
			);
			gen_code.push_str("}\n");
		},
		Statement::Assignment(ref ident, ref fields, ref expr) => {
			// Evaluate the expression
			let mut random_label = rand::thread_rng().gen_ascii_chars().take(9).collect::<String>(); random_label.insert(0, '_');
			gen_code.push_str(&format!("{} {} = {};\n",
				generate_type_name(expr_type.get(&(&(*expr) as *const Expression)).unwrap()),
				random_label,
				generate_expression(&random_label, &expr, global_vars, param_vars, local_vars, expr_type))
			);

			let mut random_label2 = rand::thread_rng().gen_ascii_chars().take(9).collect::<String>(); random_label2.insert(0, '_');
			let (actual_ident, ident_type) = find_identifier(ident, global_vars, param_vars, local_vars);

			// This solely exists for error checking purposes
			gen_code.push_str(&format!("uintptr_t {} = (uintptr_t) {};\n",
				random_label2,
				generate_ident(true, String::new(), actual_ident, ident_type, fields)
			));

			gen_code.push_str(&format!("{} = (uintptr_t) {};\n",
				generate_ident(false, String::new(), actual_ident, ident_type, fields).split_whitespace().skip(1).collect::<String>()
			, random_label));
		},
		Statement::FunCall(ref expr) => {
			// A little bit of code duplication since every expression sets a variable at the end.
			// This is the only time where it is a problem since a function call can result in a void which
			// we can not assign to a variable. This is not a problem in expression since the type checker
			// ensures that void functions are not used in expressions.
			if let Expression::FunCall(ref ident, ref exprs) = expr{
				// Evaluate the arguments and construct the string that is passed
				let mut arguments = String::new();
				for expr in exprs {
					let mut random_label = rand::thread_rng().gen_ascii_chars().take(9).collect::<String>(); random_label.insert(0, '_');
					gen_code.push_str(&format!("{} {} = {};\n",
						generate_type_name(expr_type.get(&(&(*expr) as *const Expression)).unwrap()),
						random_label,
						generate_expression(&random_label, &expr, global_vars, param_vars, local_vars, expr_type))
					);

					arguments.push_str(&format!("{},", random_label));
				}
				// Get rid of the trailing comma
				arguments.pop();

				gen_code.push_str(&format!("{}({});\n", ident, arguments));
			}
		},
		Statement::Return(ref option_expr) => match *option_expr {
			Some(ref expr) => {
				if let Type::TArrow(_, ref fun_ret_type) = fun.ftype {
					let mut random_label = rand::thread_rng().gen_ascii_chars().take(9).collect::<String>(); random_label.insert(0, '_');
					gen_code.push_str(&format!("{} {} = {};\n",
						generate_type_name(fun_ret_type),
						random_label,
						generate_expression(&random_label, expr, global_vars, param_vars, local_vars, expr_type))
					);

					gen_code.push_str(&format!("return {};\n", random_label));
				}
			},
			None => gen_code.push_str("return;\n")
		}
	}

	gen_code
}

fn generate_type_name(type_name: &Type) -> String {
	match *type_name {
		Type::TInt => String::from("int"),
		Type::TBool => String::from("bool"),
		Type::TChar => String::from("char"),
		Type::TTuple(_, _) => String::from("tuple*"),
		Type::TList(_) => String::from("list*"),
		Type::TVoid => String::from("void"),
		Type::TArrow(_, _) => unreachable!()
	}
}

fn generate_ident(with_err: bool, gen_code: String, ident: &str, ident_type: &Type, fields: &[Field]) -> String {
	if gen_code.is_empty() {
		generate_ident(with_err, format!("({}) {}", generate_type_name(ident_type), ident), ident, ident_type, fields)
	} else if fields.is_empty() {
		gen_code
	} else {
		let first_field = fields.first().unwrap();
		let next_fields = fields.split_at(1).1;
		match *first_field {
			// We have to do some extra work as asking for the head of an empty list doesn't make much sense.
			Field::Head => match *ident_type {
				Type::TList(ref ltype) => {
					generate_ident(with_err, if with_err {
							format!("({}) (({})->tl == 0 ? RuntimeErr() : ({})->hd)",
								generate_type_name(ltype), gen_code, gen_code)
						} else {
							format!("({}) ({})->hd",
								generate_type_name(ltype), gen_code)
						}, ident, ident_type, next_fields)
				},
				_ => unreachable!()
			},
			// It is a deliberate choice that asking for the tail of an empty list is an error.
			Field::Tail => match *ident_type {
				Type::TList(_) => {
					generate_ident(with_err, if with_err {
						format!("(list*) (({})->tl == 0 ? RuntimeErr() : ({})->tl)", gen_code, gen_code)
					} else {
						format!("(list*) ({})->tl", gen_code)
					}, ident, ident_type, next_fields)
				},
				_ => unreachable!()
			},
			Field::First => match *ident_type {
				Type::TTuple(ref fst_type, _) => {
					generate_ident(with_err, format!("({}) ({})->fst", generate_type_name(fst_type), gen_code),
						ident, fst_type, next_fields)
				},
				_ => unreachable!()
			},
			Field::Second => match *ident_type {
				Type::TTuple(_, ref snd_type) => {
					generate_ident(with_err, format!("({}) ({})->snd", generate_type_name(snd_type), gen_code),
						ident, snd_type, next_fields)
				},
				_ => unreachable!()
			}
		}
	}
}

fn generate_expression(name: &str, expr: &Expression, global_vars: &[(Ident, Type)], param_vars: &[(Ident, Type)],
	local_vars: &[(Ident, Type, Ident)], expr_type: &HashMap<*const Expression, Type>) -> String {

	let mut gen_code = String::new();
	match *expr {
		Expression::Ident(ref ident, ref fields) => {
			let (actual_ident, ident_type) = find_identifier(ident, global_vars, param_vars, local_vars);
			gen_code.push_str(&generate_ident(true, String::new(), actual_ident, ident_type, fields));
		},
		Expression::Op2(ref lexpr, ref op2, ref rexpr) => {
			// Need this because the arguments have to be evaluated first before being passed as parameters.
			gen_code.push_str("0;\n");

			generate_op2(name, lexpr, op2, rexpr, &mut gen_code, global_vars, param_vars, local_vars, expr_type);
		},
		Expression::Op1(ref op, ref expr) => {
			match *op {
				Op1::Not => {
					// The semantic analysis ensures the not operator only gets booleans.
					gen_code.push_str(&generate_expression(name, &expr, global_vars, param_vars, local_vars, expr_type));
					gen_code.push_str(&format!(";\n{} = !{}", name, name));
				},
				Op1::Negation => {
					// The semantic analysis ensures the negation operator only gets integers.
					gen_code.push_str(&generate_expression(name, &expr, global_vars, param_vars, local_vars, expr_type));
					gen_code.push_str(&format!(";\n{} = -{}", name, name));
				}
			}
		},
		Expression::Lit(ref x) => match *x {
			Literal::Int(ref val) => gen_code.push_str(&format!("{}", *val)),
			Literal::Bool(ref val) => gen_code.push_str(if *val { "true" } else { "false" }),
			Literal::Char(ref val) => gen_code.push_str(&format!("'{}'", *val)),
			Literal::EmptyList => {
				gen_code.push_str("malloc(sizeof(list));\n");
				gen_code.push_str(&format!("{}->tl = 0", name));
			},
		},
		Expression::FunCall(ref ident, ref exprs) => {
			// Need this because the arguments have to be evaluated first before being passed as parameters.
			gen_code.push_str("0;\n");

			// Evaluate the arguments and construct the string that is passed
			let mut arguments = String::new();
			for expr in exprs {
				let mut random_label = rand::thread_rng().gen_ascii_chars().take(9).collect::<String>(); random_label.insert(0, '_');
				gen_code.push_str(&format!("{} {} = {};\n",
					generate_type_name(expr_type.get(&(&(*expr) as *const Expression)).unwrap()),
					random_label,
					generate_expression(&random_label, &expr, global_vars, param_vars, local_vars, expr_type))
				);

				arguments.push_str(&format!("{},", random_label));
			}
			// Get rid of the trailing comma
			arguments.pop();

			gen_code.push_str(&format!("{} = {}({})", name, ident, arguments));
		},
		Expression::Tuple(ref left, ref right) => {
			gen_code.push_str("malloc(sizeof(tuple));\n");
			// Left side
			let mut random_label = rand::thread_rng().gen_ascii_chars().take(9).collect::<String>(); random_label.insert(0, '_');
			gen_code.push_str(&format!("{} {} = {};\n",
				generate_type_name(expr_type.get(&(&(**left) as *const Expression)).unwrap()),
				random_label,
				generate_expression(&random_label, &left, global_vars, param_vars, local_vars, expr_type))
			);
			gen_code.push_str(&format!("{}->fst = (uintptr_t) {};\n", name, random_label));

			// Right side
			let mut random_label2 = rand::thread_rng().gen_ascii_chars().take(9).collect::<String>(); random_label2.insert(0, '_');
			gen_code.push_str(&format!("{} {} = {};\n",
				generate_type_name(expr_type.get(&(&(**right) as *const Expression)).unwrap()),
				random_label2,
				generate_expression(&random_label2, &right, &global_vars, param_vars, local_vars, expr_type))
			);
			gen_code.push_str(&format!("{}->snd = (uintptr_t) {}", name, random_label2));
		}
	}

	gen_code
}

fn generate_op2(name: &str, lexpr: &Expression, op2: &Op2, rexpr: &Expression,
	gen_code: &mut String, global_vars: &[(Ident, Type)], param_vars: &[(Ident, Type)], local_vars: &[(Ident, Type, Ident)],
	expr_type: &HashMap<*const Expression, Type>) {

	// Evaluate the left expression
	let mut random_label = rand::thread_rng().gen_ascii_chars().take(9).collect::<String>(); random_label.insert(0, '_');
	gen_code.push_str(&format!("{} {} = {};\n",
		generate_type_name(expr_type.get(&(lexpr as *const Expression)).unwrap()),
		random_label,
		generate_expression(&random_label, lexpr, global_vars, param_vars, local_vars, expr_type))
	);

	// Evaluate the right expression
	let mut random_label2 = rand::thread_rng().gen_ascii_chars().take(9).collect::<String>(); random_label2.insert(0, '_');
	gen_code.push_str(&format!("{} {} = {};\n",
		generate_type_name(expr_type.get(&(rexpr as *const Expression)).unwrap()),
		random_label2,
		generate_expression(&random_label2, rexpr, global_vars, param_vars, local_vars, expr_type))
	);

	match *op2 {
		// These operators are only defined for when the left and right side are values rather than pointers
		Op2::Addition => gen_code.push_str(&format!("{} = {} + {}", name, random_label, random_label2)),
		Op2::Subtraction => gen_code.push_str(&format!("{} = {} - {}", name, random_label, random_label2)),
		Op2::Multiplication => gen_code.push_str(&format!("{} = {} * {}", name, random_label, random_label2)),
		Op2::Division => gen_code.push_str(&format!("{} = {} / {}", name, random_label, random_label2)),
		Op2::Modulo => gen_code.push_str(&format!("{} = {} % {}", name, random_label, random_label2)),
		Op2::LessThan => gen_code.push_str(&format!("{} = {} < {}", name, random_label, random_label2)),
		Op2::GreaterThan => gen_code.push_str(&format!("{} = {} > {}", name, random_label, random_label2)),
		Op2::LessEquals => gen_code.push_str(&format!("{} = {} <= {}", name, random_label, random_label2)),
		Op2::GreaterEquals => gen_code.push_str(&format!("{} = {} >= {}", name, random_label, random_label2)),
		// Since left and right are evaluated separately the short-circuiting here is not a problem
		Op2::And => gen_code.push_str(&format!("{} = {} && {}", name, random_label, random_label2)),
		Op2::Or => gen_code.push_str(&format!("{} = {} || {}", name, random_label, random_label2)),
		Op2::Equals => {
			match expr_type.get(&(lexpr as *const Expression)).unwrap() {
				| Type::TInt
				| Type::TBool
				| Type::TChar => gen_code.push_str(&format!("{} = {} == {}", name, random_label, random_label2)),
				// TODO: Actual equality instead of only the references
				Type::TTuple(_, _) => {
					gen_code.push_str(&format!("{} = {} == {}", name, random_label, random_label2));
				},
				Type::TList(_) => {
					gen_code.push_str(&format!("{} = {} == {}", name, random_label, random_label2));
				},
				_ => unreachable!()
			}
		},
		Op2::NotEquals => {
			generate_op2(name, lexpr, &Op2::Equals, rexpr, gen_code, global_vars, param_vars, local_vars, expr_type);
			gen_code.push_str(&format!(";\n{} = !{}", name, name));
		},
		Op2::Cons => {
			gen_code.push_str(&format!("{} = malloc(sizeof(list));\n", name));
			gen_code.push_str(&format!("{}->hd = (uintptr_t) {};\n", name, random_label));
			gen_code.push_str(&format!("{}->tl = (uintptr_t) {}", name, random_label2));
		}
	}
}

// Given the name of an identifier in the AST it will result in the actual name and the type of the variable.
// This is necessary because local variables can not always shadow variables with the same name in C while in
// SPL they can thus local variables are randomized in the generated code.
// (local variable shadowing a parameter with the same name is not allowed for example)
fn find_identifier<'a>(ident: &str, global_vars: &'a [(Ident, Type)], param_vars: &'a [(Ident, Type)],
	local_vars: &'a [(Ident, Type, Ident)]) -> (&'a Ident, &'a Type) {

	if let Some(pos) = local_vars.iter().position(|ref x| x.0 == *ident) {
		return (&local_vars[pos].2, &local_vars[pos].1);
	};

	if let Some(pos) = param_vars.iter().position(|ref x| x.0 == *ident) {
		return (&param_vars[pos].0, &param_vars[pos].1);
	};

	if let Some(pos) = global_vars.iter().position(|ref x| x.0 == *ident) {
		return (&global_vars[pos].0, &global_vars[pos].1);
	};

	unreachable!()
}
