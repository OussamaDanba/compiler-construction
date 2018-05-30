use parser::{Ident, SPL, Function, Variable, Type, Statement, Expression, Field, Op2, Op1, Literal};
use std::collections::HashMap;
use rand::{self, Rng};

pub fn code_generator(ast: &SPL, expr_type: &HashMap<*const Expression, Type>) -> String {
	// Some standard things we almost always need so just add them.
	let includes = String::from("#include <stdbool.h>\n#include <stdio.h>\n#include <stdint.h>\n#include <stdlib.h>\n");
	let tuple = String::from("typedef struct {\n\tuintptr_t fst;\n\tuintptr_t snd;\n} _tuple;\n");
	let list = String::from("typedef struct {\n\tuintptr_t hd;\n\tuintptr_t tl;\n} _list;\n");

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
	gen_code.push_str("bool isEmpty(_list* x) {\n\treturn x->tl == 0;\n}\n\n");

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
					let random_label = gen_random_label();

					gen_code.push_str(&format!("{} {} = {};\n",
						generate_type_name(&var.vtype),
						random_label,
						generate_expression(&random_label, &var.value, global_vars, &param_vars, &local_vars, expr_type)));


					// Ensure local vars shadow previous ones by removing the previous one
					if let Some(pos) = local_vars.iter().position(|x| x.0 == var.name) {
						local_vars.remove(pos);
					}
					local_vars.push((var.name.clone(), var.vtype.clone(), random_label.clone()));
				}

				for stmt in &fun.stmts {
					let statement_code = generate_statements(stmt, fun, global_vars, &param_vars, &local_vars, expr_type);
					gen_code.push_str(&statement_code);
				}

				gen_code.push_str("}\n\n");
			}

		}
	}

	gen_code
}

fn generate_statements(stmt: &Statement, fun: &Function, global_vars: &[(Ident, Type)], param_vars: &[(Ident, Type)],
	local_vars: &[(Ident, Type, Ident)], expr_type: &HashMap<*const Expression, Type>) -> String {

	let mut gen_code = String::new();
	match *stmt {
		Statement::If(ref expr, ref if_stmts, ref else_stmts) => {
			// Evaluate the expression
			let random_label = gen_random_label();
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
			let random_label = gen_random_label();
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
			let random_label = gen_random_label();
			gen_code.push_str(&format!("{} {} = {};\n",
				generate_type_name(expr_type.get(&(&(*expr) as *const Expression)).unwrap()),
				random_label,
				generate_expression(&random_label, &expr, global_vars, param_vars, local_vars, expr_type))
			);

			let (actual_ident, ident_type) = find_identifier(ident, global_vars, param_vars, local_vars);
			// This solely exists for error checking purposes
			gen_code.push_str(&format!("uintptr_t {} = (uintptr_t) {};\n",
				gen_random_label(),
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
					let random_label = gen_random_label();
					gen_code.push_str(&format!("{} {} = {};\n",
						generate_type_name(expr_type.get(&(&(*expr) as *const Expression)).unwrap()),
						random_label,
						generate_expression(&random_label, &expr, global_vars, param_vars, local_vars, expr_type))
					);

					arguments.push_str(&format!("{},", random_label));
				}
				// Get rid of the trailing comma
				arguments.pop();

				// print is generated inline as it is overloaded
				if ident == "print" {
					// print is guaranteed to only have one expression and has return type Void
					let expr_type = expr_type.get(&(&exprs[0] as *const Expression)).unwrap();
					gen_code.push_str(&generate_print(&arguments, expr_type));

					// Add a newline at the end
					gen_code.push_str("printf(\"\\n\");\n");
				} else {
					gen_code.push_str(&format!("{}({});\n", ident, arguments));
				}
			}
		},
		Statement::Return(ref option_expr) => match *option_expr {
			Some(ref expr) => {
				if let Type::TArrow(_, ref fun_ret_type) = fun.ftype {
					let random_label = gen_random_label();
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
		Type::TTuple(_, _) => String::from("_tuple*"),
		Type::TList(_) => String::from("_list*"),
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
						format!("(_list*) (({})->tl == 0 ? RuntimeErr() : ({})->tl)", gen_code, gen_code)
					} else {
						format!("(_list*) ({})->tl", gen_code)
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

			generate_op2(None, name, &gen_random_label(), &gen_random_label(), lexpr, op2, rexpr, &mut gen_code,
				global_vars, param_vars, local_vars, expr_type);
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
				gen_code.push_str("malloc(sizeof(_list));\n");
				gen_code.push_str(&format!("{}->tl = 0", name));
			},
		},
		Expression::FunCall(ref ident, ref exprs) => {
			// Need this because the arguments have to be evaluated first before being passed as parameters.
			gen_code.push_str("0;\n");

			// Evaluate the arguments and construct the string that is passed
			let mut arguments = String::new();
			for expr in exprs {
				let random_label = gen_random_label();
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
			gen_code.push_str("malloc(sizeof(_tuple));\n");
			// Left side
			let random_label = gen_random_label();
			gen_code.push_str(&format!("{} {} = {};\n",
				generate_type_name(expr_type.get(&(&(**left) as *const Expression)).unwrap()),
				random_label,
				generate_expression(&random_label, &left, global_vars, param_vars, local_vars, expr_type))
			);
			gen_code.push_str(&format!("{}->fst = (uintptr_t) {};\n", name, random_label));

			// Right side
			let random_label2 = gen_random_label();
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

fn generate_op2(type_known: Option<Type>, name: &str, lname: &str, rname: &str, lexpr: &Expression, op2: &Op2, rexpr: &Expression,
	gen_code: &mut String, global_vars: &[(Ident, Type)], param_vars: &[(Ident, Type)], local_vars: &[(Ident, Type, Ident)],
	expr_type: &HashMap<*const Expression, Type>) {

	if type_known.is_none() {
		// Evaluate the left expression
		gen_code.push_str(&format!("{} {} = {};\n",
			generate_type_name(expr_type.get(&(lexpr as *const Expression)).unwrap()),
			lname,
			generate_expression(&lname, lexpr, global_vars, param_vars, local_vars, expr_type))
		);

		// Evaluate the right expression
		gen_code.push_str(&format!("{} {} = {};\n",
			generate_type_name(expr_type.get(&(rexpr as *const Expression)).unwrap()),
			rname,
			generate_expression(&rname, rexpr, global_vars, param_vars, local_vars, expr_type))
		);
	}

	match *op2 {
		// These operators are only defined for when the left and right side are values rather than pointers
		Op2::Addition => gen_code.push_str(&format!("{} = {} + {}", name, lname, rname)),
		Op2::Subtraction => gen_code.push_str(&format!("{} = {} - {}", name, lname, rname)),
		Op2::Multiplication => gen_code.push_str(&format!("{} = {} * {}", name, lname, rname)),
		Op2::Division => gen_code.push_str(&format!("{} = {} / {}", name, lname, rname)),
		Op2::Modulo => gen_code.push_str(&format!("{} = {} % {}", name, lname, rname)),
		Op2::LessThan => gen_code.push_str(&format!("{} = {} < {}", name, lname, rname)),
		Op2::GreaterThan => gen_code.push_str(&format!("{} = {} > {}", name, lname, rname)),
		Op2::LessEquals => gen_code.push_str(&format!("{} = {} <= {}", name, lname, rname)),
		Op2::GreaterEquals => gen_code.push_str(&format!("{} = {} >= {}", name, lname, rname)),
		// Since left and right are evaluated separately the short-circuiting here is not a problem
		Op2::And => gen_code.push_str(&format!("{} = {} && {}", name, lname, rname)),
		Op2::Or => gen_code.push_str(&format!("{} = {} || {}", name, lname, rname)),
		Op2::Equals => {
			let type_known = if type_known.is_none() {
				expr_type.get(&(lexpr as *const Expression)).unwrap().clone()
			} else {
				type_known.unwrap()
			};

			match type_known {
				| Type::TInt
				| Type::TBool
				| Type::TChar => gen_code.push_str(&format!("{} = {} == {}", name, lname, rname)),
				Type::TTuple(ref left, ref right) => {
					let random_label = gen_random_label();
					let random_label2 = gen_random_label();
					let random_label3 = gen_random_label();
					let random_label4 = gen_random_label();
					let random_label5 = gen_random_label();
					let random_label6 = gen_random_label();

					// Equality of left side
					gen_code.push_str(&format!("{} {} = ({}) {}->fst;\n", generate_type_name(left), random_label, generate_type_name(left), lname));
					gen_code.push_str(&format!("{} {} = ({}) {}->fst;\n", generate_type_name(left), random_label2, generate_type_name(left), rname));
					gen_code.push_str(&format!("bool {} = true;\n", random_label3));
					generate_op2(Some(*left.clone()), &random_label3, &random_label, &random_label2, lexpr, op2, rexpr, gen_code, global_vars, param_vars, local_vars, expr_type);

					// Equality of right side
					gen_code.push_str(&format!(";\n{} {} = ({}) {}->snd;\n", generate_type_name(right), random_label4, generate_type_name(right), lname));
					gen_code.push_str(&format!("{} {} = ({}) {}->snd;\n", generate_type_name(right), random_label5, generate_type_name(right), rname));
					gen_code.push_str(&format!("bool {} = true;\n", random_label6));
					generate_op2(Some(*right.clone()), &random_label6, &random_label4, &random_label5, lexpr, op2, rexpr, gen_code, global_vars, param_vars, local_vars, expr_type);

					gen_code.push_str(&format!(";\n{} = {} && {}", name, random_label3, random_label6));
				},
				Type::TList(ref inner) => {
					let random_bool = gen_random_label();
					let random_label = gen_random_label();
					let random_label2 = gen_random_label();
					let random_label3 = gen_random_label();

					// Special handling for empty lists
					if let Type::TVoid = **inner {
						gen_code.push_str(&format!("{} = (_list*) {}->tl == 0 && (_list*) {}->tl == 0", name, lname, rname));
					} else {
						// Code that continuously checks if the heads are the same or not
						gen_code.push_str(&format!("bool {} = true;\n", random_bool));
						gen_code.push_str(&format!("while(!isEmpty({}) && !isEmpty({})) {{\n", lname, rname));
						gen_code.push_str(&format!("{} {} = ({}) {}->hd;\n", generate_type_name(inner), random_label, generate_type_name(inner), lname));
						gen_code.push_str(&format!("{} {} = ({}) {}->hd;\n", generate_type_name(inner), random_label2, generate_type_name(inner), rname));
						gen_code.push_str(&format!("bool {} = true;\n", random_label3));
						generate_op2(Some(*inner.clone()), &random_label3, &random_label, &random_label2, lexpr, op2, rexpr, gen_code, global_vars, param_vars, local_vars, expr_type);
						gen_code.push_str(&format!(";\n{} = {} && {};\n", random_bool, random_bool, random_label3));
						gen_code.push_str(&format!("{} = (_list*) {}->tl;\n", lname, lname));
						gen_code.push_str(&format!("{} = (_list*) {}->tl;\n", rname, rname));
						gen_code.push_str("}\n");

						// Code that checks if we are indeed equal or if one of them is empty
						gen_code.push_str(&format!("{} = isEmpty({}) && isEmpty({}) ? {} : false", name, lname, rname, random_bool));
					}
				},
				_ => unreachable!()
			}
		},
		Op2::NotEquals => {
			let type_known = if type_known.is_none() {
				expr_type.get(&(lexpr as *const Expression)).unwrap().clone()
			} else {
				type_known.unwrap()
			};

			generate_op2(Some(type_known), name, lname, rname, lexpr, &Op2::Equals, rexpr, gen_code, global_vars, param_vars, local_vars, expr_type);
			gen_code.push_str(&format!(";\n{} = !{}", name, name));
		},
		Op2::Cons => {
			gen_code.push_str(&format!("{} = malloc(sizeof(_list));\n", name));
			gen_code.push_str(&format!("{}->hd = (uintptr_t) {};\n", name, lname));
			gen_code.push_str(&format!("{}->tl = (uintptr_t) {}", name, rname));
		}
	}
}

fn generate_print(var_name: &str, expr_type: &Type) -> String {
	let mut gen_code = String::new();
	match *expr_type {
		Type::TInt => {
			gen_code.push_str(&format!("printf(\"%i\", {});\n", var_name));
		},
		Type::TBool => {
			gen_code.push_str(&format!("if({}) {{\nprintf(\"True\");\n}} else {{\nprintf(\"False\");\n}}\n", var_name));
		},
		Type::TChar => {
			gen_code.push_str(&format!("printf(\"%c\", {});\n", var_name));
		},
		Type::TTuple(ref l, ref r) => {
			gen_code.push_str("printf(\"(\");\n");
			gen_code.push_str(&generate_print(&format!("(({}) {}->fst)", generate_type_name(l), var_name), l));
			gen_code.push_str("printf(\", \");\n");
			gen_code.push_str(&generate_print(&format!("(({}) {}->snd)", generate_type_name(r), var_name), r));
			gen_code.push_str("printf(\")\");\n");
		},
		Type::TList(ref inner) => {
			gen_code.push_str(&format!("while(!isEmpty({})) {{\n{}{}{}}}\n",
				var_name,
				generate_print(&format!("(({}) {}->hd)", generate_type_name(inner), var_name), inner),
				"printf(\" : \");\n",
				format!("{} = (_list*) {}->tl;\n", var_name, var_name))
			);
			gen_code.push_str("printf(\"[]\");\n");
		},
		Type::TVoid => (),
		_ => unreachable!()
	}

	gen_code
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

fn gen_random_label() -> String {
	let mut random = rand::thread_rng().gen_ascii_chars().take(9).collect::<String>();
	random.insert(0, '_');
	random
}
