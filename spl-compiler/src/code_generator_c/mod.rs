use parser::{Ident, SPL, Function, Variable, Type, Statement, Expression, Field, Op2, Op1, Literal};
use std::collections::HashMap;
use rand::{self, Rng};

pub fn code_generator(ast: &SPL, expr_type: &HashMap<*const Expression, Type>) -> String {
	// Some standard things we almost always need so just add them.
	let includes = String::from("#include <stdbool.h>\n#include <stdio.h>\n#include <stdint.h>\n#include <stdlib.h>\n");
	let tuple = String::from("typedef struct {\n\tuintptr_t fst;\n\tuintptr_t snd;\n} tuple;\n");
	let list = String::from("typedef struct {\n\tuintptr_t hd;\n\tuintptr_t tl;\n} list;\n");

	let (global_vars, global_decls) = generate_globals_decls(&ast.vars, expr_type);

	format!("{}\n{}\n{}\n{}\n{}\n{}\n", includes, tuple, list, global_decls,
		generate_functions(ast, &global_vars, expr_type), generate_globals_inits(&ast.vars, &global_vars, expr_type))
}

fn generate_globals_decls(vars: &[Variable], expr_type: &HashMap<*const Expression, Type>) -> (Vec<(Ident, Type)>, String) {
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
	gen_code.push_str("\nint main() {\n");
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
	gen_code.push_str("bool isEmpty(uintptr_t x) {\n\treturn ((list*) x)->tl == 0;\n}\n\n");

	// We have a default RuntimeErr function which handles runtime errors. A user can define this himself if they
	// wanted to.
	if ast.funs.iter().find(|x| x.name == "RuntimeErr").is_none() {
		gen_code.push_str("int RuntimeErr() {\n\texit(-1);\n}\n");
	}

	for fun in &ast.funs {
		// print and isEmpty require special handling
		if fun.name != "print" && fun.name != "isEmpty" {
			// TODO: completely
			// TODO: rename main to spl_main
		}
	}

	gen_code
}

fn generate_type_name(type_name: &Type) -> String {
	match *type_name {
		Type::TInt => String::from("int"),
		Type::TBool => String::from("bool"),
		Type::TChar => String::from("char"),
		Type::TTuple(ref left, ref right) => String::from("tuple*"),
		Type::TList(ref inner) => String::from("list*"),
		Type::TVoid => unreachable!(),
		Type::TArrow(_, _) => unreachable!()
	}
}

fn generate_ident(gen_code: String, ident: &Ident, ident_type: &Type, fields: &[Field]) -> String {
	if gen_code.is_empty() {
		generate_ident(format!("({}) {}", generate_type_name(ident_type), ident), ident, ident_type, fields)
	} else if fields.is_empty() {
		gen_code
	} else {
		let first_field = fields.first().unwrap();
		let next_fields = fields.split_at(1).1;
		match *first_field {
			// We have to do some extra work as asking for the head of an empty list doesn't make much sense.
			Field::Head => match *ident_type {
				Type::TList(ref ltype) => {
					generate_ident(
						format!("({}) (({})->tl == 0 ? RuntimeErr() : ({})->hd)",
							generate_type_name(ltype), gen_code, gen_code),
						ident, ident_type, next_fields)
				},
				_ => unreachable!()
			},
			// It is a deliberate choice that asking for the tail of an empty list is an error.
			Field::Tail => match *ident_type {
				Type::TList(ref ltype) => {
					generate_ident( format!("(list*) (({})->tl == 0 ? RuntimeErr() : ({})->tl)", gen_code, gen_code),
						ident, ident_type, next_fields)
				},
				_ => unreachable!()
			},
			Field::First => match *ident_type {
				Type::TTuple(ref fst_type, _) => {
					generate_ident(format!("({}) ({})->fst", generate_type_name(fst_type), gen_code),
						ident, fst_type, next_fields)
				},
				_ => unreachable!()
			},
			Field::Second => match *ident_type {
				Type::TTuple(_, ref snd_type) => {
					generate_ident(format!("({}) ({})->snd", generate_type_name(snd_type), gen_code),
						ident, snd_type, next_fields)
				},
				_ => unreachable!()
			}
		}
	}
}

fn generate_expression(name: &Ident, expr: &Expression, global_vars: &[(Ident, Type)], param_vars: &[(Ident, Type)],
	local_vars: &[(Ident, Type, Ident)], expr_type: &HashMap<*const Expression, Type>) -> String {

	let mut gen_code = String::new();
	match *expr {
		Expression::Ident(ref ident, ref fields) => {
			let (actual_ident, ident_type) = find_identifier(ident, global_vars, param_vars, local_vars);
			gen_code.push_str(&generate_ident(String::new(), actual_ident, ident_type, fields));
		},
		Expression::Op2(ref lexpr, ref op2, ref rexpr) => {
			// Need this because the arguments have to be evaluated first before being passed as parameters.
			gen_code.push_str("0;\n");

			generate_op2(name, lexpr, op2, rexpr, &mut gen_code, global_vars, param_vars, local_vars, expr_type);
		},
		Expression::Op1(ref op, ref expr) => {
			match *op {
				Op1::Not => {
					// The semantic analysis ensures the not operator only gets booleans. Parenthesis are placed
					// since some expressions need them.
					gen_code.push_str(&format!("!({})", generate_expression(name, expr, global_vars, param_vars, local_vars, expr_type)));
				},
				Op1::Negation => {
					// The semantic analysis ensures the negation operator only gets integers. Parenthesis are placed
					// since some expressions need them.
					gen_code.push_str(&format!("-({})", generate_expression(name, expr, global_vars, param_vars, local_vars, expr_type)));
				}
			}
		},
		Expression::Lit(ref x) => match *x {
			Literal::Int(ref val) => gen_code.push_str(&format!("{}", *val)),
			Literal::Bool(ref val) => gen_code.push_str(&format!("{}", if *val { "true" } else { "false" })),
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

fn generate_op2(name: &Ident, lexpr: &Expression, op2: &Op2, rexpr: &Expression,
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
				Type::TTuple(ref l, ref r) => {
					gen_code.push_str(&format!("{} = {} == {}", name, random_label, random_label2));
				},
				Type::TList(ref inner) => {
					gen_code.push_str(&format!("{} = {} == {}", name, random_label, random_label2));
				},
				_ => unreachable!()
			}
		},
		Op2::NotEquals => {
			generate_op2(name, lexpr, &Op2::Equals, rexpr, gen_code, global_vars, param_vars, local_vars, expr_type);
			gen_code.push_str(&format!("\n{} = !{}", name, name));
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
