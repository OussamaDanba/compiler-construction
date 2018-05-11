use parser::{Ident, SPL, Function, Variable, Type, Statement, Expression, Field, Op2, Op1, Literal};
use std::collections::HashMap;
use rand::{self, Rng};

pub fn code_generator(ast: &SPL, expr_type: HashMap<*const Expression, Type>) -> String {
	// Store the top of the stack + 1 in R5
	let top = String::from("ldrr R5 SP\n");

	// Gets all the global variables and the code to initialize at the start of the stack
	let (global_vars, gen_code) = generate_globals(&ast.vars, &expr_type);

	format!("{}{}bsr main\nhalt\n{}", top, gen_code, generate_functions(ast, &global_vars, &expr_type))
}

fn generate_globals(vars: &Vec<Variable>, expr_type: &HashMap<*const Expression, Type>) -> (Vec<(Ident, Type)>, String) {
	// We need to store all the global variables we have somewhere so that we can find them later when evaluating expressions
	let mut global_vars = Vec::new();
	for var in vars {
		global_vars.push((var.name.clone(), var.vtype.clone()));
	}

	let mut gen_code = String::new();
	for var in vars {
		gen_code.push_str(&generate_expression(&var.value, &global_vars, &Vec::new(), &Vec::new(), expr_type));
	}

	(global_vars, gen_code)
}

fn generate_functions(ast: &SPL, global_vars: &Vec<(Ident, Type)>, expr_type: &HashMap<*const Expression, Type>) -> String {
	let mut gen_code = String::new();
	for fun in &ast.funs {
		// print and isEmpty require special handling as they are polymorphic
		if fun.name != "print" && fun.name != "isEmpty" {
			let fun_args = match fun.ftype {
				Type::TArrow(ref args, _) => args,
				_ => panic!("Function type was not ftype!")
			};

			// We need to store all the parameter vars we have somewhere so that we can find them later when evaluating expressions
			let param_vars: Vec<(Ident, Type)> = fun.args.iter().zip(fun_args).map(|(x, y)| (x.clone(), y.clone())).collect();
			// We need to store all the local vars we have somewhere so that we can find them later when evaluating expressions
			let mut local_vars = Vec::new();
			for var in &fun.vars {
				local_vars.push((var.name.clone(), var.vtype.clone()));
			}

			// Generate the label for the current function
			gen_code.push_str(&format!("{}:\n", fun.name));

			// No need to do anything with the function arguments since the caller puts them on the stack for us

			// Reserve space for the locals of this function
			gen_code.push_str(&format!("link {}\n", fun.vars.len()));

			// Code for the locals:
			// Every expression results in a value (which is either a plain value or a pointer). So generate the
			// code that evaluates this and puts this value at the top of the stack. Afterwards move it to the
			// place where it belongs.
			for (index, var) in fun.vars.iter().enumerate() {
				let expression_code = generate_expression(&var.value, global_vars, &param_vars, &local_vars, expr_type);
				gen_code.push_str(&expression_code);

				// Store computed value at the place where it belongs
				gen_code.push_str(&format!("stl {}\n", index + 1));
			}

			// Code for the statements:
			// TODO: actually write the code

			// TODO: store in RR (don't forget about void functions!)

			// Function clean up routine. Clears the entire stack frame. (including the parameters the previous function
			// set on the stack for this function)
			if fun_args.len() > 0 {
				gen_code.push_str(&format!("unlink\nsts -{}\najs -{}\nret\n", fun_args.len(), fun_args.len() - 1));
			} else {
				gen_code.push_str("unlink\nret\n");
			}
		} else {
			// TODO: special handling
		}
	}

	// We have a default RuntimeErr function which handles runtime errors. A user can define this himself if they
	// wanted to.
	if ast.funs.iter().find(|x| x.name == "RuntimeErr").is_none() {
		gen_code.push_str("RuntimeErr:\ntrap 0\nhalt\n");
	}

	gen_code
}

// Expression can contain variables of all scopes so we need to pass which variables are present in what scope
// so that the generated code uses the correct variable.
fn generate_expression(expr: &Expression, global_vars: &Vec<(Ident, Type)>, param_vars: &Vec<(Ident, Type)>,
	local_vars: &Vec<(Ident, Type)>, expr_type: &HashMap<*const Expression, Type>) -> String {

	let mut gen_code = String::new();
	match *expr {
		Expression::Ident(ref ident, ref fields) => {
			let (var, code) = find_identifier(ident, global_vars, param_vars, local_vars);
			gen_code.push_str(&code);

			if !fields.is_empty() {
				gen_code = fields.iter().fold(gen_code, |mut acc, ref x| {
					match **x {
						Field::First => { acc.push_str("ldh 0\n"); acc },
						Field::Second => { acc.push_str("ldh -1\n"); acc },
						// First we check if we are dealing with an empty list since hd/tl can not be checked at compile
						// time for that. We simply abort if that is the case as we were given wrong code.
						Field::Head => {
							acc.push_str("ldmh 0 2\nswp\nldc 0\neq\nbrt RuntimeErr\n");
							acc
						},
						Field::Tail => {
							acc.push_str("ldh -1\nlds 0\nldc 0\neq\nbrt RuntimeErr\n");
							acc
						}
					}
				});
			}
		},
		Expression::Op2(ref lexpr, ref op2, ref rexpr) => {
			// Split into a separate function for convenience
			generate_op2(None, lexpr, op2, rexpr, &mut gen_code, global_vars, param_vars, local_vars, expr_type);
		},
		Expression::Op1(ref op, ref expr) => {
			gen_code.push_str(&generate_expression(expr, global_vars, param_vars, local_vars, expr_type));
			match *op {
				Op1::Not => {
					// The semantic analysis ensures the not operator only gets booleans. Since booleans are not tuples
					// or lists we know for a fact that they are on the stack which makes things easier.
					gen_code.push_str("not\n");
				},
				Op1::Negation => {
					// The semantic analysis ensures the negation operator only gets integers. Since integers are not tuples
					// or lists we know for a fact that they are on the stack which makes things easier.
					gen_code.push_str("neg\n");
				}
			}
		},
		Expression::Lit(ref x) => match *x {
			Literal::Int(ref val) => gen_code.push_str(&format!("ldc {}\n", *val)),
			Literal::Bool(ref val) => gen_code.push_str(&format!("ldc {}\n", if *val { -1 } else { 0 } )),
			Literal::Char(ref val) => gen_code.push_str(&format!("ldc {}\n", *val as u32)),
			Literal::EmptyList => {
				// Empty list is encoded as 2 words where the second word has a value of 0x0.
				gen_code.push_str("ldc 0\nldc 0\nstmh 2\n");
			},
		},
		Expression::FunCall(ref ident, ref exprs) => {
			// Evaluate all the expressions. This will result in n values on the stack which are then left there
			// to implicitly be passed to the function that is called.
			// Note that they are put on the stack in reverse order. This makes it easier to find them later when
			// we are given the mark pointer.
			for sub_expr in exprs.iter().rev() {
				gen_code.push_str(&generate_expression(sub_expr, global_vars, param_vars, local_vars, expr_type));
			}

			// Branch to the function. This will clean up the stack after itself.
			gen_code.push_str(&format!("bsr {}\n", ident));

			// Put the return value on the stack
			gen_code.push_str("ldr RR\n");
		},
		Expression::Tuple(ref left, ref right) => {
			// For tuples we evaluate the two expressions of the stack and then store them on a heap.
			// The result is a pointer to the heap.
			// Note that we put them in reverse order on the stack so that the pointer points to left instead of
			// right.
			gen_code.push_str(&generate_expression(right, global_vars, param_vars, local_vars, expr_type));
			gen_code.push_str(&generate_expression(left, global_vars, param_vars, local_vars, expr_type));

			gen_code.push_str("stmh 2\n");
		}
	}

	gen_code
}

fn generate_op2(type_known: Option<Type>, lexpr: &Expression, op2: &Op2, rexpr: &Expression, gen_code: &mut String, global_vars: &Vec<(Ident, Type)>,
	param_vars: &Vec<(Ident, Type)>, local_vars: &Vec<(Ident, Type)>, expr_type: &HashMap<*const Expression, Type>) {

	// In almost all cases we want to evaluate the left and right side. How we deal with the result of this is specific
	// to the operator.
	if type_known.is_none() {
		gen_code.push_str(&generate_expression(lexpr, global_vars, param_vars, local_vars, expr_type));
		gen_code.push_str(&generate_expression(rexpr, global_vars, param_vars, local_vars, expr_type));
	}

	match *op2 {
		// These operators are only defined for when the left and right side are values rather than pointers
		Op2::Addition => gen_code.push_str("add\n"),
		Op2::Subtraction => gen_code.push_str("sub\n"),
		Op2::Multiplication => gen_code.push_str("mul\n"),
		Op2::Division => gen_code.push_str("div\n"),
		Op2::Modulo => gen_code.push_str("mod\n"),
		Op2::LessThan => gen_code.push_str("lt\n"),
		Op2::GreaterThan => gen_code.push_str("gt\n"),
		Op2::LessEquals => gen_code.push_str("le\n"),
		Op2::GreaterEquals => gen_code.push_str("ge\n"),
		Op2::And => gen_code.push_str("and\n"),
		Op2::Or => gen_code.push_str("or\n"),
		// These operators can have pointers thus require more work
		Op2::Equals => {
			let type_known = if type_known.is_none() {
				expr_type.get(&(lexpr as *const Expression)).unwrap().clone()
			} else {
				type_known.unwrap()
			};

			match type_known {
				| Type::TInt
				| Type::TBool
				| Type::TChar => gen_code.push_str("eq\n"),
				Type::TTuple(ref l, ref r) => {
					gen_code.push_str("lds 0\nldh 0\nlds -2\nldh 0\n");
					generate_op2(Some((**l).clone()), lexpr, op2, rexpr, gen_code, global_vars, param_vars, local_vars, expr_type);

					gen_code.push_str("lds -1\nldh -1\nlds -3\nldh -1\n");
					generate_op2(Some((**r).clone()), lexpr, op2, rexpr, gen_code, global_vars, param_vars, local_vars, expr_type);

					gen_code.push_str("eq\nsts -2\najs -1\n");
				},
				Type::TList(ref inner) => {
					// This code needs to jump/loop so generate labels for it
					let random_label = rand::thread_rng().gen_ascii_chars().take(10).collect::<String>();
					let random_label2 = rand::thread_rng().gen_ascii_chars().take(10).collect::<String>();
					let random_label3 = rand::thread_rng().gen_ascii_chars().take(10).collect::<String>();
					gen_code.push_str(&format!("{}:\n", random_label));

					// Checks the tail of the list to see if only one of the two has an empty list as tail.
					// If that's the case we know for sure the lists are not equal (since they have different length).
					gen_code.push_str(&format!("lds 0\nldh -1\nlds -2\nldh -1\nldc 0\neq\nlds -1\nldc 0\neq\neq\nsts -1\nbrf {}\n", random_label2));

					// Load values from the heap
					gen_code.push_str("ajs 1\nlds -1\nldh 0\nlds -3\nldh 0\n");

					// Check explicitly if we're at the emptylist so we don't accidentally try to dereference
					// a null pointer.
					gen_code.push_str(&format!("lds 0\nlds -2\neq\nbrt {}\n", random_label3));

					// Compare the heads of lists
					generate_op2(Some((**inner).clone()), lexpr, op2, rexpr, gen_code, global_vars, param_vars, local_vars, expr_type);
					gen_code.push_str(&format!("eq\nbrf {}\nldh -1\najs -1\nldh -1\najs 1\nbra {}\n", random_label2, random_label));

					// In case we had an empty list we need to do a bit of stack adjustment before falling through.
					gen_code.push_str(&format!("{}:\n", random_label3));
					gen_code.push_str("ajs -3\n");

					// Cleanup
					gen_code.push_str(&format!("{}:\n", random_label2));
					gen_code.push_str("ajs 1\nsts -2\najs -1\n");
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
			generate_op2(Some(type_known), lexpr, &Op2::Equals, rexpr, gen_code, global_vars, param_vars, local_vars, expr_type);
			gen_code.push_str("not\n");
		},
		Op2::Cons => {
			// Lists are generated as linked lists. So when given a pointer to a list you are given a
			// pointer to the head. In the word next to is a pointer to the head of the tail. And so on.
			// Every list always ends with the empty list. The empty list itself is encoded as having a pointer to 0x0
			// (which is guaranteed to be the code segment and thus never actually used for data).
			// Note the swap so the stack values end up in the heap in a more convenient way. Technically we can generate
			// more efficient code for some code duplication here.
			gen_code.push_str("swp\nstmh 2\n");
		}
	}
}

// A helper function to find an identifier and puts it at the top of the stack. First checks the local variables,
// then the parameters, and only then the global variables.
fn find_identifier<'a>(ident: &Ident, global_vars: &'a Vec<(Ident, Type)>, param_vars: &'a Vec<(Ident, Type)>,
	local_vars: &'a Vec<(Ident, Type)>) -> (&'a (Ident, Type), String) {

	match local_vars.iter().position(|ref x| x.0 == *ident) {
		Some(pos) => {
			return (&local_vars[pos], format!("ldl {}\n", pos + 1));
		},
		None => ()
	}

	match param_vars.iter().position(|ref x| x.0 == *ident) {
		Some(pos) => {
			// Note that we skip over the return address!
			return (&param_vars[pos], format!("ldl -{}\n", pos + 2));
		},
		None => ()
	}

	match global_vars.iter().position(|ref x| x.0 == *ident) {
		Some(pos) => {
			// Push R5 on the stack and use it to load the global variable
			return (&global_vars[pos], format!("ldr R5\nlda {}\n", pos + 1));
		},
		None => ()
	}

	panic!("Unreachable code in find_identifier")
}
