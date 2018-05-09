use parser::{Ident, SPL, Function, Variable, Type, Statement, Expression, Field, Op2, Op1, Literal};
use std::collections::HashMap;

pub fn code_generator(ast: &SPL, expr_type: HashMap<*const Expression, Type>) -> String {
	// Store the top of the stack + 1 in R5
	let top = String::from("ldrr R5 SP\n");

	format!("{}{}bsr main\nhalt\n{}", top, generate_globals(&ast.vars, &expr_type), generate_functions(&ast.funs, &expr_type))
}

fn generate_globals(vars: &Vec<Variable>, expr_type: &HashMap<*const Expression, Type>) -> String {
	// TODO
	String::new()
}

fn generate_functions(funs: &Vec<Function>, expr_type: &HashMap<*const Expression, Type>) -> String {
	let mut gen_code = String::new();
	for fun in funs {
		if(fun.name != "print" && fun.name != "isEmpty") {
			gen_code.push_str(&format!("{}:\n", fun.name));

			// No need to do anything with the function arguments since the caller puts them on the stack for us

			gen_code.push_str(&format!("link {}\n", fun.vars.len()));

			// TODO: actual function bodies


			// TODO: store in RR


			let args_count = match fun.ftype {
				Type::TArrow(ref args, _) => args.len(),
				_ => panic!("Function type was not ftype!")
			};

			// Function clean up routine. Clears the entire stack frame.
			if args_count > 0 {
				gen_code.push_str(&format!("unlink\nsts -{}\nret\n", args_count));
			} else {
				gen_code.push_str("unlink\nret\n");
			}

		} else {
			// TODO: special handling
		}

	}



	gen_code
}
