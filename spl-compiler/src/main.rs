#[macro_use]
extern crate combine;
extern crate rand;

use std::env;
use std::fs::File;
use std::io::prelude::*;

mod scanner;
mod parser;
mod semantic_analysis;
mod code_generator_ssm;
mod code_generator_c;

fn main() {
	// Read in specified file into a String. We don't want to panic here so we use exit.
	let args: Vec<String> = env::args().collect();
	let filename: &str = args.get(1).unwrap_or_else(|| {
		println!("No filename specified.");
		::std::process::exit(1)
	});

	let mut file: File = File::open(filename).unwrap_or_else(|e| {
		println!("{}", e);
		::std::process::exit(1)
	});

	let mut file_contents = String::new();
	file.read_to_string(&mut file_contents).unwrap_or_else(|e| {
		println!("{}", e);
		::std::process::exit(1)
	});

	let tokens: Vec<(scanner::Token, usize)>;
	match scanner::scan(&file_contents) {
		Err(index) => {
			let (line_number, line) = index_to_context(&file_contents, index);
			println!("Scanner error at line {} near {}:\n{}",
				line_number, file_contents.chars().collect::<Vec<char>>()[index], line);
			return ()
		},
		Ok(t) => tokens = scanner_postprocessing(t, &file_contents),
	}

	let only_tokens: Vec<scanner::Token> = tokens.iter().map(|x| (x.0).clone()).collect();
	let mut spl: parser::SPL;
	match parser::parse(&only_tokens) {
		Err((token, index)) => {
			let (line_number, line) = index_to_context(&file_contents, tokens[index].1);
			println!("Parser error at line {}. Unexpected token {:?}:\n{}",
				line_number, token, line);
			return ()
		},
		Ok(x) => spl = x
	}

	println!("Parsed program pretty printed:\n{}\n", spl);

	// Hardcode the functions print and isEmpty into the AST.
	// Since we do not support polymorphic types we see "TVoid" as a placeholder to pass arity checking.
	// We don't care about the bodies since those will be generated during code generation
	spl.funs.push(parser::Function {
		name: String::from("print"),
		args: vec![String::from("a")],
		ftype: parser::Type::TArrow(vec![parser::Type::TVoid], Box::new(parser::Type::TVoid)),
		vars: Vec::new(),
		stmts: Vec::new()
	});
	spl.funs.push(parser::Function {
		name: String::from("isEmpty"),
		args: vec![String::from("a")],
		ftype: parser::Type::TArrow(vec![parser::Type::TList(Box::new(parser::Type::TVoid))], Box::new(parser::Type::TBool)),
		vars: Vec::new(),
		stmts: Vec::new()
	});

	let sem_result = semantic_analysis::semantic_analysis(&spl);
	if sem_result.is_none() {
		return;
	}

	let generated_code_ssm = code_generator_ssm::code_generator(&spl, sem_result.as_ref().unwrap());

	let mut output_filename = filename.to_owned();
	output_filename.push_str(".ssm");
	let mut file = match File::create(&output_filename) {
		Err(_) => {
			println!("Could not create ssm file.");
			::std::process::exit(1)
		},
		Ok(file) => file,
	};

	match file.write_all(generated_code_ssm.as_bytes()) {
		Err(_) => {
			println!("Could not write to ssm file.");
			::std::process::exit(1)
		},
		Ok(_) => println!("Code was generated into {}", output_filename)
	}

	let generated_code_c = code_generator_c::code_generator(&spl, sem_result.as_ref().unwrap());

	let mut output_filename = filename.to_owned();
	output_filename.push_str(".c");
	let mut file = match File::create(&output_filename) {
		Err(_) => {
			println!("Could not create c file.");
			::std::process::exit(1)
		},
		Ok(file) => file,
	};

	match file.write_all(generated_code_c.as_bytes()) {
		Err(_) => {
			println!("Could not write to c file.");
			::std::process::exit(1)
		},
		Ok(_) => println!("Code was generated into {}", output_filename)
	}
}

// Given an index it returns the line number and actual line the index is in
fn index_to_context(input: &str, index: usize) -> (usize, String) {
	let (front, back) = input.split_at(index);
	let mut front: Vec<char> = front.chars().
		rev().take_while(|&x| x != '\r' && x != '\n').collect::<Vec<char>>().into_iter().
		rev().collect();
	let mut back: Vec<char> = back.chars().take_while(|&x| x != '\r' && x != '\n').collect();

	front.append(&mut back);
	let line: String = front.into_iter().collect();

	// If for whatever reason the line can not be found it defaults to saying line 1
	(input.lines().position(|x| x == line).unwrap_or(0) + 1, line)
}

// Converts each token length pair to token index pair. Additionally removes comments as we do not care about those.
fn scanner_postprocessing(mut input: Vec<(scanner::Token, usize)>, file_contents: &str) -> Vec<(scanner::Token, usize)> {
	let mut accumulator: usize = 0;
	for pair in &mut input {
		let temp = pair.1;
		pair.1 = accumulator;
		accumulator += temp;
	}

	// Some tokens have whitespace characters before them. We trim this whitespace here
	for pair in &mut input {
		let split_string = file_contents.split_at(pair.1).1;
		pair.1 += split_string.len() - split_string.chars().skip_while(|c| c.is_whitespace()).collect::<Vec<char>>().len()
	}

	input.into_iter().filter(|x| match x.0 {
		scanner::Token::TokenComment(_) => false,
		_ => true,
	}).collect()
}
