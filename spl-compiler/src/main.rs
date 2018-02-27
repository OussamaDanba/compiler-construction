#[macro_use]
extern crate combine;

use std::env;
use std::fs::File;
use std::io::prelude::*;

mod scanner;
mod parser;

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

	let mut tokens: Vec<scanner::Token>;
	match scanner::scan(&file_contents) {
		Err(index) => {
			let (line_number, line) = index_to_context(&file_contents, index);
			println!("Scanner error at line {} near {}:\n{}",
				line_number, file_contents.chars().collect::<Vec<char>>()[index], line);
			return ()
		},
		Ok(t) => tokens = t,
	}

	// Remove comment tokens as we do not care about those
	tokens = tokens.into_iter().filter(|x| match *x {
		scanner::Token::TokenComment(_) => false,
		_ => true,
	}).collect();

	println!("Produced tokens: {:?}", tokens);

	parser::parse(&tokens);
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
