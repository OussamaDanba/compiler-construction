use std::env;
use std::fs::File;
use std::io::prelude::*;

mod scanner;

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

	let tokens: Vec<scanner::Token>;
	match scanner::scan(&file_contents) {
		Err(index) => {
			let (line_number, line) = index_to_context(&file_contents, index);
			println!("Scanner error at line {} near {}:\n{}",
				line_number, file_contents.chars().collect::<Vec<char>>()[index], line);
			return ()
		},
		Ok(t) => tokens = t,
	}

	println!("Produced tokens: {:?}", tokens);
}

// Given an index it returns the line number and actual line the index is in
fn index_to_context(input: &str, index: usize) -> (usize, String) {
	(0, String::new())
}