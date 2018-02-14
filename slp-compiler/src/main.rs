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

	println!("{:?}", scanner::scan(&file_contents));
}
