use std::env;
use std::fs::File;
use std::io::prelude::*;

fn main() {
	// Read in specified file into a String. We don't want to panick here so we use exit. 
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

	println!("{}", file_contents);
}
