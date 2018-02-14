pub use self::token::Token;
mod token;

pub fn scan(input: &str) -> Option<Vec<Token>> {
	let trimmed_input = input.trim_left();

	// Base case
	if trimmed_input.is_empty() {
		return Some(Vec::new());
	}

	// Do functions here that take string and produce token + rest of string
	let result = scan_comment(trimmed_input)
				.or_else(|| scan_reserved(trimmed_input))
				.or_else(|| scan_int(trimmed_input))
				.or_else(|| scan_bool(trimmed_input))
				.or_else(|| scan_char(trimmed_input))
				.or_else(|| scan_identifiers(trimmed_input));

	match result {
		None => None,
		Some((token, new_input)) => {
			let mut resulting_tokens = Vec::new();
			resulting_tokens.push(token);

			let new_tokens = scan(new_input);
			match new_tokens {
				None => None,
				Some(mut x) => {
					resulting_tokens.append(&mut x);
					Some(resulting_tokens)
				}
			}
		}
	}
}

fn scan_comment(input: &str) -> Option<(Token, &str)> {
	if input.starts_with("//") {
		let mut new_input: Vec<&str> = input.splitn(2, '\n').collect();
		new_input[0] = new_input[0].trim_left_matches("//").trim();

		// In the case the file ends with a single line comment
		if new_input.len() < 2 {
			new_input.push("");
		}

		return Some((
			Token::TokenComment(String::from(new_input[0])),
			new_input[1]
		));
	}
	if input.starts_with("/*") {
		let mut new_input: Vec<&str> = input.splitn(2, "*/").collect();
		new_input[0] = new_input[0].trim_left_matches("/*").trim_right_matches("*/").trim();

		// In the case the file ends with a comment or there is no */ and the whole file is taken as a comment.
		// The second case is not checked for here but is left for the parser
		if new_input.len() < 2 {
			new_input.push("");
		}

		return Some((
			Token::TokenComment(String::from(new_input[0])),
			new_input[1]
		));
	}

	None
}

fn scan_reserved(input: &str) -> Option<(Token, &str)> {
	recognize_keyword(input, "var", Token::TokenVar)
	.or_else(|| recognize_keyword(input, "Void", Token::TokenVoid))
	.or_else(|| recognize_keyword(input, "Int", Token::TokenIntKeyword))
	.or_else(|| recognize_keyword(input, "Bool", Token::TokenBoolKeyword))
	.or_else(|| recognize_keyword(input, "Char", Token::TokenCharKeyword))
	.or_else(|| recognize_keyword(input, "if", Token::TokenIf))
	.or_else(|| recognize_keyword(input, "else", Token::TokenElse))
	.or_else(|| recognize_keyword(input, "while", Token::TokenWhile))
	.or_else(|| recognize_keyword(input, "return", Token::TokenReturn))
	// Two characters tokens have to be before single character tokens
	.or_else(|| recognize_symbol(input, "->", Token::TokenArrow))
	.or_else(|| recognize_symbol(input, "::", Token::TokenDoubleColon))
	.or_else(|| recognize_symbol(input, "==", Token::TokenDoubleEquals))
	.or_else(|| recognize_symbol(input, "<=", Token::TokenLtEquals))
	.or_else(|| recognize_symbol(input, ">=", Token::TokenGtEquals))
	.or_else(|| recognize_symbol(input, "!=", Token::TokenNotEquals))
	.or_else(|| recognize_symbol(input, "&&", Token::TokenDoubleAmp))
	.or_else(|| recognize_symbol(input, "||", Token::TokenDoublePipe))
	.or_else(|| recognize_symbol(input, "=", Token::TokenEquals))
	.or_else(|| recognize_symbol(input, "(", Token::TokenParenOpen))
	.or_else(|| recognize_symbol(input, ")", Token::TokenParenClose))
	.or_else(|| recognize_symbol(input, "[", Token::TokenBracketOpen))
	.or_else(|| recognize_symbol(input, "]", Token::TokenBracketClose))
	.or_else(|| recognize_symbol(input, "{", Token::TokenBraceOpen))
	.or_else(|| recognize_symbol(input, "}", Token::TokenBraceClose))
	.or_else(|| recognize_symbol(input, ":", Token::TokenColon))
	.or_else(|| recognize_symbol(input, ";", Token::TokenSemiColon))
	.or_else(|| recognize_symbol(input, ",", Token::TokenComma))
	.or_else(|| recognize_symbol(input, ".", Token::TokenDot))
	.or_else(|| recognize_symbol(input, "+", Token::TokenPlus))
	.or_else(|| recognize_symbol(input, "-", Token::TokenMinus))
	.or_else(|| recognize_symbol(input, "*", Token::TokenMult))
	.or_else(|| recognize_symbol(input, "/", Token::TokenDiv))
	.or_else(|| recognize_symbol(input, "%", Token::TokenMod))
	.or_else(|| recognize_symbol(input, "<", Token::TokenLt))
	.or_else(|| recognize_symbol(input, ">", Token::TokenGt))
	.or_else(|| recognize_symbol(input, "!", Token::TokenNot))
}

fn scan_int(input: &str) -> Option<(Token, &str)> {
	if input.starts_with(|c: char| c.is_digit(10)) {
		match input.find(|c: char| !c.is_digit(10)) {
			// Assume everything belongs to the int
			None => {
				return Some((
					Token::TokenInt(input.parse().unwrap()),
					""
				));
			},
			Some(i) => {
				let (integer, new_input) = input.split_at(i);
				return Some((
					Token::TokenInt(integer.parse().unwrap()),
					new_input
				));
			}
		};
	}

	None
}

fn scan_bool(input: &str) -> Option<(Token, &str)> {
	if input.starts_with("True") {
		let new_input: Vec<&str> = input.splitn(2, "True").collect();

		if !new_input[1].starts_with(is_identifier_char) {
			return Some((
				Token::TokenBool(true),
				new_input[1]
			));
		}
	}
	if input.starts_with("False") {
		let new_input: Vec<&str> = input.splitn(2, "False").collect();

		if !new_input[1].starts_with(is_identifier_char) {
			return Some((
				Token::TokenBool(false),
				new_input[1]
			));
		}
	}

	None
}

fn scan_char(input: &str) -> Option<(Token, &str)> {
	if input.starts_with('\'') {
		let (potential_char, new_input) = input.split_at(3);
		let potential_char_vec: Vec<char> = potential_char.chars().collect();

		if potential_char.len() < 3 || potential_char_vec[2] != '\'' {
			return None;
		}

		return Some((
			Token::TokenChar(potential_char_vec[1]),
			new_input
		));
	}

	None
}

fn scan_identifiers(input: &str) -> Option<(Token, &str)> {
	if input.starts_with(char::is_alphabetic) {
		let identifier: String = input.chars().take_while(|x| is_identifier_char(*x)).collect();
		let new_input = input.trim_left_matches(is_identifier_char);

		return Some((
			Token::TokenIdentifier(identifier),
			new_input
		));
	}

	None
}

// Exists mainly to prevent code duplication
fn recognize_keyword<'a>(input: &'a str, recognizer: &str, token: Token) -> Option<(Token, &'a str)> {
	if input.starts_with(recognizer) {
		let new_input: Vec<&str> = input.splitn(2, recognizer).collect();

		// This condition is needed to make sure a keyword is not part of an identifier
		if !new_input[1].starts_with(is_identifier_char) {
			return Some((
				token,
				new_input[1]
			));
		}
	}

	None
}

fn recognize_symbol<'a>(input: &'a str, recognizer: &str, token: Token) -> Option<(Token, &'a str)> {
	if input.starts_with(recognizer) {
		let new_input: Vec<&str> = input.splitn(2, recognizer).collect();
		return Some((
			token,
			new_input[1]
		));
	}

	None
}

fn is_identifier_char(c: char) -> bool {
	c == '_' || c.is_alphanumeric()
}
