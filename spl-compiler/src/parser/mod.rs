pub use self::ast::*;
mod ast;

use scanner::Token;
use combine::{satisfy, satisfy_map, token, many, optional, Stream, Parser};

// parser!{
// 	fn parse_type[I]()(I) -> Type
// 	  where [I: Stream<Item=Token>]
// 	{
// 		let tuple_parser = (token(Token::TokenParenOpen),
// 			parse_type(),
// 			token(Token::TokenComma),
// 			parse_type(),
// 			token(Token::TokenParenClose))
// 			.map(|t| Type::TTuple(Box::new(t.1), Box::new(t.3)));

// 		let list_parser = (token(Token::TokenBracketOpen),
// 			parse_type(),
// 			token(Token::TokenBracketClose))
// 			.map(|t| Type::TList(Box::new(t.1)));

// 		choice!(
// 			token(Token::TokenIntKeyword).map(|_| Type::TInt),
// 			token(Token::TokenBoolKeyword).map(|_| Type::TBool),
// 			token(Token::TokenCharKeyword).map(|_| Type::TChar),
// 			tuple_parser,
// 			list_parser,
// 			parse_identifier()
// 		)
// 	}
// }

parser!{
	fn parse_identifier[I]()(I) -> Ident
	  where [I: Stream<Item=Token>]
	{
		satisfy_map(|x: Token| match x {
			Token::TokenIdentifier(ident) => Some(ident),
			_ => None,
		})
	}
}

parser!{
	fn parse_field[I]()(I) -> Vec<Field>
	  where [I: Stream<Item=Token>]
	{
		many((token(Token::TokenDot), satisfy_map(|x: Token| match x {
			Token::TokenIdentifier(ident) => match ident.as_ref() {
				"hd" => Some(Field::Head),
				"tl" => Some(Field::Tail),
				"fst" => Some(Field::First),
				"snd" => Some(Field::Second),
				_ => None,
			},
			_ => None,
		})).map(|t| t.1))
	}
}

parser!{
	fn parse_literal[I]()(I) -> Literal
	  where [I: Stream<Item=Token>]
	{
		let parse_int = (optional(token(Token::TokenMinus)), satisfy_map(|x| match x {
			Token::TokenInt(i) => Some(i),
			_ => None
		})).map(|(min, num)| Literal::Int( match min {
			Some(_) => num.wrapping_neg(),
			_ => num
		}));

		let parse_char = satisfy_map(|x| match x {
			Token::TokenChar(c) => Some(c),
			_ => None
		}).map(|c| Literal::Char(c));

		let parse_bool = satisfy_map(|x| match x {
			Token::TokenBool(b) => Some(b),
			_ => None
		}).map(|b| Literal::Bool(b));

		let parse_empty_list = (token(Token::TokenBracketOpen), token(Token::TokenBracketClose))
			.map(|_| Literal::EmptyList);

		choice!(
			parse_int,
			parse_char,
			parse_bool,
			parse_empty_list
		)
	}
}

parser!{
	fn parse_expression_no_op2[I]()(I) -> Expression
	  where [I: Stream<Item=Token>]
	{
		choice!(
			(parse_identifier(), parse_field()).map(|(i, fs)| Expression::Ident(i, fs)),
			parse_literal().map(|lit| Expression::Lit(lit)),
			token(Token::TokenIntKeyword).map(|_| Expression::Lit(Literal::EmptyList))
		)
	}
}

pub fn parse(tokens: &[Token]) {
	println!("{:?}", parse_expression_no_op2().parse(tokens));
}
