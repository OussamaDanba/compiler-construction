pub use self::ast::*;
mod ast;

use scanner::Token;
use combine::{satisfy, token, many, Stream, Parser};

parser!{
	fn parse_type[I]()(I) -> Type
	  where [I: Stream<Item=Token>]
	{
		let tuple_parser = (token(Token::TokenParenOpen),
			parse_type(),
			token(Token::TokenComma),
			parse_type(),
			token(Token::TokenParenClose))
			.map(|t| Type::TTuple(Box::new(t.1), Box::new(t.3)));

		let list_parser = (token(Token::TokenBracketOpen),
			parse_type(),
			token(Token::TokenBracketClose))
			.map(|t| Type::TList(Box::new(t.1)));

		choice!(
			token(Token::TokenIntKeyword).map(|_| Type::TInt),
			token(Token::TokenBoolKeyword).map(|_| Type::TBool),
			token(Token::TokenCharKeyword).map(|_| Type::TChar),
			tuple_parser,
			list_parser,
			parse_identifier()
		)
	}
}

parser!{
	fn parse_identifier[I]()(I) -> Type
	  where [I: Stream<Item=Token>]
	{
		satisfy(|x: Token| match x {
			Token::TokenIdentifier(_) => true,
			_ => false
		}).map(|x: Token| match x {
			Token::TokenIdentifier(ident) => Type::TIdent(ident),
			_ => panic!("Unreachable")
		})
	}
}

pub fn parse(tokens: &[Token]) {
	println!("{:?}", many::<Vec<Type>, _>(parse_type()).parse(tokens));
}
