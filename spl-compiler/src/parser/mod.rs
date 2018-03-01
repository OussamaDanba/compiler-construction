pub use self::ast::*;
mod ast;

use scanner::Token;
use combine::{satisfy_map, token, many, optional, try, Stream, Parser};

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
		}).map(Literal::Char);

		let parse_bool = satisfy_map(|x| match x {
			Token::TokenBool(b) => Some(b),
			_ => None
		}).map(Literal::Bool);

		let parse_empty_list = (token(Token::TokenBracketOpen), token(Token::TokenBracketClose))
			.map(|_| Literal::EmptyList);

		// try is needed here as we may consume part of the tokens when failing
		choice!(
			try(parse_int),
			try(parse_char),
			try(parse_bool),
			try(parse_empty_list)
		)
	}
}

parser!{
	fn parse_exp[I]()(I) -> Expression
	  where [I: Stream<Item=Token>]
	{
		(parse_exp2(), optional((token(Token::TokenMod), parse_exp())))
			.map(|(l, r)| match r {
				Some((_, x)) => Expression::Op2(Box::new(l), Op2::Modulo, Box::new(x)),
				_ => l
		})
	}
}

parser!{
	fn parse_exp2[I]()(I) -> Expression
	  where [I: Stream<Item=Token>]
	{
		let allowed_tokens = choice!(
			token(Token::TokenPlus),
			token(Token::TokenMinus)
		);

		let mapping = |x| match x {
			Token::TokenPlus => Op2::Addition,
			Token::TokenMinus => Op2::Subtraction,
			_ => panic!("Unreachable")
		};

		(parse_exp3(), optional((allowed_tokens, parse_exp2())))
			.map(|(l, r)| match r {
				Some((token, x)) => Expression::Op2(Box::new(l), mapping(token), Box::new(x)),
				_ => l
		})
	}
}

parser!{
	fn parse_exp3[I]()(I) -> Expression
	  where [I: Stream<Item=Token>]
	{
		let allowed_tokens = choice!(
			token(Token::TokenMult),
			token(Token::TokenDiv)
		);

		let mapping = |x| match x {
			Token::TokenMult => Op2::Multiplication,
			Token::TokenDiv => Op2::Division,
			_ => panic!("Unreachable")
		};

		(parse_exp4(), optional((allowed_tokens, parse_exp3())))
			.map(|(l, r)| match r {
				Some((token, x)) => Expression::Op2(Box::new(l), mapping(token), Box::new(x)),
				_ => l
		})
	}
}

parser!{
	fn parse_exp4[I]()(I) -> Expression
	  where [I: Stream<Item=Token>]
	{
		let allowed_tokens = choice!(
			token(Token::TokenDoubleEquals),
			token(Token::TokenLt),
			token(Token::TokenGt),
			token(Token::TokenLtEquals),
			token(Token::TokenGtEquals),
			token(Token::TokenNotEquals)
		);

		let mapping = |x| match x {
			Token::TokenDoubleEquals => Op2::Equals,
			Token::TokenLt => Op2::LessThan,
			Token::TokenGt => Op2::GreaterThan,
			Token::TokenLtEquals => Op2::LessEquals,
			Token::TokenGtEquals => Op2::GreaterEquals,
			Token::TokenNotEquals => Op2::NotEquals,
			_ => panic!("Unreachable")
		};

		(parse_exp5(), optional((allowed_tokens, parse_exp4())))
			.map(|(l, r)| match r {
				Some((token, x)) => Expression::Op2(Box::new(l), mapping(token), Box::new(x)),
				_ => l
		})
	}
}

parser!{
	fn parse_exp5[I]()(I) -> Expression
	  where [I: Stream<Item=Token>]
	{
		(parse_exp6(), optional((token(Token::TokenDoublePipe), parse_exp5())))
			.map(|(l, r)| match r {
				Some((_, x)) => Expression::Op2(Box::new(l), Op2::Or, Box::new(x)),
				_ => l
		})
	}
}

parser!{
	fn parse_exp6[I]()(I) -> Expression
	  where [I: Stream<Item=Token>]
	{
		(parse_exp7(), optional((token(Token::TokenDoubleAmp), parse_exp6())))
			.map(|(l, r)| match r {
				Some((_, x)) => Expression::Op2(Box::new(l), Op2::And, Box::new(x)),
				_ => l
		})
	}
}

parser!{
	fn parse_exp7[I]()(I) -> Expression
	  where [I: Stream<Item=Token>]
	{
		(parse_expression_no_op2(), optional((token(Token::TokenColon), parse_exp7())))
			.map(|(l, r)| match r {
				Some((_, x)) => Expression::Op2(Box::new(l), Op2::Cons, Box::new(x)),
				_ => l
		})
	}
}

parser!{
	fn parse_expression_no_op2[I]()(I) -> Expression
	  where [I: Stream<Item=Token>]
	{
		let op1_token = choice!(
			token(Token::TokenNot),
			token(Token::TokenMinus)
		);

		let op1_parse = (op1_token, parse_exp()).map(|(token, exp)| match token {
			Token::TokenNot => Expression::Op1(Op1::Not, Box::new(exp)),
			Token::TokenMinus => Expression::Op1(Op1::Negation, Box::new(exp)),
			_ => panic!("Unreachable")
		});

		let actargs_parse = (parse_exp(), many((token(Token::TokenComma), parse_exp()).map(|x| x.1)))
				.map(|(exp, mut exps): (Expression, Vec<Expression>)| { exps.push(exp); exps } );
		let funcall_parse = (parse_identifier(), token(Token::TokenParenOpen), optional(actargs_parse), token(Token::TokenParenClose)).map(|(ident, _, actargs, _)| match actargs {
			Some(x) => Expression::FunCall(ident, x),
			_ => Expression::FunCall(ident, Vec::new())
		});

		// try is needed here as we may consume part of the tokens when failing
		choice!(
			try((parse_identifier(), parse_field()).map(|(i, fs)| Expression::Ident(i, fs))),
			try(parse_literal().map(Expression::Lit)),
			try(op1_parse),
			try((token(Token::TokenParenOpen), parse_exp(), token(Token::TokenParenClose)).map(|x| x.1)),
			try(funcall_parse),
			try((token(Token::TokenParenOpen), parse_exp(), token(Token::TokenComma), parse_exp(), token(Token::TokenParenClose)).map(|(_, l, _, r, _)| Expression::Tuple(Box::new(l), Box::new(r))))
		)
	}
}

pub fn parse(tokens: &[Token]) {
	println!("{:?}", (parse_exp()).parse(tokens));
}
