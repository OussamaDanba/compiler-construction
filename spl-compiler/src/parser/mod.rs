pub use self::ast::*;
mod ast;

use std::mem;
use scanner::Token;
use combine::{many, many1, optional, Parser, satisfy_map, sep_by, Stream, token, try};
use combine::primitives::Error;

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
		(parse_exp2(), optional((token(Token::TokenDoublePipe), parse_exp())))
			.map(|(l, r)| match r {
				Some((_, x)) => Expression::Op2(Box::new(l), Op2::Or, Box::new(x)),
				_ => l
		})
	}
}

parser!{
	fn parse_exp2[I]()(I) -> Expression
	  where [I: Stream<Item=Token>]
	{
		(parse_exp3(), optional((token(Token::TokenDoubleAmp), parse_exp2())))
			.map(|(l, r)| match r {
				Some((_, x)) => Expression::Op2(Box::new(l), Op2::And, Box::new(x)),
				_ => l
		})
	}
}

parser!{
	fn parse_exp3[I]()(I) -> Expression
	  where [I: Stream<Item=Token>]
	{
		(parse_exp4(), optional((token(Token::TokenColon), parse_exp3())))
			.map(|(l, r)| match r {
				Some((_, x)) => Expression::Op2(Box::new(l), Op2::Cons, Box::new(x)),
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
		let allowed_tokens = choice!(
			token(Token::TokenPlus),
			token(Token::TokenMinus)
		);

		let mapping = |x| match x {
			Token::TokenPlus => Op2::Addition,
			Token::TokenMinus => Op2::Subtraction,
			_ => panic!("Unreachable")
		};

		(parse_exp6(), many::<Vec<(_, Expression)>, _>((allowed_tokens, parse_exp6())))
			.map(|(l, r)| {
				if r.is_empty() {
					l
				} else {
					let x: Vec<(Op2, Expression)> = r.into_iter().map(|(token, x)| (mapping(token), x)).collect();
					x.into_iter().fold(l, |acc, (op, exp)| Expression::Op2(Box::new(acc), op, Box::new(exp)))
				}
			})
	}
}

parser!{
	fn parse_exp6[I]()(I) -> Expression
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

		(parse_exp7(), many::<Vec<(_, Expression)>, _>((allowed_tokens, parse_exp7())))
			.map(|(l, r)| {
				if r.is_empty() {
					l
				} else {
					let x: Vec<(Op2, Expression)> = r.into_iter().map(|(token, x)| (mapping(token), x)).collect();
					x.into_iter().fold(l, |acc, (op, exp)| Expression::Op2(Box::new(acc), op, Box::new(exp)))
				}
			})
	}
}

parser!{
	fn parse_exp7[I]()(I) -> Expression
	  where [I: Stream<Item=Token>]
	{
		(parse_expression_no_op2(), many::<Vec<(_, Expression)>, _>((token(Token::TokenMod), parse_expression_no_op2())))
			.map(|(l, r)| {
				if r.is_empty() {
					l
				} else {
					let x: Vec<(Op2, Expression)> = r.into_iter().map(|(_, x)| (Op2::Modulo, x)).collect();
					x.into_iter().fold(l, |acc, (op, exp)| Expression::Op2(Box::new(acc), op, Box::new(exp)))
				}
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

		let op1_parse = (op1_token, parse_expression_no_op2()).map(|(token, exp)| match token {
			Token::TokenNot => Expression::Op1(Op1::Not, Box::new(exp)),
			Token::TokenMinus => Expression::Op1(Op1::Negation, Box::new(exp)),
			_ => panic!("Unreachable")
		});

		choice!(
			try(parse_literal().map(Expression::Lit)),
			try(op1_parse),
			try((token(Token::TokenParenOpen), parse_exp(), token(Token::TokenParenClose)).map(|x| x.1)),
			try(parse_funcall()),
			try((token(Token::TokenParenOpen), parse_exp(), token(Token::TokenComma), parse_exp(), token(Token::TokenParenClose)).map(|(_, l, _, r, _)| Expression::Tuple(Box::new(l), Box::new(r)))),
			try((parse_identifier(), parse_field()).map(|(i, fs)| Expression::Ident(i, fs)))
		)
	}
}

parser!{
	fn parse_funcall[I]()(I) -> Expression
	  where [I: Stream<Item=Token>]
	{
		let actargs_parse = (parse_exp(), many((token(Token::TokenComma), parse_exp()).map(|x| x.1)))
			.map(|(exp, mut exps): (Expression, Vec<Expression>)| { exps.insert(0, exp); exps } );

		(parse_identifier(), token(Token::TokenParenOpen), optional(actargs_parse), token(Token::TokenParenClose)).map(|(ident, _, actargs, _)| match actargs {
			Some(x) => Expression::FunCall(ident, x),
			_ => Expression::FunCall(ident, Vec::new())
		})
	}
}

parser!{
	fn parse_statement[I]()(I) -> Statement
	  where [I: Stream<Item=Token>]
	{
		let if_parser = (
			token(Token::TokenIf),
			token(Token::TokenParenOpen),
			parse_exp(),
			token(Token::TokenParenClose),
			token(Token::TokenBraceOpen),
			many(parse_statement()),
			token(Token::TokenBraceClose),
			optional((
				token(Token::TokenElse),
				token(Token::TokenBraceOpen),
				many(parse_statement()),
				token(Token::TokenBraceClose)
			))
		).map(|(_, _, exp, _, _, stmts, _, option_stmts)| match option_stmts {
			Some(x) => Statement::If(exp, stmts, x.2),
			None => Statement::If(exp, stmts, Vec::new())
		});

		let while_parse = (
			token(Token::TokenWhile),
			token(Token::TokenParenOpen),
			parse_exp(),
			token(Token::TokenParenClose),
			token(Token::TokenBraceOpen),
			many(parse_statement()),
			token(Token::TokenBraceClose)
		).map(|(_, _, exp, _, _, stmts, _)| Statement::While(exp, stmts));

		let assignment_parse = (parse_identifier(), parse_field(), token(Token::TokenEquals), parse_exp(), token(Token::TokenSemiColon))
			.map(|(ident, fields, _, exp, _)| Statement::Assignment(ident, fields, exp));

		let return_parse = (token(Token::TokenReturn), optional(parse_exp()), token(Token::TokenSemiColon))
			.map(|(_, exp, _)| Statement::Return(exp));

		choice!(
			try(if_parser),
			try(while_parse),
			try(assignment_parse),
			try((parse_funcall(), token(Token::TokenSemiColon)).map(|(func, _)| Statement::FunCall(func))),
			try(return_parse)
		)
	}
}

parser!{
	fn parse_funtype[I]()(I) -> Type
	  where [I: Stream<Item=Token>]
	{
		let parse_rettype = choice!(
			token(Token::TokenVoid).map(|_| Type::TVoid),
			parse_type()
		);

		(
			many(parse_type()),
			token(Token::TokenArrow),
			parse_rettype
		).map(|(ftypes, _, rettype)| Type::TArrow(ftypes, Box::new(rettype)))
	}
}

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
			try(token(Token::TokenIntKeyword).map(|_| Type::TInt)),
			try(token(Token::TokenBoolKeyword).map(|_| Type::TBool)),
			try(token(Token::TokenCharKeyword).map(|_| Type::TChar)),
			try(tuple_parser),
			try(list_parser)
		)
	}
}

parser!{
	fn parse_vardecl[I]()(I) -> Variable
	  where [I: Stream<Item=Token>]
	{
		try((
			parse_type(),
			parse_identifier(),
			token(Token::TokenEquals),
			parse_exp(),
			token(Token::TokenSemiColon)
		)).map(|(vtype, ident, _, exp, _)| Variable {
			name: ident,
			vtype,
			value: exp
		})
	}
}

parser!{
	fn parse_fundecl[I]()(I) -> Function
	  where [I: Stream<Item=Token>]
	{
		(
			parse_identifier(),
			token(Token::TokenParenOpen),
			sep_by(parse_identifier(), token(Token::TokenComma)),
			token(Token::TokenParenClose),
			token(Token::TokenDoubleColon),
			parse_funtype(),
			token(Token::TokenBraceOpen),
			many(parse_vardecl()),
			many1(parse_statement()),
			token(Token::TokenBraceClose)
		).map(|(ident, _, fargs, _, _, funtype, _, vardecls, stmts, _)| Function {
			name: ident,
			args: fargs,
			ftype: funtype,
			vars: vardecls,
			stmts
		})
	}
}

parser!{
	fn parse_spl[I]()(I) -> SPL
	  where [I: Stream<Item=Token>]
	{
		// This is essentially an Either which Rust does not have by default
		enum Decl {
			Var(Variable),
			Fun(Function)
		};

		let parse_decl = choice!(
			try(parse_vardecl().map(Decl::Var)),
			try(parse_fundecl().map(Decl::Fun))
		);

		many1(parse_decl).map(|decls: Vec<Decl>| {
			let mut vars = Vec::new();
			let mut funs = Vec::new();
			for decl in decls {
				match decl {
					Decl::Var(var) => vars.push(var),
					Decl::Fun(fun) => funs.push(fun)
				}
			}
			SPL {
				vars,
				funs
			}
		})
	}
}

// Either returns a correctly parsed program or the unexpected token it got and its corresponding index position
pub fn parse(tokens: &[Token]) -> Result<SPL, (Token, usize)> {
	match parse_spl().parse(tokens) {
		Ok(x) => Ok(x.0),
		Err(e) => Err((
			e.errors.iter().filter_map(|err| match *err {
				Error::Unexpected(ref x) => match *x {
					super::combine::primitives::Info::Token(ref t) => Some(t),
					_ => None
				},
				_ => None
			}).collect::<Vec<&Token>>()[0].clone(),
			e.translate_position(tokens).position / mem::size_of::<Token>()
		))
	}
}
