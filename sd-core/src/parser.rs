use nom::{
    branch::alt,
    combinator::map,
    multi::{many0, separated_list0, separated_list1},
    sequence::{separated_pair, tuple},
    IResult, InputLength,
};

use crate::term::{Expr, Term, Thunk, Variable};
use crate::{
    lexer::{Token, Tokens},
    term::Operation,
};

impl<'source> InputLength for Tokens<'source> {
    fn input_len(&self) -> usize {
        self.0.source().len() - self.0.span().end
    }
}

fn token(token: Token) -> impl FnMut(Tokens<'_>) -> IResult<Tokens<'_>, Token> {
    move |mut input: Tokens<'_>| match input.0.next() {
        Some(first) if first == token => IResult::Ok((input, first)),
        _ => IResult::Err(nom::Err::Failure(nom::error::Error::new(
            input,
            nom::error::ErrorKind::IsA,
        ))),
    }
}

fn identifier(mut input: Tokens<'_>) -> IResult<Tokens<'_>, String> {
    match input.0.next() {
        Some(Token::Identifier(id)) => IResult::Ok((input, id)),
        _ => IResult::Err(nom::Err::Failure(nom::error::Error::new(
            input,
            nom::error::ErrorKind::IsA,
        ))),
    }
}

fn parse_expr(input: Tokens<'_>) -> IResult<Tokens<'_>, Expr> {
    alt((map(parse_term, Expr::Term), parse_bind))(input)
}

fn parse_bind(input: Tokens<'_>) -> IResult<Tokens<'_>, Expr> {
    map(
        tuple((
            token(Token::Bind),
            separated_list1(
                token(Token::Comma),
                separated_pair(parse_variable, token(Token::Equals), parse_term),
            ),
            token(Token::In),
            parse_expr,
        )),
        |(_, pairs, _, expr)| Expr::Bind(pairs, Box::new(expr)),
    )(input)
}

fn parse_term(input: Tokens<'_>) -> IResult<Tokens<'_>, Term> {
    alt((map(parse_variable, Term::Var), parse_app))(input)
}

fn parse_app(input: Tokens<'_>) -> IResult<Tokens<'_>, Term> {
    map(
        tuple((
            parse_operation,
            token(Token::LeftParan),
            separated_list0(token(Token::Comma), parse_variable),
            token(Token::Semicolon),
            separated_list0(token(Token::Comma), parse_thunk),
            token(Token::RightParan),
        )),
        |(op, _, vars, _, thunks, _)| Term::App(op, vars, thunks),
    )(input)
}

fn parse_thunk(input: Tokens<'_>) -> IResult<Tokens<'_>, Thunk> {
    map(
        tuple((many0(parse_variable), token(Token::Period), parse_expr)),
        |(vars, _, expr)| Thunk(vars, expr),
    )(input)
}

fn parse_variable(input: Tokens<'_>) -> IResult<Tokens<'_>, Variable> {
    map(identifier, |v| Variable(v))(input)
}

fn parse_operation(input: Tokens<'_>) -> IResult<Tokens<'_>, Operation> {
    map(identifier, |op| Operation(op))(input)
}
