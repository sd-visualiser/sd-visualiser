use nom::{
    branch::alt,
    combinator::map,
    multi::{many0, separated_list0},
    sequence::tuple,
    IResult, InputLength,
};

use crate::expr::{ActiveOp, Expr, PassiveOp, Term, Thunk, Value, Variable};
use crate::lexer::{Token, Tokens};

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

fn int(mut input: Tokens<'_>) -> IResult<Tokens<'_>, usize> {
    match input.0.next() {
        Some(Token::Integer(n)) => IResult::Ok((input, n)),
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
    alt((
        map(parse_value, Expr::Val),
        map(
            tuple((
                token(Token::Bind),
                parse_variable,
                token(Token::Equals),
                parse_term,
                token(Token::In),
                parse_expr,
            )),
            |(_, var, _, term, _, expr)| Expr::Bind(var, term, Box::new(expr)),
        ),
    ))(input)
}

fn parse_term(input: Tokens<'_>) -> IResult<Tokens<'_>, Term> {
    alt((
        map(parse_value, Term::Val),
        map(
            tuple((
                parse_active_op,
                token(Token::LeftParan),
                separated_list0(token(Token::Comma), parse_variable),
                token(Token::Semicolon),
                separated_list0(token(Token::Comma), parse_thunk),
                token(Token::RightParan),
            )),
            |(op, _, vars, _, thunks, _)| Term::ActiveOp(op, vars, thunks),
        ),
    ))(input)
}

fn parse_value(input: Tokens<'_>) -> IResult<Tokens<'_>, Value> {
    alt((
        map(parse_variable, Value::Var),
        map(
            tuple((
                parse_passive_op,
                token(Token::LeftParan),
                separated_list0(token(Token::Comma), parse_variable),
                token(Token::Semicolon),
                separated_list0(token(Token::Comma), parse_thunk),
                token(Token::RightParan),
            )),
            |(op, _, vars, _, thunks, _)| Value::PassiveOp(op, vars, thunks),
        ),
    ))(input)
}

fn parse_thunk(input: Tokens<'_>) -> IResult<Tokens<'_>, Thunk> {
    map(
        tuple((many0(parse_variable), token(Token::Period), parse_expr)),
        |(vars, _, expr)| Thunk(vars, expr),
    )(input)
}

fn parse_variable(input: Tokens<'_>) -> IResult<Tokens<'_>, Variable> {
    map(identifier, |x| Variable(x))(input)
}

fn parse_active_op(input: Tokens<'_>) -> IResult<Tokens<'_>, ActiveOp> {
    alt((
        map(token(Token::Plus), |_| ActiveOp::Plus),
        map(token(Token::Times), |_| ActiveOp::Times),
    ))(input)
}

fn parse_passive_op(input: Tokens<'_>) -> IResult<Tokens<'_>, PassiveOp> {
    map(int, PassiveOp::Int)(input)
}
