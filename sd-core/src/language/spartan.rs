#![allow(clippy::clone_on_copy)]

use std::fmt::{Display, Write};

use from_pest::Void;
use pest::Span;
use pest_ast::FromPest;
use pest_derive::Parser;

#[cfg(test)]
use serde::Serialize;

#[derive(Parser)]
#[grammar = "language/spartan.pest"]
pub struct SpartanParser;

fn span_into_str(span: Span) -> &str {
    span.as_str()
}

#[derive(Clone, Debug, FromPest, PartialEq, Eq)]
#[pest_ast(rule(Rule::expr))]
pub struct Expr {
    pub binds: Vec<BindClause>,
    pub value: Value,
}

#[derive(Clone, Debug, FromPest, PartialEq, Eq)]
#[pest_ast(rule(Rule::bind_clause))]
pub struct BindClause {
    pub var: Variable,
    pub term: Term,
}

#[derive(Clone, Debug, FromPest, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[pest_ast(rule(Rule::variable))]
pub struct Variable(#[pest_ast(outer(with(span_into_str), with(str::to_string)))] pub String);

#[derive(Clone, Debug, FromPest, PartialEq, Eq)]
#[pest_ast(rule(Rule::term))]
pub enum Term {
    Value(Value),
    ActiveOp(ActiveOp, Vec<Arg>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(Serialize))]
pub enum ActiveOp {
    Plus,
    Minus,
    Times,
    Eq,
    And,
    Or,
    Not,
    If,
    App,
    Rec,
}

impl<'pest> from_pest::FromPest<'pest> for ActiveOp {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(
        pest: &mut pest::iterators::Pairs<'pest, Self::Rule>,
    ) -> Result<Self, from_pest::ConversionError<Self::FatalError>> {
        match pest.next().map(|pair| pair.as_str()) {
            Some("plus") => Ok(Self::Plus),
            Some("minus") => Ok(Self::Minus),
            Some("times") => Ok(Self::Times),
            Some("eq") => Ok(Self::Eq),
            Some("and") => Ok(Self::And),
            Some("or") => Ok(Self::Or),
            Some("not") => Ok(Self::Not),
            Some("if") => Ok(Self::If),
            Some("app") => Ok(Self::App),
            Some("rec") => Ok(Self::Rec),
            _ => Err(from_pest::ConversionError::NoMatch),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(Serialize))]
pub enum PassiveOp {
    Int(usize),
    Bool(bool),
    Lambda,
}

impl<'pest> from_pest::FromPest<'pest> for PassiveOp {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(
        pest: &mut pest::iterators::Pairs<'pest, Self::Rule>,
    ) -> Result<Self, from_pest::ConversionError<Self::FatalError>> {
        match pest.next().map(|pair| pair.as_str()) {
            Some("true") => Ok(Self::Bool(true)),
            Some("false") => Ok(Self::Bool(false)),
            Some("lambda") => Ok(Self::Lambda),
            Some(str) => str::parse(str)
                .map(Self::Int)
                .map_err(|_err| from_pest::ConversionError::NoMatch),
            _ => Err(from_pest::ConversionError::NoMatch),
        }
    }
}

#[derive(Clone, Debug, FromPest, PartialEq, Eq)]
#[pest_ast(rule(Rule::value))]
pub enum Value {
    Var(Variable),
    PassiveOp(PassiveOp, Vec<Arg>),
}

#[derive(Clone, Debug, FromPest, PartialEq, Eq)]
#[pest_ast(rule(Rule::thunk))]
pub struct Thunk {
    pub args: Vec<Variable>,
    pub body: Expr,
}

#[derive(Clone, Debug, FromPest, PartialEq, Eq)]
#[pest_ast(rule(Rule::arg))]
pub enum Arg {
    Value(Value),
    Thunk(Thunk),
}

impl Display for ActiveOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus => f.write_char('+'),
            Self::Minus => f.write_char('-'),
            Self::Times => f.write_char('×'),
            Self::Eq => f.write_char('='),
            Self::And => f.write_str("and"),
            Self::Or => f.write_str("or"),
            Self::Not => f.write_str("not"),
            Self::If => f.write_str("if"),
            Self::App => f.write_char('@'),
            Self::Rec => f.write_char('μ'),
        }
    }
}

impl Display for PassiveOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(d) => f.write_str(&d.to_string()),
            Self::Bool(b) => f.write_str(&b.to_string()),
            Self::Lambda => f.write_char('λ'),
        }
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl From<&str> for Variable {
    fn from(value: &str) -> Self {
        Variable(value.to_string())
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use anyhow::{Context, Result};
    use from_pest::FromPest;
    use pest::Parser;
    use rstest::{fixture, rstest};

    use super::{Expr, Rule, SpartanParser};

    #[fixture]
    pub(crate) fn basic_program() -> Result<Expr> {
        let mut pairs = SpartanParser::parse(Rule::program, "bind x = 1 in x")
            .context("Could not parse basic program")?;
        Ok(Expr::from_pest(&mut pairs).unwrap())
    }

    #[fixture]
    pub(crate) fn free_vars() -> Result<Expr> {
        let mut pairs = SpartanParser::parse(Rule::program, "bind x = y in z")
            .context("Could not parse free variable program")?;
        Ok(Expr::from_pest(&mut pairs).unwrap())
    }

    // Make this something meaningful
    #[fixture]
    pub(crate) fn thunks() -> Result<Expr> {
        let mut pairs =
            SpartanParser::parse(Rule::program, include_str!("../../../examples/thunks.sd"))
                .context("Could not parse fact program")?;
        Ok(Expr::from_pest(&mut pairs).unwrap())
    }

    #[fixture]
    pub(crate) fn fact() -> Result<Expr> {
        let mut pairs =
            SpartanParser::parse(Rule::program, include_str!("../../../examples/fact.sd"))
                .context("Could not parse fact program")?;
        Ok(Expr::from_pest(&mut pairs).unwrap())
    }

    #[rstest]
    #[case(basic_program())]
    #[case(free_vars())]
    #[case(thunks())]
    #[case(fact())]
    fn check_parse(#[case] expr: Result<Expr>) -> Result<()> {
        expr?;
        Ok(())
    }
}
