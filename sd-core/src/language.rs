#![allow(clippy::clone_on_copy)]

use std::fmt::{Display, Write};

use pest::Span;
use pest_ast::FromPest;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "language.pest"]
pub struct SDParser;

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
    Thunk(Thunk),
    ActiveOp(ActiveOp, Vec<Value>),
    Value(Value),
}

#[derive(Clone, Copy, Debug, FromPest, PartialEq, Eq, Hash)]
#[pest_ast(rule(Rule::active_op))]
pub enum ActiveOp {
    Plus,
}

#[derive(Clone, Copy, Debug, FromPest, PartialEq, Eq, Hash)]
#[pest_ast(rule(Rule::passive_op))]
pub enum PassiveOp {
    Int(#[pest_ast(outer(with(span_into_str), with(str::parse), with(Result::unwrap)))] usize),
}

#[derive(Clone, Debug, FromPest, PartialEq, Eq)]
#[pest_ast(rule(Rule::value))]
pub enum Value {
    PassiveOp(PassiveOp, Vec<Value>),
    Var(Variable),
}

#[derive(Clone, Debug, FromPest, PartialEq, Eq)]
#[pest_ast(rule(Rule::thunk))]
pub struct Thunk {
    pub args: Vec<Variable>,
    pub body: Expr,
}

impl Display for ActiveOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ActiveOp::Plus => f.write_char('+'),
        }
    }
}

impl Display for PassiveOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PassiveOp::Int(d) => f.write_str(&d.to_string()),
        }
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}
