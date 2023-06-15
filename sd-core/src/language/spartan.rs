#![allow(clippy::clone_on_copy)]

use std::fmt::{Display, Write};

use from_pest::{ConversionError, FromPest, Void};
use pest::iterators::Pairs;
use pest_ast::FromPest;
use pest_derive::Parser;
#[cfg(test)]
use serde::Serialize;

use super::span_into_str;

pub struct Spartan;

impl super::Language for Spartan {
    type Op = Op;
    type Var = Variable;
    type Addr = Addr;
    type VarDef = Variable;

    type Rule = Rule;

    fn expr_rule() -> Self::Rule {
        Rule::expr
    }
    fn bind_rule() -> Self::Rule {
        Rule::bind
    }
    fn value_rule() -> Self::Rule {
        Rule::value
    }
    fn thunk_rule() -> Self::Rule {
        Rule::thunk
    }
}

pub type Expr = super::Expr<Spartan>;
pub type Bind = super::Bind<Spartan>;
pub type Value = super::Value<Spartan>;
pub type Thunk = super::Thunk<Spartan>;

#[derive(Parser)]
#[grammar = "language/spartan.pest"]
pub struct SpartanParser;

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::variable))]
pub struct Variable(#[pest_ast(outer(with(span_into_str), with(str::to_string)))] pub String);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub enum Op {
    Plus,
    Minus,
    Times,
    Div,
    Rem,
    And,
    Or,
    Not,
    If,
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
    App,
    Lambda,
    Atom,
    Deref,
    Assign,
    Bool(bool),
    Number(usize),
}

impl From<&str> for Op {
    fn from(str: &str) -> Self {
        match str {
            "plus" => Self::Plus,
            "minus" => Self::Minus,
            "times" => Self::Times,
            "div" => Self::Div,
            "rem" => Self::Rem,
            "and" => Self::And,
            "or" => Self::Or,
            "not" => Self::Not,
            "if" => Self::If,
            "eq" => Self::Eq,
            "neq" => Self::Neq,
            "lt" => Self::Lt,
            "leq" => Self::Leq,
            "gt" => Self::Gt,
            "geq" => Self::Geq,
            "app" => Self::App,
            "lambda" => Self::Lambda,
            "atom" => Self::Atom,
            "deref" => Self::Deref,
            "assign" => Self::Assign,
            "true" => Self::Bool(true),
            "false" => Self::Bool(false),
            _ => Self::Number(str.parse().unwrap_or_default()),
        }
    }
}

impl<'pest> FromPest<'pest> for Op {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(
        pest: &mut Pairs<'pest, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        pest.next()
            .map(|pair| pair.as_str().into())
            .ok_or(ConversionError::NoMatch)
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus => f.write_char('+'),
            Self::Minus => f.write_char('-'),
            Self::Times => f.write_char('×'),
            Self::Div => f.write_char('/'),
            Self::Rem => f.write_char('%'),
            Self::And => f.write_char('∧'),
            Self::Or => f.write_char('∨'),
            Self::Not => f.write_char('¬'),
            Self::If => f.write_str("if"),
            Self::Eq => f.write_char('='),
            Self::Neq => f.write_char('≠'),
            Self::Lt => f.write_char('<'),
            Self::Leq => f.write_char('≤'),
            Self::Gt => f.write_char('>'),
            Self::Geq => f.write_char('≥'),
            Self::App => f.write_char('@'),
            Self::Lambda => f.write_char('λ'),
            Self::Atom => f.write_char('&'),
            Self::Deref => f.write_char('!'),
            Self::Assign => f.write_str(":="),
            Self::Bool(b) => f.write_str(&b.to_string()),
            Self::Number(n) => f.write_str(&n.to_string()),
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
        Variable(value.to_owned())
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Addr;

impl Display for Addr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("")
    }
}

impl<'pest> FromPest<'pest> for Addr {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(_: &mut Pairs<'pest, Rule>) -> Result<Self, ConversionError<Void>> {
        Ok(Addr)
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use std::path::Path;

    use dir_test::{dir_test, Fixture};
    use from_pest::FromPest;
    use pest::Parser;

    use super::{Expr, Rule, SpartanParser};

    pub fn parse_sd(raw_path: &str) -> (&str, Expr) {
        let path = Path::new(raw_path);
        let program = std::fs::read_to_string(path).unwrap();
        let mut pairs = SpartanParser::parse(Rule::program, &program).unwrap_or_else(|err| {
            panic!(
                "could not parse program {:?}\n{err:?}",
                path.file_stem().unwrap()
            )
        });
        let name = path.file_stem().unwrap().to_str().unwrap();
        let expr = Expr::from_pest(&mut pairs).unwrap();
        (name, expr)
    }

    #[allow(clippy::needless_pass_by_value)]
    #[dir_test(dir: "$CARGO_MANIFEST_DIR/../examples", glob: "**/*.sd", loader: crate::language::spartan::tests::parse_sd, postfix: "check_parse")]
    fn check_parse(fixture: Fixture<(&str, Expr)>) {
        let (_name, _expr) = fixture.content();
    }
}
