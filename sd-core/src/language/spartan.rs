#![allow(clippy::clone_on_copy)]

use std::fmt::{Display, Write};

use from_pest::Void;
use ordered_float::NotNaN;
use pest_ast::FromPest;
use pest_derive::Parser;
#[cfg(test)]
use serde::Serialize;

use super::span_into_str;

#[derive(Parser)]
#[grammar = "language/spartan.pest"]
pub struct SpartanParser;

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::expr))]
pub struct Expr {
    pub binds: Vec<BindClause>,
    pub value: Value,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::bind_clause))]
pub struct BindClause {
    pub var: Variable,
    pub value: Value,
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::variable))]
pub struct Variable(#[pest_ast(outer(with(span_into_str), with(str::to_string)))] pub String);

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub enum Op {
    Plus,
    Minus,
    Times,
    Eq,
    And,
    Or,
    Not,
    If,
    App,
    Lambda,
    Bool(bool),
    Number(NotNaN<f64>),
    String(String),     // string literal
    Identifier(String), // any other identifier
}

impl<'pest> from_pest::FromPest<'pest> for Op {
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
            Some("lambda") => Ok(Self::Lambda),
            Some("true") => Ok(Self::Bool(true)),
            Some("false") => Ok(Self::Bool(false)),
            Some(str) => {
                if let Ok(n) = str::parse::<f64>(str) {
                    return Ok(Self::Number(NotNaN::new(n).unwrap()));
                }
                if str.starts_with('"') && str.ends_with('"') {
                    return Ok(Self::String(str[1..str.len() - 1].to_string()));
                }
                // Technically this should be unreachable
                Ok(Self::Identifier(str.to_owned()))
            }
            _ => Err(from_pest::ConversionError::NoMatch),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::value))]
pub enum Value {
    Variable(Variable),
    Op {
        op: Op,
        vs: Vec<Value>,
        ds: Vec<Thunk>,
    },
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::thunk))]
pub struct Thunk {
    pub args: Vec<Variable>,
    pub body: Expr,
}

impl Display for Op {
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
            Self::Lambda => f.write_char('λ'),
            Self::Bool(b) => f.write_str(&b.to_string()),
            Self::Number(n) => f.write_str(&n.to_string()),
            Self::String(_) => f.write_str("str"),
            Self::Identifier(op) => f.write_str(op),
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
