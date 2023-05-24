#![allow(clippy::clone_on_copy)]

use super::span_into_str;
use super::spartan;

use ordered_float::NotNaN;
use pest_ast::FromPest;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "language/chil.pest"]
pub struct ChilParser;

fn parse_addr_first(input: &str) -> char {
    input.chars().next().unwrap()
}

fn parse_addr_second(input: &str) -> usize {
    input[1..].parse().unwrap()
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::expr))]
pub struct Expr {
    pub binds: Vec<BindClause>,
    pub value: Value,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::bind_clause))]
pub struct BindClause {
    pub var: VariableDef,
    pub value: Value,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::thunk))]
pub struct Thunk {
    pub addr: Addr,
    pub args: Vec<VariableDef>,
    pub body: Expr,
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

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Op(pub Vec<String>);

impl<'pest> from_pest::FromPest<'pest> for Op {
    type Rule = Rule;
    type FatalError = from_pest::Void;

    fn from_pest(
        pest: &mut pest::iterators::Pairs<'pest, Self::Rule>,
    ) -> Result<Self, from_pest::ConversionError<Self::FatalError>> {
        match pest.next().map(|pair| pair.as_str()) {
            Some(str) => Ok(Self(str.split('/').map(str::to_string).collect())),
            _ => Err(from_pest::ConversionError::NoMatch),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::ty))]
pub enum Type {
    Atomic(AtomicType),
    Function(FunctionType),
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::atomic_ty))]
pub struct AtomicType(#[pest_ast(outer(with(span_into_str), with(str::to_string)))] pub String);

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::function_ty))]
pub struct FunctionType {
    pub ins: Vec<Type>,
    pub out: AtomicType,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::variable))]
pub enum Variable {
    Addr(Addr),
    Identifier(Identifier, Addr),
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::variable_def))]
pub enum VariableDef {
    Inferred(Variable),
    Manifest { var: Variable, ty: Type },
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::addr))]
pub struct Addr(
    #[pest_ast(outer(with(span_into_str), with(parse_addr_first)))] pub char,
    #[pest_ast(outer(with(span_into_str), with(parse_addr_second)))] pub usize,
);

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::identifier))]
pub struct Identifier(#[pest_ast(outer(with(span_into_str), with(str::to_string)))] pub String);

// Conversion to spartan

impl From<Expr> for spartan::Expr {
    fn from(expr: Expr) -> Self {
        Self {
            binds: expr.binds.into_iter().map(|bind| bind.into()).collect(),
            value: expr.value.into(),
        }
    }
}

impl From<BindClause> for spartan::BindClause {
    fn from(bind: BindClause) -> Self {
        Self {
            var: bind.var.into(),
            value: bind.value.into(),
        }
    }
}

impl From<Thunk> for spartan::Thunk {
    fn from(thunk: Thunk) -> Self {
        Self {
            args: thunk.args.into_iter().map(Into::into).collect(),
            body: thunk.body.into(),
        }
    }
}

impl From<Value> for spartan::Value {
    fn from(value: Value) -> Self {
        match value {
            Value::Variable(var) => Self::Variable(var.into()),
            Value::Op { op, vs, ds } => Self::Op {
                op: op.into(),
                vs: vs.into_iter().map(Into::into).collect(),
                ds: ds.into_iter().map(Into::into).collect(),
            },
        }
    }
}

impl From<Op> for spartan::Op {
    fn from(op: Op) -> Self {
        let parts = op.0;

        if parts[0] == "+" || parts[0] == "throwing+" {
            return Self::Plus;
        }
        if parts[0] == "-" || parts[0] == "throwing-" {
            return Self::Minus;
        }
        if parts[0] == "*" || parts[0] == "throwing*" {
            return Self::Times;
        }
        if parts[0] == "==" || parts[0] == "throwing==" {
            return Self::Eq;
        }

        if parts[0] == "&&" {
            return Self::And;
        }
        if parts[0] == "||" {
            return Self::Or;
        }
        if parts[0] == "!" {
            return Self::Not;
        }

        if parts[0] == "apply" {
            return Self::App;
        }
        if parts[0] == "func" {
            return Self::Lambda;
        }

        if parts[0] == "bool" {
            if parts[1] == "true" {
                return Self::Bool(true);
            }
            if parts[1] == "false" {
                return Self::Bool(false);
            }
        }
        if parts[0] == "int64" || parts[0] == "float64" {
            if let Ok(x) = parts[1].parse::<f64>() {
                if let Ok(n) = NotNaN::new(x) {
                    return Self::Number(n);
                }
            }
        }
        if parts[0] == "string" && parts[1].starts_with('"') && parts[1].ends_with('"') {
            return Self::String(parts[1].trim_matches('"').to_string());
        }

        Self::Identifier(parts.join("/"))
    }
}

impl From<Variable> for spartan::Variable {
    fn from(var: Variable) -> Self {
        Self(match var {
            Variable::Addr(addr) => format!("var_{}", addr.1),
            Variable::Identifier(name, _addr) => name.0,
        })
    }
}

impl From<VariableDef> for spartan::Variable {
    fn from(var: VariableDef) -> Self {
        match var {
            VariableDef::Inferred(var) => var.into(),
            VariableDef::Manifest { var, .. } => var.into(),
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use anyhow::{Context, Result};
    use from_pest::FromPest;
    use pest::Parser;
    use rstest::{fixture, rstest};

    use super::{ChilParser, Expr, Rule};

    #[fixture]
    fn basic() -> Result<Expr> {
        let mut pairs =
            ChilParser::parse(Rule::program, include_str!("../../../examples/basic.chil"))
                .context("Could not parse program")?;
        Ok(Expr::from_pest(&mut pairs).unwrap())
    }

    #[fixture]
    fn fibonacci() -> Result<Expr> {
        let mut pairs = ChilParser::parse(
            Rule::program,
            include_str!("../../../examples/fibonacci.chil"),
        )
        .context("Could not parse program")?;
        Ok(Expr::from_pest(&mut pairs).unwrap())
    }

    #[rstest]
    #[case(basic())]
    #[case(fibonacci())]
    fn check_parse(#[case] expr: Result<Expr>) -> Result<()> {
        expr?;
        Ok(())
    }
}
