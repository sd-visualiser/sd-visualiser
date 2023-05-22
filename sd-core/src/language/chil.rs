#![allow(clippy::clone_on_copy)]

use pest::Span;
use pest_ast::FromPest;
use pest_derive::Parser;

use super::spartan;

#[derive(Parser)]
#[grammar = "language/chil.pest"]
pub struct ChilParser;

fn span_into_str(span: Span) -> &str {
    span.as_str()
}

fn remove_first(value: &str) -> &str {
    let mut chars = value.chars();
    chars.next();
    chars.as_str()
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
    pub id: Identifier,
    pub args: Vec<VariableDef>,
    pub body: Expr,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::value))]
pub enum Value {
    Var(Variable),
    Op(Op, Vec<Arg>),
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::arg))]
pub enum Arg {
    Value(Value),
    Thunk(Thunk),
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::op))]
pub struct Op(#[pest_ast(outer(with(span_into_str), with(str::to_string)))] pub String);

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
    Id(Identifier),
    Name(Name, Identifier),
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::variable_def))]
pub enum VariableDef {
    Inferred(Variable),
    Manifest { var: Variable, ty: Type },
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::name))]
pub struct Name(#[pest_ast(outer(with(span_into_str), with(str::to_string)))] pub String);

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::identifier))]
pub struct Identifier(
    #[pest_ast(outer(
        with(span_into_str),
        with(remove_first),
        with(str::parse),
        with(Result::unwrap)
    ))]
    pub usize,
);

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
            args: thunk
                .args
                .into_iter()
                .map(|var_def| var_def.into())
                .collect(),
            body: thunk.body.into(),
        }
    }
}

impl From<Value> for spartan::Value {
    fn from(value: Value) -> Self {
        match value {
            Value::Var(var) => Self::Var(var.into()),
            Value::Op(op, args) => {
                Self::Op(op.into(), args.into_iter().map(|arg| arg.into()).collect())
            }
        }
    }
}

impl From<Op> for spartan::Op {
    fn from(_op: Op) -> Self {
        Self::Plus
    }
}

impl From<Variable> for spartan::Variable {
    fn from(var: Variable) -> Self {
        Self(match var {
            Variable::Id(id) => format!("var_{}", id.0),
            Variable::Name(name, _id) => name.0,
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

impl From<Arg> for spartan::Arg {
    fn from(arg: Arg) -> Self {
        match arg {
            Arg::Value(value) => Self::Value(value.into()),
            Arg::Thunk(thunk) => Self::Thunk(thunk.into()),
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

    #[rstest]
    #[case(basic())]
    fn check_parse(#[case] expr: Result<Expr>) -> Result<()> {
        expr?;
        Ok(())
    }
}
