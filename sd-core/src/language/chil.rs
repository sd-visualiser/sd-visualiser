#![allow(clippy::clone_on_copy)]

use from_pest::{ConversionError, FromPest, Void};
use ordered_float::NotNaN;
use pest::iterators::Pairs;
use pest_ast::FromPest;
use pest_derive::Parser;

use super::{span_into_str, spartan};

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct ChilSpec;

impl super::Language for ChilSpec {
    type Op = Op;
    type Var = Variable;
    type Ty = Option<Type>;
    type Addr = Addr;

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

pub type Expr = super::Expr<ChilSpec>;
pub type Bind = super::Bind<ChilSpec>;
pub type Value = super::Value<ChilSpec>;
pub type Thunk = super::Thunk<ChilSpec>;

#[derive(Parser)]
#[grammar = "language/chil.pest"]
pub struct ChilParser;

fn parse_addr_first(input: &str) -> char {
    input.chars().next().unwrap()
}

fn parse_addr_second(input: &str) -> usize {
    input[1..].parse().unwrap()
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Op(pub String);

impl<'pest> FromPest<'pest> for Op {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(
        pest: &mut Pairs<'pest, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        match pest.next().map(|pair| pair.as_str()) {
            Some(str) => Ok(Self(str.to_owned())),
            _ => Err(ConversionError::NoMatch),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::ty))]
pub enum Type {
    Base(BaseType),
    Generic(GenericType),
    Tuple(TupleType),
    Function(FunctionType),
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::base_ty))]
pub struct BaseType(#[pest_ast(outer(with(span_into_str), with(str::to_string)))] pub String);

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::generic_ty))]
pub struct GenericType {
    pub base: BaseType,
    pub params: Vec<Type>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::tuple_ty))]
pub struct TupleType {
    pub types: Vec<Type>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::function_ty))]
pub struct FunctionType {
    pub domain: TupleType,
    pub codomain: Box<Type>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::variable))]
pub enum Variable {
    Addr(Addr),
    Identifier(Identifier, Addr),
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::addr))]
pub struct Addr(
    #[pest_ast(outer(with(span_into_str), with(parse_addr_first)))] pub char,
    #[pest_ast(outer(with(span_into_str), with(parse_addr_second)))] pub usize,
);

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Identifier(pub String);

impl<'pest> FromPest<'pest> for Identifier {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(
        pest: &mut Pairs<'pest, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        match pest.next().map(|pair| pair.as_str()) {
            Some(str) => Ok(Identifier(str.to_owned())),
            _ => Err(ConversionError::NoMatch),
        }
    }
}

// Conversion to spartan

impl From<Expr> for spartan::Expr {
    fn from(expr: Expr) -> Self {
        Self {
            binds: expr.binds.into_iter().map(Into::into).collect(),
            value: expr.value.into(),
        }
    }
}

impl From<Bind> for spartan::Bind {
    fn from(bind: Bind) -> Self {
        Self {
            var: bind.var.into(),
            ty: spartan::Type,
            value: bind.value.into(),
        }
    }
}

impl From<Thunk> for spartan::Thunk {
    fn from(thunk: Thunk) -> Self {
        Self {
            addr: spartan::Addr,
            args: thunk
                .args
                .into_iter()
                .map(|(var, _)| (var.into(), spartan::Type))
                .collect(),
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
        let str = op.0;

        match str.as_str() {
            "+" | "throwing+" => return Self::Plus,
            "-" | "throwing-" => return Self::Minus,
            "*" | "throwing*" => return Self::Times,
            "/" | "throwing/" => return Self::Div,
            "%" | "throwing%" => return Self::Rem,
            "&&" | "throwing&&" => return Self::And,
            "||" | "throwing||" => return Self::Or,
            "!" | "throwing!" => return Self::Not,
            "==" | "throwing==" => return Self::Eq,
            "!=" | "throwing!=" => return Self::Neq,
            "<" | "throwing<" => return Self::Lt,
            "<=" | "throwing<=" => return Self::Leq,
            ">" | "throwing>" => return Self::Gt,
            ">=" | "throwing>=" => return Self::Geq,
            "func" => return Self::Lambda,
            "unit" => return Self::Unit,
            "seq" => return Self::Seq,
            "atom" => return Self::Atom,
            "deref" => return Self::Deref,
            "asg" => return Self::Assign,
            "bool/true" => return Self::Bool(true),
            "bool/false" => return Self::Bool(false),
            _ => (),
        }

        if str.starts_with("apply/") {
            return Self::App;
        }
        if let Some(rest) = str.strip_prefix("int64/").or(str.strip_prefix("float64/")) {
            if let Ok(n) = rest.parse::<f64>() {
                return Self::Number(NotNaN::new(n).unwrap());
            }
        }
        if let Some(rest) = str.strip_prefix("string/") {
            if rest.starts_with('"') && rest.ends_with('"') {
                return Self::String(rest.trim_matches('"').to_owned());
            }
        }

        Self::Identifier(str)
    }
}

impl From<Variable> for spartan::Variable {
    fn from(var: Variable) -> Self {
        Self(match var {
            Variable::Addr(addr) => format!("var_{}", addr.1),
            Variable::Identifier(name, addr) => format!("{}_{}", name.0, addr.1),
        })
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use std::path::Path;

    use dir_test::{dir_test, Fixture};
    use from_pest::FromPest;
    use pest::Parser;

    use super::{ChilParser, Expr, Rule};

    pub fn parse_chil(raw_path: &str) -> (&str, Expr) {
        let path = Path::new(raw_path);
        let program = std::fs::read_to_string(path).unwrap();
        let mut pairs = ChilParser::parse(Rule::program, &program).unwrap_or_else(|err| {
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
    #[dir_test(dir: "$CARGO_MANIFEST_DIR/../examples", glob: "**/*.chil", loader: crate::language::chil::tests::parse_chil, postfix: "check_parse")]
    fn check_parse(fixture: Fixture<(&str, Expr)>) {
        let (_name, _expr) = fixture.content();
    }
}
