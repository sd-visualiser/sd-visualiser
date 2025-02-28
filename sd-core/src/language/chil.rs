#![allow(clippy::clone_on_copy)]

use std::{
    fmt::{Display, Write},
    hash::{Hash, Hasher},
};

use from_pest::{ConversionError, FromPest, Void};
use pest::iterators::Pairs;
use pest_ast::FromPest;
use pest_derive::Parser;
#[cfg(test)]
use serde::Serialize;

use super::{Fresh, GetVar, OpInfo, span_into_str};
use crate::{
    common::{Empty, Matchable},
    hypergraph::traits::{WireType, WithType},
};

pub struct Chil;

impl super::Language for Chil {
    type Op = Op;
    type Var = Variable;
    type Addr = Addr;
    type VarDef = VariableDef;
    type BlockAddr = Empty;
    type Symbol = Empty;
}

impl TryFrom<<Chil as super::Language>::Addr> for <Chil as super::Language>::Symbol {
    type Error = &'static str;

    fn try_from(_: <Chil as super::Language>::Addr) -> Result<Self, Self::Error> {
        Err("no symbols in chil")
    }
}

pub type Expr = super::Expr<Chil>;
pub type Bind = super::Bind<Chil>;
pub type Value = super::Value<Chil>;
pub type Thunk = super::Thunk<Chil>;

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
#[cfg_attr(test, derive(Serialize))]
pub struct Op(pub String);

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.as_str() {
            "+" | "throwing+" => f.write_char('+'),
            "-" | "throwing-" => f.write_char('-'),
            "*" | "throwing*" => f.write_char('*'),
            "/" | "throwing/" => f.write_char('/'),
            "%" | "throwing%" => f.write_char('%'),
            "&&" | "throwing&&" => f.write_str("&&"),
            "||" | "throwing||" => f.write_str("||"),
            "!" | "throwing!" => f.write_char('!'),
            "==" | "throwing==" => f.write_char('='),
            "!=" | "throwing!=" => f.write_char('≠'),
            "<" | "throwing<" => f.write_char('<'),
            "<=" | "throwing<=" => f.write_char('≤'),
            ">" | "throwing>" => f.write_char('>'),
            ">=" | "throwing>=" => f.write_char('≥'),
            "func" => f.write_char('λ'),
            "seq" => f.write_char(';'),
            "unit" => f.write_str("()"),
            str => {
                if str == "asg" || str.starts_with("asg/") {
                    return f.write_char('≔');
                }
                if str.starts_with("apply/") {
                    return f.write_char('@');
                }
                if str.starts_with("tuple/") {
                    return f.write_char('⊗');
                }
                if str.starts_with("typeCast/") {
                    return f.write_str("typeCast");
                }
                if let Some(rest) = None
                    .or_else(|| str.strip_prefix("bool/"))
                    .or_else(|| str.strip_prefix("int64/"))
                    .or_else(|| str.strip_prefix("float64/"))
                    .or_else(|| str.strip_prefix("string/"))
                {
                    return f.write_str(rest);
                }
                f.write_str(str)
            }
        }
    }
}

impl Matchable for Op {
    fn is_match(&self, query: &str) -> bool {
        self.0 == query
    }
}

impl<'pest> FromPest<'pest> for Op {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(
        pest: &mut Pairs<'pest, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        let mut clone = pest.clone();
        let pair = clone.next().ok_or(ConversionError::NoMatch)?;
        if pair.as_rule() != Rule::op {
            return Err(ConversionError::NoMatch);
        }
        *pest = clone;
        Ok(Self(pair.as_str().to_owned()))
    }
}

impl OpInfo<Chil> for Op {}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::variable))]
#[cfg_attr(test, derive(Serialize))]
pub struct Variable {
    pub name: Option<Identifier>,
    pub addr: Addr,
}

impl WithType for Variable {
    fn get_type(&self) -> WireType {
        WireType::Data
    }
}

impl From<Empty> for Variable {
    fn from(value: Empty) -> Self {
        match value {}
    }
}

impl Matchable for Variable {
    fn is_match(&self, query: &str) -> bool {
        // If a variable is "foo(id: %0)", then we match "foo(id: %0)", "foo", and "%0".
        self.to_string() == query
            || self.name.as_ref().is_some_and(|id| id.0 == query)
            || self.addr.is_match(query)
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.name {
            None => write!(f, "{}", self.addr),
            Some(name) => write!(f, "{}(id: {})", name, self.addr),
        }
    }
}

impl Fresh for Variable {
    fn fresh(number: usize) -> Self {
        Self {
            name: None,
            addr: Addr('?', number),
        }
    }
}

#[derive(Clone, Eq, Debug, FromPest)]
#[pest_ast(rule(Rule::addr))]
#[cfg_attr(test, derive(Serialize))]
pub struct Addr(
    #[pest_ast(outer(with(span_into_str), with(parse_addr_first)))] pub char,
    #[pest_ast(outer(with(span_into_str), with(parse_addr_second)))] pub usize,
);

impl Matchable for Addr {
    fn is_match(&self, query: &str) -> bool {
        self.to_string() == query
    }
}

impl PartialEq for Addr {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
    }
}

impl Hash for Addr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.1.hash(state);
    }
}

impl Display for Addr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.0, self.1)
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub struct Identifier(pub String);

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl<'pest> FromPest<'pest> for Identifier {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(
        pest: &mut Pairs<'pest, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        let mut clone = pest.clone();
        let pair = clone.next().ok_or(ConversionError::NoMatch)?;
        if pair.as_rule() != Rule::identifier {
            return Err(ConversionError::NoMatch);
        }
        *pest = clone;
        Ok(Self(pair.as_str().to_owned()))
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::variable_def))]
#[cfg_attr(test, derive(Serialize))]
pub struct VariableDef {
    pub var: Variable,
    pub r#type: Option<Type>,
}

impl Display for VariableDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.var)
    }
}

impl Matchable for VariableDef {
    fn is_match(&self, query: &str) -> bool {
        self.var.is_match(query)
    }
}

impl GetVar<Variable> for VariableDef {
    fn var(&self) -> &Variable {
        &self.var
    }

    fn into_var(self) -> Variable {
        self.var
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::ty))]
#[cfg_attr(test, derive(Serialize))]
pub enum Type {
    Base(BaseType),
    Generic(GenericType),
    Tuple(TupleType),
    Function(FunctionType),
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::base_ty))]
#[cfg_attr(test, derive(Serialize))]
pub struct BaseType(#[pest_ast(outer(with(span_into_str), with(str::to_string)))] pub String);

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::generic_ty))]
#[cfg_attr(test, derive(Serialize))]
pub struct GenericType {
    pub base: BaseType,
    pub params: Vec<Type>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::tuple_ty))]
#[cfg_attr(test, derive(Serialize))]
pub struct TupleType {
    pub types: Vec<Type>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::function_ty))]
#[cfg_attr(test, derive(Serialize))]
pub struct FunctionType {
    pub domain: TupleType,
    pub codomain: Box<Type>,
}

// Conversions from pest parse trees

impl<'pest> FromPest<'pest> for Expr {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(
        pest: &mut Pairs<'pest, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        let mut clone = pest.clone();
        let pair = clone.next().ok_or(ConversionError::NoMatch)?;
        if pair.as_rule() != Rule::expr {
            return Err(ConversionError::NoMatch);
        }
        let mut inner = pair.into_inner();
        let expr = Expr {
            binds: FromPest::from_pest(&mut inner)?,
            values: FromPest::from_pest(&mut inner)?,
        };
        if inner.next().is_some() {
            return Err(ConversionError::Extraneous {
                current_node: stringify!(Expr),
            });
        }
        *pest = clone;
        Ok(expr)
    }
}

impl<'pest> FromPest<'pest> for Bind {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(
        pest: &mut Pairs<'pest, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        let mut clone = pest.clone();
        let pair = clone.next().ok_or(ConversionError::NoMatch)?;
        if pair.as_rule() != Rule::bind {
            return Err(ConversionError::NoMatch);
        }
        let mut inner = pair.into_inner();
        let bind = Bind {
            defs: FromPest::from_pest(&mut inner)?,
            value: FromPest::from_pest(&mut inner)?,
        };
        if inner.next().is_some() {
            return Err(ConversionError::Extraneous {
                current_node: stringify!(Bind),
            });
        }
        *pest = clone;
        Ok(bind)
    }
}

impl<'pest> FromPest<'pest> for Value {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(
        pest: &mut Pairs<'pest, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        let mut clone = pest.clone();
        let pair = clone.next().ok_or(ConversionError::NoMatch)?;
        if pair.as_rule() == Rule::value {
            let value = Err(ConversionError::NoMatch)
                .or_else(|_: ConversionError<Void>| {
                    let mut inner = pair.clone().into_inner();
                    let value = Value::Variable(FromPest::from_pest(&mut inner)?);
                    if inner.next().is_some() {
                        return Err(ConversionError::Extraneous {
                            current_node: stringify!(Value),
                        });
                    }
                    Ok(value)
                })
                .or_else(|_: ConversionError<Void>| {
                    let mut inner = pair.into_inner();
                    let value = Value::Op {
                        op: FromPest::from_pest(&mut inner)?,
                        args: FromPest::from_pest(&mut inner)?,
                    };
                    if inner.next().is_some() {
                        return Err(ConversionError::Extraneous {
                            current_node: stringify!(Value),
                        });
                    }
                    Ok(value)
                })?;
            *pest = clone;
            Ok(value)
        } else {
            // If the pair is not a value, then it must be a thunk.
            FromPest::from_pest(pest).map(Value::Thunk)
        }
    }
}

impl<'pest> FromPest<'pest> for Thunk {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(
        pest: &mut Pairs<'pest, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        let mut clone = pest.clone();
        let pair = clone.next().ok_or(ConversionError::NoMatch)?;
        if pair.as_rule() != Rule::thunk {
            return Err(ConversionError::NoMatch);
        }
        let mut inner = pair.into_inner();
        let thunk = Thunk {
            addr: FromPest::from_pest(&mut inner)?,
            args: FromPest::from_pest(&mut inner)?,
            body: FromPest::from_pest(&mut inner)?,
            blocks: vec![],
        };
        if inner.next().is_some() {
            return Err(ConversionError::Extraneous {
                current_node: stringify!(Thunk),
            });
        }
        *pest = clone;
        Ok(thunk)
    }
}

// Conversion to sd-lang

#[cfg(test)]
impl From<Op> for super::sd_lang::Op {
    fn from(op: Op) -> Self {
        match op.0.as_str() {
            "+" | "throwing+" => Self::Plus,
            "-" | "throwing-" => Self::Minus,
            "*" | "throwing*" => Self::Times,
            "/" | "throwing/" => Self::Div,
            "%" | "throwing%" => Self::Rem,
            "&&" | "throwing&&" => Self::And,
            "||" | "throwing||" => Self::Or,
            "!" | "throwing!" => Self::Not,
            "==" | "throwing==" => Self::Eq,
            "!=" | "throwing!=" => Self::Neq,
            "<" | "throwing<" => Self::Lt,
            "<=" | "throwing<=" => Self::Leq,
            ">" | "throwing>" => Self::Gt,
            ">=" | "throwing>=" => Self::Geq,
            "func" => Self::Lambda,
            "atom" => Self::Atom,
            "deref" => Self::Deref,
            "asg" => Self::Assign,
            "bool/true" => Self::Bool(true),
            "bool/false" => Self::Bool(false),
            str => {
                if str.starts_with("apply/") {
                    return Self::App;
                }
                if let Some(rest) = str.strip_prefix("int64/") {
                    return Self::Number(rest.parse().unwrap_or_default());
                }
                Self::Plus // dummy placeholder
            }
        }
    }
}

#[cfg(test)]
impl From<Variable> for super::sd_lang::Variable {
    fn from(var: Variable) -> Self {
        Self(format!("var_{}", var.addr.1))
    }
}

#[cfg(test)]
impl From<Addr> for crate::common::Unit {
    fn from(_addr: Addr) -> Self {
        Self
    }
}

#[cfg(test)]
impl From<VariableDef> for super::sd_lang::Variable {
    fn from(def: VariableDef) -> Self {
        def.var.into()
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use std::path::Path;

    use dir_test::{Fixture, dir_test};
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
