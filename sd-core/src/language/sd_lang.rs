#![allow(clippy::clone_on_copy)]

use std::{
    collections::BTreeMap,
    fmt::{Debug, Display, Write},
    str::FromStr,
};

use derive_more::Display;
use ecolor::Color32;
use from_pest::{ConversionError, FromPest, Void};
use pest::iterators::Pairs;
use pest_ast::FromPest;
use pest_derive::Parser;
#[cfg(test)]
use serde::Serialize;

use super::{Fresh, GetVar, OpInfo, span_into_str};
use crate::{
    common::{Empty, Matchable, Unit},
    hypergraph::traits::{WireType, WithType},
};

pub struct SdLang;

impl super::Language for SdLang {
    type Op = Op;
    type Var = Variable;
    type Addr = Unit;
    type VarDef = VariableDef;
    type BlockAddr = Empty;
    type Symbol = Empty;
}

impl TryFrom<<SdLang as super::Language>::Addr> for <SdLang as super::Language>::Symbol {
    type Error = &'static str;

    fn try_from(_: <SdLang as super::Language>::Addr) -> Result<Self, Self::Error> {
        Err("no symbols in sd-lang")
    }
}

pub type Expr = super::Expr<SdLang>;
pub type Bind = super::Bind<SdLang>;
pub type Value = super::Value<SdLang>;
pub type Thunk = super::Thunk<SdLang>;

#[derive(Parser)]
#[grammar = "language/sd-lang.pest"]
pub struct SdLangParser;

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
    Tuple,
    Detuple,
    Bool(bool),
    Number(usize),
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
            Self::Tuple => f.write_str("()"),
            Self::Detuple => f.write_str(")("),
            Self::Bool(b) => f.write_str(&b.to_string()),
            Self::Number(n) => f.write_str(&n.to_string()),
        }
    }
}

impl FromStr for Op {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "plus" => Ok(Self::Plus),
            "minus" => Ok(Self::Minus),
            "times" => Ok(Self::Times),
            "div" => Ok(Self::Div),
            "rem" => Ok(Self::Rem),
            "and" => Ok(Self::And),
            "or" => Ok(Self::Or),
            "not" => Ok(Self::Not),
            "if" => Ok(Self::If),
            "eq" => Ok(Self::Eq),
            "neq" => Ok(Self::Neq),
            "lt" => Ok(Self::Lt),
            "leq" => Ok(Self::Leq),
            "gt" => Ok(Self::Gt),
            "geq" => Ok(Self::Geq),
            "app" => Ok(Self::App),
            "lambda" => Ok(Self::Lambda),
            "atom" => Ok(Self::Atom),
            "deref" => Ok(Self::Deref),
            "assign" => Ok(Self::Assign),
            "tuple" => Ok(Self::Tuple),
            "detuple" => Ok(Self::Detuple),
            "true" => Ok(Self::Bool(true)),
            "false" => Ok(Self::Bool(false)),
            _ => s.parse().map(Self::Number).map_err(|_err| ()),
        }
    }
}

impl Matchable for Op {
    fn is_match(&self, query: &str) -> bool {
        match self {
            Self::Plus => query == "plus",
            Self::Minus => query == "minus",
            Self::Times => query == "times",
            Self::Div => query == "div",
            Self::Rem => query == "rem",
            Self::And => query == "and",
            Self::Or => query == "or",
            Self::Not => query == "not",
            Self::If => query == "if",
            Self::Eq => query == "eq",
            Self::Neq => query == "neq",
            Self::Lt => query == "lt",
            Self::Leq => query == "leq",
            Self::Gt => query == "gt",
            Self::Geq => query == "geq",
            Self::App => query == "app",
            Self::Lambda => query == "lambda",
            Self::Atom => query == "atom",
            Self::Deref => query == "deref",
            Self::Assign => query == "assign",
            Self::Tuple => query == "tuple",
            Self::Detuple => query == "detuple",
            Self::Bool(b) => b.to_string() == query,
            Self::Number(n) => n.to_string() == query,
        }
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
        let op = pair
            .as_str()
            .parse()
            .map_err(|()| ConversionError::NoMatch)?;
        *pest = clone;
        Ok(op)
    }
}

impl OpInfo<SdLang> for Op {}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, FromPest, Display)]
#[cfg_attr(test, derive(Serialize))]
#[pest_ast(rule(Rule::variable))]
pub struct Variable(#[pest_ast(outer(with(span_into_str), with(str::to_string)))] pub String);

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
        self.0 == query
    }
}

impl Fresh for Variable {
    fn fresh(number: usize) -> Self {
        Self(format!("?{number}"))
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, Display)]
#[cfg_attr(test, derive(Serialize))]
#[display("{var}")]
pub struct VariableDef {
    pub var: Variable,
    pub meta: Option<BTreeMap<String, String>>,
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

impl WithType for VariableDef {
    fn get_type(&self) -> WireType {
        match &self.meta {
            Some(attrs) if attrs.contains_key("colour") || attrs.contains_key("color") => {
                let colour_str = attrs.get("colour").or_else(|| attrs.get("color")).unwrap();
                match colour_str.to_uppercase().as_str() {
                    "TRANSPARENT" | "INVISIBLE" => WireType::Colour(Color32::TRANSPARENT),
                    "BLACK" => WireType::Colour(Color32::BLACK),
                    "DARK_GRAY" => WireType::Colour(Color32::DARK_GRAY),
                    "GRAY" => WireType::Colour(Color32::GRAY),
                    "LIGHT_GRAY" => WireType::Colour(Color32::LIGHT_GRAY),
                    "WHITE" => WireType::Colour(Color32::WHITE),
                    "BROWN" => WireType::Colour(Color32::BROWN),
                    "DARK_RED" => WireType::Colour(Color32::DARK_RED),
                    "RED" => WireType::Colour(Color32::RED),
                    "LIGHT_RED" => WireType::Colour(Color32::LIGHT_RED),
                    "CYAN" => WireType::Colour(Color32::CYAN),
                    "MAGENTA" => WireType::Colour(Color32::MAGENTA),
                    "YELLOW" => WireType::Colour(Color32::YELLOW),
                    "ORANGE" => WireType::Colour(Color32::ORANGE),
                    "LIGHT_YELLOW" => WireType::Colour(Color32::LIGHT_YELLOW),
                    "KHAKI" => WireType::Colour(Color32::KHAKI),
                    "DARK_GREEN" => WireType::Colour(Color32::DARK_GREEN),
                    "GREEN" => WireType::Colour(Color32::GREEN),
                    "LIGHT_GREEN" => WireType::Colour(Color32::LIGHT_GREEN),
                    "DARK_BLUE" => WireType::Colour(Color32::DARK_BLUE),
                    "BLUE" => WireType::Colour(Color32::BLUE),
                    "LIGHT_BLUE" => WireType::Colour(Color32::LIGHT_BLUE),
                    "PURPLE" => WireType::Colour(Color32::PURPLE),
                    "GOLD" => WireType::Colour(Color32::GOLD),
                    str => Color32::from_hex(str)
                        .map(WireType::Colour)
                        .unwrap_or_else(|_| {
                            tracing::error!("invalid colour {:?}", colour_str);
                            WireType::Data
                        }),
                }
            }
            _ => WireType::Data,
        }
    }
}

impl<'pest> FromPest<'pest> for Unit {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(_: &mut Pairs<'pest, Rule>) -> Result<Self, ConversionError<Void>> {
        Ok(Unit)
    }
}

// Conversions from pest parse trees

impl<'pest> FromPest<'pest> for VariableDef {
    type Rule = Rule;
    type FatalError = Void;

    fn from_pest(
        pest: &mut Pairs<'pest, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        let mut clone = pest.clone();
        let pair = clone.next().ok_or(ConversionError::NoMatch)?;
        if pair.as_rule() != Rule::variable_def {
            return Err(ConversionError::NoMatch);
        }
        let mut inner = pair.into_inner();
        let var = FromPest::from_pest(&mut inner)?;
        let meta = match inner.next() {
            Some(meta) if meta.as_rule() == Rule::var_meta => Some(
                meta.into_inner()
                    .filter_map(|attr| {
                        if attr.as_rule() != Rule::attribute_entry {
                            tracing::error!("unexpected rule {:?}", attr.as_rule());
                            return None;
                        }
                        let mut inner = attr.into_inner();
                        let key = inner.next().unwrap().as_str().to_owned();
                        let quoted = inner.next().unwrap().as_str();
                        if quoted[0..1] != *"\"" || quoted[quoted.len() - 1..] != *"\"" {
                            return None;
                        }
                        let unquoted = quoted[1..quoted.len() - 1].to_owned();
                        Some((key, unquoted))
                    })
                    .collect(),
            ),
            _ => None,
        };
        let vardef = VariableDef { var, meta };
        if inner.clone().next().is_some() {
            return Err(ConversionError::Extraneous {
                current_node: stringify!(VariableDef),
            });
        }
        *pest = clone;
        Ok(vardef)
    }
}

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
        if inner.clone().next().is_some() {
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
        if pair.as_rule() != Rule::value {
            return Err(ConversionError::NoMatch);
        }
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
                let mut inner = pair.clone().into_inner();
                let value = Value::Thunk(FromPest::from_pest(&mut inner)?);
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
        let addr = FromPest::from_pest(&mut inner)?;
        let args = FromPest::from_pest(&mut inner)?;
        let body = FromPest::from_pest(&mut inner)?;
        let reqs = match inner.next() {
            Some(with) if with.as_rule() == Rule::with => {
                FromPest::from_pest(&mut with.into_inner())?
            }
            _ => Vec::default(),
        };
        let thunk = Thunk {
            addr,
            args,
            reqs,
            body,
            blocks: Vec::default(),
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

pub fn parse_sd_lang(raw_path: &str) -> (&str, Expr) {
    let path = std::path::Path::new(raw_path);
    let program = std::fs::read_to_string(path).unwrap();
    let mut pairs = <SdLangParser as pest::Parser<_>>::parse(Rule::program, &program)
        .unwrap_or_else(|err| {
            panic!(
                "could not parse program {:?}\n{err:?}",
                path.file_stem().unwrap()
            )
        });
    let name = path.file_stem().unwrap().to_str().unwrap();
    let expr = Expr::from_pest(&mut pairs).unwrap();
    (name, expr)
}

#[cfg(test)]
pub(crate) mod tests {

    use dir_test::{Fixture, dir_test};

    use super::Expr;

    #[allow(clippy::needless_pass_by_value)]
    #[dir_test(dir: "$CARGO_MANIFEST_DIR/../examples", glob: "**/*.sd", loader: crate::language::sd_lang::parse_sd_lang, postfix: "check_parse")]
    fn check_parse(fixture: Fixture<(&str, Expr)>) {
        let (_name, _expr) = fixture.content();
    }
}
