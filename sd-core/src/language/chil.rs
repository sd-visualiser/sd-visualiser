#![allow(clippy::clone_on_copy)]

use std::fmt::{Display, Write};

use from_pest::{ConversionError, FromPest, Void};
use pest::iterators::Pairs;
use pest_ast::FromPest;
use pest_derive::Parser;

use super::{span_into_str, AsVar};

pub struct Chil;

impl super::Language for Chil {
    type Op = Op;
    type Var = Variable;
    type Addr = Addr;
    type VarDef = VariableDef;

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
                if str.starts_with("apply/") {
                    return f.write_char('@');
                }
                if str.starts_with("tuple/") {
                    return f.write_char('⊗');
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

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::variable))]
pub struct Variable {
    pub name: Option<Identifier>,
    pub addr: Addr,
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.name {
            None => write!(f, "{}", self.addr),
            Some(name) => write!(f, "{}(id: {})", name, self.addr),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, FromPest)]
#[pest_ast(rule(Rule::addr))]
pub struct Addr(
    #[pest_ast(outer(with(span_into_str), with(parse_addr_first)))] pub char,
    #[pest_ast(outer(with(span_into_str), with(parse_addr_second)))] pub usize,
);

impl Display for Addr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.0, self.1)
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
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
pub struct VariableDef {
    pub var: Variable,
    pub r#type: Option<Type>,
}

impl AsVar<Variable> for VariableDef {
    fn as_var(&self) -> &Variable {
        &self.var
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

// Conversion to spartan

#[cfg(test)]
impl From<Op> for super::spartan::Op {
    fn from(op: Op) -> Self {
        match op.0.as_str() {
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
            "atom" => return Self::Atom,
            "deref" => return Self::Deref,
            "asg" => return Self::Assign,
            "bool/true" => return Self::Bool(true),
            "bool/false" => return Self::Bool(false),
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
impl From<Variable> for super::spartan::Variable {
    fn from(var: Variable) -> Self {
        Self(format!("var_{}", var.addr.1))
    }
}

#[cfg(test)]
impl From<Addr> for super::spartan::Addr {
    fn from(_addr: Addr) -> Self {
        Self
    }
}

#[cfg(test)]
impl From<VariableDef> for super::spartan::Variable {
    fn from(def: VariableDef) -> Self {
        def.var.into()
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
