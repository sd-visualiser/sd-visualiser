use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

use derivative::Derivative;
use from_pest::{ConversionError, FromPest, Void};
use pest::{iterators::Pairs, RuleType};

use crate::{common::Matchable, prettyprinter::PrettyPrint};

pub mod chil;
pub mod spartan;

pub(crate) fn span_into_str(span: pest::Span) -> &str {
    span.as_str()
}

pub trait GetVar<V> {
    fn var(&self) -> &V;
    fn into_var(self) -> V;
}

impl<V> GetVar<V> for V {
    fn var(&self) -> &V {
        self
    }

    fn into_var(self) -> V {
        self
    }
}

pub trait Fresh {
    fn fresh(number: usize) -> Self;
}

/// `Display` should give the symbolic representation (e.g. "+").
/// `PrettyPrint` should give the textual representation (e.g. "plus").
pub trait Syntax: Clone + Eq + Hash + Debug + Send + Sync + Display + PrettyPrint {}
impl<T: Clone + Eq + Hash + Debug + Send + Sync + Display + PrettyPrint> Syntax for T {}

pub trait Language {
    type Op: Syntax;
    type Var: Syntax + Matchable + Fresh;
    type Addr: Syntax + Matchable;
    type VarDef: Syntax + Matchable + GetVar<Self::Var>;

    type Rule: RuleType;
    fn expr_rule() -> Self::Rule;
    fn bind_rule() -> Self::Rule;
    fn value_rule() -> Self::Rule;
    fn thunk_rule() -> Self::Rule;
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub struct Expr<T: Language> {
    pub binds: Vec<Bind<T>>,
    pub values: Vec<Value<T>>,
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub struct Bind<T: Language> {
    pub defs: Vec<T::VarDef>,
    pub value: Value<T>,
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub enum Value<T: Language> {
    Variable(T::Var),
    Thunk(Thunk<T>),
    Op { op: T::Op, args: Vec<Value<T>> },
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub struct Thunk<T: Language> {
    pub addr: T::Addr,
    pub args: Vec<T::VarDef>,
    pub body: Expr<T>,
}

// Conversions between languages

impl<T: Language> Expr<T> {
    pub fn into<U: Language>(self) -> Expr<U>
    where
        U::Op: From<T::Op>,
        U::Var: From<T::Var>,
        U::Addr: From<T::Addr>,
        U::VarDef: From<T::VarDef>,
    {
        Expr {
            binds: self.binds.into_iter().map(Bind::into).collect(),
            values: self.values.into_iter().map(Value::into).collect(),
        }
    }
}

impl<T: Language> Bind<T> {
    pub fn into<U: Language>(self) -> Bind<U>
    where
        U::Op: From<T::Op>,
        U::Var: From<T::Var>,
        U::Addr: From<T::Addr>,
        U::VarDef: From<T::VarDef>,
    {
        Bind {
            defs: self.defs.into_iter().map(Into::into).collect(),
            value: self.value.into(),
        }
    }
}

impl<T: Language> Value<T> {
    pub fn into<U: Language>(self) -> Value<U>
    where
        U::Op: From<T::Op>,
        U::Var: From<T::Var>,
        U::Addr: From<T::Addr>,
        U::VarDef: From<T::VarDef>,
    {
        match self {
            Self::Variable(var) => Value::Variable(var.into()),
            Self::Thunk(thunk) => Value::Thunk(thunk.into()),
            Self::Op { op, args } => Value::Op {
                op: op.into(),
                args: args.into_iter().map(Value::into).collect(),
            },
        }
    }
}

impl<T: Language> Thunk<T> {
    pub fn into<U: Language>(self) -> Thunk<U>
    where
        U::Op: From<T::Op>,
        U::Var: From<T::Var>,
        U::Addr: From<T::Addr>,
        U::VarDef: From<T::VarDef>,
    {
        Thunk {
            addr: self.addr.into(),
            args: self.args.into_iter().map(Into::into).collect(),
            body: self.body.into(),
        }
    }
}

// Conversions from pest parse trees

impl<'pest, T> FromPest<'pest> for Expr<T>
where
    T: Language,
    T::Op: FromPest<'pest, Rule = T::Rule, FatalError = Void>,
    T::Var: FromPest<'pest, Rule = T::Rule, FatalError = Void>,
    T::Addr: FromPest<'pest, Rule = T::Rule, FatalError = Void>,
    T::VarDef: FromPest<'pest, Rule = T::Rule, FatalError = Void>,
{
    type Rule = T::Rule;
    type FatalError = Void;

    fn from_pest(
        pest: &mut Pairs<'pest, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        let mut clone = pest.clone();
        let pair = clone.next().ok_or(ConversionError::NoMatch)?;
        if pair.as_rule() != T::expr_rule() {
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

impl<'pest, T> FromPest<'pest> for Bind<T>
where
    T: Language,
    T::Op: FromPest<'pest, Rule = T::Rule, FatalError = Void>,
    T::Var: FromPest<'pest, Rule = T::Rule, FatalError = Void>,
    T::Addr: FromPest<'pest, Rule = T::Rule, FatalError = Void>,
    T::VarDef: FromPest<'pest, Rule = T::Rule, FatalError = Void>,
{
    type Rule = T::Rule;
    type FatalError = Void;

    fn from_pest(
        pest: &mut Pairs<'pest, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        let mut clone = pest.clone();
        let pair = clone.next().ok_or(ConversionError::NoMatch)?;
        if pair.as_rule() != T::bind_rule() {
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

impl<'pest, T> FromPest<'pest> for Value<T>
where
    T: Language,
    T::Op: FromPest<'pest, Rule = T::Rule, FatalError = Void>,
    T::Var: FromPest<'pest, Rule = T::Rule, FatalError = Void>,
    T::Addr: FromPest<'pest, Rule = T::Rule, FatalError = Void>,
    T::VarDef: FromPest<'pest, Rule = T::Rule, FatalError = Void>,
{
    type Rule = T::Rule;
    type FatalError = Void;

    fn from_pest(
        pest: &mut Pairs<'pest, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        let mut clone = pest.clone();
        let pair = clone.next().ok_or(ConversionError::NoMatch)?;
        if pair.as_rule() == T::value_rule() {
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
        } else if pair.as_rule() == T::thunk_rule() {
            let thunk = FromPest::from_pest(pest)?;
            Ok(Value::Thunk(thunk))
        } else {
            Err(ConversionError::NoMatch)
        }
    }
}

impl<'pest, T> FromPest<'pest> for Thunk<T>
where
    T: Language,
    T::Op: FromPest<'pest, Rule = T::Rule, FatalError = Void>,
    T::Var: FromPest<'pest, Rule = T::Rule, FatalError = Void>,
    T::Addr: FromPest<'pest, Rule = T::Rule, FatalError = Void>,
    T::VarDef: FromPest<'pest, Rule = T::Rule, FatalError = Void>,
{
    type Rule = T::Rule;
    type FatalError = Void;

    fn from_pest(
        pest: &mut Pairs<'pest, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError>> {
        let mut clone = pest.clone();
        let pair = clone.next().ok_or(ConversionError::NoMatch)?;
        if pair.as_rule() != T::thunk_rule() {
            return Err(ConversionError::NoMatch);
        }
        let mut inner = pair.into_inner();
        let thunk = Thunk {
            addr: FromPest::from_pest(&mut inner)?,
            args: FromPest::from_pest(&mut inner)?,
            body: FromPest::from_pest(&mut inner)?,
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

#[cfg(test)]
pub(crate) mod tests {
    use std::{ffi::OsStr, path::Path};

    use super::{
        chil::tests::parse_chil,
        spartan::{self, tests::parse_sd},
    };

    pub fn parse(raw_path: &str) -> (&str, &str, spartan::Expr) {
        let path = Path::new(raw_path);
        match path.extension() {
            Some(ext) if ext == OsStr::new("sd") => {
                let (name, expr) = parse_sd(raw_path);
                ("sd", name, expr)
            }
            Some(ext) if ext == OsStr::new("chil") => {
                let (name, expr) = parse_chil(raw_path);
                ("chil", name, expr.into())
            }
            _ => unreachable!(),
        }
    }
}
