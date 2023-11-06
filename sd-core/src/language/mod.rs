use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

use derivative::Derivative;

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
pub trait Syntax:
    Clone + Eq + Hash + Debug + Send + Sync + Display + Matchable + PrettyPrint
{
}
impl<T: Clone + Eq + Hash + Debug + Send + Sync + Display + Matchable + PrettyPrint> Syntax for T {}

pub struct BlockWithArgs<T: Language + ?Sized> {
    pub block: T::BlockAddr,
    pub args: Vec<Value<T>>,
}

pub enum CF<T: Language + ?Sized> {
    Return,
    Brs(Vec<BlockWithArgs<T>>),
}

pub trait ControlFlow<T: Language + ?Sized> {
    fn get_cf(&self) -> Option<CF<T>>;

    #[must_use]
    fn get_apply() -> Option<T::Op> {
        None
    }
}

pub trait Language {
    type Op: Syntax + ControlFlow<Self>;
    type Var: Syntax + Fresh;
    type Addr: Syntax;
    type BlockAddr: Syntax;
    type VarDef: Syntax + GetVar<Self::Var>;
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub struct Expr<T: Language + ?Sized> {
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
pub struct Bind<T: Language + ?Sized> {
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
pub enum Value<T: Language + ?Sized> {
    Variable(T::Var),
    Thunk(Thunk<T>),
    Op { op: T::Op, args: Vec<Value<T>> },
}

impl<T: Language> ControlFlow<T> for Value<T> {
    fn get_cf(&self) -> Option<CF<T>> {
        match self {
            Value::Op { op, .. } => op.get_cf(),
            _ => None,
        }
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub struct Thunk<T: Language + ?Sized> {
    pub addr: T::Addr,
    pub args: Vec<T::VarDef>,
    pub body: Expr<T>,
    pub blocks: Vec<Block<T>>,
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub struct Block<T: Language + ?Sized> {
    pub addr: T::BlockAddr,
    pub args: Vec<T::VarDef>,
    pub expr: Expr<T>,
}

// Conversions between languages

impl<T: Language> Expr<T> {
    pub fn into<U: Language>(self) -> Expr<U>
    where
        U::Op: From<T::Op>,
        U::Var: From<T::Var>,
        U::Addr: From<T::Addr>,
        U::VarDef: From<T::VarDef>,
        U::BlockAddr: From<T::BlockAddr>,
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
        U::BlockAddr: From<T::BlockAddr>,
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
        U::BlockAddr: From<T::BlockAddr>,
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
        U::BlockAddr: From<T::BlockAddr>,
    {
        Thunk {
            addr: self.addr.into(),
            args: self.args.into_iter().map(Into::into).collect(),
            body: self.body.into(),
            blocks: self.blocks.into_iter().map(Block::into).collect(),
        }
    }
}

impl<T: Language> Block<T> {
    pub fn into<U: Language>(self) -> Block<U>
    where
        U::Op: From<T::Op>,
        U::Var: From<T::Var>,
        U::Addr: From<T::Addr>,
        U::VarDef: From<T::VarDef>,
        U::BlockAddr: From<T::BlockAddr>,
    {
        Block {
            addr: self.addr.into(),
            args: self.args.into_iter().map(Into::into).collect(),
            expr: self.expr.into(),
        }
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
