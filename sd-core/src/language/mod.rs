use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

use derivative::Derivative;

use crate::{common::Matchable, hypergraph::traits::WithType, prettyprinter::PrettyPrint};

pub mod chil;
#[cfg(not(target_arch = "wasm32"))]
pub mod llvm_ir;
pub mod mlir;
pub mod sd_lang;

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

pub enum CF<T: Language + ?Sized> {
    Return,
    Brs(Vec<T::BlockAddr>),
}

pub trait OpInfo<T: Language + ?Sized> {
    fn get_cf(&self) -> Option<CF<T>> {
        None
    }
    fn symbols_used(&self) -> impl Iterator<Item = T::Symbol> {
        std::iter::empty()
    }
    fn sym_name(&self) -> Option<T::Symbol> {
        None
    }
}

pub trait Language {
    type Op: Syntax + OpInfo<Self>;
    type Var: Syntax + Fresh + From<Self::Symbol> + WithType;
    type Addr: Syntax;
    type BlockAddr: Syntax;
    type VarDef: Syntax + GetVar<Self::Var>;
    type Symbol: Syntax + TryFrom<Self::Addr, Error: Debug>;
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = ""),
    Default(bound = "")
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
    pub reqs: Vec<T::Var>,
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
            reqs: self.reqs.into_iter().map(Into::into).collect(),
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

    use serde::Serialize;

    use super::{
        Expr, Language,
        chil::tests::parse_chil,
        llvm_ir::tests::parse_llvm_ir,
        mlir::{
            self,
            internal::{TopLevelItem, tests::parse_mlir},
        },
        sd_lang::tests::parse_sd_lang,
    };
    use crate::{graph::SyntaxHypergraph, hypergraph::petgraph::to_pet, language::llvm_ir::LlvmIr};

    pub trait ExprTest {
        fn free_var_test(&self) -> Box<dyn std::fmt::Debug>;
        fn graph_test(&self, name: &str, lang: &str, sym_name_link: bool) -> anyhow::Result<()>;
    }

    impl<T: Language + 'static> ExprTest for Expr<T>
    where
        <T as Language>::Op: Serialize,
        <T as Language>::BlockAddr: Serialize,
        <T as Language>::Var: Serialize,
        <T as Language>::VarDef: Serialize,
    {
        fn free_var_test(&self) -> Box<dyn std::fmt::Debug> {
            Box::new(self.free_vars(false))
        }

        fn graph_test(&self, name: &str, lang: &str, sym_name_link: bool) -> anyhow::Result<()> {
            let graph: SyntaxHypergraph<T> = self.to_graph(sym_name_link)?;
            let name = if sym_name_link {
                format!("hypergraph_with_sym_{name}.{lang}")
            } else {
                format!("hypergraph_{name}.{lang}")
            };
            insta::assert_ron_snapshot!(name, to_pet(&graph));
            Ok(())
        }
    }

    impl ExprTest for crate::language::llvm_ir::Expr {
        fn free_var_test(&self) -> Box<dyn std::fmt::Debug> {
            Box::new(self.free_vars(false))
        }

        fn graph_test(&self, name: &str, lang: &str, sym_name_link: bool) -> anyhow::Result<()> {
            let graph: SyntaxHypergraph<LlvmIr> = self.to_graph(sym_name_link)?;
            let name = if sym_name_link {
                format!("hypergraph_with_sym_{name}_debug.{lang}")
            } else {
                format!("hypergraph_{name}_debug.{lang}")
            };
            // cannot serialise LLVM IR expressions, so use Debug instead
            insta::assert_debug_snapshot!(name, to_pet(&graph));
            Ok(())
        }
    }

    pub fn parse(raw_path: &str) -> (&str, &str, Box<dyn ExprTest>) {
        let path = Path::new(raw_path);
        match path.extension() {
            Some(ext) if ext == OsStr::new("sd") => {
                let (name, expr) = parse_sd_lang(raw_path);
                ("sd", name, Box::new(expr))
            }
            Some(ext) if ext == OsStr::new("chil") => {
                let (name, expr) = parse_chil(raw_path);
                ("chil", name, Box::new(expr))
            }
            Some(ext) if ext == OsStr::new("ll") => {
                let (name, expr) = parse_llvm_ir(raw_path);
                ("llvm-ir", name, Box::new(expr))
            }
            Some(ext) if ext == OsStr::new("mlir") => {
                let (name, items) = parse_mlir(raw_path);
                let ops: Vec<mlir::internal::Operation> = items
                    .into_iter()
                    .filter_map(|x| match x {
                        TopLevelItem::Operation(y) => Some(y),
                        TopLevelItem::Other(_) => None,
                    })
                    .collect();
                let expr = mlir::Expr::from(ops);
                ("mlir", name, Box::new(expr))
            }
            _ => unreachable!(),
        }
    }
}
