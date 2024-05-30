#![allow(clippy::clone_on_copy)]

use std::fmt::Display;

use itertools::Itertools;

pub mod internal;

use pretty::RcDoc;
#[cfg(test)]
use serde::Serialize;

use self::internal::Attribute;
use super::{Fresh, Language, OpInfo, CF};
use crate::{
    common::{Matchable, Unit},
    hypergraph::traits::{WireType, WithType},
    prettyprinter::PrettyPrint,
};

pub struct Mlir;

impl Language for Mlir {
    type Op = Op;
    type Var = Var;
    type Addr = Unit;
    type VarDef = Var;
    type BlockAddr = BlockAddr;
    type Symbol = Symbol;
}

pub type Expr = super::Expr<Mlir>;
pub type Bind = super::Bind<Mlir>;
pub type Value = super::Value<Mlir>;
pub type Thunk = super::Thunk<Mlir>;
pub type Block = super::Block<Mlir>;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub struct Op {
    pub name: String,
    pub successors: Vec<BlockAddr>,
    pub attributes: String,
    pub sym_name: Option<String>,
    pub symbols: Vec<String>,
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

impl Matchable for Op {
    fn is_match(&self, query: &str) -> bool {
        self.name == query
    }
}

impl OpInfo<Mlir> for Op {
    fn get_cf(&self) -> Option<CF<Mlir>> {
        if !self.successors.is_empty() {
            Some(CF::Brs(self.successors.clone()))
        } else if self.name.contains("return") || self.name == "arc.output" || self.name == "yield"
        {
            Some(CF::Return)
        } else {
            None
        }
    }

    fn symbols_used(&self) -> impl Iterator<Item = Symbol> {
        self.symbols.iter().map(|x| Symbol(x.clone()))
    }

    fn sym_name(&self) -> Option<Symbol> {
        self.sym_name.as_ref().map(|x| Symbol(x.clone()))
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub enum Var {
    Var { id: String },
    VarIdx { id: String, index: usize },
    Symbol(Symbol),
}

impl WithType for Var {
    fn get_type(&self) -> WireType {
        match self {
            Var::Symbol(_) => WireType::SymName,
            _ => WireType::Data,
        }
    }
}

impl From<Symbol> for Var {
    fn from(value: Symbol) -> Self {
        Var::Symbol(value)
    }
}

impl Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Var::Var { id } => f.write_str(id),
            Var::VarIdx { id, index } => write!(f, "{id}#{index}"),
            Var::Symbol(s) => s.fmt(f),
        }
    }
}

impl Matchable for Var {
    fn is_match(&self, query: &str) -> bool {
        match self {
            Var::Var { id } => id == query,
            Var::VarIdx { id, .. } => id == query,
            Var::Symbol(s) => s.is_match(query),
        }
    }
}

impl Fresh for Var {
    fn fresh(number: usize) -> Self {
        Var::Var {
            id: format!("?{number}"),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub struct BlockAddr(pub String);

impl Display for BlockAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl Matchable for BlockAddr {
    fn is_match(&self, query: &str) -> bool {
        self.0 == query
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub struct Symbol(pub String);

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl Matchable for Symbol {
    fn is_match(&self, query: &str) -> bool {
        self.0 == query || self.0 == query.chars().filter(|c| c != &'@').collect::<String>()
    }
}

impl PrettyPrint for Symbol {
    fn to_doc(&self) -> pretty::RcDoc<'_, ()> {
        RcDoc::text(&self.0)
    }
}

// Conversion from internal AST.

impl From<internal::Value> for Var {
    fn from(value: internal::Value) -> Self {
        if let Some(i) = value.index {
            Var::VarIdx {
                id: value.id,
                index: i.0,
            }
        } else {
            Var::Var { id: value.id }
        }
    }
}

impl From<internal::TypedArg> for Var {
    fn from(arg: internal::TypedArg) -> Self {
        Var::Var { id: arg.id }
    }
}

impl From<internal::OpResult> for Vec<Var> {
    fn from(op_result: internal::OpResult) -> Vec<Var> {
        if let Some(idx) = op_result.index {
            (0..idx.0)
                .map(|x| Var::VarIdx {
                    id: op_result.id.clone(),
                    index: x,
                })
                .collect()
        } else {
            vec![Var::Var { id: op_result.id }]
        }
    }
}

impl From<internal::BlockId> for BlockAddr {
    fn from(id: internal::BlockId) -> Self {
        BlockAddr(id.0)
    }
}

impl From<internal::Successor> for BlockAddr {
    fn from(successor: internal::Successor) -> Self {
        BlockAddr(successor.id)
    }
}

impl From<Vec<internal::Operation>> for Expr {
    fn from(ops: Vec<internal::Operation>) -> Self {
        Expr {
            binds: ops.into_iter().map_into().collect(),
            values: vec![],
        }
    }
}

impl From<internal::Operation> for Bind {
    fn from(op: internal::Operation) -> Self {
        Bind {
            defs: op.result.into_iter().map_into::<Vec<Var>>().concat(),
            value: op.operation.into(),
        }
    }
}

impl From<internal::Region> for Thunk {
    fn from(region: internal::Region) -> Self {
        Thunk {
            addr: Unit,
            args: vec![],
            body: region
                .entry_block
                .map(|block| block.operations.into())
                .unwrap_or_default(),
            blocks: region.blocks.into_iter().map_into().collect(),
        }
    }
}

impl From<internal::Block> for Block {
    fn from(block: internal::Block) -> Self {
        Block {
            addr: block.label.id.into(),
            args: block.label.args.into_iter().map_into().collect(),
            expr: block.operations.into(),
        }
    }
}

impl From<internal::GenericOperation> for Value {
    fn from(generic_op: internal::GenericOperation) -> Self {
        Value::Op {
            op: Op {
                name: generic_op.op,
                successors: generic_op.successors.into_iter().map_into().collect(),
                attributes: if generic_op.attributes.len() + generic_op.properties.len() == 0 {
                    String::new()
                } else {
                    format!(
                        "{{{}}}",
                        generic_op
                            .attributes
                            .iter()
                            .chain(generic_op.properties.iter())
                            .join(", ")
                    )
                },
                sym_name: generic_op
                    .attributes
                    .iter()
                    .chain(generic_op.properties.iter())
                    .map(Attribute::is_sym_name)
                    .find(Option::is_some)
                    .flatten(),
                symbols: generic_op
                    .attributes
                    .iter()
                    .chain(generic_op.properties.iter())
                    .filter_map(|attr| attr.get_symbol())
                    .collect(),
            },
            args: generic_op
                .operands
                .into_iter()
                .map(|x| Value::Variable(x.into()))
                .chain(
                    generic_op
                        .regions
                        .into_iter()
                        .map(|x| Value::Thunk(x.into())),
                )
                .collect(),
        }
    }
}
