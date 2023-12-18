#![allow(clippy::clone_on_copy)]

use std::fmt::Display;

use itertools::Itertools;

pub mod internal;

use super::{ControlFlow, Fresh, Language, CF};
use crate::common::{Matchable, Unit};

pub struct Mlir;

impl Language for Mlir {
    type Op = Op;
    type Var = Var;
    type Addr = Unit;
    type VarDef = Var;
    type BlockAddr = BlockAddr;
}

pub type Expr = super::Expr<Mlir>;
pub type Bind = super::Bind<Mlir>;
pub type Value = super::Value<Mlir>;
pub type Thunk = super::Thunk<Mlir>;
pub type Block = super::Block<Mlir>;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Op {
    pub name: String,
    pub successors: Vec<BlockAddr>,
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

impl ControlFlow<Mlir> for Op {
    fn get_cf(&self) -> Option<CF<Mlir>> {
        if !self.successors.is_empty() {
            Some(CF::Brs(self.successors.clone()))
        } else if self.name.contains("return") {
            Some(CF::Return)
        } else {
            None
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Var {
    pub id: String,
    pub index: Option<usize>,
}

impl Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(idx) = self.index {
            write!(f, "{}#{idx}", self.id)
        } else {
            f.write_str(&self.id)
        }
    }
}

impl Matchable for Var {
    fn is_match(&self, query: &str) -> bool {
        self.id == query
    }
}

impl Fresh for Var {
    fn fresh(number: usize) -> Self {
        Var {
            id: format!("?{number}"),
            index: None,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
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

// Conversion from internal AST.

impl From<internal::Value> for Var {
    fn from(value: internal::Value) -> Self {
        Var {
            id: value.id,
            index: value.index.map(|idx| idx.0),
        }
    }
}

impl From<internal::TypedArg> for Var {
    fn from(arg: internal::TypedArg) -> Self {
        Var {
            id: arg.id,
            index: None,
        }
    }
}

impl From<internal::OpResult> for Var {
    fn from(op_result: internal::OpResult) -> Self {
        Var {
            id: op_result.id,
            index: op_result.index.map(|idx| idx.0),
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
            defs: op.result.into_iter().map_into().collect(),
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
