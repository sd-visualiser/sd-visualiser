use crate::language::grammar::{ActiveOp, PassiveOp};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Slice {
    pub ops: Vec<MonoidalOp>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MonoidalGraph {
    pub inputs: usize,
    pub slices: Vec<Slice>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Operation {
    Active(ActiveOp),
    Passive(PassiveOp),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MonoidalOp {
    Copy { copies: usize },
    // Tuple {
    //     inputs: usize,
    // },
    // Untuple {
    //     outputs: usize,
    // },
    Operation { inputs: usize, op_name: Operation },
    Thunk { args: usize, body: MonoidalGraph },
    Swap,
}

impl MonoidalOp {
    /// Returns number of inputs of an operation
    pub fn number_of_inputs(&self) -> usize {
        match self {
            Self::Copy { .. } => 1,
            Self::Operation { inputs, .. } => *inputs,
            Self::Thunk { args, body } => body.inputs - args,
            Self::Swap => 2,
        }
    }

    /// Returns number of outputs of an operation
    pub fn number_of_outputs(&self) -> usize {
        match self {
            MonoidalOp::Copy { copies } => *copies,
            // MonoidalOp::Tuple { .. } => 1,
            // MonoidalOp::Untuple { outputs } => *outputs,
            MonoidalOp::Operation { .. } => 1,
            MonoidalOp::Thunk { .. } => 1,
            MonoidalOp::Swap => 2,
        }
    }
}

pub const ID: MonoidalOp = MonoidalOp::Copy { copies: 1 };

pub const DELETE: MonoidalOp = MonoidalOp::Copy { copies: 0 };
