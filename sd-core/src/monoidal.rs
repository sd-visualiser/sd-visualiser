#[derive(Clone, Debug)]
pub struct Slice {
    pub ops: Vec<MonoidalOp>,
}

#[derive(Clone, Debug)]
pub struct MonoidalGraph {
    pub inputs: usize,
    pub slices: Vec<Slice>,
}

#[derive(Clone, Debug)]
pub enum MonoidalOp {
    Id,
    Copy { copies: usize },
    Delete,
    // Tuple {
    //     inputs: usize,
    // },
    // Untuple {
    //     outputs: usize,
    // },
    Operation {
        inputs: usize,
        op_name: (),
    },
    Thunk {
        args: usize,
        body: MonoidalGraph,
    },
    Swap,
}

impl MonoidalOp {
    /// Returns number of inputs of an operation
    pub fn number_of_inputs(&self) -> usize {
        match self {
            Self::Id => 1,
            Self::Copy { .. } => 1,
            Self::Delete => 1,
            Self::Operation { inputs, .. } => *inputs,
            Self::Thunk { args, body } => body.inputs - args,
            Self::Swap => 2,
        }
    }

    /// Returns number of outputs of an operation
    pub fn number_of_outputs(&self) -> usize {
        match self {
            MonoidalOp::Id => 1,
            MonoidalOp::Copy { copies } => *copies,
            MonoidalOp::Delete => 0,
            // MonoidalOp::Tuple { .. } => 1,
            // MonoidalOp::Untuple { outputs } => *outputs,
            MonoidalOp::Operation {  .. } => 1,
            MonoidalOp::Thunk { .. } => 1,
            MonoidalOp::Swap => 2,
        }
    }
}
