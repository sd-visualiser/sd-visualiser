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
    Unit,
    Operation { inputs: usize, op_name: Operation },
    Thunk { args: usize, body: MonoidalGraph },
    Swap,
}

impl MonoidalOp {
    /// Returns number of inputs of an operation
    pub fn number_of_inputs(&self) -> usize {
        match self {
            Self::Copy { .. } => 1,
            Self::Unit => 0,
            Self::Operation { inputs, .. } => *inputs,
            Self::Thunk { args, body } => body.inputs - args,
            Self::Swap => 2,
        }
    }

    /// Returns number of outputs of an operation
    pub fn number_of_outputs(&self) -> usize {
        match self {
            MonoidalOp::Copy { copies } => *copies,
            MonoidalOp::Unit => 1,
            MonoidalOp::Operation { .. } => 1,
            MonoidalOp::Thunk { .. } => 1,
            MonoidalOp::Swap => 2,
        }
    }
}

pub const ID: MonoidalOp = MonoidalOp::Copy { copies: 1 };

pub const DELETE: MonoidalOp = MonoidalOp::Copy { copies: 0 };

// Unfolding

impl MonoidalGraph {
    pub fn unfold(self) -> Self {
        Self {
            inputs: self.inputs,
            slices: self
                .slices
                .into_iter()
                .flat_map(|slice| {
                    // Turn each operation into a list of slices.
                    let sss = slice
                        .ops
                        .into_iter()
                        .map(|op| match op {
                            MonoidalOp::Thunk { args, body } => {
                                // Unfold the body of the thunk and add an extra slice at the start.
                                let mut slices = body.slices;
                                slices.insert(
                                    0,
                                    Slice {
                                        ops: std::iter::repeat(ID)
                                            .take(body.inputs - args)
                                            .chain(std::iter::repeat(MonoidalOp::Unit).take(args))
                                            .collect(),
                                    },
                                );
                                slices
                            }
                            _ => vec![Slice { ops: vec![op] }],
                        })
                        .collect::<Vec<_>>();

                    let max_height = sss.iter().map(|ss| ss.len()).max().unwrap();
                    let mut slices = Vec::with_capacity(max_height);
                    for i in 0..max_height {
                        slices.push(Slice {
                            ops: sss
                                .iter()
                                .flat_map(|ss| {
                                    ss.get(i)
                                        .cloned()
                                        .unwrap_or_else(|| {
                                            let n = ss
                                                .last()
                                                .unwrap()
                                                .ops
                                                .iter()
                                                .map(MonoidalOp::number_of_outputs)
                                                .sum();
                                            Slice { ops: vec![ID; n] }
                                        })
                                        .ops
                                })
                                .collect(),
                        });
                    }
                    slices
                })
                .collect(),
        }
    }
}
