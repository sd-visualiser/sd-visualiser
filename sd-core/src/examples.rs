use crate::{
    graph::Op,
    hypergraph::NodeIndex,
    language::{ActiveOp, PassiveOp},
    monoidal::{MonoidalGraph, MonoidalOp, Slice},
};

/// Corrresponds to the program `bind x = 1() in x`.
pub fn int() -> MonoidalGraph<Op> {
    use MonoidalOp::*;

    MonoidalGraph {
        inputs: 0,
        slices: vec![Slice {
            ops: vec![Operation {
                addr: NodeIndex(0),
                inputs: 0,
                op_name: PassiveOp::Int(1).into(),
                selected: false,
            }],
        }],
        prefix: vec![],
    }
}

pub fn copy() -> MonoidalGraph<Op> {
    use MonoidalOp::*;

    MonoidalGraph {
        inputs: 1,
        slices: vec![
            Slice {
                ops: vec![Copy { copies: 2 }],
            },
            Slice {
                ops: vec![Copy { copies: 2 }, MonoidalOp::ID],
            },
        ],
        prefix: vec![],
    }
}

pub fn thunk() -> MonoidalGraph<Op> {
    use MonoidalOp::*;

    let plus = MonoidalGraph {
        inputs: 2,
        slices: vec![Slice {
            ops: vec![Operation {
                addr: NodeIndex(0),
                inputs: 2,
                op_name: ActiveOp::Plus.into(),
                selected: false,
            }],
        }],
        prefix: vec![NodeIndex(0)],
    };

    MonoidalGraph {
        inputs: 3,
        slices: vec![Slice {
            ops: vec![
                Thunk {
                    addr: NodeIndex(0),
                    args: 1,
                    body: plus,
                    expanded: true,
                },
                Operation {
                    addr: NodeIndex(1),
                    inputs: 2,
                    op_name: ActiveOp::Plus.into(),
                    selected: false,
                },
            ],
        }],
        prefix: vec![],
    }
}
