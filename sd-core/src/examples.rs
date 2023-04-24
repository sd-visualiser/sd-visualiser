use crate::{
    graph::Op,
    language::{ActiveOp, PassiveOp},
    monoidal::{MonoidalGraph, MonoidalOp, Slice},
};

/// Corrresponds to the program `bind x = 1() in x`.
pub fn int() -> MonoidalGraph<Op> {
    use MonoidalOp::*;

    MonoidalGraph {
        inputs: 0,
        slices: vec![Slice {
            ops: vec![(
                Operation {
                    inputs: 0,
                    op_name: PassiveOp::Int(1).into(),
                    selected: false,
                },
                None,
            )],
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
                ops: vec![(Copy { copies: 2 }, None)],
            },
            Slice {
                ops: vec![(Copy { copies: 2 }, None), (MonoidalOp::ID, None)],
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
            ops: vec![(
                Operation {
                    inputs: 2,
                    op_name: ActiveOp::Plus.into(),
                    selected: false,
                },
                None,
            )],
        }],
        prefix: vec![],
    };

    MonoidalGraph {
        inputs: 3,
        slices: vec![Slice {
            ops: vec![
                (
                    Thunk {
                        args: 1,
                        body: plus,
                        expanded: true,
                    },
                    None,
                ),
                (
                    Operation {
                        inputs: 2,
                        op_name: ActiveOp::Plus.into(),
                        selected: false,
                    },
                    None,
                ),
            ],
        }],
        prefix: vec![],
    }
}
