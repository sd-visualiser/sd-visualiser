use crate::{
    language::grammar::ActiveOp,
    monoidal::{MonoidalGraph, MonoidalOp, Slice, ID},
};

pub fn copy() -> MonoidalGraph {
    use MonoidalOp::*;

    MonoidalGraph {
        inputs: 1,
        slices: vec![
            Slice {
                ops: vec![(Copy { copies: 2 }, vec![])],
            },
            Slice {
                ops: vec![(Copy { copies: 2 }, vec![]), ID],
            },
        ],
    }
}

pub fn thunk() -> MonoidalGraph {
    use MonoidalOp::*;

    let plus = MonoidalGraph {
        inputs: 2,
        slices: vec![Slice {
            ops: vec![(
                Operation {
                    inputs: 2,
                    op_name: ActiveOp::Plus(()).into(),
                },
                vec![],
            )],
        }],
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
                    vec![],
                ),
                (
                    Operation {
                        inputs: 2,
                        op_name: ActiveOp::Plus(()).into(),
                    },
                    vec![],
                ),
            ],
        }],
    }
}
