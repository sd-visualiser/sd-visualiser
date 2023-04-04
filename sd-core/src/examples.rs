use crate::{
    language::grammar::ActiveOp,
    monoidal::{self, MonoidalGraph, MonoidalOp, Slice, ID},
};

pub fn copy() -> MonoidalGraph {
    use MonoidalOp::*;

    MonoidalGraph {
        inputs: 2,
        slices: vec![
            Slice {
                ops: vec![Copy { copies: 2 }, ID],
            },
            Slice {
                ops: vec![Copy { copies: 2 }, ID, ID],
            },
        ],
    }
}

pub fn thunk() -> MonoidalGraph {
    use MonoidalOp::*;

    let plus = MonoidalGraph {
        inputs: 2,
        slices: vec![Slice {
            ops: vec![Operation {
                inputs: 2,
                op_name: monoidal::Operation::Active(ActiveOp::Plus(())),
            }],
        }],
    };

    MonoidalGraph {
        inputs: 1,
        slices: vec![Slice {
            ops: vec![Thunk {
                args: 1,
                body: plus,
            }],
        }],
    }
}
