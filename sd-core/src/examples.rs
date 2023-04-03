use crate::monoidal::{MonoidalGraph, MonoidalOp, Slice, ID};

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
