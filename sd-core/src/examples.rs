use crate::monoidal::{MonoidalGraph, MonoidalOp, Slice};

pub fn copy() -> MonoidalGraph {
    use MonoidalOp::*;

    MonoidalGraph {
        inputs: 2,
        slices: vec![
            Slice {
                ops: vec![Copy { copies: 2 }, Copy { copies: 1 }],
            },
            Slice {
                ops: vec![Copy { copies: 2 }, Copy { copies: 1 }, Copy { copies: 1 }],
            },
        ],
    }
}
