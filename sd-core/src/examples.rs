use crate::monoidal::{MonoidalGraph, MonoidalOp, Slice};

pub fn copy() -> MonoidalGraph {
    use MonoidalOp::*;

    MonoidalGraph {
        inputs: 2,
        slices: vec![
            Slice {
                ops: vec![Copy { copies: 2 }, Id],
            },
            Slice {
                ops: vec![Copy { copies: 2 }, Id, Id],
            },
        ],
    }
}
