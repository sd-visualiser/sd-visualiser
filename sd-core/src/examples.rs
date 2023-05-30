use ordered_float::NotNaN;

use crate::{
    common::{Addr, InOut, Slice},
    language::spartan::Op,
    monoidal::{MonoidalGraph, MonoidalOp},
};

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct SyntaxOp {
    op: Op,
    inputs: usize,
}

impl InOut for SyntaxOp {
    fn number_of_inputs(&self) -> usize {
        self.inputs
    }

    fn number_of_outputs(&self) -> usize {
        1
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct SyntaxThunk {
    inputs: usize,
}

impl InOut for SyntaxThunk {
    fn number_of_inputs(&self) -> usize {
        self.inputs
    }

    fn number_of_outputs(&self) -> usize {
        1
    }
}

pub struct Syntax;

impl Addr for Syntax {
    type InPort = ();
    type OutPort = ();
    type Thunk = SyntaxThunk;
    type Operation = SyntaxOp;
}

/// Corrresponds to the program `bind x = 1 in x`.
#[must_use]
pub fn int() -> MonoidalGraph<Syntax> {
    MonoidalGraph {
        unordered_inputs: vec![],
        ordered_inputs: vec![],
        slices: vec![Slice {
            ops: vec![MonoidalOp::Operation {
                addr: SyntaxOp {
                    op: Op::Number(NotNaN::new(1.0).unwrap()),
                    inputs: 0,
                },
                selected: false,
            }],
        }],
        outputs: vec![()],
    }
}

#[must_use]
pub fn copy() -> MonoidalGraph<Syntax> {
    MonoidalGraph {
        unordered_inputs: vec![],
        ordered_inputs: vec![()],
        slices: vec![
            Slice {
                ops: vec![MonoidalOp::Copy {
                    addr: (),
                    copies: 2,
                }],
            },
            Slice {
                ops: vec![
                    MonoidalOp::Copy {
                        addr: (),
                        copies: 2,
                    },
                    MonoidalOp::Copy {
                        addr: (),
                        copies: 1,
                    },
                ],
            },
        ],
        outputs: vec![(), (), ()],
    }
}

#[must_use]
pub fn thunk() -> MonoidalGraph<Syntax> {
    let plus = MonoidalGraph {
        unordered_inputs: vec![],
        ordered_inputs: vec![(), ()],
        slices: vec![Slice {
            ops: vec![MonoidalOp::Operation {
                addr: SyntaxOp {
                    op: Op::Plus,
                    inputs: 2,
                },
                selected: false,
            }],
        }],
        outputs: vec![()],
    };

    MonoidalGraph {
        unordered_inputs: vec![],
        ordered_inputs: vec![(), (), ()],
        slices: vec![Slice {
            ops: vec![
                MonoidalOp::Thunk {
                    addr: SyntaxThunk { inputs: 1 },
                    body: plus,
                    expanded: true,
                },
                MonoidalOp::Operation {
                    addr: SyntaxOp {
                        op: Op::Plus,
                        inputs: 2,
                    },
                    selected: false,
                },
            ],
        }],
        outputs: vec![(), ()],
    }
}
