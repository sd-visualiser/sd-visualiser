use delegate::delegate;

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

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum SyntaxNode {
    Operation(SyntaxOp),
    Thunk(SyntaxThunk),
}

impl InOut for SyntaxNode {
    delegate! {
        to match self {
            SyntaxNode::Operation(op) => op,
            SyntaxNode::Thunk(thunk) => thunk,
        } {
            #[allow(clippy::inline_always)]
            fn number_of_inputs(&self) -> usize;
            #[allow(clippy::inline_always)]
            fn number_of_outputs(&self) -> usize;
        }
    }
}

impl From<SyntaxOp> for SyntaxNode {
    fn from(value: SyntaxOp) -> Self {
        SyntaxNode::Operation(value)
    }
}

impl From<SyntaxThunk> for SyntaxNode {
    fn from(value: SyntaxThunk) -> Self {
        SyntaxNode::Thunk(value)
    }
}

pub struct Syntax;

impl Addr for Syntax {
    type Edge = ();
    type Thunk = SyntaxThunk;
    type Operation = SyntaxOp;
    type Node = SyntaxNode;
}

/// Corrresponds to the program `bind x = 1 in x`.
#[must_use]
pub fn int() -> MonoidalGraph<Syntax> {
    MonoidalGraph {
        free_inputs: vec![],
        bound_inputs: vec![],
        slices: vec![Slice {
            ops: vec![MonoidalOp::Operation {
                addr: SyntaxOp {
                    op: Op::Number(1),
                    inputs: 0,
                },
            }],
        }],
        outputs: vec![()],
    }
}

#[must_use]
pub fn copy() -> MonoidalGraph<Syntax> {
    MonoidalGraph {
        free_inputs: vec![],
        bound_inputs: vec![()],
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
        free_inputs: vec![],
        bound_inputs: vec![(), ()],
        slices: vec![Slice {
            ops: vec![MonoidalOp::Operation {
                addr: SyntaxOp {
                    op: Op::Plus,
                    inputs: 2,
                },
            }],
        }],
        outputs: vec![()],
    };

    MonoidalGraph {
        free_inputs: vec![],
        bound_inputs: vec![(), (), ()],
        slices: vec![Slice {
            ops: vec![
                MonoidalOp::Thunk {
                    addr: SyntaxThunk { inputs: 1 },
                    body: plus,
                },
                MonoidalOp::Operation {
                    addr: SyntaxOp {
                        op: Op::Plus,
                        inputs: 2,
                    },
                },
            ],
        }],
        outputs: vec![(), ()],
    }
}
