use crate::{
    hypergraph::{
        generic::{Ctx, Node},
        traits::{EdgeLike, Graph, NodeLike, WithWeight},
    },
    language::spartan::Op,
    monoidal::{
        graph::{MonoidalGraph, MonoidalOp},
        Slice,
    },
};

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct SyntaxEdge;

impl EdgeLike for SyntaxEdge {
    type Ctx = Syntax;

    fn source(&self) -> Option<Node<Syntax>> {
        panic!("unsupported")
    }

    fn targets(&self) -> Box<dyn DoubleEndedIterator<Item = Option<Node<Syntax>>> + '_> {
        panic!("unsupported")
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct SyntaxOp {
    op: Op,
    inputs: usize,
}

impl NodeLike for SyntaxOp {
    type Ctx = Syntax;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = SyntaxEdge> + '_> {
        panic!("unsupported")
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = SyntaxEdge> + '_> {
        panic!("unsupported")
    }

    fn backlink(&self) -> Option<SyntaxThunk> {
        panic!("unsupported")
    }

    fn number_of_inputs(&self) -> usize {
        self.inputs
    }

    fn number_of_outputs(&self) -> usize {
        1
    }
}

impl WithWeight for SyntaxOp {
    type Weight = Op;

    fn weight(&self) -> &Self::Weight {
        &self.op
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct SyntaxThunk {
    inputs: usize,
}

impl NodeLike for SyntaxThunk {
    type Ctx = Syntax;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = SyntaxEdge> + '_> {
        panic!("unsupported")
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = SyntaxEdge> + '_> {
        panic!("unsupported")
    }

    fn backlink(&self) -> Option<SyntaxThunk> {
        panic!("unsupported")
    }

    fn number_of_inputs(&self) -> usize {
        self.inputs
    }

    fn number_of_outputs(&self) -> usize {
        1
    }
}

impl Graph for SyntaxThunk {
    type Ctx = Syntax;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = SyntaxEdge> + '_> {
        panic!("unsupported")
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = SyntaxEdge> + '_> {
        panic!("unsupported")
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = SyntaxEdge> + '_> {
        panic!("unsupported")
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<Syntax>> + '_> {
        panic!("unsupported")
    }

    fn graph_backlink(&self) -> Option<SyntaxThunk> {
        panic!("unsupported")
    }
}

pub struct Syntax;

impl Ctx for Syntax {
    type Edge = SyntaxEdge;
    type Thunk = SyntaxThunk;
    type Operation = SyntaxOp;
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
        outputs: vec![SyntaxEdge],
    }
}

#[must_use]
pub fn copy() -> MonoidalGraph<Syntax> {
    MonoidalGraph {
        free_inputs: vec![],
        bound_inputs: vec![SyntaxEdge],
        slices: vec![
            Slice {
                ops: vec![MonoidalOp::Copy {
                    addr: SyntaxEdge,
                    copies: 2,
                }],
            },
            Slice {
                ops: vec![
                    MonoidalOp::Copy {
                        addr: SyntaxEdge,
                        copies: 2,
                    },
                    MonoidalOp::Copy {
                        addr: SyntaxEdge,
                        copies: 1,
                    },
                ],
            },
        ],
        outputs: vec![SyntaxEdge; 3],
    }
}

#[must_use]
pub fn thunk() -> MonoidalGraph<Syntax> {
    let plus = MonoidalGraph {
        free_inputs: vec![],
        bound_inputs: vec![SyntaxEdge; 2],
        slices: vec![Slice {
            ops: vec![MonoidalOp::Operation {
                addr: SyntaxOp {
                    op: Op::Plus,
                    inputs: 2,
                },
            }],
        }],
        outputs: vec![SyntaxEdge],
    };

    MonoidalGraph {
        free_inputs: vec![],
        bound_inputs: vec![SyntaxEdge; 3],
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
        outputs: vec![SyntaxEdge; 2],
    }
}
