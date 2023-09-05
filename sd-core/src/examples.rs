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

pub struct DummyCtx;

impl Ctx for DummyCtx {
    type Edge = DummyEdge;
    type Operation = DummyOperation;
    type Thunk = DummyThunk;
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct DummyEdge;

impl EdgeLike for DummyEdge {
    type Ctx = DummyCtx;

    fn source(&self) -> Option<Node<DummyCtx>> {
        panic!("unsupported")
    }

    fn targets(&self) -> Box<dyn DoubleEndedIterator<Item = Option<Node<DummyCtx>>> + '_> {
        panic!("unsupported")
    }
}

impl WithWeight for DummyEdge {
    type Weight = ();

    fn weight(&self) -> Self::Weight {}
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct DummyOperation {
    op: Op,
    inputs: usize,
}

impl NodeLike for DummyOperation {
    type Ctx = DummyCtx;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = DummyEdge> + '_> {
        Box::new(vec![DummyEdge; self.inputs].into_iter())
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = DummyEdge> + '_> {
        Box::new(std::iter::once(DummyEdge))
    }

    fn backlink(&self) -> Option<DummyThunk> {
        panic!("unsupported")
    }

    fn number_of_inputs(&self) -> usize {
        self.inputs
    }

    fn number_of_outputs(&self) -> usize {
        1
    }
}

impl WithWeight for DummyOperation {
    type Weight = Op;

    fn weight(&self) -> Self::Weight {
        self.op
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct DummyThunk {
    inputs: usize,
}

impl NodeLike for DummyThunk {
    type Ctx = DummyCtx;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = DummyEdge> + '_> {
        panic!("unsupported")
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = DummyEdge> + '_> {
        panic!("unsupported")
    }

    fn backlink(&self) -> Option<DummyThunk> {
        panic!("unsupported")
    }

    fn number_of_inputs(&self) -> usize {
        self.inputs
    }

    fn number_of_outputs(&self) -> usize {
        1
    }
}

impl Graph for DummyThunk {
    type Ctx = DummyCtx;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = DummyEdge> + '_> {
        panic!("unsupported")
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = DummyEdge> + '_> {
        panic!("unsupported")
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = DummyEdge> + '_> {
        panic!("unsupported")
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<DummyCtx>> + '_> {
        panic!("unsupported")
    }

    fn graph_backlink(&self) -> Option<DummyThunk> {
        panic!("unsupported")
    }
}

impl WithWeight for DummyThunk {
    type Weight = ();

    fn weight(&self) -> Self::Weight {}
}

/// Corrresponds to the program `bind x = 1 in x`.
#[must_use]
pub fn int() -> MonoidalGraph<DummyCtx> {
    MonoidalGraph {
        free_inputs: vec![],
        bound_inputs: vec![],
        slices: vec![Slice {
            ops: vec![MonoidalOp::Operation {
                addr: DummyOperation {
                    op: Op::Number(1),
                    inputs: 0,
                },
            }],
        }],
        outputs: vec![DummyEdge],
    }
}

#[must_use]
pub fn copy() -> MonoidalGraph<DummyCtx> {
    MonoidalGraph {
        free_inputs: vec![],
        bound_inputs: vec![DummyEdge],
        slices: vec![
            Slice {
                ops: vec![MonoidalOp::Copy {
                    addr: DummyEdge,
                    copies: 2,
                }],
            },
            Slice {
                ops: vec![
                    MonoidalOp::Copy {
                        addr: DummyEdge,
                        copies: 2,
                    },
                    MonoidalOp::Copy {
                        addr: DummyEdge,
                        copies: 1,
                    },
                ],
            },
        ],
        outputs: vec![DummyEdge; 3],
    }
}

#[must_use]
pub fn thunk() -> MonoidalGraph<DummyCtx> {
    let plus = MonoidalGraph {
        free_inputs: vec![],
        bound_inputs: vec![DummyEdge; 2],
        slices: vec![Slice {
            ops: vec![MonoidalOp::Operation {
                addr: DummyOperation {
                    op: Op::Plus,
                    inputs: 2,
                },
            }],
        }],
        outputs: vec![DummyEdge],
    };

    MonoidalGraph {
        free_inputs: vec![],
        bound_inputs: vec![DummyEdge; 3],
        slices: vec![Slice {
            ops: vec![
                MonoidalOp::Thunk {
                    addr: DummyThunk { inputs: 1 },
                    body: plus,
                },
                MonoidalOp::Operation {
                    addr: DummyOperation {
                        op: Op::Plus,
                        inputs: 2,
                    },
                },
            ],
        }],
        outputs: vec![DummyEdge; 2],
    }
}
