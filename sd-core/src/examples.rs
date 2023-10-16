use crate::{
    hypergraph::{
        generic::{Ctx, Endpoint, Node},
        traits::{EdgeLike, Graph, Keyable, NodeLike, WithWeight},
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

    fn source(&self) -> Endpoint<DummyCtx> {
        panic!("unsupported")
    }

    fn targets(&self) -> Box<dyn DoubleEndedIterator<Item = Endpoint<DummyCtx>> + '_> {
        panic!("unsupported")
    }
}

impl Keyable for DummyEdge {
    type Key = Self;

    fn key(&self) -> Self::Key {
        self.clone()
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
    outputs: usize,
}

impl NodeLike for DummyOperation {
    type Ctx = DummyCtx;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = DummyEdge> + '_> {
        Box::new((0..self.inputs).map(|_| DummyEdge))
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = DummyEdge> + '_> {
        Box::new((0..self.outputs).map(|_| DummyEdge))
    }

    fn backlink(&self) -> Option<DummyThunk> {
        panic!("unsupported")
    }

    fn number_of_inputs(&self) -> usize {
        self.inputs
    }

    fn number_of_outputs(&self) -> usize {
        self.outputs
    }
}

impl Keyable for DummyOperation {
    type Key = Self;

    fn key(&self) -> Self::Key {
        self.clone()
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
        Box::new((0..self.inputs).map(|_| DummyEdge))
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

impl Graph for DummyThunk {
    type Ctx = DummyCtx;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = DummyEdge> + '_> {
        panic!("unsupported")
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = DummyEdge> + '_> {
        panic!("unsupported")
    }

    fn free_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = DummyEdge> + '_> {
        panic!("unsupported")
    }

    fn bound_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = DummyEdge> + '_> {
        panic!("unsupported")
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<DummyCtx>> + '_> {
        panic!("unsupported")
    }

    fn graph_backlink(&self) -> Option<DummyThunk> {
        panic!("unsupported")
    }

    fn number_of_free_graph_inputs(&self) -> usize {
        panic!("unsupported")
    }

    fn number_of_bound_graph_inputs(&self) -> usize {
        panic!("unsupported")
    }

    fn number_of_free_graph_outputs(&self) -> usize {
        panic!("unsupported")
    }

    fn number_of_bound_graph_outputs(&self) -> usize {
        panic!("unsupported")
    }
}

impl Keyable for DummyThunk {
    type Key = Self;

    fn key(&self) -> Self::Key {
        self.clone()
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
                    outputs: 1,
                },
            }],
        }],
        free_outputs: vec![],
        bound_outputs: vec![DummyEdge],
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
        free_outputs: vec![],
        bound_outputs: vec![DummyEdge; 3],
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
                    outputs: 1,
                },
            }],
        }],
        free_outputs: vec![],
        bound_outputs: vec![DummyEdge],
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
                        outputs: 1,
                    },
                },
            ],
        }],
        free_outputs: vec![],
        bound_outputs: vec![DummyEdge; 2],
    }
}
