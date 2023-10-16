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
    node_inputs: usize,
    free_inputs: usize,
    bound_inputs: usize,
    node_outputs: usize,
    free_outputs: usize,
    bound_outputs: usize,
}

impl NodeLike for DummyThunk {
    type Ctx = DummyCtx;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = DummyEdge> + '_> {
        Box::new((0..self.free_inputs + self.node_inputs).map(|_| DummyEdge))
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = DummyEdge> + '_> {
        Box::new((0..self.free_outputs + self.node_outputs).map(|_| DummyEdge))
    }

    fn backlink(&self) -> Option<DummyThunk> {
        panic!("unsupported")
    }

    fn number_of_inputs(&self) -> usize {
        self.free_inputs + self.node_inputs
    }

    fn number_of_outputs(&self) -> usize {
        self.free_outputs + self.node_outputs
    }
}

impl Graph for DummyThunk {
    type Ctx = DummyCtx;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = DummyEdge> + '_> {
        Box::new((0..self.free_inputs).map(|_| DummyEdge))
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = DummyEdge> + '_> {
        Box::new((0..self.bound_inputs).map(|_| DummyEdge))
    }

    fn free_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = DummyEdge> + '_> {
        Box::new((0..self.free_outputs).map(|_| DummyEdge))
    }

    fn bound_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = DummyEdge> + '_> {
        Box::new((0..self.bound_outputs).map(|_| DummyEdge))
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<DummyCtx>> + '_> {
        panic!("unsupported")
    }

    fn graph_backlink(&self) -> Option<DummyThunk> {
        panic!("unsupported")
    }

    fn number_of_free_graph_inputs(&self) -> usize {
        self.free_inputs
    }

    fn number_of_bound_graph_inputs(&self) -> usize {
        self.bound_inputs
    }

    fn number_of_free_graph_outputs(&self) -> usize {
        self.free_outputs
    }

    fn number_of_bound_graph_outputs(&self) -> usize {
        self.bound_outputs
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
        free_inputs: vec![DummyEdge],
        bound_inputs: vec![DummyEdge],
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
                    addr: DummyThunk {
                        node_inputs: 0,
                        free_inputs: 1,
                        bound_inputs: 1,
                        node_outputs: 0,
                        free_outputs: 0,
                        bound_outputs: 1,
                    },
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
