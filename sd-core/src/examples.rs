use delegate::delegate;

use crate::{
    common::Addr,
    hypergraph::traits::{EdgeLike, Graph, NodeLike, WithWeight},
    language::spartan::Op,
    monoidal::{
        graph::{MonoidalGraph, MonoidalOp},
        Slice,
    },
};

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct SyntaxEdge;

impl EdgeLike for SyntaxEdge {
    type T = Syntax;

    fn source(&self) -> Option<<Self::T as Addr>::Node> {
        panic!("unsupported")
    }

    fn targets(&self) -> Box<dyn DoubleEndedIterator<Item = Option<<Self::T as Addr>::Node>> + '_> {
        panic!("unsupported")
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct SyntaxOp {
    op: Op,
    inputs: usize,
}

impl NodeLike for SyntaxOp {
    type T = Syntax;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_> {
        panic!("unsupported")
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_> {
        panic!("unsupported")
    }

    fn backlink(&self) -> Option<<Self::T as Addr>::Thunk> {
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
    type T = Syntax;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_> {
        panic!("unsupported")
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_> {
        panic!("unsupported")
    }

    fn backlink(&self) -> Option<<Self::T as Addr>::Thunk> {
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
    type T = Syntax;

    fn free_graph_inputs(
        &self,
    ) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_> {
        panic!("unsupported")
    }

    fn bound_graph_inputs(
        &self,
    ) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_> {
        panic!("unsupported")
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_> {
        panic!("unsupported")
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Node> + '_> {
        panic!("unsupported")
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum SyntaxNode {
    Operation(SyntaxOp),
    Thunk(SyntaxThunk),
}

impl NodeLike for SyntaxNode {
    type T = Syntax;

    delegate! {
        to match self {
            SyntaxNode::Operation(op) => op,
            SyntaxNode::Thunk(thunk) => thunk,
        } {
            #[allow(clippy::inline_always)]
            fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_>;
            #[allow(clippy::inline_always)]
            fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_>;
            #[allow(clippy::inline_always)]
            fn backlink(&self) -> Option<<Self::T as Addr>::Thunk>;
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

impl TryFrom<SyntaxNode> for SyntaxOp {
    type Error = ();

    fn try_from(node: SyntaxNode) -> Result<Self, Self::Error> {
        match node {
            SyntaxNode::Operation(op) => Ok(op),
            SyntaxNode::Thunk(_) => Err(()),
        }
    }
}

impl TryFrom<SyntaxNode> for SyntaxThunk {
    type Error = ();

    fn try_from(node: SyntaxNode) -> Result<Self, Self::Error> {
        match node {
            SyntaxNode::Operation(_) => Err(()),
            SyntaxNode::Thunk(thunk) => Ok(thunk),
        }
    }
}

pub struct Syntax;

impl Addr for Syntax {
    type Edge = SyntaxEdge;
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
