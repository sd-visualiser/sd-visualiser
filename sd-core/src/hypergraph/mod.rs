use std::{
    fmt::Debug,
    sync::{Arc, Weak},
};

use by_address::ByThinAddress;
use derivative::Derivative;

pub mod builder;
pub mod cut;
pub mod generic;
mod internal;
pub mod petgraph;
pub mod reachability;
pub mod subgraph;
pub mod traits;
pub mod utils;
mod weakbyaddress;

use self::{
    generic::Ctx,
    internal::{
        InPortInternal, NodeInternal, OperationInternal, OutPortInternal, ThunkInternal,
        WeakNodeInternal,
    },
    traits::{EdgeLike, Graph, NodeLike, WithWeight},
    weakbyaddress::WeakByAddress,
};

pub trait Weight {
    type EdgeWeight: Clone + Debug + Send + Sync;
    type OperationWeight: Clone + Debug + Send + Sync;
    type ThunkWeight: Clone + Debug + Send + Sync;
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct Edge<W: Weight>(ByThinAddress<Arc<OutPortInternal<W>>>);

impl<W: Weight> Debug for Edge<W> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Edge")
            .field("weight", &self.weight())
            .field("ptr", &Arc::as_ptr(&self.0))
            .finish()
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = ""),
    Debug(bound = ""),
    Default(bound = "")
)]
pub struct Hypergraph<W: Weight> {
    nodes: Vec<NodeInternal<W>>,
    graph_inputs: Vec<ByThinAddress<Arc<OutPortInternal<W>>>>,
    graph_outputs: Vec<ByThinAddress<Arc<InPortInternal<W>>>>,
}

impl<W: Weight> Ctx for Hypergraph<W> {
    type Edge = Edge<W>;
    type Thunk = Thunk<W>;
    type Operation = Operation<W>;
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct Operation<W: Weight>(ByThinAddress<Arc<OperationInternal<W>>>);

impl<W: Weight> Debug for Operation<W> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Operation")
            .field("weight", &self.weight())
            .field("inputs", &self.inputs().collect::<Vec<_>>())
            .field("outputs", &self.outputs().collect::<Vec<_>>())
            .finish()
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct Thunk<W: Weight>(ByThinAddress<Arc<ThunkInternal<W>>>);

impl<W: Weight> Debug for Thunk<W> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Thunk")
            .field("free_inputs", &self.free_graph_inputs().collect::<Vec<_>>())
            .field(
                "bound_inputs",
                &self.bound_graph_inputs().collect::<Vec<_>>(),
            )
            .field("nodes", &self.nodes().collect::<Vec<_>>())
            .field(
                "outputs",
                &self.graph_outputs().zip(self.outputs()).collect::<Vec<_>>(),
            )
            .finish()
    }
}

pub type Node<W> = generic::Node<Hypergraph<W>>;

impl<W: Weight> WeakNodeInternal<W> {
    pub(super) fn unwrap_node(&self) -> Node<W> {
        match self {
            WeakNodeInternal::Operation(op_weak) => {
                Node::Operation(Operation(ByThinAddress(op_weak.upgrade().unwrap())))
            }
            WeakNodeInternal::Thunk(thunk_weak) => {
                Node::Thunk(Thunk(ByThinAddress(thunk_weak.upgrade().unwrap())))
            }
        }
    }
}

impl<W: Weight> WithWeight for Edge<W> {
    type Weight = W::EdgeWeight;

    fn weight(&self) -> Self::Weight {
        self.0.weight.clone()
    }
}

impl<W: Weight> EdgeLike for Edge<W> {
    type Ctx = Hypergraph<W>;

    fn source(&self) -> Option<Node<W>> {
        self.0.node.as_ref().map(WeakNodeInternal::unwrap_node)
    }

    fn targets(&self) -> Box<dyn DoubleEndedIterator<Item = Option<Node<W>>> + '_> {
        Box::new(
            self.0
                .links
                .try_read()
                .expect("Lock unexpectedly taken")
                .iter()
                .map(|WeakByAddress(in_port)| {
                    let in_port = in_port
                        .upgrade()
                        .expect("got dangling reference to in_port");
                    in_port.node.as_ref().map(WeakNodeInternal::unwrap_node)
                })
                .collect::<Vec<_>>()
                .into_iter(),
        )
    }
}

impl<W: Weight> Graph for Hypergraph<W> {
    type Ctx = Hypergraph<W>;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<W>> + '_> {
        Box::new(self.graph_inputs.iter().cloned().map(Edge))
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<W>> + '_> {
        Box::new(std::iter::empty())
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<W>> + '_> {
        Box::new(
            self.graph_outputs
                .iter()
                .cloned()
                .map(|in_port| Edge(ByThinAddress(in_port.link()))),
        )
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<W>> + '_> {
        Box::new(self.nodes.iter().cloned().map(|node| match node {
            NodeInternal::Operation(operation) => Node::Operation(Operation(operation)),
            NodeInternal::Thunk(thunk) => Node::Thunk(Thunk(thunk)),
        }))
    }

    fn graph_backlink(&self) -> Option<Thunk<W>> {
        None
    }
}

impl<W: Weight> Graph for Thunk<W> {
    type Ctx = Hypergraph<W>;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<W>> + '_> {
        Box::new(
            self.0
                .free_inputs
                .get()
                .expect("Could not lock")
                .iter()
                .cloned()
                .map(Edge),
        )
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<W>> + '_> {
        Box::new(
            self.0
                .bound_inputs
                .iter()
                .map(|out_port| Edge(ByThinAddress(out_port.clone()))),
        )
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<W>> + '_> {
        Box::new(
            self.0
                .inner_outputs
                .iter()
                .map(|in_port| Edge(ByThinAddress(in_port.link()))),
        )
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<W>> + '_> {
        Box::new(
            self.0
                .nodes
                .try_read()
                .expect("nodes lock unexpectedly taken")
                .iter()
                .cloned()
                .map(|node| match node {
                    NodeInternal::Operation(operation) => Node::Operation(Operation(operation)),
                    NodeInternal::Thunk(thunk) => Node::Thunk(Thunk(thunk)),
                })
                .collect::<Vec<_>>()
                .into_iter(),
        )
    }

    fn graph_backlink(&self) -> Option<Thunk<W>> {
        Some(self.clone())
    }
}

impl<W: Weight> WithWeight for Operation<W> {
    type Weight = W::OperationWeight;

    fn weight(&self) -> Self::Weight {
        self.0.weight.clone()
    }
}

impl<W: Weight> NodeLike for Operation<W> {
    type Ctx = Hypergraph<W>;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<W>> + '_> {
        Box::new(
            self.0
                .inputs
                .iter()
                .map(|in_port| Edge(ByThinAddress(in_port.link()))),
        )
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<W>> + '_> {
        Box::new(
            self.0
                .outputs
                .iter()
                .map(|out_port| Edge(ByThinAddress(out_port.clone()))),
        )
    }

    fn backlink(&self) -> Option<Thunk<W>> {
        self.0
            .backlink
            .as_ref()
            .and_then(Weak::upgrade)
            .map(|thunk_internal| Thunk(ByThinAddress(thunk_internal)))
    }

    #[must_use]
    fn number_of_inputs(&self) -> usize {
        self.0.inputs.len()
    }

    #[must_use]
    fn number_of_outputs(&self) -> usize {
        self.0.outputs.len()
    }
}

impl<W: Weight> NodeLike for Thunk<W> {
    type Ctx = Hypergraph<W>;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<W>> + '_> {
        Box::new(
            self.0
                .free_inputs
                .get()
                .expect("Failed to unlock")
                .iter()
                .cloned()
                .map(Edge),
        )
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<W>> + '_> {
        Box::new(
            self.0
                .outer_outputs
                .iter()
                .map(|out_port| Edge(ByThinAddress(out_port.clone()))),
        )
    }

    fn backlink(&self) -> Option<Thunk<W>> {
        self.0
            .backlink
            .as_ref()
            .and_then(Weak::upgrade)
            .map(|thunk_internal| Thunk(ByThinAddress(thunk_internal)))
    }

    #[must_use]
    fn number_of_inputs(&self) -> usize {
        self.0.free_inputs.get().expect("Failed to unlock").len()
    }

    #[must_use]
    fn number_of_outputs(&self) -> usize {
        self.0.outer_outputs.len()
    }
}

impl<W: Weight> WithWeight for Thunk<W> {
    type Weight = W::ThunkWeight;

    fn weight(&self) -> Self::Weight {
        self.0.weight.clone()
    }
}
