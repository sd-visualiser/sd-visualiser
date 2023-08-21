use std::{
    fmt::Debug,
    sync::{Arc, Weak},
};

use by_address::ByThinAddress;
use derivative::Derivative;

use crate::common::{Addr, Matchable};

pub mod builder;
pub mod generic;
mod internal;
pub mod petgraph;
pub mod reachability;
pub mod subgraph;
pub mod traits;
pub mod utils;
mod weakbyaddress;

use self::{
    internal::{
        InPortInternal, NodeInternal, OperationInternal, OutPortInternal, ThunkInternal,
        WeakNodeInternal,
    },
    traits::{EdgeLike, Graph, NodeLike, WithWeight},
    weakbyaddress::WeakByAddress,
};

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct Edge<V, E>(ByThinAddress<Arc<OutPortInternal<V, E>>>);

impl<V, E: Debug> Debug for Edge<V, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Edge")
            .field("weight", self.weight())
            .field("ptr", &Arc::as_ptr(&self.0))
            .finish()
    }
}

#[derive(Debug, Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = ""),
    Default(bound = "")
)]
pub struct Hypergraph<V, E> {
    nodes: Vec<NodeInternal<V, E>>,
    graph_inputs: Vec<ByThinAddress<Arc<OutPortInternal<V, E>>>>,
    graph_outputs: Vec<ByThinAddress<Arc<InPortInternal<V, E>>>>,
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct Operation<V, E>(ByThinAddress<Arc<OperationInternal<V, E>>>);

impl<V: Debug, E: Debug> Debug for Operation<V, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Operation")
            .field("weight", self.weight())
            .field("inputs", &self.inputs().collect::<Vec<_>>())
            .field("outputs", &self.outputs().collect::<Vec<_>>())
            .finish()
    }
}

impl<V, E> Matchable for Operation<V, E>
where
    E: Matchable,
{
    fn is_match(&self, variable: &str) -> bool {
        self.outputs()
            .next()
            .map(|edge| edge.weight().is_match(variable))
            .unwrap_or_default()
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct Thunk<V, E>(ByThinAddress<Arc<ThunkInternal<V, E>>>);

impl<V: Debug, E: Debug> Debug for Thunk<V, E> {
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

impl<V, E> Matchable for Thunk<V, E>
where
    E: Matchable,
{
    fn is_match(&self, variable: &str) -> bool {
        self.outputs()
            .next()
            .map(|edge| edge.weight().is_match(variable))
            .unwrap_or_default()
    }
}

pub type Node<V, E> = generic::Node<Hypergraph<V, E>>;

impl<V, E> WeakNodeInternal<V, E> {
    pub(super) fn unwrap_node(&self) -> Node<V, E> {
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

impl<V, E> WithWeight for Edge<V, E> {
    type Weight = E;

    fn weight(&self) -> &Self::Weight {
        &self.0.weight
    }
}

impl<V, E> EdgeLike for Edge<V, E> {
    type T = Hypergraph<V, E>;

    fn source(&self) -> Option<Node<V, E>> {
        self.0.node.as_ref().map(WeakNodeInternal::unwrap_node)
    }

    fn targets(&self) -> Box<dyn DoubleEndedIterator<Item = Option<Node<V, E>>> + '_> {
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

impl<V, E> Graph for Hypergraph<V, E> {
    type T = Hypergraph<V, E>;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(self.graph_inputs.iter().cloned().map(Edge))
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(std::iter::empty())
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(
            self.graph_outputs
                .iter()
                .cloned()
                .map(|in_port| Edge(ByThinAddress(in_port.link()))),
        )
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<V, E>> + '_> {
        Box::new(self.nodes.iter().cloned().map(|node| match node {
            NodeInternal::Operation(operation) => Node::Operation(Operation(operation)),
            NodeInternal::Thunk(thunk) => Node::Thunk(Thunk(thunk)),
        }))
    }

    fn graph_backlink(&self) -> Option<<Self::T as Addr>::Thunk> {
        None
    }
}

impl<V, E> Graph for Thunk<V, E> {
    type T = Hypergraph<V, E>;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
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

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(
            self.0
                .bound_inputs
                .iter()
                .map(|out_port| Edge(ByThinAddress(out_port.clone()))),
        )
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(
            self.0
                .inner_outputs
                .iter()
                .map(|in_port| Edge(ByThinAddress(in_port.link()))),
        )
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<V, E>> + '_> {
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

    fn graph_backlink(&self) -> Option<<Self::T as Addr>::Thunk> {
        Some(self.clone())
    }
}

impl<V, E> WithWeight for Operation<V, E> {
    type Weight = V;

    fn weight(&self) -> &Self::Weight {
        &self.0.weight
    }
}

impl<V, E> NodeLike for Operation<V, E> {
    type T = Hypergraph<V, E>;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(
            self.0
                .inputs
                .iter()
                .map(|in_port| Edge(ByThinAddress(in_port.link()))),
        )
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(
            self.0
                .outputs
                .iter()
                .map(|out_port| Edge(ByThinAddress(out_port.clone()))),
        )
    }

    fn backlink(&self) -> Option<Thunk<V, E>> {
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

impl<V, E> NodeLike for Thunk<V, E> {
    type T = Hypergraph<V, E>;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
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

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(
            self.0
                .outer_outputs
                .iter()
                .map(|out_port| Edge(ByThinAddress(out_port.clone()))),
        )
    }

    fn backlink(&self) -> Option<Thunk<V, E>> {
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
