use std::{
    fmt::Debug,
    sync::{Arc, Weak},
};

use by_address::ByThinAddress;
use derivative::Derivative;
use indexmap::IndexMap;

use crate::{
    common::{Addr, InOut, Matchable},
    selection::SelectionMap,
    weak_map::WeakMap,
};

pub mod builder;
mod internal;
pub mod petgraph;
pub mod reachability;
pub mod subgraph;
pub mod traits;
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
            .field(
                "free_vars",
                &self.unbound_graph_inputs().collect::<Vec<_>>(),
            )
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

#[derive(Debug, Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub enum Node<V, E> {
    Operation(Operation<V, E>),
    Thunk(Thunk<V, E>),
}

impl<V, E> From<Operation<V, E>> for Node<V, E> {
    fn from(value: Operation<V, E>) -> Self {
        Node::Operation(value)
    }
}

impl<V, E> From<Thunk<V, E>> for Node<V, E> {
    fn from(value: Thunk<V, E>) -> Self {
        Node::Thunk(value)
    }
}

impl<V, E> TryFrom<Node<V, E>> for Operation<V, E> {
    type Error = ();

    fn try_from(node: Node<V, E>) -> Result<Self, Self::Error> {
        match node {
            Node::Operation(op) => Ok(op),
            Node::Thunk(_) => Err(()),
        }
    }
}

impl<V, E> TryFrom<Node<V, E>> for Thunk<V, E> {
    type Error = ();

    fn try_from(node: Node<V, E>) -> Result<Self, Self::Error> {
        match node {
            Node::Operation(_) => Err(()),
            Node::Thunk(thunk) => Ok(thunk),
        }
    }
}

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
    type T = (V, E);

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

    fn number_of_targets(&self) -> usize {
        self.0
            .links
            .try_read()
            .expect("Lock unexpectedly taken")
            .len()
    }
}

impl<V, E> Graph for Hypergraph<V, E> {
    type T = (V, E);
    fn unbound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(self.graph_inputs.iter().cloned().map(|o| Edge(o)))
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
}

impl<V, E> Graph for Thunk<V, E> {
    type T = (V, E);
    fn unbound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(
            self.0
                .free_variable_edges
                .get()
                .expect("Could not lock")
                .clone()
                .into_iter()
                .map(Edge),
        )
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(self.0.bound_variables.iter().cloned().map(|o| Edge(o)))
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(
            self.0
                .outputs
                .keys()
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
}

impl<V, E> WithWeight for Operation<V, E> {
    type Weight = V;

    fn weight(&self) -> &Self::Weight {
        &self.0.weight
    }
}

impl<V, E> NodeLike for Operation<V, E> {
    type T = (V, E);

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(
            self.0
                .inputs
                .iter()
                .map(|i| Edge(ByThinAddress(i.link.read().unwrap().upgrade().unwrap()))),
        )
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(
            self.0
                .outputs
                .iter()
                .cloned()
                .map(|o| Edge(ByThinAddress(o))),
        )
    }

    fn backlink(&self) -> Option<Thunk<V, E>> {
        self.0
            .backlink
            .as_ref()
            .and_then(Weak::upgrade)
            .map(|thunk_internal| Thunk(ByThinAddress(thunk_internal)))
    }
}

impl<V, E> NodeLike for Thunk<V, E> {
    type T = (V, E);

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(
            self.0
                .free_variable_edges
                .get()
                .expect("Failed to unlock")
                .clone()
                .into_iter()
                .map(|out_port| Edge(out_port)),
        )
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(
            self.0
                .outputs
                .values()
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
}

impl<V, E> NodeLike for Node<V, E> {
    type T = (V, E);

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        match self {
            Node::Operation(op) => op.inputs(),
            Node::Thunk(thunk) => thunk.inputs(),
        }
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        match self {
            Node::Operation(op) => op.outputs(),
            Node::Thunk(thunk) => thunk.outputs(),
        }
    }

    fn backlink(&self) -> Option<Thunk<V, E>> {
        match self {
            Node::Operation(op) => op.backlink(),
            Node::Thunk(thunk) => thunk.backlink(),
        }
    }
}

impl<V, E> InOut for Operation<V, E> {
    #[must_use]
    fn number_of_inputs(&self) -> usize {
        self.0.inputs.len()
    }

    #[must_use]
    fn number_of_outputs(&self) -> usize {
        self.0.outputs.len()
    }
}

impl<V, E> InOut for Thunk<V, E> {
    #[must_use]
    fn number_of_inputs(&self) -> usize {
        self.0
            .free_variable_edges
            .get()
            .expect("Failed to unlock")
            .len()
    }

    #[must_use]
    fn number_of_outputs(&self) -> usize {
        self.0.outputs.len()
    }
}

impl<V, E> InOut for Node<V, E> {
    #[must_use]
    fn number_of_inputs(&self) -> usize {
        match self {
            Node::Operation(op) => op.number_of_inputs(),
            Node::Thunk(thunk) => thunk.number_of_inputs(),
        }
    }

    #[must_use]
    fn number_of_outputs(&self) -> usize {
        match self {
            Node::Operation(op) => op.number_of_outputs(),
            Node::Thunk(thunk) => thunk.number_of_outputs(),
        }
    }
}

pub fn create_expanded<G>(graph: &G) -> WeakMap<<G::T as Addr>::Thunk, bool>
where
    G: Graph,
    <G::T as Addr>::Thunk: Graph<T = G::T>,
{
    fn helper<T>(set: &mut IndexMap<T::Thunk, bool>, thunk: T::Thunk)
    where
        T: Addr,
        T::Thunk: Graph<T = T>,
    {
        for t in thunk.thunks() {
            helper::<T>(set, t);
        }
        set.insert(thunk, true);
    }

    let mut set = IndexMap::new();

    for thunk in graph.thunks() {
        helper::<G::T>(&mut set, thunk);
    }

    WeakMap(set)
}

#[must_use]
pub fn create_selected<G>(graph: &G) -> SelectionMap<G::T>
where
    G: Graph,
    <G::T as Addr>::Thunk: Graph<T = G::T>,
{
    fn helper<T>(set: &mut IndexMap<T::Node, bool>, thunk: &T::Thunk)
    where
        T: Addr,
        T::Thunk: Graph<T = T>,
    {
        for node in thunk.nodes() {
            if let Ok(thunk) = T::Thunk::try_from(node.clone()) {
                helper::<T>(set, &thunk);
            }
            set.insert(node, false);
        }
    }

    let mut set = IndexMap::new();

    for node in graph.nodes() {
        if let Ok(thunk) = <G::T as Addr>::Thunk::try_from(node.clone()) {
            helper::<G::T>(&mut set, &thunk);
        }
        set.insert(node, false);
    }

    SelectionMap::from(set)
}
