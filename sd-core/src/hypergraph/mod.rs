use std::{
    fmt::Debug,
    sync::{Arc, Weak},
};

use by_address::ByThinAddress;
use derivative::Derivative;
use indexmap::IndexMap;

use crate::{common::InOut, selection::SelectionMap, weak_map::WeakMap};

pub mod builder;
mod internal;
mod weakbyaddress;

use self::{
    internal::{
        HypergraphInternal, NodeInternal, OperationInternal, OutPortInternal, ThunkInternal,
        WeakNodeInternal,
    },
    weakbyaddress::WeakByAddress,
};

pub mod reachability;
pub mod subgraph;

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
#[derivative(Clone(bound = ""), Default(bound = ""))]
pub struct Hypergraph<V, E>(HypergraphInternal<V, E>);

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

impl<V, E> Edge<V, E> {
    #[must_use]
    pub fn weight(&self) -> &E {
        &self.0.weight
    }

    #[must_use]
    pub fn source(&self) -> Option<Node<V, E>> {
        self.0.node.as_ref().map(WeakNodeInternal::unwrap_node)
    }

    pub fn targets(&self) -> impl Iterator<Item = Option<Node<V, E>>> {
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
            .into_iter()
    }

    #[must_use]
    pub fn number_of_targets(&self) -> usize {
        self.0
            .links
            .try_read()
            .expect("Lock unexpectedly taken")
            .len()
    }
}

pub trait Graph {
    type NodeWeight;
    type EdgeWeight;
    fn bound_graph_inputs(
        &self,
    ) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::NodeWeight, Self::EdgeWeight>> + '_>;
    fn unbound_graph_inputs(
        &self,
    ) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::NodeWeight, Self::EdgeWeight>> + '_>;
    fn graph_inputs<'a>(
        &'a self,
    ) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::NodeWeight, Self::EdgeWeight>> + 'a>
    where
        Self::EdgeWeight: 'a,
        Self::NodeWeight: 'a,
    {
        Box::new(self.unbound_graph_inputs().chain(self.bound_graph_inputs()))
    }
    fn graph_outputs(
        &self,
    ) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::NodeWeight, Self::EdgeWeight>> + '_>;
    fn nodes(
        &self,
    ) -> Box<dyn DoubleEndedIterator<Item = Node<Self::NodeWeight, Self::EdgeWeight>> + '_>;
    fn operations<'a>(
        &'a self,
    ) -> Box<dyn DoubleEndedIterator<Item = Operation<Self::NodeWeight, Self::EdgeWeight>> + 'a>
    where
        Self::NodeWeight: 'a,
        Self::EdgeWeight: 'a,
    {
        Box::new(self.nodes().filter_map(|node| match node {
            Node::Operation(operation) => Some(operation),
            Node::Thunk(_) => None,
        }))
    }
    fn thunks<'a>(
        &'a self,
    ) -> Box<dyn DoubleEndedIterator<Item = Thunk<Self::NodeWeight, Self::EdgeWeight>> + 'a>
    where
        Self::NodeWeight: 'a,
        Self::EdgeWeight: 'a,
    {
        Box::new(self.nodes().filter_map(|node| match node {
            Node::Operation(_) => None,
            Node::Thunk(thunk) => Some(thunk),
        }))
    }
}

impl<V, E> Graph for Hypergraph<V, E> {
    type NodeWeight = V;
    type EdgeWeight = E;
    fn unbound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(
            self.0
                .graph_inputs
                .iter()
                .cloned()
                .map(|o| Edge(ByThinAddress(o))),
        )
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(std::iter::empty())
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(
            self.0
                .graph_outputs
                .iter()
                .cloned()
                .map(|in_port| Edge(ByThinAddress(in_port.link()))),
        )
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<V, E>> + '_> {
        Box::new(self.0.nodes.iter().cloned().map(|node| match node {
            NodeInternal::Operation(operation) => {
                Node::Operation(Operation(ByThinAddress(operation)))
            }
            NodeInternal::Thunk(thunk) => Node::Thunk(Thunk(ByThinAddress(thunk))),
        }))
    }
}

impl<V, E> Graph for Thunk<V, E> {
    type NodeWeight = V;
    type EdgeWeight = E;
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
                    NodeInternal::Operation(operation) => {
                        Node::Operation(Operation(ByThinAddress(operation)))
                    }
                    NodeInternal::Thunk(thunk) => Node::Thunk(Thunk(ByThinAddress(thunk))),
                })
                .collect::<Vec<_>>()
                .into_iter(),
        )
    }
}

impl<V, E> Operation<V, E> {
    #[must_use]
    pub fn inputs(&self) -> impl DoubleEndedIterator<Item = Edge<V, E>> + '_ {
        self.0
            .inputs
            .iter()
            .map(|i| Edge(ByThinAddress(i.link.read().unwrap().upgrade().unwrap())))
    }

    #[must_use]
    pub fn outputs(&self) -> impl DoubleEndedIterator<Item = Edge<V, E>> + '_ {
        self.0
            .outputs
            .iter()
            .cloned()
            .map(|o| Edge(ByThinAddress(o)))
    }

    #[must_use]
    pub fn weight(&self) -> &V {
        &self.0.weight
    }

    #[must_use]
    pub fn backlink(&self) -> Option<Thunk<V, E>> {
        self.0
            .backlink
            .as_ref()
            .and_then(Weak::upgrade)
            .map(|thunk_internal| Thunk(ByThinAddress(thunk_internal)))
    }
}

impl<V, E> Thunk<V, E> {
    #[must_use]
    pub fn inputs(&self) -> impl DoubleEndedIterator<Item = Edge<V, E>> + '_ {
        self.0
            .free_variable_edges
            .get()
            .expect("Failed to unlock")
            .clone()
            .into_iter()
            .map(|out_port| Edge(out_port))
    }

    #[must_use]
    pub fn outputs(&self) -> impl DoubleEndedIterator<Item = Edge<V, E>> + '_ {
        self.0
            .outputs
            .values()
            .map(|out_port| Edge(ByThinAddress(out_port.clone())))
    }

    #[must_use]
    pub fn backlink(&self) -> Option<Thunk<V, E>> {
        self.0
            .backlink
            .as_ref()
            .and_then(Weak::upgrade)
            .map(|thunk_internal| Thunk(ByThinAddress(thunk_internal)))
    }
}

impl<V, E> Node<V, E> {
    #[must_use]
    pub fn inputs(&self) -> Box<dyn Iterator<Item = Edge<V, E>> + '_> {
        match self {
            Node::Operation(op) => Box::new(op.inputs()),
            Node::Thunk(thunk) => Box::new(thunk.inputs()),
        }
    }

    #[must_use]
    pub fn outputs(&self) -> Box<dyn Iterator<Item = Edge<V, E>> + '_> {
        match self {
            Node::Operation(op) => Box::new(op.outputs()),
            Node::Thunk(thunk) => Box::new(thunk.outputs()),
        }
    }

    #[must_use]
    pub fn backlink(&self) -> Option<Thunk<V, E>> {
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

impl<V, E> Hypergraph<V, E> {
    #[must_use]
    pub fn create_expanded(&self) -> WeakMap<Thunk<V, E>, bool> {
        fn helper<V, E>(set: &mut IndexMap<Thunk<V, E>, bool>, thunk: Thunk<V, E>) {
            for t in thunk.thunks() {
                helper(set, t);
            }
            set.insert(thunk, true);
        }

        let mut set = IndexMap::new();

        for thunk in self.thunks() {
            helper(&mut set, thunk);
        }

        WeakMap(set)
    }

    #[must_use]
    pub fn create_selected(&self) -> SelectionMap<(V, E)> {
        fn helper<V, E>(set: &mut IndexMap<Node<V, E>, bool>, thunk: &Thunk<V, E>) {
            for node in thunk.nodes() {
                if let Node::Thunk(thunk) = &node {
                    helper(set, thunk);
                }
                set.insert(node, false);
            }
        }

        let mut set = IndexMap::new();

        for node in self.nodes() {
            if let Node::Thunk(thunk) = &node {
                helper(&mut set, thunk);
            }
            set.insert(node, false);
        }

        SelectionMap::from(set)
    }
}
