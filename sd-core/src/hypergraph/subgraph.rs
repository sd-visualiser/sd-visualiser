use std::sync::Arc;

use derivative::Derivative;
use indexmap::IndexSet;

use super::{
    generic::{Edge, EdgeWeight, Endpoint, Key, Node, OperationWeight, Thunk, ThunkWeight},
    traits::{EdgeLike, Graph, Keyable, NodeLike, WithWeight},
    Weight,
};
use crate::{
    codeable::{Code, Codeable},
    common::Matchable,
    hypergraph::generic::Ctx,
    selection::SelectionMap,
};

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub struct Subgraph<T: Ctx> {
    pub selection: Arc<SelectionMap<T>>,
}

impl<T: Ctx> Subgraph<T> {
    #[must_use]
    pub fn new(mut selection: SelectionMap<T>) -> Self {
        selection.normalize();
        Self {
            selection: Arc::new(selection),
        }
    }

    /// Remove a node from the subgraph.
    pub fn remove(&mut self, node: &Node<T>) {
        let mut selection = (*self.selection).clone();
        selection[node] = false;
        selection.normalize();
        self.selection = Arc::new(selection);
    }

    /// Extend the subgraph with the given nodes.
    pub fn extend(&mut self, nodes: impl Iterator<Item = Node<T>>) {
        let mut selection = (*self.selection).clone();
        for node in nodes {
            selection[&node] = true;
        }
        selection.normalize();
        self.selection = Arc::new(selection);
    }
}

pub type SubNode<T> = Node<Subgraph<T>>;

impl<T: Ctx> SubNode<T> {
    fn new(node: Node<T>, selection: Arc<SelectionMap<T>>) -> Self {
        match node {
            Node::Operation(op) => Node::Operation(SubOperation { op, selection }),
            Node::Thunk(thunk) => Node::Thunk(SubThunk { thunk, selection }),
        }
    }

    pub fn into_inner(self) -> Node<T> {
        match self {
            Node::Operation(SubOperation { op, .. }) => Node::Operation(op),
            Node::Thunk(SubThunk { thunk, .. }) => Node::Thunk(thunk),
        }
    }
}

pub type SubEndpoint<T> = Endpoint<Subgraph<T>>;

impl<T: Ctx> SubEndpoint<T> {
    fn new(endpoint: Endpoint<T>, selection: Arc<SelectionMap<T>>) -> Self {
        match endpoint {
            Endpoint::Node(node) if selection[&node] => Endpoint::Node(Node::new(node, selection)),
            Endpoint::Boundary(Some(thunk)) if selection[&Node::Thunk(thunk.clone())] => {
                Endpoint::Boundary(Some(SubThunk { thunk, selection }))
            }
            _ => Endpoint::Boundary(None),
        }
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub struct SubEdge<T: Ctx> {
    edge: T::Edge,
    selection: Arc<SelectionMap<T>>,
}

impl<T: Ctx> SubEdge<T> {
    pub fn inner(&self) -> &T::Edge {
        &self.edge
    }

    pub fn into_inner(self) -> T::Edge {
        self.edge
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub struct SubOperation<T: Ctx> {
    op: T::Operation,
    selection: Arc<SelectionMap<T>>,
}

impl<T: Ctx> SubOperation<T> {
    pub fn inner(&self) -> &T::Operation {
        &self.op
    }

    pub fn into_inner(self) -> T::Operation {
        self.op
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub struct SubThunk<T: Ctx> {
    thunk: T::Thunk,
    selection: Arc<SelectionMap<T>>,
}

impl<T: Ctx> SubThunk<T> {
    pub fn inner(&self) -> &T::Thunk {
        &self.thunk
    }

    pub fn into_inner(self) -> T::Thunk {
        self.thunk
    }
}

impl<T: Ctx> Ctx for Subgraph<T> {
    type Edge = SubEdge<T>;
    type Operation = SubOperation<T>;
    type Thunk = SubThunk<T>;
}

impl<T: Ctx> Graph for Subgraph<T> {
    type Ctx = Subgraph<T>;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        let mut inputs: IndexSet<T::Edge> = IndexSet::default();
        for node in self.selection.roots() {
            for edge in node.inputs() {
                if edge
                    .source()
                    .into_node()
                    .map_or(true, |node| !self.selection[&node])
                {
                    inputs.insert(edge);
                }
            }
        }
        Box::new(inputs.into_iter().map(|edge| SubEdge {
            edge,
            selection: self.selection.clone(),
        }))
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(std::iter::empty())
    }

    fn free_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(std::iter::empty())
    }

    fn bound_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        let mut outputs: Vec<T::Edge> = Vec::default();
        for node in self.selection.roots() {
            for edge in node.outputs() {
                for target in edge.targets() {
                    if target
                        .into_node()
                        .map_or(true, |node| !self.selection[&node])
                    {
                        outputs.push(edge.clone());
                    }
                }
            }
        }
        Box::new(outputs.into_iter().map(|edge| SubEdge {
            edge,
            selection: self.selection.clone(),
        }))
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<Self::Ctx>> + '_> {
        Box::new(
            self.selection
                .roots()
                .map(|node| Node::new(node, self.selection.clone())),
        )
    }

    fn graph_backlink(&self) -> Option<Thunk<Self::Ctx>> {
        None
    }

    fn number_of_free_graph_inputs(&self) -> usize {
        self.free_graph_inputs().count() // can't do any better
    }

    fn number_of_bound_graph_inputs(&self) -> usize {
        0
    }

    fn number_of_free_graph_outputs(&self) -> usize {
        0
    }

    fn number_of_bound_graph_outputs(&self) -> usize {
        self.bound_graph_outputs().count() // can't do any better
    }
}

impl<T: Ctx> Graph for SubThunk<T> {
    type Ctx = Subgraph<T>;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.thunk.free_graph_inputs().map(|edge| SubEdge {
            edge,
            selection: self.selection.clone(),
        }))
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.thunk.bound_graph_inputs().map(|edge| SubEdge {
            edge,
            selection: self.selection.clone(),
        }))
    }

    fn free_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.thunk.free_graph_outputs().map(|edge| SubEdge {
            edge,
            selection: self.selection.clone(),
        }))
    }

    fn bound_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = SubEdge<T>> + '_> {
        Box::new(self.thunk.bound_graph_outputs().map(|edge| SubEdge {
            edge,
            selection: self.selection.clone(),
        }))
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<Self::Ctx>> + '_> {
        Box::new(
            self.thunk
                .nodes()
                .map(|node| Node::new(node, self.selection.clone())),
        )
    }

    fn graph_backlink(&self) -> Option<Thunk<Self::Ctx>> {
        Some(self.clone())
    }

    fn number_of_free_graph_inputs(&self) -> usize {
        self.thunk.number_of_free_graph_inputs()
    }

    fn number_of_bound_graph_inputs(&self) -> usize {
        self.thunk.number_of_bound_graph_inputs()
    }

    fn number_of_free_graph_outputs(&self) -> usize {
        self.thunk.number_of_free_graph_outputs()
    }

    fn number_of_bound_graph_outputs(&self) -> usize {
        self.thunk.number_of_bound_graph_outputs()
    }
}

impl<T: Ctx> NodeLike for SubOperation<T> {
    type Ctx = Subgraph<T>;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.op.inputs().map(|edge| SubEdge {
            edge,
            selection: self.selection.clone(),
        }))
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.op.outputs().map(|edge| SubEdge {
            edge,
            selection: self.selection.clone(),
        }))
    }

    fn backlink(&self) -> Option<Thunk<Self::Ctx>> {
        self.op
            .backlink()
            .filter(|thunk| self.selection[&Node::Thunk(thunk.clone())])
            .map(|thunk| SubThunk {
                thunk,
                selection: self.selection.clone(),
            })
    }

    fn number_of_inputs(&self) -> usize {
        self.op.number_of_inputs()
    }

    fn number_of_outputs(&self) -> usize {
        self.op.number_of_outputs()
    }
}

impl<T: Ctx> NodeLike for SubThunk<T> {
    type Ctx = Subgraph<T>;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.thunk.inputs().map(|edge| SubEdge {
            edge,
            selection: self.selection.clone(),
        }))
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.thunk.outputs().map(|edge| SubEdge {
            edge,
            selection: self.selection.clone(),
        }))
    }

    fn backlink(&self) -> Option<Thunk<Self::Ctx>> {
        self.thunk
            .backlink()
            .filter(|thunk| self.selection[&Node::Thunk(thunk.clone())])
            .map(|thunk| SubThunk {
                thunk,
                selection: self.selection.clone(),
            })
    }

    fn number_of_inputs(&self) -> usize {
        self.thunk.number_of_inputs()
    }

    fn number_of_outputs(&self) -> usize {
        self.thunk.number_of_outputs()
    }
}

impl<T: Ctx> EdgeLike for SubEdge<T> {
    type Ctx = Subgraph<T>;

    fn source(&self) -> Endpoint<Self::Ctx> {
        SubEndpoint::new(self.edge.source(), self.selection.clone())
    }

    fn targets(&self) -> Box<dyn DoubleEndedIterator<Item = Endpoint<Self::Ctx>> + '_> {
        Box::new(
            self.edge
                .targets()
                .map(|target| SubEndpoint::new(target, self.selection.clone())),
        )
    }
}

impl<T: Ctx> Keyable for Subgraph<T> {
    type Key = Self;

    fn key(&self) -> Self::Key {
        self.clone()
    }
}

impl<T: Ctx> Keyable for SubEdge<T> {
    type Key = Key<T::Edge>;

    fn key(&self) -> Self::Key {
        self.edge.key()
    }
}

impl<T: Ctx> Keyable for SubOperation<T> {
    type Key = Key<T::Operation>;

    fn key(&self) -> Self::Key {
        self.op.key()
    }
}

impl<T: Ctx> Keyable for SubThunk<T> {
    type Key = Key<T::Thunk>;

    fn key(&self) -> Self::Key {
        self.thunk.key()
    }
}

impl<T: Ctx> WithWeight for SubEdge<T> {
    type Weight = EdgeWeight<T>;

    fn weight(&self) -> Self::Weight {
        self.edge.weight()
    }
}

impl<T: Ctx> WithWeight for SubOperation<T> {
    type Weight = OperationWeight<T>;

    fn weight(&self) -> Self::Weight {
        self.op.weight()
    }
}

impl<T: Ctx> WithWeight for SubThunk<T> {
    type Weight = ThunkWeight<T>;

    fn weight(&self) -> Self::Weight {
        self.thunk.weight()
    }
}

impl<T: Ctx> Codeable for SubEdge<T>
where
    T::Edge: Codeable,
{
    type Code = Code<T::Edge>;

    fn code(&self) -> Self::Code {
        self.edge.code()
    }
}

impl<T: Ctx> Codeable for SubOperation<T>
where
    T::Operation: Codeable,
{
    type Code = Code<T::Operation>;

    fn code(&self) -> Self::Code {
        self.op.code()
    }
}

impl<T: Ctx> Codeable for SubThunk<T>
where
    T::Thunk: Codeable,
{
    type Code = Code<T::Thunk>;

    fn code(&self) -> Self::Code {
        self.thunk.code()
    }
}

impl<T: Ctx> Matchable for SubEdge<T>
where
    T::Edge: Matchable,
{
    fn is_match(&self, query: &str) -> bool {
        self.edge.is_match(query)
    }
}

impl<T: Ctx> Matchable for SubOperation<T>
where
    T::Operation: Matchable,
{
    fn is_match(&self, query: &str) -> bool {
        self.op.is_match(query)
    }
}

impl<T: Ctx> Matchable for SubThunk<T>
where
    T::Thunk: Matchable,
{
    fn is_match(&self, query: &str) -> bool {
        self.thunk.is_match(query)
    }
}

pub trait ExtensibleEdge: EdgeLike {
    fn extend_source(&self) -> Option<Node<Self::Ctx>> {
        None
    }

    fn extend_targets(&self) -> Box<dyn DoubleEndedIterator<Item = Node<Self::Ctx>> + '_> {
        Box::new(std::iter::empty())
    }
}

impl<W: Weight> ExtensibleEdge for super::Edge<W> {}

impl<T: Ctx> ExtensibleEdge for SubEdge<T> {
    fn extend_source(&self) -> Option<Node<Self::Ctx>> {
        self.edge
            .source()
            .into_node()
            .filter(|node| !self.selection[node])
            .map(|node| Node::new(node, self.selection.clone()))
    }

    fn extend_targets(&self) -> Box<dyn DoubleEndedIterator<Item = Node<Self::Ctx>> + '_> {
        Box::new(
            self.edge
                .targets()
                .filter_map(Endpoint::into_node)
                .filter(|node| !self.selection[node])
                .map(|node| Node::new(node, self.selection.clone())),
        )
    }
}
