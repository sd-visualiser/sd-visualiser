use std::sync::Arc;

use derivative::Derivative;
use indexmap::IndexSet;

use super::{
    generic::{EdgeWeight, Node, OperationWeight, ThunkWeight},
    traits::{EdgeLike, Graph, NodeLike, WithWeight},
    Weight,
};
use crate::{common::Matchable, hypergraph::generic::Ctx, selection::SelectionMap};

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
    pub fn remove(&mut self, node: SubNode<T>) {
        let mut selection = (*self.selection).clone();
        selection[&node.inner()] = false;
        selection.normalize();
        self.selection = Arc::new(selection);
    }

    /// Extend the subgraph with the given nodes.
    ///
    /// The nodes will likely come from the methods in [`ExtensibleEdge`].
    pub fn extend(&mut self, nodes: impl Iterator<Item = SubNode<T>>) {
        let mut selection = (*self.selection).clone();
        for node in nodes {
            selection[&node.inner()] = true;
        }
        selection.normalize();
        self.selection = Arc::new(selection);
    }
}

pub type SubNode<T> = Node<Subgraph<T>>;

impl<T: Ctx> SubNode<T> {
    fn new(inner: Node<T>, selection: Arc<SelectionMap<T>>) -> Self {
        match inner {
            Node::Operation(inner) => Node::Operation(SubOperation { inner, selection }),
            Node::Thunk(inner) => Node::Thunk(SubThunk { inner, selection }),
        }
    }

    fn inner(self) -> Node<T> {
        match self {
            Node::Operation(SubOperation { inner, .. }) => Node::Operation(inner),
            Node::Thunk(SubThunk { inner, .. }) => Node::Thunk(inner),
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
    pub inner: T::Edge,
    #[derivative(PartialEq = "ignore", Hash = "ignore", Debug = "ignore")]
    pub selection: Arc<SelectionMap<T>>,
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
    pub inner: T::Operation,
    #[derivative(PartialEq = "ignore", Hash = "ignore", Debug = "ignore")]
    pub selection: Arc<SelectionMap<T>>,
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
    pub inner: T::Thunk,
    #[derivative(PartialEq = "ignore", Hash = "ignore", Debug = "ignore")]
    pub selection: Arc<SelectionMap<T>>,
}

impl<T: Ctx> Ctx for Subgraph<T> {
    type Edge = SubEdge<T>;
    type Operation = SubOperation<T>;
    type Thunk = SubThunk<T>;
}

impl<T: Ctx> Graph for Subgraph<T> {
    type Ctx = Subgraph<T>;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = SubEdge<T>> + '_> {
        let mut inputs: IndexSet<T::Edge> = IndexSet::default();
        for node in self.selection.roots() {
            for edge in node.inputs() {
                if edge
                    .source()
                    .as_ref()
                    .map_or(true, |node| !self.selection[node])
                {
                    inputs.insert(edge);
                }
            }
        }
        Box::new(inputs.into_iter().map(|edge| SubEdge {
            inner: edge,
            selection: self.selection.clone(),
        }))
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = SubEdge<T>> + '_> {
        Box::new(std::iter::empty())
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = SubEdge<T>> + '_> {
        let mut outputs: Vec<T::Edge> = Vec::default();
        for node in self.selection.roots() {
            for edge in node.outputs() {
                for target in edge.targets() {
                    if target.as_ref().map_or(true, |node| !self.selection[node]) {
                        outputs.push(edge.clone());
                    }
                }
            }
        }
        Box::new(outputs.into_iter().map(|edge| SubEdge {
            inner: edge,
            selection: self.selection.clone(),
        }))
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = SubNode<T>> + '_> {
        Box::new(
            self.selection
                .roots()
                .map(|node| Node::new(node, self.selection.clone())),
        )
    }

    fn graph_backlink(&self) -> Option<SubThunk<T>> {
        None
    }
}

impl<T: Ctx> Graph for SubThunk<T> {
    type Ctx = Subgraph<T>;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = SubEdge<T>> + '_> {
        Box::new(self.inner.free_graph_inputs().map(|edge| SubEdge {
            inner: edge,
            selection: self.selection.clone(),
        }))
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = SubEdge<T>> + '_> {
        Box::new(self.inner.bound_graph_inputs().map(|edge| SubEdge {
            inner: edge,
            selection: self.selection.clone(),
        }))
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = SubNode<T>> + '_> {
        Box::new(
            self.inner
                .nodes()
                .map(|node| Node::new(node, self.selection.clone())),
        )
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = SubEdge<T>> + '_> {
        Box::new(self.inner.graph_outputs().map(|edge| SubEdge {
            inner: edge,
            selection: self.selection.clone(),
        }))
    }

    fn graph_backlink(&self) -> Option<SubThunk<T>> {
        Some(self.clone())
    }
}

impl<T: Ctx> NodeLike for SubOperation<T> {
    type Ctx = Subgraph<T>;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = SubEdge<T>> + '_> {
        Box::new(self.inner.inputs().map(|edge| SubEdge {
            inner: edge,
            selection: self.selection.clone(),
        }))
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = SubEdge<T>> + '_> {
        Box::new(self.inner.outputs().map(|edge| SubEdge {
            inner: edge,
            selection: self.selection.clone(),
        }))
    }

    fn backlink(&self) -> Option<SubThunk<T>> {
        self.inner
            .backlink()
            .filter(|thunk| self.selection[&Node::Thunk(thunk.clone())])
            .map(|thunk| SubThunk {
                inner: thunk,
                selection: self.selection.clone(),
            })
    }

    fn number_of_inputs(&self) -> usize {
        self.inner.number_of_inputs()
    }

    fn number_of_outputs(&self) -> usize {
        self.inner.number_of_outputs()
    }
}

impl<T: Ctx> NodeLike for SubThunk<T> {
    type Ctx = Subgraph<T>;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = SubEdge<T>> + '_> {
        Box::new(self.inner.inputs().map(|edge| SubEdge {
            inner: edge,
            selection: self.selection.clone(),
        }))
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = SubEdge<T>> + '_> {
        Box::new(self.inner.outputs().map(|edge| SubEdge {
            inner: edge,
            selection: self.selection.clone(),
        }))
    }

    fn backlink(&self) -> Option<SubThunk<T>> {
        self.inner
            .backlink()
            .filter(|thunk| self.selection[&Node::Thunk(thunk.clone())])
            .map(|thunk| SubThunk {
                inner: thunk,
                selection: self.selection.clone(),
            })
    }

    fn number_of_inputs(&self) -> usize {
        self.inner.number_of_inputs()
    }

    fn number_of_outputs(&self) -> usize {
        self.inner.number_of_outputs()
    }
}

impl<T: Ctx> EdgeLike for SubEdge<T> {
    type Ctx = Subgraph<T>;

    fn source(&self) -> Option<SubNode<T>> {
        self.inner
            .source()
            .filter(|node| self.selection[node])
            .map(|node| Node::new(node, self.selection.clone()))
    }

    fn targets(&self) -> Box<dyn DoubleEndedIterator<Item = Option<SubNode<T>>> + '_> {
        Box::new(self.inner.targets().map(|target| {
            target
                .filter(|node| self.selection[node])
                .map(|node| Node::new(node, self.selection.clone()))
        }))
    }
}

impl<T: Ctx> WithWeight for SubEdge<T> {
    type Weight = EdgeWeight<T>;

    fn weight(&self) -> Self::Weight {
        self.inner.weight()
    }
}

impl<T: Ctx> WithWeight for SubOperation<T> {
    type Weight = OperationWeight<T>;

    fn weight(&self) -> Self::Weight {
        self.inner.weight()
    }
}

impl<T: Ctx> WithWeight for SubThunk<T> {
    type Weight = ThunkWeight<T>;

    fn weight(&self) -> Self::Weight {
        self.inner.weight()
    }
}

impl<T: Ctx> Matchable for SubEdge<T>
where
    T::Edge: Matchable,
{
    fn is_match(&self, query: &str) -> bool {
        self.inner.is_match(query)
    }
}

impl<T: Ctx> Matchable for SubOperation<T>
where
    T::Operation: Matchable,
{
    fn is_match(&self, query: &str) -> bool {
        self.inner.is_match(query)
    }
}

impl<T: Ctx> Matchable for SubThunk<T>
where
    T::Thunk: Matchable,
{
    fn is_match(&self, query: &str) -> bool {
        self.inner.is_match(query)
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
        self.inner
            .source()
            .filter(|node| !self.selection[node])
            .map(|node| Node::new(node, self.selection.clone()))
    }

    fn extend_targets(&self) -> Box<dyn DoubleEndedIterator<Item = Node<Self::Ctx>> + '_> {
        Box::new(
            self.inner
                .targets()
                .flatten()
                .filter(|node| !self.selection[node])
                .map(|node| Node::new(node, self.selection.clone())),
        )
    }
}
