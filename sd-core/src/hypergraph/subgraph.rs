use std::{fmt::Debug, sync::Arc};

use derivative::Derivative;
use indexmap::IndexSet;

use super::traits::{EdgeLike, Graph, NodeLike, WithWeight};
use crate::{
    common::{Addr, InOut},
    selection::SelectionMap,
};

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = "")
)]
pub struct Subgraph<T: Addr> {
    pub selection: Arc<SelectionMap<T>>,
}

impl<T: Addr> Subgraph<T>
where
    T::Node: NodeLike<T = T>,
    T::Thunk: NodeLike<T = T>,
{
    #[must_use]
    pub fn new(mut selection: SelectionMap<T>) -> Self {
        selection.normalize();
        Self {
            selection: Arc::new(selection),
        }
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "T::Node: Debug")
)]
pub struct SubNode<T: Addr> {
    pub inner: T::Node,
    #[derivative(PartialEq = "ignore", Hash = "ignore", Debug = "ignore")]
    pub selection: Arc<SelectionMap<T>>,
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "T::Edge: Debug")
)]
pub struct SubEdge<T: Addr> {
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
    Debug(bound = "T::Operation: Debug")
)]
pub struct SubOperation<T: Addr> {
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
    Debug(bound = "T::Thunk: Debug")
)]
pub struct SubThunk<T: Addr> {
    pub inner: T::Thunk,
    #[derivative(PartialEq = "ignore", Hash = "ignore", Debug = "ignore")]
    pub selection: Arc<SelectionMap<T>>,
}

impl<T: Addr> InOut for SubNode<T> {
    fn number_of_inputs(&self) -> usize {
        self.inner.number_of_inputs()
    }

    fn number_of_outputs(&self) -> usize {
        self.inner.number_of_outputs()
    }
}

impl<T: Addr> InOut for SubOperation<T> {
    fn number_of_inputs(&self) -> usize {
        self.inner.number_of_inputs()
    }

    fn number_of_outputs(&self) -> usize {
        self.inner.number_of_outputs()
    }
}

impl<T: Addr> InOut for SubThunk<T> {
    fn number_of_inputs(&self) -> usize {
        self.inner.number_of_inputs()
    }

    fn number_of_outputs(&self) -> usize {
        self.inner.number_of_outputs()
    }
}

impl<T: Addr> From<SubOperation<T>> for SubNode<T> {
    fn from(op: SubOperation<T>) -> Self {
        Self {
            inner: op.inner.into(),
            selection: op.selection,
        }
    }
}

impl<T: Addr> From<SubThunk<T>> for SubNode<T> {
    fn from(thunk: SubThunk<T>) -> Self {
        Self {
            inner: thunk.inner.into(),
            selection: thunk.selection,
        }
    }
}

impl<T: Addr> TryFrom<SubNode<T>> for SubOperation<T> {
    type Error = <T::Operation as TryFrom<T::Node>>::Error;

    fn try_from(node: SubNode<T>) -> Result<Self, Self::Error> {
        Ok(Self {
            inner: node.inner.try_into()?,
            selection: node.selection,
        })
    }
}

impl<T: Addr> TryFrom<SubNode<T>> for SubThunk<T> {
    type Error = <T::Thunk as TryFrom<T::Node>>::Error;

    fn try_from(node: SubNode<T>) -> Result<Self, Self::Error> {
        Ok(Self {
            inner: node.inner.try_into()?,
            selection: node.selection,
        })
    }
}

pub struct Sub<T: Addr> {
    _phantom: std::marker::PhantomData<T>,
}

impl<T: Addr> Addr for Sub<T> {
    type Node = SubNode<T>;
    type Edge = SubEdge<T>;
    type Operation = SubOperation<T>;
    type Thunk = SubThunk<T>;
}

impl<T: Addr> Graph for Subgraph<T>
where
    T::Node: NodeLike<T = T>,
    T::Edge: EdgeLike<T = T>,
{
    type T = Sub<T>;

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
        Box::new(self.selection.roots().map(|node| SubNode {
            inner: node,
            selection: self.selection.clone(),
        }))
    }
}

impl<T: Addr> Graph for SubThunk<T>
where
    T::Thunk: Graph<T = T>,
{
    type T = Sub<T>;

    fn free_graph_inputs(
        &self,
    ) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_> {
        Box::new(self.inner.free_graph_inputs().map(|edge| SubEdge {
            inner: edge,
            selection: self.selection.clone(),
        }))
    }

    fn bound_graph_inputs(
        &self,
    ) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_> {
        Box::new(self.inner.bound_graph_inputs().map(|edge| SubEdge {
            inner: edge,
            selection: self.selection.clone(),
        }))
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Node> + '_> {
        Box::new(self.inner.nodes().map(|node| SubNode {
            inner: node,
            selection: self.selection.clone(),
        }))
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_> {
        Box::new(self.inner.graph_outputs().map(|edge| SubEdge {
            inner: edge,
            selection: self.selection.clone(),
        }))
    }
}

impl<T: Addr> NodeLike for SubNode<T>
where
    T::Node: NodeLike<T = T>,
{
    type T = Sub<T>;

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
            .filter(|thunk| self.selection[&T::Node::from(thunk.clone())])
            .map(|thunk| SubThunk {
                inner: thunk,
                selection: self.selection.clone(),
            })
    }
}

impl<T: Addr> NodeLike for SubOperation<T>
where
    T::Operation: NodeLike<T = T>,
{
    type T = Sub<T>;

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
            .filter(|thunk| self.selection[&T::Node::from(thunk.clone())])
            .map(|thunk| SubThunk {
                inner: thunk,
                selection: self.selection.clone(),
            })
    }
}

impl<T: Addr> NodeLike for SubThunk<T>
where
    T::Thunk: NodeLike<T = T>,
{
    type T = Sub<T>;

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
            .filter(|thunk| self.selection[&T::Node::from(thunk.clone())])
            .map(|thunk| SubThunk {
                inner: thunk,
                selection: self.selection.clone(),
            })
    }
}

impl<T: Addr> EdgeLike for SubEdge<T>
where
    T::Edge: EdgeLike<T = T>,
{
    type T = Sub<T>;

    fn source(&self) -> Option<SubNode<T>> {
        self.inner
            .source()
            .filter(|node| self.selection[node])
            .map(|node| SubNode {
                inner: node,
                selection: self.selection.clone(),
            })
    }

    fn targets(&self) -> Box<dyn DoubleEndedIterator<Item = Option<SubNode<T>>> + '_> {
        Box::new(self.inner.targets().map(|target| {
            target
                .filter(|node| self.selection[node])
                .map(|node| SubNode {
                    inner: node,
                    selection: self.selection.clone(),
                })
        }))
    }

    fn number_of_normalised_targets(&self) -> usize {
        self.inner.number_of_normalised_targets()
    }
}

impl<T: Addr> WithWeight for SubEdge<T>
where
    T::Edge: WithWeight,
{
    type Weight = <T::Edge as WithWeight>::Weight;

    fn weight(&self) -> &Self::Weight {
        self.inner.weight()
    }
}

impl<T: Addr> WithWeight for SubOperation<T>
where
    T::Operation: WithWeight,
{
    type Weight = <T::Operation as WithWeight>::Weight;

    fn weight(&self) -> &Self::Weight {
        self.inner.weight()
    }
}

pub trait ExtensibleEdge: EdgeLike {
    fn extend_source(&self) -> Option<<Self::T as Addr>::Node>;
    fn extend_targets(&self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Node> + '_>;
}

impl<V, E> ExtensibleEdge for super::Edge<V, E> {
    fn extend_source(&self) -> Option<<Self::T as Addr>::Node> {
        None
    }

    fn extend_targets(&self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Node> + '_> {
        Box::new(std::iter::empty())
    }
}

impl<T: Addr> ExtensibleEdge for SubEdge<T>
where
    T::Edge: EdgeLike<T = T>,
{
    fn extend_source(&self) -> Option<<Self::T as Addr>::Node> {
        self.inner
            .source()
            .filter(|node| !self.selection[node])
            .map(|node| SubNode {
                inner: node,
                selection: self.selection.clone(),
            })
    }

    fn extend_targets(&self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Node> + '_> {
        Box::new(
            self.inner
                .targets()
                .flatten()
                .filter(|node| !self.selection[node])
                .map(|node| SubNode {
                    inner: node,
                    selection: self.selection.clone(),
                }),
        )
    }
}

pub trait ModifiableGraph: Graph {
    fn remove(&mut self, node: <Self::T as Addr>::Node);
    fn extend(&mut self, nodes: impl Iterator<Item = <Self::T as Addr>::Node>);
}

impl<V, E> ModifiableGraph for super::Hypergraph<V, E> {
    fn remove(&mut self, _node: <Self::T as Addr>::Node) {}
    fn extend(&mut self, _nodes: impl Iterator<Item = <Self::T as Addr>::Node>) {}
}

impl<T: Addr> ModifiableGraph for Subgraph<T>
where
    T::Node: NodeLike<T = T>,
    T::Edge: EdgeLike<T = T>,
    T::Thunk: NodeLike<T = T>,
{
    fn remove(&mut self, node: <Self::T as Addr>::Node) {
        let mut extended = (*self.selection).clone();
        extended[&node.inner] = false;
        extended.normalize();
        self.selection = Arc::new(extended);
    }

    fn extend(&mut self, nodes: impl Iterator<Item = <Self::T as Addr>::Node>) {
        let mut extended = (*self.selection).clone();
        for node in nodes {
            extended[&node.inner] = true;
        }
        extended.normalize();
        self.selection = Arc::new(extended);
    }
}
