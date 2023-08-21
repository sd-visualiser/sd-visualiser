use std::{fmt::Debug, hash::Hash};

use derivative::Derivative;

use crate::hypergraph::{
    traits::{EdgeLike, Graph, NodeLike},
    Edge, Hypergraph, Node, Operation, Thunk,
};

pub trait Addr {
    type Edge: Clone + Eq + PartialEq + Hash + EdgeLike<T = Self>;
    type Thunk: Clone
        + Eq
        + PartialEq
        + Hash
        + NodeLike<T = Self>
        + Graph<T = Self>
        + TryFrom<Self::Node>;
    type Operation: Clone + Eq + PartialEq + Hash + NodeLike<T = Self> + TryFrom<Self::Node>;
    type Node: Clone
        + Eq
        + PartialEq
        + Hash
        + NodeLike<T = Self>
        + From<Self::Thunk>
        + From<Self::Operation>;
}

impl<V, E> Addr for Hypergraph<V, E> {
    type Edge = Edge<V, E>;
    type Thunk = Thunk<V, E>;
    type Operation = Operation<V, E>;
    type Node = Node<V, E>;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Direction {
    Forward,
    Backward,
}

impl Direction {
    #[must_use]
    pub const fn flip(self) -> Self {
        match self {
            Direction::Forward => Direction::Backward,
            Direction::Backward => Direction::Forward,
        }
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "T::Edge: Debug")
)]
pub struct Link<T: Addr>(pub T::Edge, pub Direction);

/// Specifies an operation which has inputs and outputs.
pub trait InOut {
    fn number_of_inputs(&self) -> usize;
    fn number_of_outputs(&self) -> usize;
}

pub trait InOutIter: InOut {
    type T: Addr;
    fn input_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<Self::T>> + 'a>;
    fn output_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<Self::T>> + 'a>;
}

/// Check if an object matches a variable name
pub trait Matchable {
    fn is_match(&self, variable: &str) -> bool;
}
