use std::{fmt::Debug, hash::Hash};

use derivative::Derivative;

use crate::hypergraph::{Edge, Node, Operation, Thunk};

pub trait Addr {
    type Edge: Clone + Eq + PartialEq + Hash;
    type Thunk: Clone + Eq + PartialEq + Hash + InOut;
    type Operation: Clone + Eq + PartialEq + Hash + InOut;
    type Node: Clone + Eq + PartialEq + Hash + InOut + From<Self::Thunk> + From<Self::Operation>;
}

impl<V, E> Addr for (V, E) {
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

pub trait InOutIter {
    type T: Addr;
    fn input_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<Self::T>> + 'a>;
    fn output_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<Self::T>> + 'a>;
}

impl<V, E> InOutIter for Node<V, E> {
    type T = (V, E);

    fn input_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<(V, E)>> + 'a> {
        Box::new(self.inputs().map(|edge| Link(edge, Direction::Forward)))
    }

    fn output_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<(V, E)>> + 'a> {
        Box::new(self.outputs().map(|edge| Link(edge, Direction::Forward)))
    }
}

impl<V, E> InOutIter for Operation<V, E> {
    type T = (V, E);

    fn input_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<(V, E)>> + 'a> {
        Box::new(self.inputs().map(|edge| Link(edge, Direction::Forward)))
    }

    fn output_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<(V, E)>> + 'a> {
        Box::new(self.outputs().map(|edge| Link(edge, Direction::Forward)))
    }
}

impl<V, E> InOutIter for Thunk<V, E> {
    type T = (V, E);

    fn input_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<(V, E)>> + 'a> {
        Box::new(self.inputs().map(|edge| Link(edge, Direction::Forward)))
    }

    fn output_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<(V, E)>> + 'a> {
        Box::new(self.outputs().map(|edge| Link(edge, Direction::Forward)))
    }
}

/// Check if an object matches a variable name
pub trait Matchable {
    fn is_match(&self, variable: &str) -> bool;
}
