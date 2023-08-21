use std::fmt::Debug;

use derivative::Derivative;

use super::traits::{EdgeLike, Graph, NodeLike};

/// A context records which type of graph we are working with.
///
/// Normally the type implement `Ctx` will also implement `GraphLike` (see `Hypergraph`).
pub trait Ctx {
    type Edge: EdgeLike<Ctx = Self>;
    type Operation: NodeLike<Ctx = Self>;
    type Thunk: NodeLike<Ctx = Self> + Graph<Ctx = Self>;
}

pub type Edge<T> = <T as Ctx>::Edge;
pub type Operation<T> = <T as Ctx>::Operation;
pub type Thunk<T> = <T as Ctx>::Thunk;

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "T::Operation: Debug, T::Thunk: Debug")
)]
pub enum Node<T: Ctx> {
    Operation(T::Operation),
    Thunk(T::Thunk),
}

impl<T: Ctx> Node<T> {
    pub fn into_operation(self) -> Option<T::Operation> {
        match self {
            Node::Operation(op) => Some(op),
            Node::Thunk(_) => None,
        }
    }

    pub fn into_thunk(self) -> Option<T::Thunk> {
        match self {
            Node::Operation(_) => None,
            Node::Thunk(thunk) => Some(thunk),
        }
    }
}

impl<T: Ctx> NodeLike for Node<T> {
    type Ctx = T;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = T::Edge> + '_> {
        match self {
            Node::Operation(op) => op.inputs(),
            Node::Thunk(thunk) => thunk.inputs(),
        }
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = T::Edge> + '_> {
        match self {
            Node::Operation(op) => op.outputs(),
            Node::Thunk(thunk) => thunk.outputs(),
        }
    }

    fn backlink(&self) -> Option<T::Thunk> {
        match self {
            Node::Operation(op) => op.backlink(),
            Node::Thunk(thunk) => thunk.backlink(),
        }
    }

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
