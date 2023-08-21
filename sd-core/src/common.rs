use std::{fmt::Debug, hash::Hash};

use derivative::Derivative;

use crate::hypergraph::{
    generic::{Ctx, Edge},
    traits::{NodeLike, WithWeight},
};

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
pub struct Link<T: Ctx>(pub T::Edge, pub Direction);

/// Specifies an operation which has inputs and outputs.
pub trait InOut {
    fn number_of_inputs(&self) -> usize;
    fn number_of_outputs(&self) -> usize;
}

pub trait InOutIter: InOut {
    type T: Ctx;
    fn input_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<Self::T>> + 'a>;
    fn output_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<Self::T>> + 'a>;
}

/// Check if an object matches a variable name
pub trait Matchable {
    fn is_match(&self, variable: &str) -> bool;
}

impl<V> Matchable for &V
where
    V: NodeLike,
    Edge<V::Ctx>: WithWeight,
    <Edge<V::Ctx> as WithWeight>::Weight: Matchable,
{
    fn is_match(&self, variable: &str) -> bool {
        self.outputs().any(|edge| edge.weight().is_match(variable))
    }
}
