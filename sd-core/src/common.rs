use std::{fmt::Debug, hash::Hash};

use crate::hypergraph::{
    self,
    generic::{Ctx, Edge},
    traits::{NodeLike, WithWeight},
    Weight,
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

pub type Link<T> = (Edge<T>, Direction);

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

/// Check if an object matches a query.
pub trait Matchable {
    fn is_match(&self, query: &str) -> bool;
}

impl<W: Weight> Matchable for hypergraph::Edge<W> {
    fn is_match(&self, _query: &str) -> bool {
        false
    }
}

impl<W: Weight> Matchable for hypergraph::Operation<W>
where
    W::EdgeWeight: Matchable,
    W::OperationWeight: Matchable,
{
    fn is_match(&self, query: &str) -> bool {
        self.weight().is_match(query) || self.outputs().any(|edge| edge.weight().is_match(query))
    }
}

impl<W: Weight> Matchable for hypergraph::Thunk<W>
where
    W::ThunkWeight: Matchable,
{
    fn is_match(&self, query: &str) -> bool {
        self.weight().is_match(query)
    }
}
