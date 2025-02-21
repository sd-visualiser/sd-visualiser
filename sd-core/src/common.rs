use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

use either::Either;
use pretty::RcDoc;
#[cfg(test)]
use serde::Serialize;

use crate::{
    hypergraph::{
        self, Weight,
        generic::{Ctx, Edge},
        traits::{NodeLike, WithWeight},
    },
    prettyprinter::PrettyPrint,
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

impl Matchable for String {
    fn is_match(&self, query: &str) -> bool {
        self == query
    }
}

impl Matchable for Box<String> {
    fn is_match(&self, query: &str) -> bool {
        self.as_ref() == query
    }
}

impl<S: Matchable, T: Matchable> Matchable for Either<S, T> {
    fn is_match(&self, query: &str) -> bool {
        match self {
            Either::Left(l) => l.is_match(query),
            Either::Right(r) => r.is_match(query),
        }
    }
}

#[derive(Clone, Copy, Hash, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(Serialize))]
pub enum Empty {}

impl Display for Empty {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {}
    }
}

impl Matchable for Empty {
    fn is_match(&self, _: &str) -> bool {
        match *self {}
    }
}

impl PrettyPrint for Empty {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match *self {}
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub struct Unit;

impl Display for Unit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("")
    }
}

impl Matchable for Unit {
    fn is_match(&self, _: &str) -> bool {
        false
    }
}

impl PrettyPrint for Unit {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::nil()
    }
}
