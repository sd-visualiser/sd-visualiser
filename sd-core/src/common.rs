use std::{
    collections::{HashMap, VecDeque},
    fmt::Debug,
    hash::Hash,
};

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

/// Consume the first `n` items of an iterator `iter`
pub(crate) fn advance_by<I: Iterator>(iter: &mut I, n: usize) {
    if n != 0 {
        iter.nth(n - 1);
    }
}

/// Possible destinations of an edge in a "permutation layer"
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PermutationOutput {
    /// The edge passes through the layer to the edge indexed by the given number
    Output(usize),
    /// The edge is deleted in this layer
    Deleted,
    /// The edge is part of a cup or cap with the edge indexed by the given number
    Paired(usize),
}

impl From<PermutationOutput> for Option<usize> {
    fn from(value: PermutationOutput) -> Self {
        if let PermutationOutput::Output(a) = value {
            Some(a)
        } else {
            None
        }
    }
}

/// Calculates the permutation between two layers
///
/// # Inputs
/// `start`: An iterator identifying the items before permuting
/// `end`: An iterator identifying the items after permuting
///
/// # Returns
/// The items in `start` paired with a `PermutationOutput` specifying where they should be sent
pub(crate) fn generate_permutation<'a, T: Addr>(
    start: impl Iterator<Item = Link<T>> + 'a,
    end: impl Iterator<Item = Link<T>> + 'a,
) -> Vec<(Link<T>, PermutationOutput)> {
    // Create a mapping of edges to indices they appear in the output
    let mut end_map: HashMap<Link<T>, VecDeque<usize>> = HashMap::default();
    for (idx, x) in end.enumerate() {
        end_map.entry(x).or_default().push_back(idx);
    }

    // Initialise the output by assigning each edge to be deleted
    let mut out: Vec<_> = start.map(|x| (x, PermutationOutput::Deleted)).collect();

    // Pair up edges that need pairing
    for i in 0..out.len() {
        let k @ Link(x, dir) = &out[i].0;
        if *dir == Direction::Backward && !end_map.contains_key(k) {
            if let Some(j) = out
                .iter()
                .enumerate()
                .filter(|(_, (Link(y, dir), _))| y == x && *dir == Direction::Forward)
                .map(|(a, _)| a)
                .min_by_key(|a| a.abs_diff(i))
            {
                out[j].1 = PermutationOutput::Paired(i);
                out[i].1 = PermutationOutput::Paired(j);
            }
        }
    }

    // Don't delete edges that appear in the outputs
    for (k, output) in &mut out {
        if *output == PermutationOutput::Deleted {
            if let Some(u) = end_map.get_mut(k).and_then(VecDeque::pop_front) {
                *output = PermutationOutput::Output(u);
            }
        }
    }

    out
}
