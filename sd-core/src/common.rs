use std::{
    collections::{HashMap, VecDeque},
    fmt::Debug,
    hash::Hash,
};

use derivative::Derivative;
use num::rational::Ratio;
use tracing::debug;

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

pub type Link<V, E> = (Edge<V, E>, Direction);

/// Specifies an operation which has inputs and outputs.
pub trait InOut {
    fn number_of_inputs(&self) -> usize;
    fn number_of_outputs(&self) -> usize;
}

pub trait InOutIter {
    type V;
    type E;
    fn inputs<'a>(&'a self) -> Box<dyn Iterator<Item = Link<Self::V, Self::E>> + 'a>;
    fn outputs<'a>(&'a self) -> Box<dyn Iterator<Item = Link<Self::V, Self::E>> + 'a>;
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Derivative)]
#[derivative(Default(bound = ""))]
pub struct Slice<O> {
    pub ops: Vec<O>,
}

impl<O: InOut> InOut for Slice<O> {
    fn number_of_inputs(&self) -> usize {
        self.ops.iter().map(InOut::number_of_inputs).sum()
    }

    fn number_of_outputs(&self) -> usize {
        self.ops.iter().map(InOut::number_of_outputs).sum()
    }
}

impl<O: InOutIter> InOutIter for Slice<O> {
    type V = O::V;
    type E = O::E;

    fn inputs<'a>(&'a self) -> Box<dyn Iterator<Item = Link<O::V, O::E>> + 'a> {
        Box::new(self.ops.iter().flat_map(InOutIter::inputs))
    }

    fn outputs<'a>(&'a self) -> Box<dyn Iterator<Item = Link<O::V, O::E>> + 'a> {
        Box::new(self.ops.iter().flat_map(InOutIter::outputs))
    }
}

/// Stores the decomposition of a hypergraph into layer by layer operations of type `O`
#[derive(Derivative)]
#[derivative(
    Clone(bound = "O: Clone"),
    Eq(bound = "O: Eq"),
    PartialEq(bound = "O: PartialEq"),
    Hash(bound = "O: Hash"),
    Debug(bound = "T::Edge: Debug, O: Debug"),
    Default(bound = "")
)]
pub struct MonoidalTerm<T: Addr, O> {
    /// Free inputs to the term
    pub free_inputs: Vec<T::Edge>,
    /// Bound inputs to the term which should not be reordered
    pub bound_inputs: Vec<T::Edge>,
    /// Layers of operations of type `O`
    pub slices: Vec<Slice<O>>,
    /// Outputs of the term
    pub outputs: Vec<T::Edge>,
}

impl<T: Addr, O: InOut + Debug> MonoidalTerm<T, O> {
    /// Check that each slice of a monoidal term has a consistent number of inputs and outputs
    pub(crate) fn check_in_out_count(&self) {
        let mut input_count = self.free_inputs.len() + self.bound_inputs.len();
        for slice in &self.slices {
            assert!(
                input_count == slice.number_of_inputs(),
                "{slice:?} has the wrong number of inputs"
            );

            input_count = slice.number_of_outputs();
        }
        assert_eq!(input_count, self.outputs.len());
    }
}

impl<'a, A, B> From<&'a Slice<A>> for Slice<B>
where
    B: From<&'a A>,
{
    fn from(value: &'a Slice<A>) -> Self {
        Self {
            ops: value.ops.iter().map(B::from).collect(),
        }
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
pub(crate) fn generate_permutation<'a, T>(
    start: impl Iterator<Item = (T, Direction)> + 'a,
    end: impl Iterator<Item = (T, Direction)> + 'a,
) -> Vec<((T, Direction), PermutationOutput)>
where
    T: 'a + PartialEq + Eq + Hash,
{
    // Create a mapping of edges to indices they appear in the output
    let mut end_map: HashMap<(T, Direction), VecDeque<usize>> = HashMap::default();
    for (idx, x) in end.enumerate() {
        end_map.entry(x).or_default().push_back(idx);
    }

    // Initialise the output by assigning each edge to be deleted
    let mut out: Vec<_> = start.map(|x| (x, PermutationOutput::Deleted)).collect();

    // Pair up edges that need pairing
    for i in 0..out.len() {
        let k @ (x, dir) = &out[i].0;
        if *dir == Direction::Backward && !end_map.contains_key(k) {
            if let Some(j) = out
                .iter()
                .enumerate()
                .filter(|(_, ((y, dir), _))| y == x && *dir == Direction::Forward)
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

impl<T: InOutIter> MonoidalTerm<(T::V, T::E), T>
where
    T::E: Debug,
{
    /// Reorder the operations on each slice of a monoidal term to attempt to reduce the amount of swapping
    pub fn minimise_swaps(&mut self) {
        fn fold_slice<'a, T: InOutIter>(
            edges_below: Box<dyn Iterator<Item = Link<T::V, T::E>> + 'a>,
            slice: &'a mut Slice<T>,
        ) -> Box<dyn Iterator<Item = Link<T::V, T::E>> + 'a>
        where
            T::E: Debug,
        {
            slice.minimise_swaps(edges_below);
            slice.inputs()
        }

        let edges_below = self.slices.iter_mut().rev().fold(
            Box::new(
                self.outputs
                    .iter()
                    .map(|edge| (edge.clone(), Direction::Forward)),
            ) as Box<dyn Iterator<Item = Link<T::V, T::E>>>,
            fold_slice::<T>,
        );

        let perm_map: HashMap<Link<T::V, T::E>, PermutationOutput> = generate_permutation(
            self.free_inputs
                .iter()
                .cloned()
                .map(|edge| (edge, Direction::Forward)),
            edges_below,
        )
        .into_iter()
        .collect();

        self.free_inputs.sort_by_key(|edge| {
            perm_map
                .get(&(edge.clone(), Direction::Forward))
                .copied()
                .and_then(Into::into)
                .unwrap_or(usize::MAX)
        });
    }
}

impl<T: InOutIter> Slice<T>
where
    T::E: Debug,
{
    /// Reorder the operations in a slice to try to reduce the number of swapping needed to link with the edges in `edges_below`
    pub fn minimise_swaps(&mut self, edges_below: impl Iterator<Item = Link<T::V, T::E>>) {
        let outputs = self.outputs();

        let perm_map: HashMap<Link<T::V, T::E>, PermutationOutput> =
            generate_permutation(outputs, edges_below)
                .into_iter()
                .collect();

        debug!("Map: {:#?}", perm_map);

        self.ops.sort_by_cached_key(|op| -> Ratio<usize> {
            match op
                .outputs()
                .filter_map(|link| perm_map.get(&link).copied().and_then(Into::into))
                .fold((0, 0), |(a, b), c: usize| (a + c, b + 1))
            {
                (_, 0) => usize::MAX.into(),
                (x, y) => Ratio::new_raw(x, y),
            }
        });
    }
}

impl<T: Addr, O> MonoidalTerm<T, Slice<O>> {
    /// Flattens a monoidal term where each operation is itself a slice by inlining these slices
    #[must_use]
    pub fn flatten_graph(self) -> MonoidalTerm<T, O> {
        let MonoidalTerm {
            free_inputs: unordered_inputs,
            bound_inputs: ordered_inputs,
            slices,
            outputs,
        } = self;
        MonoidalTerm {
            free_inputs: unordered_inputs,
            bound_inputs: ordered_inputs,
            slices: slices.into_iter().map(Slice::flatten_slice).collect(),
            outputs,
        }
    }
}

impl<T> Slice<Slice<T>> {
    /// Flattens a slice where each operation is itself a slice by inlining the slices
    #[must_use]
    pub fn flatten_slice(self) -> Slice<T> {
        Slice {
            ops: self
                .ops
                .into_iter()
                .flat_map(|slice| slice.ops.into_iter())
                .collect(),
        }
    }
}
