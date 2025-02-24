use std::{collections::HashMap, fmt::Debug, hash::Hash};

use derivative::Derivative;
use num::rational::Ratio;

use crate::{
    common::{Direction, InOut, InOutIter, Link},
    hypergraph::generic::Ctx,
    monoidal::permutation::{PermutationOutput, generate_permutation},
};

pub mod graph;
pub mod permutation;
pub mod wired_graph;

////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Eq, PartialEq, Hash, Debug, Derivative)]
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
    type T = O::T;

    fn input_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<O::T>> + 'a> {
        Box::new(self.ops.iter().flat_map(InOutIter::input_links))
    }

    fn output_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<O::T>> + 'a> {
        Box::new(self.ops.iter().flat_map(InOutIter::output_links))
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

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Stores the decomposition of a hypergraph into layer by layer operations of type `O`
#[derive(Derivative)]
#[derivative(
    Clone(bound = "O: Clone"),
    Eq(bound = "O: Eq"),
    PartialEq(bound = "O: PartialEq"),
    Hash(bound = "O: Hash"),
    Debug(bound = "O: Debug")
)]
pub struct MonoidalTerm<T: Ctx, O> {
    /// Free inputs to the term
    pub free_inputs: Vec<T::Edge>,
    /// Bound inputs to the term which should not be reordered
    pub bound_inputs: Vec<T::Edge>,
    /// Layers of operations of type `O`
    pub slices: Vec<Slice<O>>,
    /// Free outputs of the term
    pub free_outputs: Vec<T::Edge>,
    /// Bound outputs of the term which should not be reordered
    pub bound_outputs: Vec<T::Edge>,
}

impl<T: Ctx, O: InOut + Debug> MonoidalTerm<T, O> {
    /// Check that each slice of a monoidal term has a consistent number of inputs and outputs
    pub(crate) fn check_in_out_count(&self) {
        let mut input_count = self.free_inputs.len() + self.bound_inputs.len();
        for slice in &self.slices {
            assert_eq!(
                input_count,
                slice.number_of_inputs(),
                "Input slice has the wrong number of inputs"
            );

            input_count = slice.number_of_outputs();
        }
        assert_eq!(
            input_count,
            self.free_outputs.len() + self.bound_outputs.len()
        );
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

impl<O: InOutIter + PartialEq + Eq + Hash + Clone + Debug> MonoidalTerm<O::T, O> {
    /// Reorder the operations on each slice of a monoidal term to attempt to reduce the amount of swapping
    pub fn minimise_swaps(&mut self, use_above: bool) {
        let mut edges_below = Box::new(
            self.free_outputs
                .iter()
                .chain(self.bound_outputs.iter())
                .map(|edge| (edge.clone(), Direction::Forward)),
        ) as Box<dyn Iterator<Item = Link<O::T>>>;
        let mut slices = self.slices.iter_mut().rev().peekable();

        while let Some(slice) = slices.next() {
            let edges_above = if use_above {
                slices.peek().map_or_else(
                    || {
                        Box::new(
                            self.free_inputs
                                .iter()
                                .chain(self.bound_inputs.iter())
                                .map(|edge| (edge.clone(), Direction::Forward)),
                        ) as Box<dyn Iterator<Item = Link<O::T>>>
                    },
                    |s| s.output_links(),
                )
            } else {
                Box::new(std::iter::empty())
            };

            slice.minimise_swaps(edges_above.into_iter(), edges_below);
            edges_below = slice.input_links();
        }

        let perm_map: HashMap<Link<O::T>, PermutationOutput> = generate_permutation::<O::T>(
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

impl<O: InOutIter + PartialEq + Eq + Hash + Clone + Debug> Slice<O> {
    /// Reorder the operations in a slice to try to reduce the number of swapping needed to link with the edges in `edges_below`
    pub fn minimise_swaps(
        &mut self,
        edges_above: impl Iterator<Item = Link<O::T>>,
        edges_below: impl Iterator<Item = Link<O::T>>,
    ) {
        let outputs = self.output_links();
        let mut edge_idx: HashMap<Link<O::T>, (usize, usize)> = HashMap::new();
        for (i, edge) in edges_above.enumerate() {
            let (x, y) = edge_idx.entry(edge).or_default();
            *x += i;
            *y += 1;
        }
        let mut perm_list_below = generate_permutation::<O::T>(outputs, edges_below);

        let perm_map: HashMap<O, Ratio<usize>> = self
            .ops
            .iter()
            .rev()
            .map(|op| {
                let outs = op.number_of_outputs();
                let (total_below, number_below) = perm_list_below
                    .split_off(perm_list_below.len() - outs)
                    .into_iter()
                    .filter_map(|(_, y)| Option::<usize>::from(y))
                    .fold((0, 0), |(a, b), c: usize| (a + c, b + 1));
                let (total_above, number_above) = op
                    .input_links()
                    .map(|x| edge_idx.get(&x).cloned().unwrap_or_default())
                    .fold((0, 0), |(a, b), (c, d)| (a + c, b + d));
                let ret = match (total_above + total_below, number_above + number_below) {
                    (0, 0) => Ratio::new(usize::MAX, 1),
                    (n, k) => Ratio::new(n, k),
                };
                (op.clone(), ret)
            })
            .collect();

        self.ops.sort_by_key(|op| perm_map[op]);
    }
}

impl<T: Ctx, O> MonoidalTerm<T, Slice<O>> {
    /// Flattens a monoidal term where each operation is itself a slice by inlining these slices
    #[must_use]
    pub fn flatten_graph(self) -> MonoidalTerm<T, O> {
        MonoidalTerm {
            free_inputs: self.free_inputs,
            bound_inputs: self.bound_inputs,
            slices: self.slices.into_iter().map(Slice::flatten_slice).collect(),
            free_outputs: self.free_outputs,
            bound_outputs: self.bound_outputs,
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
