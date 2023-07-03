use std::{collections::HashMap, fmt::Debug, hash::Hash};

use derivative::Derivative;
use num::rational::Ratio;
use tracing::debug;

use crate::common::{
    generate_permutation, Addr, Direction, InOut, InOutIter, Link, PermutationOutput,
};

pub mod graph;
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
    Debug(bound = "T::Edge: Debug, O: Debug")
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

////////////////////////////////////////////////////////////////////////////////////////////////////

impl<O: InOutIter> MonoidalTerm<O::T, O>
where
    <O::T as Addr>::Edge: Debug,
{
    /// Reorder the operations on each slice of a monoidal term to attempt to reduce the amount of swapping
    pub fn minimise_swaps(&mut self) {
        fn fold_slice<'a, O: InOutIter>(
            edges_below: Box<dyn Iterator<Item = Link<O::T>> + 'a>,
            slice: &'a mut Slice<O>,
        ) -> Box<dyn Iterator<Item = Link<O::T>> + 'a>
        where
            <O::T as Addr>::Edge: Debug,
        {
            slice.minimise_swaps(edges_below);
            slice.input_links()
        }

        let edges_below = self.slices.iter_mut().rev().fold(
            Box::new(
                self.outputs
                    .iter()
                    .map(|edge| Link(edge.clone(), Direction::Forward)),
            ) as Box<dyn Iterator<Item = Link<O::T>>>,
            fold_slice::<O>,
        );

        let perm_map: HashMap<Link<O::T>, PermutationOutput> = generate_permutation(
            self.free_inputs
                .iter()
                .cloned()
                .map(|edge| Link(edge, Direction::Forward)),
            edges_below,
        )
        .into_iter()
        .collect();

        self.free_inputs.sort_by_key(|edge| {
            perm_map
                .get(&Link(edge.clone(), Direction::Forward))
                .copied()
                .and_then(Into::into)
                .unwrap_or(usize::MAX)
        });
    }
}

impl<O: InOutIter> Slice<O>
where
    <O::T as Addr>::Edge: Debug,
{
    /// Reorder the operations in a slice to try to reduce the number of swapping needed to link with the edges in `edges_below`
    pub fn minimise_swaps(&mut self, edges_below: impl Iterator<Item = Link<O::T>>) {
        let outputs = self.output_links();

        let perm_map: HashMap<Link<O::T>, PermutationOutput> =
            generate_permutation(outputs, edges_below)
                .into_iter()
                .collect();

        debug!("Map: {:#?}", perm_map);

        self.ops.sort_by_cached_key(|op| -> Ratio<usize> {
            match op
                .output_links()
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
