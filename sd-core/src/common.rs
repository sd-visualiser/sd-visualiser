use std::{
    collections::{HashMap, VecDeque},
    hash::Hash,
};

use derivative::Derivative;
use num::rational::Ratio;
use tracing::debug;

use crate::hypergraph::{InPort, OutPort};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Direction {
    Forward,
    Backward,
}

pub type Link<V, E> = (OutPort<V, E>, Direction);

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

#[derive(Clone, Debug, PartialEq, Eq, Hash, Derivative)]
#[derivative(Default(bound = ""))]
pub struct MonoidalTerm<T: InOutIter> {
    pub unordered_inputs: Vec<OutPort<T::V, T::E>>,
    pub ordered_inputs: Vec<OutPort<T::V, T::E>>, // We need to make sure these don't get reordered
    pub slices: Vec<Slice<T>>,
    pub outputs: Vec<InPort<T::V, T::E>>,
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

pub(crate) fn advance_by<I: Iterator>(iter: &mut I, n: usize) {
    if n != 0 {
        iter.nth(n - 1);
    }
}

pub(crate) fn generate_permutation<'a, T: 'a>(
    start: impl Iterator<Item = T> + 'a,
    end: impl Iterator<Item = T> + 'a,
) -> impl Iterator<Item = (T, Option<usize>)> + 'a
where
    T: PartialEq + Eq + Hash,
{
    let mut end_map: HashMap<T, VecDeque<usize>> = HashMap::default();
    for (idx, x) in end.enumerate() {
        end_map.entry(x).or_default().push_back(idx);
    }

    start.map(move |x| {
        let index = end_map.get_mut(&x).and_then(VecDeque::pop_front);
        (x, index)
    })
}

impl<T: InOutIter> MonoidalTerm<T> {
    pub fn minimise_swaps(&mut self) {
        fn fold_slice<'a, T: InOutIter>(
            ports_below: Box<dyn Iterator<Item = Link<T::V, T::E>> + 'a>,
            slice: &'a mut Slice<T>,
        ) -> Box<dyn Iterator<Item = Link<T::V, T::E>> + 'a> {
            slice.minimise_swaps(ports_below);
            slice.inputs()
        }

        let ports_below = self.slices.iter_mut().rev().fold(
            Box::new(
                self.outputs
                    .iter()
                    .map(|in_port| (in_port.output(), Direction::Forward)),
            ) as Box<dyn Iterator<Item = Link<T::V, T::E>>>,
            fold_slice::<T>,
        );

        let perm_map: HashMap<OutPort<T::V, T::E>, Option<usize>> = generate_permutation(
            self.unordered_inputs.iter().cloned(),
            ports_below.map(|x| x.0),
        )
        .collect();

        self.unordered_inputs.sort_by_key(|out_port| {
            perm_map
                .get(out_port)
                .copied()
                .flatten()
                .unwrap_or(usize::MAX)
        });
    }
}

impl<T: InOutIter> Slice<T> {
    pub fn minimise_swaps(&mut self, ports_below: impl Iterator<Item = Link<T::V, T::E>>) {
        let outputs = self.outputs();

        let perm_map: HashMap<Link<T::V, T::E>, Option<usize>> =
            generate_permutation(outputs, ports_below).collect();

        debug!("Map: {:#?}", perm_map);

        self.ops.sort_by_cached_key(|op| -> Ratio<usize> {
            match op
                .outputs()
                .filter_map(|out_port| perm_map.get(&out_port).copied().flatten())
                .fold((0, 0), |(a, b), c| (a + c, b + 1))
            {
                (_, 0) => usize::MAX.into(),
                (x, y) => Ratio::new_raw(x, y),
            }
        });
    }
}

impl<T: InOutIter> MonoidalTerm<Slice<T>> {
    #[must_use]
    pub fn flatten_graph(self) -> MonoidalTerm<T> {
        let MonoidalTerm {
            unordered_inputs,
            ordered_inputs,
            slices,
            outputs,
        } = self;
        MonoidalTerm {
            unordered_inputs,
            ordered_inputs,
            slices: slices.into_iter().map(Slice::flatten_slice).collect(),
            outputs,
        }
    }
}

impl<T> Slice<Slice<T>> {
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
