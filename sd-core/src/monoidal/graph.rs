use derivative::Derivative;
use itertools::Itertools;
use tracing::debug;

use super::{
    permutation::{advance_by, generate_permutation, PermutationOutput},
    wired_graph::{MonoidalWiredGraph, WiredOp},
    MonoidalTerm, Slice,
};
use crate::{
    common::{Direction, InOut, InOutIter, Link},
    hypergraph::{generic::Ctx, traits::NodeLike},
};

impl<T: Ctx> Slice<MonoidalOp<T>> {
    /// From iterators of starting edges and ending edges, create slices containing caps, cups, and deletes
    /// The `downwards` argument determines whether we should make cups and deletes (if it is true) or caps
    pub fn insert_caps_cups_deletes(
        start_edges: impl Iterator<Item = Link<T>>,
        end_edges: impl Iterator<Item = Link<T>>,
        downwards: bool,
    ) -> Vec<Self> {
        let mut permutation: Vec<Option<(Link<T>, PermutationOutput)>> =
            generate_permutation::<T>(start_edges, end_edges)
                .into_iter()
                .map(Option::Some)
                .collect();

        let mut slices: Vec<Slice<MonoidalOp<T>>> = Vec::default();
        let mut finished = false;

        while !finished {
            finished = true;
            let mut perm_iter = permutation.into_iter().enumerate();
            permutation = Vec::default();

            let mut ops: Vec<MonoidalOp<T>> = Vec::default();

            while let Some((i, x)) = perm_iter.next() {
                if let Some((link, output)) = x {
                    match output {
                        PermutationOutput::Output(y) => {
                            ops.push(MonoidalOp::id_from_link(link.clone()));
                            permutation.push(Some((link, PermutationOutput::Output(y))));
                        }
                        PermutationOutput::Deleted => {
                            finished = false;
                            ops.push(MonoidalOp::Copy {
                                addr: link.0.clone(),
                                copies: 0,
                            });
                            permutation.push(None);
                        }
                        PermutationOutput::Paired(j) if i < j => {
                            finished = false;
                            permutation.push(None);
                            let mut intermediate = vec![];

                            for _ in i + 1..j {
                                let next = perm_iter.next().unwrap().1;
                                permutation.push(next.clone());
                                if let Some(next) = next {
                                    intermediate.push(next.0.clone());
                                }
                            }

                            assert_eq!(
                                perm_iter.next().unwrap().1.unwrap().1,
                                PermutationOutput::Paired(i)
                            );
                            permutation.push(None);

                            if downwards {
                                ops.push(MonoidalOp::Cup {
                                    addr: link,
                                    intermediate,
                                });
                            } else {
                                ops.push(MonoidalOp::Cap {
                                    addr: link,
                                    intermediate,
                                });
                            }
                        }
                        PermutationOutput::Paired(j) => {
                            finished = false;
                            ops.push(MonoidalOp::id_from_link(link.clone()));
                            permutation.push(Some((link, PermutationOutput::Paired(j))));
                        }
                    }
                } else {
                    permutation.push(None);
                }
            }
            if !finished {
                slices.push(Slice { ops });
            }
        }

        if !downwards {
            slices.reverse();
        }

        slices
    }

    /// From iterators of starting edges and ending edges, create slices containing swaps
    pub fn permutation_to_swaps(
        start_edges: impl Iterator<Item = Link<T>>,
        end_edges: impl Iterator<Item = Link<T>>,
    ) -> Vec<Self> {
        let permutation = generate_permutation::<T>(start_edges, end_edges)
            .into_iter()
            .map(|(x, y)| (x, Option::<usize>::from(y).unwrap()));

        let mut non_trivial = false;

        let mut slice_ops = Vec::new();

        let mut chunk_start = 0;
        let mut current_chunk_addr: Vec<Link<T>> = vec![];
        let mut current_chunk_out_to_in: Vec<usize> = vec![];

        for (link, destination) in permutation {
            let target = destination - chunk_start;

            while current_chunk_out_to_in.len() <= target {
                current_chunk_out_to_in.push(0);
            }

            current_chunk_out_to_in[target] = current_chunk_addr.len();
            current_chunk_addr.push(link);

            if current_chunk_out_to_in.len() == current_chunk_addr.len() {
                // Chunk is finished
                let addrs = std::mem::take(&mut current_chunk_addr);
                let out_to_in = std::mem::take(&mut current_chunk_out_to_in);

                chunk_start += addrs.len();

                if addrs.len() == 1 {
                    slice_ops.push(MonoidalOp::id_from_link(addrs.into_iter().next().unwrap()));
                } else {
                    slice_ops.push(MonoidalOp::Swap { addrs, out_to_in });
                    non_trivial = true;
                }
            }
        }
        if non_trivial {
            vec![Slice { ops: slice_ops }]
        } else {
            vec![]
        }
    }
}

/// A `MonoidalGraph` stores the operations of a hypergraph layer by layer
/// It stores the copies of the graph, as well as deletions, cups, and caps
///
/// In a `MonoidalGraph` the outputs of one slice should exactly line up with the inputs of the next slice
pub type MonoidalGraph<T> = MonoidalTerm<T, MonoidalOp<T>>;

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub enum MonoidalOp<T: Ctx> {
    Copy {
        addr: T::Edge,
        copies: usize,
    },
    Operation {
        addr: T::Operation,
    },
    Thunk {
        addr: T::Thunk,
        body: MonoidalGraph<T>,
    },
    Swap {
        addrs: Vec<Link<T>>,
        out_to_in: Vec<usize>,
    },
    Backlink {
        addr: T::Edge,
    },
    Cup {
        addr: Link<T>,
        intermediate: Vec<Link<T>>,
    },
    Cap {
        addr: Link<T>,
        intermediate: Vec<Link<T>>,
    },
}

impl<T: Ctx> MonoidalOp<T> {
    /// Create an identity (unary copy) or backlink from a `link`
    fn id_from_link(link: Link<T>) -> Self {
        if link.1 == Direction::Backward {
            MonoidalOp::Backlink { addr: link.0 }
        } else {
            MonoidalOp::Copy {
                addr: link.0,
                copies: 1,
            }
        }
    }

    /// Determines if `self` is an identity (unary copy) or backlink
    #[must_use]
    pub const fn is_id_or_backlink(&self) -> bool {
        match self {
            MonoidalOp::Copy { copies, .. } => *copies == 1,
            MonoidalOp::Backlink { .. } => true,
            _ => false,
        }
    }
}

impl<T: Ctx> InOut for MonoidalOp<T> {
    fn number_of_inputs(&self) -> usize {
        match self {
            Self::Copy { .. } | Self::Backlink { .. } => 1,
            Self::Operation { addr, .. } => addr.number_of_inputs(),
            Self::Thunk { addr, .. } => addr.number_of_inputs(),
            Self::Swap { addrs, .. } => addrs.len(),
            Self::Cup { intermediate, .. } => 2 + intermediate.len(),
            Self::Cap { intermediate, .. } => intermediate.len(),
        }
    }

    fn number_of_outputs(&self) -> usize {
        match self {
            Self::Copy { copies, .. } => *copies,
            Self::Operation { addr, .. } => addr.number_of_outputs(),
            Self::Thunk { addr, .. } => addr.number_of_outputs(),
            Self::Backlink { .. } => 1,
            Self::Swap { addrs, .. } => addrs.len(),
            Self::Cup { intermediate, .. } => intermediate.len(),
            Self::Cap { intermediate, .. } => 2 + intermediate.len(),
        }
    }
}

impl<T: Ctx> InOutIter for MonoidalOp<T> {
    type T = T;

    fn input_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<T>> + 'a> {
        match self {
            MonoidalOp::Copy { addr, .. } => {
                Box::new(std::iter::once((addr.clone(), Direction::Forward)))
            }
            MonoidalOp::Operation { addr, .. } => {
                Box::new(addr.inputs().map(|edge| (edge, Direction::Forward)))
            }
            MonoidalOp::Thunk { addr, body } => Box::new(
                body.free_inputs
                    .iter()
                    .cloned()
                    .chain(addr.inputs().skip(body.free_inputs.len()))
                    .map(|edge| (edge, Direction::Forward)),
            ),
            MonoidalOp::Swap { addrs, .. } => Box::new(addrs.iter().cloned()),
            MonoidalOp::Backlink { addr } => {
                Box::new(std::iter::once((addr.clone(), Direction::Backward)))
            }
            MonoidalOp::Cup { addr, intermediate } => Box::new(
                [
                    vec![addr.clone()],
                    intermediate.clone(),
                    vec![(addr.0.clone(), addr.1.flip())],
                ]
                .into_iter()
                .flatten(),
            ),
            MonoidalOp::Cap { intermediate, .. } => Box::new(intermediate.iter().cloned()),
        }
    }

    fn output_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<T>> + 'a> {
        match self {
            MonoidalOp::Copy { addr, copies } => {
                Box::new(std::iter::repeat((addr.clone(), Direction::Forward)).take(*copies))
            }
            MonoidalOp::Operation { addr, .. } => {
                Box::new(addr.outputs().map(|edge| (edge, Direction::Forward)))
            }
            MonoidalOp::Thunk { addr, body } => Box::new(
                body.free_outputs
                    .iter()
                    .cloned()
                    .chain(addr.outputs().skip(body.free_outputs.len()))
                    .map(|edge| (edge, Direction::Forward)),
            ),
            MonoidalOp::Swap { addrs, out_to_in } => {
                Box::new(out_to_in.iter().map(|idx| addrs[*idx].clone()))
            }
            MonoidalOp::Backlink { addr } => {
                Box::new(std::iter::once((addr.clone(), Direction::Backward)))
            }
            MonoidalOp::Cup { intermediate, .. } => Box::new(intermediate.iter().cloned()),
            MonoidalOp::Cap { addr, intermediate } => Box::new(
                [
                    vec![addr.clone()],
                    intermediate.clone(),
                    vec![(addr.0.clone(), addr.1.flip())],
                ]
                .into_iter()
                .flatten(),
            ),
        }
    }
}

impl<T: Ctx> From<&WiredOp<T>> for MonoidalOp<T> {
    fn from(op: &WiredOp<T>) -> Self {
        match op {
            WiredOp::Copy { addr, copies } => MonoidalOp::Copy {
                addr: addr.clone(),
                copies: *copies,
            },
            WiredOp::Operation { addr } => Self::Operation { addr: addr.clone() },
            WiredOp::Thunk { addr, body, .. } => Self::Thunk {
                addr: addr.clone(),
                body: body.into(),
            },
            WiredOp::Backlink { addr } => MonoidalOp::Backlink { addr: addr.clone() },
        }
    }
}

/// Builder to help build monoidal graphs
struct MonoidalGraphBuilder<T: Ctx> {
    /// Slices that have been added
    slices: Vec<Slice<MonoidalOp<T>>>,
    /// Stores the edges at the bottom of the last slice, or the global inputs if there are no slices
    open_edges: Vec<Link<T>>,
}

impl<T: Ctx> MonoidalGraphBuilder<T> {
    /// Returns an iterator over the open edges of the builder
    pub(crate) fn open_edges(&self) -> impl Iterator<Item = Link<T>> + '_ {
        self.open_edges.iter().cloned()
    }
}

impl<T: Ctx> Extend<Slice<MonoidalOp<T>>> for MonoidalGraphBuilder<T> {
    fn extend<I: IntoIterator<Item = Slice<MonoidalOp<T>>>>(&mut self, iter: I) {
        let mut peeking = iter.into_iter().peekable();
        if peeking.peek().is_some() {
            self.slices.extend(peeking);
            self.open_edges = self.slices.last().unwrap().output_links().collect();
        }
    }
}

impl<T: Ctx> From<&MonoidalWiredGraph<T>> for MonoidalGraph<T> {
    fn from(graph: &MonoidalWiredGraph<T>) -> Self {
        debug!("Input graph {:#?}", graph);
        let graph_inputs: Vec<Link<T>> = graph
            .free_inputs
            .iter()
            .chain(graph.bound_inputs.iter())
            .map(|edge| (edge.clone(), Direction::Forward))
            .collect();

        // Initialise the open edges to the global inputs of the graph
        let mut builder = MonoidalGraphBuilder {
            open_edges: graph_inputs,
            slices: vec![],
        };

        for next_slice in &graph.slices {
            // Generate slices for caps
            let end_slices = Slice::insert_caps_cups_deletes(
                next_slice.input_links(),
                builder.open_edges(),
                false,
            );

            // Closure to obtain the target links before cap generation
            let next_inputs = || {
                end_slices
                    .get(0)
                    .map_or(next_slice.input_links(), InOutIter::input_links)
            };

            // Add cups and deletes
            builder.extend(Slice::insert_caps_cups_deletes(
                builder.open_edges(),
                next_inputs(),
                true,
            ));

            // Add swap layer
            builder.extend(Slice::permutation_to_swaps(
                builder.open_edges(),
                next_inputs(),
            ));

            // Add caps
            builder.extend(end_slices);

            // Add the next operation layer
            builder.extend([next_slice.into()]);
        }

        // Add final cups and deletes
        builder.extend(Slice::insert_caps_cups_deletes(
            builder.open_edges(),
            graph
                .free_outputs
                .iter()
                .chain(graph.bound_outputs.iter())
                .map(|edge| (edge.clone(), Direction::Forward)),
            true,
        ));

        // add final swap layer
        builder.extend(Slice::permutation_to_swaps(
            builder.open_edges(),
            graph
                .free_outputs
                .iter()
                .chain(graph.bound_outputs.iter())
                .map(|edge| (edge.clone(), Direction::Forward)),
        ));

        let mut graph = MonoidalGraph {
            free_inputs: graph.free_inputs.clone(),
            bound_inputs: graph.bound_inputs.clone(),
            slices: builder.slices,
            free_outputs: graph.free_outputs.clone(),
            bound_outputs: graph.bound_outputs.clone(),
        };

        debug!("Monoidal Term: {:#?}", graph);

        // Perform sanity check
        graph.check_in_out_count();

        // Perform local optimisations to graph
        graph.squash_layers();

        // Recheck sanity
        graph.check_in_out_count();

        graph
    }
}

impl<T: Ctx> MonoidalGraph<T> {
    /// Perform local optimisations on a `MonoidalGraph` to try to shrink the number of layers
    fn squash_layers(&mut self) {
        self.slices = std::mem::take(&mut self.slices)
            .into_iter()
            .rev()
            .coalesce(|x, y| {
                if y.check_mergeablity(&x) {
                    Ok(y.merge_slice(x))
                } else {
                    Err((x, y))
                }
            })
            .collect();

        self.slices.reverse();
    }
}

impl<T: Ctx> Slice<MonoidalOp<T>> {
    /// Check if two layers are canonically mergable into one layer
    fn check_mergeablity(&self, other: &Self) -> bool {
        let mut first_iter = self.ops.iter();
        let mut second_iter = other.ops.iter();

        let mut canonical = true;

        while let Some(first) = first_iter.next() {
            if first.number_of_outputs() == 0 {
                canonical = false;
                continue;
            }
            let second = if canonical {
                second_iter.find(|op| op.number_of_inputs() != 0).unwrap()
            } else {
                canonical = true;
                second_iter.next().unwrap()
            };
            if first.is_id_or_backlink() {
                for _ in 1..second.number_of_inputs() {
                    if first_iter.next().map(MonoidalOp::is_id_or_backlink) != Some(true) {
                        return false;
                    }
                }
            } else {
                if !second.is_id_or_backlink() {
                    return false;
                }
                for _ in 1..first.number_of_outputs() {
                    if second_iter.next().map(MonoidalOp::is_id_or_backlink) != Some(true) {
                        return false;
                    }
                }
            }
        }

        canonical || second_iter.next().is_none()
    }

    /// Merge two slices under the assumption they are mergable
    fn merge_slice(self, other: Self) -> Self {
        // Assume that slices are mergeable
        let mut first_iter = self.ops.into_iter();
        let mut second_iter = other.ops.into_iter();

        let mut ops = Vec::new();

        while let Some(op) = first_iter.next() {
            if op.number_of_outputs() == 0 {
                ops.push(op);
            } else {
                let mut second = second_iter.next().unwrap();

                while second.number_of_inputs() == 0 {
                    ops.push(second);
                    second = second_iter.next().unwrap();
                }

                if op.is_id_or_backlink() {
                    advance_by(&mut first_iter, second.number_of_inputs() - 1);
                    ops.push(second);
                } else {
                    advance_by(&mut second_iter, op.number_of_outputs() - 1);
                    ops.push(op);
                }
            }
        }

        ops.extend(second_iter);

        Slice { ops }
    }
}

#[cfg(test)]
mod tests {
    // use anyhow::Result;
    // use rstest::rstest;

    // use super::*;
    // use crate::language::spartan::Op;

    // #[rstest]
    // #[case(vec![0,1], vec![])]
    // #[case(vec![1,0], vec![Slice { ops: vec![MonoidalOp::Swap]}])]
    // #[case(vec![1,2,0], vec![Slice { ops: vec![MonoidalOp::ID, MonoidalOp::Swap]}, Slice { ops: vec![MonoidalOp::Swap, MonoidalOp::ID]}])]
    // fn test_permutation(
    //     #[case] permutation: Vec<usize>,
    //     #[case] result: Vec<Slice<MonoidalOp<Op>>>,
    // ) -> Result<()> {
    //     assert_eq!(Slice::permutation_to_swaps(permutation), result);
    //     Ok(())
    // }
}
