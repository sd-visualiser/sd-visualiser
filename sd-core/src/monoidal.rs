use std::fmt::Debug;

use derivative::Derivative;
use itertools::Itertools;
use tracing::debug;

use crate::{
    common::{
        advance_by, generate_permutation, Addr, Direction, InOut, InOutIter, Link, MonoidalTerm,
        PermutationOutput, Slice,
    },
    monoidal_wired::{MonoidalWiredGraph, WiredOp},
};

impl<T: Addr> Slice<MonoidalOp<T>>
where
    T::Operation: Debug,
    T::Thunk: Debug,
    T::Edge: Debug,
{
    pub fn insert_caps_cups_deletes(
        start_ports: impl Iterator<Item = (T::Edge, Direction)>,
        end_ports: impl Iterator<Item = (T::Edge, Direction)>,
        downwards: bool,
    ) -> Vec<Self>
    where
        T::Edge: Debug,
    {
        #[allow(clippy::type_complexity)]
        let mut permutation: Vec<Option<((T::Edge, Direction), PermutationOutput)>> =
            generate_permutation(start_ports, end_ports)
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

    pub fn permutation_to_swaps(
        start_ports: impl Iterator<Item = (T::Edge, Direction)>,
        end_ports: impl Iterator<Item = (T::Edge, Direction)>,
    ) -> Vec<Self> {
        let permutation = generate_permutation(start_ports, end_ports)
            .into_iter()
            .map(|(x, y)| (x, Option::<usize>::from(y).unwrap()));

        let mut non_trivial = false;

        let mut slice_ops = Vec::new();

        let mut chunk_start = 0;
        let mut current_chunk_addr: Vec<(T::Edge, Direction)> = vec![];
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

pub type MonoidalGraph<T> = MonoidalTerm<T, MonoidalOp<T>>;

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "T::Edge: Debug, T::Thunk: Debug, T::Operation: Debug")
)]
pub enum MonoidalOp<T: Addr> {
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
        addrs: Vec<(T::Edge, Direction)>,
        out_to_in: Vec<usize>,
    },
    Backlink {
        addr: T::Edge,
    },
    Cup {
        addr: (T::Edge, Direction),
        intermediate: Vec<(T::Edge, Direction)>,
    },
    Cap {
        addr: (T::Edge, Direction),
        intermediate: Vec<(T::Edge, Direction)>,
    },
}

impl<T: Addr> MonoidalOp<T> {
    fn id_from_link(link: (T::Edge, Direction)) -> Self {
        if link.1 == Direction::Backward {
            MonoidalOp::Backlink { addr: link.0 }
        } else {
            MonoidalOp::Copy {
                addr: link.0,
                copies: 1,
            }
        }
    }

    #[must_use]
    pub fn is_id_or_backlink(&self) -> bool {
        match self {
            MonoidalOp::Copy { copies, .. } => *copies == 1,
            MonoidalOp::Backlink { .. } => true,
            _ => false,
        }
    }
}

impl<T: Addr> InOut for MonoidalOp<T> {
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

impl<V, E> InOutIter for MonoidalOp<(V, E)> {
    type V = V;
    type E = E;

    fn inputs<'a>(&'a self) -> Box<dyn Iterator<Item = Link<V, E>> + 'a> {
        match self {
            MonoidalOp::Copy { addr, .. } => {
                Box::new(std::iter::once((addr.clone(), Direction::Forward)))
            }
            MonoidalOp::Operation { addr, .. } => {
                Box::new(addr.inputs().map(|out_port| (out_port, Direction::Forward)))
            }
            MonoidalOp::Thunk { body, .. } => Box::new(Box::new(
                body.free_inputs
                    .iter()
                    .map(|out_port| (out_port.clone(), Direction::Forward)),
            )),
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

    fn outputs<'a>(&'a self) -> Box<dyn Iterator<Item = Link<V, E>> + 'a> {
        match self {
            MonoidalOp::Copy { addr, copies } => {
                Box::new(std::iter::repeat((addr.clone(), Direction::Forward)).take(*copies))
            }
            MonoidalOp::Operation { addr, .. } => {
                Box::new(addr.outputs().map(|x| (x, Direction::Forward)))
            }
            MonoidalOp::Thunk { addr, .. } => Box::new(Box::new(
                addr.outputs().map(|edge| (edge, Direction::Forward)),
            )),
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

impl<V: Debug, E: Debug> From<&WiredOp<V, E>> for MonoidalOp<(V, E)> {
    fn from(op: &WiredOp<V, E>) -> Self {
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

struct MonoidalGraphBuilder<V, E> {
    open_ports: Vec<Link<V, E>>,
    pub(crate) slices: Vec<Slice<MonoidalOp<(V, E)>>>,
}

impl<V, E> MonoidalGraphBuilder<V, E> {
    pub(crate) fn open_ports(&self) -> impl Iterator<Item = Link<V, E>> + '_ {
        self.open_ports.iter().cloned()
    }
}

impl<V, E> Extend<Slice<MonoidalOp<(V, E)>>> for MonoidalGraphBuilder<V, E> {
    fn extend<T: IntoIterator<Item = Slice<MonoidalOp<(V, E)>>>>(&mut self, iter: T) {
        let mut peeking = iter.into_iter().peekable();
        if peeking.peek().is_some() {
            self.slices.extend(peeking);
            self.open_ports = self.slices.last().unwrap().outputs().collect();
        }
    }
}

impl<V: Debug, E: Debug> From<&MonoidalWiredGraph<V, E>> for MonoidalGraph<(V, E)> {
    fn from(graph: &MonoidalWiredGraph<V, E>) -> Self {
        debug!("Input graph {:#?}", graph);
        let graph_inputs: Vec<Link<V, E>> = graph
            .free_inputs
            .iter()
            .chain(graph.bound_inputs.iter())
            .map(|out_port| (out_port.clone(), Direction::Forward))
            .collect();

        let mut builder = MonoidalGraphBuilder {
            open_ports: graph_inputs,
            slices: vec![],
        };

        for next_slice in &graph.slices {
            builder.extend(Slice::insert_caps_cups_deletes(
                builder.open_ports(),
                next_slice.inputs(),
                true,
            ));

            let end_slices =
                Slice::insert_caps_cups_deletes(next_slice.inputs(), builder.open_ports(), false);

            builder.extend(Slice::permutation_to_swaps(
                builder.open_ports(),
                end_slices
                    .get(0)
                    .map_or(next_slice.inputs(), InOutIter::inputs),
            ));

            builder.extend(end_slices);

            builder.extend([next_slice.into()]);
        }

        builder.extend(Slice::insert_caps_cups_deletes(
            builder.open_ports(),
            graph
                .outputs
                .iter()
                .map(|edge| (edge.clone(), Direction::Forward)),
            true,
        ));

        builder.extend(Slice::permutation_to_swaps(
            builder.open_ports(),
            graph
                .outputs
                .iter()
                .map(|edge| (edge.clone(), Direction::Forward)),
        ));

        let mut graph = MonoidalGraph {
            free_inputs: graph.free_inputs.clone(),
            bound_inputs: graph.bound_inputs.clone(),
            slices: builder.slices,
            outputs: graph.outputs.clone(),
        };

        debug!("Monoidal Term: {:#?}", graph);

        graph.check_in_out_count();

        graph.squash_layers();

        graph.check_in_out_count();

        graph
    }
}

impl<T: Addr> MonoidalGraph<T>
where
    T::Operation: Debug,
    T::Thunk: Debug,
    T::Edge: Debug,
{
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

impl<T: Addr> Slice<MonoidalOp<T>>
where
    T::Operation: Debug,
    T::Thunk: Debug,
    T::Edge: Debug,
{
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
    use anyhow::Result;
    use rstest::rstest;

    use super::*;
    use crate::language::spartan::Op;

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
