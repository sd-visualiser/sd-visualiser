use std::fmt::Debug;

use derivative::Derivative;
use itertools::Itertools;

use crate::{
    common::{
        advance_by, generate_permutation, Addr, Direction, InOut, InOutIter, Link, MonoidalTerm,
        PermutationOutput, Slice,
    },
    hypergraph::OutPort,
    monoidal_wired::{MonoidalWiredGraph, WiredOp},
};

impl<T: Addr> Slice<MonoidalOp<T>> {
    pub fn insert_caps_cups_deletes(
        start_ports: impl Iterator<Item = (T::OutPort, Direction)>,
        end_ports: impl Iterator<Item = (T::OutPort, Direction)>,
        downwards: bool,
    ) -> Vec<Self>
    where
        T::OutPort: Debug,
    {
        #[allow(clippy::type_complexity)]
        let mut permutation: Vec<Option<((T::OutPort, Direction), PermutationOutput)>> =
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
        start_ports: impl Iterator<Item = (T::OutPort, Direction)>,
        end_ports: impl Iterator<Item = (T::OutPort, Direction)>,
    ) -> Vec<Self> {
        let mut permutation: Vec<((T::OutPort, Direction), usize)> =
            generate_permutation(start_ports, end_ports)
                .into_iter()
                .map(|(x, y)| (x, Option::<usize>::from(y).unwrap()))
                .collect();

        let mut finished = false;

        let mut slices = Vec::new();

        while !finished {
            let mut slice_ops = Vec::new();
            let mut permutation_iter = permutation.iter_mut().peekable();
            finished = true;

            while let Some(s) = permutation_iter.next() {
                match permutation_iter.peek() {
                    Some((port, x)) if &s.1 > x => {
                        finished = false;
                        slice_ops.push(MonoidalOp::Swap {
                            addrs: vec![s.0.clone(), port.clone()],
                            out_to_in: vec![1, 0],
                        });
                        let t = permutation_iter.next().unwrap();
                        std::mem::swap(s, t);
                    }
                    _ => {
                        slice_ops.push(MonoidalOp::id_from_link(s.0.clone()));
                    }
                }
            }

            if !finished {
                slices.push(Slice { ops: slice_ops });
            }
        }

        slices
    }
}

pub type MonoidalGraph<T> = MonoidalTerm<T, MonoidalOp<T>>;

// impl<V, E> MonoidalGraph<O> {
//     pub fn selected(&self) -> Option<(Vec<NodeIndex>, HashSet<NodeIndex>)> {
//         // Addresses of selected nodes.
//         let mut selections = HashSet::default();

//         // Internal selection state (prefix and addresses) of the first selected thunk.
//         let mut thunk = None;

//         for slice in &self.slices {
//             for op in &slice.ops {
//                 match op {
//                     MonoidalOp::Operation { addr, selected, .. } => {
//                         if *selected {
//                             selections.insert(*addr);
//                         }
//                     }
//                     MonoidalOp::Thunk { addr, body, .. } => {
//                         if let Some((mut prefix, subselections)) = body.selected() {
//                             selections.insert(*addr);

//                             if thunk.is_none() {
//                                 prefix.insert(0, *addr);
//                                 thunk = Some((prefix, subselections));
//                             }
//                         }
//                     }
//                     _ => {}
//                 }
//             }
//         }

//         match (selections.len(), thunk) {
//             (0, _) => None,
//             (1, Some((prefix, subselections))) => Some((prefix, subselections)),
//             _ => Some((vec![], selections)),
//         }
//     }
// }

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "T::InPort: Debug, T::OutPort: Debug, T::Thunk: Debug, T::Operation: Debug")
)]
pub enum MonoidalOp<T: Addr> {
    Copy {
        addr: T::OutPort,
        copies: usize,
    },
    Operation {
        addr: T::Operation,
    },
    Thunk {
        addr: T::Thunk,
        body: MonoidalGraph<T>,
        expanded: bool,
    },
    Swap {
        addrs: Vec<(T::OutPort, Direction)>,
        out_to_in: Vec<usize>,
    },
    Backlink {
        addr: T::OutPort,
    },
    Cup {
        addr: (T::OutPort, Direction),
        intermediate: Vec<(T::OutPort, Direction)>,
    },
    Cap {
        addr: (T::OutPort, Direction),
        intermediate: Vec<(T::OutPort, Direction)>,
    },
}

impl<T: Addr> MonoidalOp<T> {
    fn id_from_link(link: (T::OutPort, Direction)) -> Self {
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
            Self::Swap { .. } => 2,
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
            Self::Swap { .. } => 2,
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
            MonoidalOp::Operation { addr, .. } => Box::new(
                addr.inputs()
                    .map(|in_port| (in_port.link(), Direction::Forward)),
            ),
            MonoidalOp::Thunk { addr, body, .. } => Box::new(
                body.unordered_inputs
                    .iter()
                    .chain(body.ordered_inputs.iter())
                    .filter_map(|port| {
                        addr.externalise_input(port)
                            .map(|x| (x.link(), Direction::Forward))
                    }),
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

    fn outputs<'a>(&'a self) -> Box<dyn Iterator<Item = Link<V, E>> + 'a> {
        match self {
            MonoidalOp::Copy { addr, copies } => {
                Box::new(std::iter::repeat((addr.clone(), Direction::Forward)).take(*copies))
            }
            MonoidalOp::Operation { addr, .. } => {
                Box::new(addr.outputs().map(|x| (x, Direction::Forward)))
            }
            MonoidalOp::Thunk { addr, body, .. } => Box::new(
                body.outputs
                    .iter()
                    .map(|port| (addr.externalise_output(port).unwrap(), Direction::Forward)),
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

impl<V, E> From<&WiredOp<V, E>> for MonoidalOp<(V, E)> {
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
                expanded: true,
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

impl<V, E> From<&MonoidalWiredGraph<V, E>> for MonoidalGraph<(V, E)> {
    fn from(graph: &MonoidalWiredGraph<V, E>) -> Self {
        let inputs: Vec<OutPort<V, E>> = graph
            .unordered_inputs
            .iter()
            .chain(graph.ordered_inputs.iter())
            .cloned()
            .collect();

        let graph_inputs: Vec<Link<V, E>> = inputs
            .iter()
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

            builder.extend(Slice::permutation_to_swaps(
                builder.open_ports(),
                next_slice.inputs(),
            ));

            builder.extend(Slice::insert_caps_cups_deletes(
                next_slice.inputs(),
                builder.open_ports(),
                false,
            ));

            builder.extend([next_slice.into()]);
        }

        builder.extend(Slice::insert_caps_cups_deletes(
            builder.open_ports(),
            graph
                .outputs
                .iter()
                .map(|in_port| (in_port.link(), Direction::Forward)),
            true,
        ));

        builder.extend(Slice::permutation_to_swaps(
            builder.open_ports(),
            graph
                .outputs
                .iter()
                .map(|in_port| (in_port.link(), Direction::Forward)),
        ));

        let mut graph = MonoidalGraph {
            unordered_inputs: vec![],
            ordered_inputs: inputs,
            slices: builder.slices,
            outputs: graph.outputs.clone(),
        };

        graph.squash_layers();

        graph
    }
}

impl<T: Addr> MonoidalGraph<T> {
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

impl<T: Addr> Slice<MonoidalOp<T>> {
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
