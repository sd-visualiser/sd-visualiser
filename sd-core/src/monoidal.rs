use itertools::Itertools;
use std::fmt::Debug;

use crate::{
    common::{advance_by, generate_permutation, Direction, InOut, Link, MonoidalTerm, Slice},
    hypergraph::{Operation, OutPort, Thunk},
    monoidal_wired::{MonoidalWiredGraph, WiredOp},
};

impl<V, E> Slice<MonoidalOp<V, E>> {
    pub fn permutation_to_swaps(
        start_ports: impl Iterator<Item = (OutPort<V, E>, Direction)>,
        end_ports: impl Iterator<Item = (OutPort<V, E>, Direction)>,
    ) -> Vec<Self> {
        let mut perm_iter = generate_permutation(start_ports, end_ports).peekable();

        let mut permutation: Vec<(Link<V, E>, usize)> = Vec::new();

        let mut finished = true;
        let mut slice_ops = Vec::new();

        while let Some((port_1, option)) = perm_iter.next() {
            if let Some(x) = option {
                match perm_iter.peek() {
                    Some((port_2, Some(y))) if x > *y => {
                        finished = false;
                        slice_ops.push(MonoidalOp::Swap {
                            out_1: port_1.clone(),
                            out_2: port_2.clone(),
                        });
                        let Some((port_2, Some(y))) = perm_iter.next() else { unreachable!() };
                        permutation.push((port_2, y));
                        permutation.push((port_1, x));
                    }
                    _ => {
                        slice_ops.push(MonoidalOp::Copy {
                            addr: port_1.0.clone(),
                            copies: 1,
                        });
                        permutation.push((port_1, x));
                    }
                }
            } else {
                finished = false;
                slice_ops.push(MonoidalOp::Copy {
                    addr: port_1.0.clone(),
                    copies: 0,
                });
            }
        }

        let mut slices = Vec::new();

        if !finished {
            slices.push(Slice { ops: slice_ops });
        }

        while !finished {
            let mut permutation_iter = permutation.iter_mut().peekable();
            finished = true;
            slice_ops = Vec::new();

            while let Some(s) = permutation_iter.next() {
                match permutation_iter.peek() {
                    Some((port, x)) if &s.1 > x => {
                        finished = false;
                        slice_ops.push(MonoidalOp::Swap {
                            out_1: s.0.clone(),
                            out_2: port.clone(),
                        });
                        let t = permutation_iter.next().unwrap();
                        std::mem::swap(s, t);
                    }
                    _ => {
                        slice_ops.push(MonoidalOp::Copy {
                            addr: s.0 .0.clone(),
                            copies: 1,
                        });
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

pub type MonoidalGraph<V, E> = MonoidalTerm<MonoidalOp<V, E>>;

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

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum MonoidalOp<V, E> {
    Copy {
        addr: OutPort<V, E>,
        copies: usize,
    },
    Operation {
        addr: Operation<V, E>,
        selected: bool,
    },
    Thunk {
        addr: Thunk<V, E>,
        body: MonoidalGraph<V, E>,
        expanded: bool,
    },
    Swap {
        out_1: (OutPort<V, E>, Direction),
        out_2: (OutPort<V, E>, Direction),
    },
    Backlink {
        addr: OutPort<V, E>,
    },
    Cup {
        addr: OutPort<V, E>,
    },
    Cap {
        addr: OutPort<V, E>,
    },
}

impl<V, E> InOut for MonoidalOp<V, E> {
    type V = V;
    type E = E;

    fn number_of_inputs(&self) -> usize {
        match self {
            Self::Copy { .. } | Self::Backlink { .. } => 1,
            Self::Operation { addr, .. } => addr.number_of_inputs(),
            Self::Thunk { addr, .. } => addr.number_of_inputs(),
            Self::Swap { .. } | Self::Cup { .. } => 2,
            Self::Cap { .. } => 0,
        }
    }

    fn number_of_outputs(&self) -> usize {
        match self {
            Self::Copy { copies, .. } => *copies,
            Self::Operation { addr, .. } => addr.number_of_outputs(),
            Self::Thunk { addr, .. } => addr.number_of_outputs(),
            Self::Backlink { .. } => 1,
            Self::Cup { .. } => 0,
            Self::Swap { .. } | Self::Cap { .. } => 2,
        }
    }

    fn inputs<'a>(&'a self) -> Box<dyn Iterator<Item = Link<V, E>> + 'a> {
        match self {
            MonoidalOp::Copy { addr, copies } => {
                Box::new(std::iter::repeat((addr.clone(), Direction::Forward)).take(*copies))
            }
            MonoidalOp::Operation { addr, .. } => Box::new(
                addr.inputs()
                    .map(|in_port| (in_port.output(), Direction::Forward)),
            ),
            MonoidalOp::Thunk { addr, body, .. } => Box::new(
                body.unordered_inputs
                    .iter()
                    .chain(body.ordered_inputs.iter())
                    .filter_map(|port| {
                        addr.externalise_input(port)
                            .map(|x| (x.output(), Direction::Forward))
                    }),
            ),
            MonoidalOp::Swap { out_1, out_2 } => Box::new(vec![out_1, out_2].into_iter().cloned()),
            MonoidalOp::Backlink { addr } => {
                Box::new(std::iter::once((addr.clone(), Direction::Backward)))
            }
            MonoidalOp::Cup { addr } => Box::new(
                [
                    (addr.clone(), Direction::Forward),
                    (addr.clone(), Direction::Backward),
                ]
                .into_iter(),
            ),
            MonoidalOp::Cap { .. } => Box::new(std::iter::empty()),
        }
    }

    fn outputs<'a>(&'a self) -> Box<dyn Iterator<Item = Link<V, E>> + 'a> {
        match self {
            MonoidalOp::Copy { addr, .. } => {
                Box::new(std::iter::once((addr.clone(), Direction::Forward)))
            }
            MonoidalOp::Operation { addr, .. } => {
                Box::new(addr.outputs().map(|x| (x, Direction::Forward)))
            }
            MonoidalOp::Thunk { addr, body, .. } => Box::new(
                body.outputs
                    .iter()
                    .map(|port| (addr.externalise_output(port).unwrap(), Direction::Forward)),
            ),
            MonoidalOp::Swap { out_1, out_2 } => Box::new(vec![out_2, out_1].into_iter().cloned()),
            MonoidalOp::Backlink { addr } => {
                Box::new(std::iter::once((addr.clone(), Direction::Backward)))
            }
            MonoidalOp::Cup { .. } => Box::new(std::iter::empty()),
            MonoidalOp::Cap { addr } => Box::new(
                [
                    (addr.clone(), Direction::Forward),
                    (addr.clone(), Direction::Backward),
                ]
                .into_iter(),
            ),
        }
    }
}

impl<V, E> MonoidalOp<V, E> {
    #[must_use]
    pub fn is_id(&self) -> bool {
        match self {
            MonoidalOp::Copy { copies, .. } => *copies == 1,
            _ => false,
        }
    }
}

impl<V: Debug, E: Debug> From<&WiredOp<V, E>> for MonoidalOp<V, E> {
    fn from(op: &WiredOp<V, E>) -> Self {
        match op {
            WiredOp::Copy { addr, copies } => MonoidalOp::Copy {
                addr: addr.clone(),
                copies: *copies,
            },
            WiredOp::Operation { addr } => Self::Operation {
                addr: addr.clone(),
                selected: false,
            },
            WiredOp::Thunk { addr, body, .. } => Self::Thunk {
                addr: addr.clone(),
                body: body.into(),
                expanded: true,
            },
            WiredOp::Backlink { addr } => MonoidalOp::Backlink { addr: addr.clone() },
        }
    }
}

impl<V: Debug, E: Debug> From<&MonoidalWiredGraph<V, E>> for MonoidalGraph<V, E> {
    fn from(graph: &MonoidalWiredGraph<V, E>) -> Self {
        let inputs: Vec<OutPort<V, E>> = graph
            .unordered_inputs
            .iter()
            .chain(graph.ordered_inputs.iter())
            .cloned()
            .collect();

        let graph_inputs: Vec<(OutPort<V, E>, Direction)> = inputs
            .iter()
            .map(|out_port| (out_port.clone(), Direction::Forward))
            .collect();

        let (open_ports, mut slices): (_, Vec<Slice<MonoidalOp<V, E>>>) = graph.slices.iter().fold(
            (graph_inputs, vec![]),
            |(open_ports, mut slices), next_slice| {
                slices.extend(Slice::permutation_to_swaps(
                    open_ports.into_iter(),
                    next_slice.inputs(),
                ));

                let next_outputs = next_slice.outputs().collect();

                slices.push(next_slice.into());

                (next_outputs, slices)
            },
        );

        slices.extend(Slice::permutation_to_swaps(
            open_ports.into_iter(),
            graph
                .outputs
                .iter()
                .map(|in_port| (in_port.output(), Direction::Forward)),
        ));

        let mut graph = MonoidalGraph {
            unordered_inputs: vec![],
            ordered_inputs: inputs,
            slices,
            outputs: graph.outputs.clone(),
        };

        graph.squash_layers();

        graph
    }
}

impl<V, E> MonoidalGraph<V, E> {
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

impl<V, E> Slice<MonoidalOp<V, E>> {
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
            if first.is_id() {
                for _ in 1..second.number_of_inputs() {
                    if first_iter.next().map(MonoidalOp::is_id) != Some(true) {
                        return false;
                    }
                }
            } else {
                if !second.is_id() {
                    return false;
                }
                for _ in 1..first.number_of_outputs() {
                    if second_iter.next().map(MonoidalOp::is_id) != Some(true) {
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

                if op.is_id() {
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

    use crate::language::spartan::Op;

    use super::*;

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
