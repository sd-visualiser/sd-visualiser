use itertools::Itertools;
use std::fmt::Debug;

use crate::{
    common::{advance_by, generate_permutation, InOut, Slice},
    hypergraph::{InPort, Operation, OutPort, Thunk},
    monoidal_wired::{MonoidalWiredGraph, MonoidalWiredOp},
};
use derivative::Derivative;
use tracing::debug;

impl<V, E> Slice<MonoidalOp<V, E>> {
    pub fn permutation_to_swaps(
        start_ports: impl Iterator<Item = OutPort<V, E>>,
        end_ports: impl Iterator<Item = OutPort<V, E>>,
    ) -> Vec<Self> {
        let mut perm_iter = generate_permutation(start_ports, end_ports).peekable();

        let mut permutation: Vec<(OutPort<V, E>, usize)> = Vec::new();

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
                            addr: port_1.clone(),
                            copies: 1,
                        });
                        permutation.push((port_1, x));
                    }
                }
            } else {
                finished = false;
                slice_ops.push(MonoidalOp::Copy {
                    addr: port_1.clone(),
                    copies: 0,
                });
            }
        }

        let mut slices = Vec::new();

        if !finished {
            slices.push(Slice { ops: slice_ops })
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
                            addr: s.0.clone(),
                            copies: 1,
                        });
                    }
                }
            }

            if !finished {
                slices.push(Slice { ops: slice_ops })
            }
        }

        slices
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Derivative)]
#[derivative(Default(bound = ""))]
pub struct MonoidalGraph<V, E> {
    pub inputs: Vec<OutPort<V, E>>,
    pub slices: Vec<Slice<MonoidalOp<V, E>>>,
    pub outputs: Vec<InPort<V, E>>,
}

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
        out_1: OutPort<V, E>,
        out_2: OutPort<V, E>,
    },
}

impl<V, E> InOut<V, E> for MonoidalOp<V, E> {
    fn number_of_inputs(&self) -> usize {
        match self {
            Self::Copy { .. } => 1,
            Self::Operation { addr, .. } => addr.number_of_inputs(),
            Self::Thunk { addr, .. } => addr.number_of_inputs(),
            Self::Swap { .. } => 2,
        }
    }

    fn number_of_outputs(&self) -> usize {
        match self {
            Self::Copy { copies, .. } => *copies,
            Self::Operation { addr, .. } => addr.number_of_outputs(),
            Self::Thunk { addr, .. } => addr.number_of_outputs(),
            Self::Swap { .. } => 2,
        }
    }

    fn inputs<'a>(&'a self) -> Box<dyn Iterator<Item = OutPort<V, E>> + 'a>
    where
        V: 'a,
        E: 'a,
    {
        match self {
            MonoidalOp::Copy { addr, copies } => {
                Box::new(std::iter::repeat(addr.clone()).take(*copies))
            }
            MonoidalOp::Operation { addr, .. } => {
                Box::new(addr.inputs().map(|in_port| in_port.output()))
            }
            MonoidalOp::Thunk { addr, body, .. } => Box::new(
                body.inputs
                    .iter()
                    .filter_map(|port| addr.externalise_input(port).map(|x| x.output())),
            ),
            MonoidalOp::Swap { out_1, out_2 } => Box::new(vec![out_1, out_2].into_iter().cloned()),
        }
    }

    fn outputs<'a>(&'a self) -> Box<dyn Iterator<Item = OutPort<V, E>> + 'a>
    where
        V: 'a,
        E: 'a,
    {
        match self {
            MonoidalOp::Copy { addr, .. } => Box::new(std::iter::once(addr.clone())),
            MonoidalOp::Operation { addr, .. } => Box::new(addr.outputs()),
            MonoidalOp::Thunk { addr, body, .. } => Box::new(
                body.outputs
                    .iter()
                    .map(|port| addr.externalise_output(port).unwrap()),
            ),
            MonoidalOp::Swap { out_1, out_2 } => Box::new(vec![out_2, out_1].into_iter().cloned()),
        }
    }
}

impl<V, E> MonoidalOp<V, E> {
    pub fn is_id(&self) -> bool {
        match self {
            MonoidalOp::Copy { copies, .. } => *copies == 1,
            _ => false,
        }
    }
}

impl<V: Debug, E: Debug> From<&MonoidalWiredOp<V, E>> for MonoidalOp<V, E> {
    fn from(op: &MonoidalWiredOp<V, E>) -> Self {
        match op {
            MonoidalWiredOp::Copy { addr, copies } => MonoidalOp::Copy {
                addr: addr.clone(),
                copies: *copies,
            },
            MonoidalWiredOp::Operation { addr } => Self::Operation {
                addr: addr.clone(),
                selected: false,
            },
            MonoidalWiredOp::Thunk { addr, body, .. } => Self::Thunk {
                addr: addr.clone(),
                body: body.into(),
                expanded: true,
            },
        }
    }
}

impl<V: Debug, E: Debug> From<&MonoidalWiredGraph<V, E>> for MonoidalGraph<V, E> {
    fn from(graph: &MonoidalWiredGraph<V, E>) -> Self {
        let graph_inputs = {
            let mut x = graph.unbound_inputs.clone();
            x.append(&mut graph.bound_inputs.clone());
            x
        };
        let (open_ports, mut slices): (_, Vec<Slice<MonoidalOp<V, E>>>) = graph.slices.iter().fold(
            (graph_inputs.clone(), vec![]),
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
            graph.outputs.iter().map(|in_port| in_port.output()),
        ));

        let mut graph = MonoidalGraph {
            inputs: graph_inputs,
            slices,
            outputs: graph.outputs.clone(),
        };

        debug!("Presquash: {:#?}", graph);
        graph.squash_layers();
        debug!("Postsquash: {:#?}", graph);

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
            if !first.is_id() {
                if !second.is_id() {
                    return false;
                }
                for _ in 1..first.number_of_outputs() {
                    if second_iter.next().map(|x| x.is_id()) != Some(true) {
                        return false;
                    }
                }
            } else {
                for _ in 1..second.number_of_inputs() {
                    if first_iter.next().map(|x| x.is_id()) != Some(true) {
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

                if !op.is_id() {
                    advance_by(&mut second_iter, op.number_of_outputs() - 1);
                    ops.push(op);
                }

                if !second.is_id() {
                    advance_by(&mut first_iter, second.number_of_inputs() - 1);
                    ops.push(second);
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

    use crate::graph::Op;

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
