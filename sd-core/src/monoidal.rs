use std::collections::{BTreeMap, BTreeSet};

use itertools::Itertools;
use sd_hyper::graph::{HyperGraphError, Port};
use thiserror::Error;

use crate::{
    graph::{HyperGraph, Op},
    language::grammar::{ActiveOp, PassiveOp},
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Slice {
    pub ops: Vec<MonoidalOp>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MonoidalGraph {
    pub inputs: usize,
    pub slices: Vec<Slice>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Operation {
    Active(ActiveOp),
    Passive(PassiveOp),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MonoidalOp {
    Copy { copies: usize },
    Unit,
    Operation { inputs: usize, op_name: Operation },
    Thunk { args: usize, body: MonoidalGraph },
    Swap,
}

impl MonoidalOp {
    /// Returns number of inputs of an operation
    pub fn number_of_inputs(&self) -> usize {
        match self {
            Self::Copy { .. } => 1,
            Self::Unit => 0,
            Self::Operation { inputs, .. } => *inputs,
            Self::Thunk { args, body } => body.inputs - args,
            Self::Swap => 2,
        }
    }

    /// Returns number of outputs of an operation
    pub fn number_of_outputs(&self) -> usize {
        match self {
            MonoidalOp::Copy { copies } => *copies,
            MonoidalOp::Unit => 1,
            MonoidalOp::Operation { .. } => 1,
            MonoidalOp::Thunk { .. } => 1,
            MonoidalOp::Swap => 2,
        }
    }
}

pub const ID: MonoidalOp = MonoidalOp::Copy { copies: 1 };

pub const DELETE: MonoidalOp = MonoidalOp::Copy { copies: 0 };

// Unfolding

impl MonoidalGraph {
    pub fn unfold(self) -> Self {
        Self {
            inputs: self.inputs,
            slices: self
                .slices
                .into_iter()
                .flat_map(|slice| {
                    // Turn each operation into a list of slices.
                    let sss = slice
                        .ops
                        .into_iter()
                        .map(|op| match op {
                            MonoidalOp::Thunk { args, body } => {
                                // Unfold the body of the thunk and add an extra slice at the start.
                                let mut slices = body.slices;
                                slices.insert(
                                    0,
                                    Slice {
                                        ops: std::iter::repeat(ID)
                                            .take(body.inputs - args)
                                            .chain(std::iter::repeat(MonoidalOp::Unit).take(args))
                                            .collect(),
                                    },
                                );
                                slices
                            }
                            _ => vec![Slice { ops: vec![op] }],
                        })
                        .collect::<Vec<_>>();

                    let max_height = sss.iter().map(|ss| ss.len()).max().unwrap();
                    let mut slices = Vec::with_capacity(max_height);
                    for i in 0..max_height {
                        slices.push(Slice {
                            ops: sss
                                .iter()
                                .flat_map(|ss| {
                                    ss.get(i)
                                        .cloned()
                                        .unwrap_or_else(|| {
                                            let n = ss
                                                .last()
                                                .unwrap()
                                                .ops
                                                .iter()
                                                .map(MonoidalOp::number_of_outputs)
                                                .sum();
                                            Slice { ops: vec![ID; n] }
                                        })
                                        .ops
                                })
                                .collect(),
                        });
                    }
                    slices
                })
                .collect(),
        }
    }
}

#[derive(Debug, Error)]
pub enum FromHyperError {
    #[error("Hypergraph contains no nodes")]
    EmptyGraph,

    #[error("Hypergraph error")]
    HyperGraphError(#[from] HyperGraphError),
}

fn permutation_to_swaps(mut permutation: Vec<usize>) -> Vec<Slice> {
    let mut slices = Vec::new();

    let mut finished = false;

    while !finished {
        let mut slice_ops = Vec::new();
        finished = true; // We set finished back to false if we make a swap
        let mut i = 0; // Iterate through windows
        while i + 1 < permutation.len() {
            if permutation[i] <= permutation[i + 1] {
                i += 1;
                slice_ops.push(ID);
            } else {
                finished = false;
                slice_ops.push(MonoidalOp::Swap);
                permutation.swap(i, i + 1);
                i += 2;
            }
        }
        if i + 1 == permutation.len() {
            slice_ops.push(ID);
        }
        if !finished {
            // Slice is non trivial
            slices.push(Slice { ops: slice_ops });
        }
    }

    slices
}

impl MonoidalGraph {
    pub fn from_hypergraph(graph: &HyperGraph) -> Result<Self, FromHyperError> {
        // List of open ports we have left to process

        // Separate the nodes into input nodes, output nodes, and other nodes by rank
        let (ranks, input_wires, output_wires) = {
            let mut r = graph.ranks_from_end();
            let mut inputs: BTreeSet<usize> = BTreeSet::new();
            let mut input_wires: usize = 0;
            let mut output_wires: Vec<Port> = Vec::new();
            for x in r.iter_mut() {
                let items = x
                    .iter()
                    .map(|x| {
                        let e = graph.get(*x)?;
                        Ok((*x, e))
                    })
                    .collect::<Result<Vec<(usize, &Op)>, FromHyperError>>()?;

                for (y, _) in items.iter().filter(|(_, e)| e.is_input()) {
                    inputs.insert(*y);
                    input_wires += graph.number_of_outputs(*y)?;
                    x.remove(y);
                }

                for (y, _) in items.iter().filter(|(_, e)| e.is_output()) {
                    output_wires.extend(graph.input_ports(*y)?);
                    x.remove(y);
                }
            }
            r.push(inputs);
            (r, input_wires, output_wires)
        };

        let mut open_wires: Vec<Port> = output_wires;

        let mut slices: Vec<Slice> = Vec::new();

        for r in ranks {
            // Gather up wires by port
            let mut by_node: BTreeMap<usize, BTreeMap<usize, Vec<usize>>> = BTreeMap::new();

            for (wire, Port { node, index }) in open_wires.into_iter().enumerate() {
                by_node
                    .entry(node)
                    .and_modify(|x| {
                        x.entry(index)
                            .and_modify(|y| y.push(wire))
                            .or_insert_with(|| vec![wire]);
                    })
                    .or_insert_with(|| BTreeMap::from([(index, vec![wire])]));
            }

            let mut parts: Vec<_> = by_node.into_iter().collect();

            open_wires = vec![];

            parts.sort_by(|(_, l1), (_, l2)| {
                usize::cmp(
                    &(l1.iter()
                        .map(|(_, x)| x.iter().sum::<usize>())
                        .sum::<usize>()
                        * l2.iter().map(|(_, x)| x.len()).sum::<usize>()),
                    &(l2.iter()
                        .map(|(_, x)| x.iter().sum::<usize>())
                        .sum::<usize>()
                        * l1.iter().map(|(_, x)| x.len()).sum::<usize>()),
                )
            });

            let permutation = parts
                .iter()
                .map(|(_, l)| l.iter().map(|(_, l)| l.clone()).concat())
                .concat();

            let swap_slices = permutation_to_swaps(permutation);

            slices.extend(swap_slices.into_iter().rev());

            // Build up a copy/delete layer and operation layer at the same time
            let mut copy_slice = Vec::new();
            let mut op_slice = Vec::new();

            let mut rank = r.clone();

            for (node, part) in parts {
                if rank.contains(&node) {
                    let inputs: Vec<_> = graph.input_ports(node)?.collect();
                    let outputs = graph.number_of_outputs(node)?;
                    let ops = match graph.get(node)? {
                        Op::Passive(p) => vec![MonoidalOp::Operation {
                            inputs: inputs.len(),
                            op_name: Operation::Passive(*p),
                        }],
                        Op::Active(a) => vec![MonoidalOp::Operation {
                            inputs: inputs.len(),
                            op_name: Operation::Active(*a),
                        }],
                        Op::Input => vec![ID; outputs],
                        Op::Output => vec![],
                        Op::Thunk { args, body } => vec![MonoidalOp::Thunk {
                            args: *args,
                            body: MonoidalGraph::from_hypergraph(body)?,
                        }],
                    };
                    op_slice.extend(ops);
                    for i in 0..outputs {
                        copy_slice.push(MonoidalOp::Copy {
                            copies: part.get(&i).map(|x| x.len()).unwrap_or(0),
                        })
                    }
                    rank.remove(&node);
                    open_wires.extend(inputs);
                } else {
                    for (index, wires) in part {
                        copy_slice.push(MonoidalOp::Copy {
                            copies: wires.len(),
                        });
                        op_slice.push(ID);
                        open_wires.push(Port { node, index })
                    }
                }
            }

            for remaining in rank {
                // Should likely deduplicate this code somehow...
                let inputs: Vec<_> = graph.input_ports(remaining)?.collect();
                let outputs = graph.number_of_outputs(remaining)?;
                let ops = match graph.get(remaining)? {
                    Op::Passive(p) => vec![MonoidalOp::Operation {
                        inputs: inputs.len(),
                        op_name: Operation::Passive(*p),
                    }],
                    Op::Active(a) => vec![MonoidalOp::Operation {
                        inputs: inputs.len(),
                        op_name: Operation::Active(*a),
                    }],
                    Op::Input => vec![ID; outputs],
                    Op::Output => vec![],
                    Op::Thunk { args, body } => vec![MonoidalOp::Thunk {
                        args: *args,
                        body: MonoidalGraph::from_hypergraph(body)?,
                    }],
                };
                op_slice.extend(ops);
                for _ in 0..outputs {
                    copy_slice.push(DELETE)
                }
                open_wires.extend(inputs);
            }

            if copy_slice
                .iter()
                .any(|x| x != &MonoidalOp::Copy { copies: 1 })
            {
                slices.push(Slice { ops: copy_slice });
            }

            if op_slice
                .iter()
                .any(|x| x != &MonoidalOp::Copy { copies: 1 })
            {
                slices.push(Slice { ops: op_slice });
            }
        }

        slices.reverse();

        Ok(MonoidalGraph {
            inputs: input_wires,
            slices,
        })
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Result;
    use rstest::rstest;

    use super::*;

    #[rstest]
    #[case(vec![0,1], vec![])]
    #[case(vec![1,0], vec![Slice { ops: vec![MonoidalOp::Swap]}])]
    #[case(vec![1,2,0], vec![Slice { ops: vec![ID,MonoidalOp::Swap]}, Slice { ops: vec![MonoidalOp::Swap, ID]}])]
    fn test_permutation(#[case] permutation: Vec<usize>, #[case] result: Vec<Slice>) -> Result<()> {
        assert_eq!(permutation_to_swaps(permutation), result);
        Ok(())
    }
}
