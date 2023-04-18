use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Debug,
};

use crate::hypergraph::{GraphNode, HyperGraph, HyperGraphError, NodeIndex, Port, PortIndex};
use itertools::Itertools;
use num::rational::Ratio;
use thiserror::Error;
use tracing::{debug, debug_span};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct WiredSlice<O> {
    pub ops: Vec<(MonoidalWiredOp<O>, Vec<NodeIndex>)>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Slice<O> {
    pub ops: Vec<(MonoidalOp<O>, Vec<NodeIndex>)>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Wiring {
    forward: Vec<BTreeSet<usize>>,
    backward: Vec<usize>,
}

impl Wiring {
    pub fn new(inputs: usize) -> Self {
        Wiring {
            forward: vec![BTreeSet::new(); inputs],
            backward: vec![],
        }
    }

    pub fn add_wire(&mut self, input: usize) {
        self.forward[input].insert(self.backward.len());
        self.backward.push(input);
    }

    pub fn to_slices<O>(&self, prefix: &[NodeIndex]) -> Vec<Slice<O>> {
        let mut slices = Slice::permutation_to_swaps(self.backward.clone(), prefix);
        let mut copy_slice = Vec::new();
        let mut is_empty = true;
        for x in &self.forward {
            let copies = x.len();
            if copies != 1 {
                is_empty = false;
            }
            copy_slice.push((MonoidalOp::Copy { copies }, prefix.to_vec()))
        }
        if !is_empty {
            slices.push(Slice { ops: copy_slice });
        }
        slices.reverse();
        slices
    }
}

impl<O> WiredSlice<O> {
    pub fn number_of_inputs(&self) -> usize {
        self.ops.iter().map(|(op, _)| op.number_of_inputs()).sum()
    }

    pub fn number_of_outputs(&self) -> usize {
        self.ops.iter().map(|(op, _)| op.number_of_outputs()).sum()
    }
}

impl<O> Slice<O> {
    pub fn number_of_inputs(&self) -> usize {
        self.ops.iter().map(|(op, _)| op.number_of_inputs()).sum()
    }

    pub fn number_of_outputs(&self) -> usize {
        self.ops.iter().map(|(op, _)| op.number_of_outputs()).sum()
    }

    pub fn permutation_to_swaps(mut permutation: Vec<usize>, prefix: &[NodeIndex]) -> Vec<Self> {
        let mut slices = Vec::new();

        let mut finished = false;

        while !finished {
            let mut slice_ops = Vec::new();
            finished = true; // We set finished back to false if we make a swap
            let mut i = 0; // Iterate through windows
            while i + 1 < permutation.len() {
                if permutation[i] <= permutation[i + 1] {
                    i += 1;
                    slice_ops.push((MonoidalOp::ID, prefix.to_vec()));
                } else {
                    finished = false;
                    slice_ops.push((MonoidalOp::Swap, prefix.to_vec()));
                    permutation.swap(i, i + 1);
                    i += 2;
                }
            }
            if i + 1 == permutation.len() {
                slice_ops.push((MonoidalOp::ID, prefix.to_vec()));
            }
            if !finished {
                // Slice is non trivial
                slices.push(Slice { ops: slice_ops });
            }
        }

        slices
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct MonoidalWiredGraph<O> {
    pub inputs: usize,
    pub slices: Vec<WiredSlice<O>>,
    pub wirings: Vec<Wiring>,
}

impl<O> Default for MonoidalWiredGraph<O> {
    fn default() -> Self {
        MonoidalWiredGraph {
            inputs: 0,
            slices: vec![],
            wirings: vec![Wiring::new(0)],
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum MonoidalWiredOp<O> {
    Id {
        port: Port,
    },
    Operation {
        inputs: Vec<Port>,
        op_name: O,
    },
    Thunk {
        inputs: Vec<Port>,
        args: usize,
        body: MonoidalWiredGraph<O>,
    },
}

impl<O> MonoidalWiredOp<O> {
    /// Returns number of inputs of an operation
    pub fn number_of_inputs(&self) -> usize {
        self.input_ports().len()
    }

    /// Returns number of outputs of an operation
    pub fn number_of_outputs(&self) -> usize {
        1
    }

    pub fn input_ports(&self) -> Vec<Port> {
        match self {
            MonoidalWiredOp::Id { port } => vec![*port],
            MonoidalWiredOp::Operation { inputs, .. } => inputs.clone(),
            MonoidalWiredOp::Thunk { inputs, .. } => inputs.clone(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct MonoidalGraph<O> {
    pub inputs: usize,
    pub slices: Vec<Slice<O>>,
}

impl<O> Default for MonoidalGraph<O> {
    fn default() -> Self {
        MonoidalGraph {
            inputs: 0,
            slices: vec![],
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum MonoidalOp<O> {
    Copy {
        copies: usize,
    },
    Operation {
        inputs: usize,
        op_name: O,
    },
    Thunk {
        args: usize,
        body: MonoidalGraph<O>,
        expanded: bool,
    },
    Swap,
}

impl<O> MonoidalOp<O> {
    /// Returns number of cinputs of an operation
    pub fn number_of_inputs(&self) -> usize {
        match self {
            Self::Copy { .. } => 1,
            Self::Operation { inputs, .. } => *inputs,
            Self::Thunk { args, body, .. } => body.inputs - args,
            Self::Swap => 2,
        }
    }

    /// Returns number of outputs of an operation
    pub fn number_of_outputs(&self) -> usize {
        match self {
            Self::Copy { copies } => *copies,
            Self::Operation { .. } => 1,
            Self::Thunk { .. } => 1,
            Self::Swap => 2,
        }
    }

    pub const ID: Self = MonoidalOp::Copy { copies: 1 };

    pub const DELETE: Self = MonoidalOp::Copy { copies: 0 };
}

#[derive(Debug, Error, Clone)]
pub enum FromHyperError {
    #[error("Hypergraph contains no nodes")]
    EmptyGraph,

    #[error("Hypergraph error")]
    HyperGraphError(#[from] HyperGraphError),
}

// This can be made a lot nicer
impl<O: Debug + Copy> MonoidalWiredGraph<O> {
    pub fn from_hypergraph(
        graph: &HyperGraph<O>,
        prefix: &[NodeIndex],
    ) -> Result<Self, FromHyperError> {
        // List of open ports we have left to process

        debug_span!("From hypergraph");
        debug!("To Process: {:?}", graph);

        // Separate the nodes into input nodes, output nodes, and other nodes by rank
        let (ranks, input_wires, output_wires) = {
            let (inputs, mut r, outputs) = graph.ranks_from_end();

            debug!("Inputs: {:?}", inputs);
            debug!("Ranks: {:?}", r);
            debug!("Outputs: {:?}", outputs);

            let input_wires = inputs
                .iter()
                .map(|x| graph.number_of_outputs(*x).unwrap())
                .sum();

            let output_wires = outputs
                .iter()
                .map(|x| graph.get_inputs(*x).unwrap().collect_vec())
                .concat();

            r.push(inputs);

            (r, input_wires, output_wires)
        };

        debug!("Input wires: {:?}", input_wires);
        debug!("Output wires: {:?}", output_wires);

        let mut open_wires: Vec<Port> = output_wires;

        let mut slices: Vec<WiredSlice<O>> = Vec::new();
        let mut wirings: Vec<Wiring> = Vec::new();

        #[derive(Debug)]
        struct OpData<O> {
            op: Option<MonoidalWiredOp<O>>,
            outputs: Vec<Port>,
            addr: Vec<NodeIndex>,
            weight: Ratio<usize>,
        }

        for r in ranks {
            let mut ops: Vec<OpData<O>> = Vec::new();
            let mut gathered_ports: BTreeSet<Port> = BTreeSet::new();

            for node in r.iter() {
                debug!("Processing node {:?}", node);
                let node = *node;
                let addr = {
                    let mut temp = prefix.to_vec();
                    temp.push(node);
                    temp
                };
                let (sum, count) = open_wires
                    .iter()
                    .enumerate()
                    .filter_map(
                        |(i, Port { node: n, index: _ })| {
                            if &node == n {
                                Some((i, 1))
                            } else {
                                None
                            }
                        },
                    )
                    .fold((0, 0), |(x, y), (a, b)| (x + a, y + b));
                let weight = if count == 0 {
                    usize::MAX.into()
                } else {
                    Ratio::new_raw(sum, count)
                };
                let outputs = (0..graph.number_of_outputs(node)?)
                    .map(|index| Port {
                        node,
                        index: PortIndex(index),
                    })
                    .collect();

                gathered_ports.extend(&outputs);
                let op = match graph.get(node)? {
                    GraphNode::Weight(op) => Some(MonoidalWiredOp::Operation {
                        inputs: graph.get_inputs(node)?.collect(),
                        op_name: *op,
                    }),
                    GraphNode::Input => None,
                    GraphNode::Output => None,
                    GraphNode::Thunk { args, body } => Some(MonoidalWiredOp::Thunk {
                        inputs: graph.get_inputs(node)?.collect(),
                        args: *args,
                        body: MonoidalWiredGraph::from_hypergraph(body, &addr)?,
                    }),
                };
                ops.push(OpData {
                    op,
                    outputs,
                    addr,
                    weight,
                })
            }

            for (i, port) in open_wires.iter().copied().enumerate() {
                if !gathered_ports.contains(&port) {
                    gathered_ports.insert(port);
                    ops.push(OpData {
                        op: Some(MonoidalWiredOp::Id { port }),
                        outputs: vec![port],
                        addr: prefix.to_vec(),
                        weight: i.into(),
                    });
                }
            }

            ops.sort_by_key(|data| data.weight);

            debug!("Operation layer generated: {:?}", ops);

            let out_nodes: BTreeMap<Port, usize> = ops
                .iter()
                .flat_map(|data| data.outputs.iter().copied())
                .enumerate()
                .map(|(x, y)| (y, x))
                .collect();

            let number_of_out_ports = gathered_ports.len();

            debug!("Out nodes: {:?}", out_nodes);

            let mut wiring = Wiring::new(number_of_out_ports);

            for p in open_wires {
                wiring.add_wire(*out_nodes.get(&p).ok_or(HyperGraphError::UnknownPort(p))?);
            }

            debug!("Wiring generated");

            open_wires = ops
                .iter()
                .map(|data| {
                    data.op
                        .as_ref()
                        .map(|x| x.input_ports())
                        .unwrap_or_default()
                })
                .concat();

            debug!("Open wires: {:?}", open_wires);

            if let Some(ops) = ops
                .into_iter()
                .map(|data| Some((data.op?, data.addr)))
                .collect::<Option<Vec<_>>>()
            {
                slices.push(WiredSlice { ops });
            }

            wirings.push(wiring);
        }

        slices.reverse();
        wirings.reverse();

        Ok(MonoidalWiredGraph {
            inputs: input_wires,
            slices,
            wirings,
        })
    }
}

impl<O: Copy> MonoidalWiredOp<O> {
    pub fn to_monoidal_op(&self, node: &[NodeIndex]) -> Result<MonoidalOp<O>, FromHyperError> {
        match self {
            MonoidalWiredOp::Id { .. } => Ok(MonoidalOp::ID),
            MonoidalWiredOp::Operation { inputs, op_name } => Ok(MonoidalOp::Operation {
                inputs: inputs.len(),
                op_name: *op_name,
            }),
            MonoidalWiredOp::Thunk { args, body, .. } => Ok(MonoidalOp::Thunk {
                args: *args,
                body: body.to_graph(node)?,
                expanded: true,
            }),
        }
    }
}

impl<O: Copy> WiredSlice<O> {
    pub fn to_slice(&self) -> Result<Slice<O>, FromHyperError> {
        Ok(Slice {
            ops: self
                .ops
                .iter()
                .map(|(x, node)| Ok((x.to_monoidal_op(node)?, node.clone())))
                .collect::<Result<Vec<_>, FromHyperError>>()?,
        })
    }
}

impl<O: Copy> MonoidalWiredGraph<O> {
    pub fn to_graph(&self, prefix: &[NodeIndex]) -> Result<MonoidalGraph<O>, FromHyperError> {
        let wiring_slices = self.wirings.iter().map(|w| w.to_slices(prefix));
        let slices: Vec<Slice<O>> = wiring_slices
            .into_iter()
            .interleave(
                self.slices
                    .iter()
                    .map(|x| Ok(vec![x.to_slice()?]))
                    .collect::<Result<Vec<_>, FromHyperError>>()?,
            )
            .concat();
        Ok(MonoidalGraph {
            inputs: self.inputs,
            slices,
        })
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Result;
    use rstest::rstest;

    use crate::graph::Op;

    use super::*;

    #[rstest]
    #[case(vec![0,1], vec![])]
    #[case(vec![1,0], vec![Slice { ops: vec![(MonoidalOp::Swap, vec![])]}])]
    #[case(vec![1,2,0], vec![Slice { ops: vec![(MonoidalOp::ID, vec![]),(MonoidalOp::Swap, vec![])]}, Slice { ops: vec![(MonoidalOp::Swap, vec![]), (MonoidalOp::ID, vec![])]}])]
    fn test_permutation(
        #[case] permutation: Vec<usize>,
        #[case] result: Vec<Slice<Op>>,
    ) -> Result<()> {
        assert_eq!(Slice::permutation_to_swaps(permutation, &[]), result);
        Ok(())
    }
}
