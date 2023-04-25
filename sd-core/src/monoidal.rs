use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use crate::{
    common::InOut,
    hypergraph::{HyperGraph, HyperGraphError, Node, NodeIndex, Port, PortIndex},
};
use itertools::Itertools;
use num::rational::Ratio;
use thiserror::Error;
use tracing::debug;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Slice<O: InOut> {
    pub ops: Vec<O>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Wiring {
    forward: Vec<HashSet<usize>>,
    backward: Vec<usize>,
}

impl Wiring {
    pub fn new(inputs: usize) -> Self {
        Wiring {
            forward: vec![HashSet::new(); inputs],
            backward: vec![],
        }
    }

    pub fn add_wire(&mut self, input: usize) {
        self.forward[input].insert(self.backward.len());
        self.backward.push(input);
    }

    pub fn to_slices<O>(&self) -> Vec<Slice<MonoidalOp<O>>> {
        let mut slices = Slice::permutation_to_swaps(self.backward.clone());
        let mut copy_slice = Vec::new();
        let mut is_empty = true;
        for x in &self.forward {
            let copies = x.len();
            if copies != 1 {
                is_empty = false;
            }
            copy_slice.push(MonoidalOp::Copy { copies })
        }
        if !is_empty {
            slices.push(Slice { ops: copy_slice });
        }
        slices.reverse();
        slices
    }
}

impl<O: InOut> InOut for Slice<O> {
    fn number_of_inputs(&self) -> usize {
        self.ops.iter().map(|op| op.number_of_inputs()).sum()
    }

    fn number_of_outputs(&self) -> usize {
        self.ops.iter().map(|op| op.number_of_outputs()).sum()
    }
}

impl<O> Slice<MonoidalOp<O>> {
    pub fn permutation_to_swaps(mut permutation: Vec<usize>) -> Vec<Self> {
        let mut slices = Vec::new();

        let mut finished = false;

        while !finished {
            let mut slice_ops = Vec::new();
            finished = true; // We set finished back to false if we make a swap
            let mut i = 0; // Iterate through windows
            while i + 1 < permutation.len() {
                if permutation[i] <= permutation[i + 1] {
                    i += 1;
                    slice_ops.push(MonoidalOp::ID);
                } else {
                    finished = false;
                    slice_ops.push(MonoidalOp::Swap);
                    permutation.swap(i, i + 1);
                    i += 2;
                }
            }
            if i + 1 == permutation.len() {
                slice_ops.push(MonoidalOp::ID);
            }
            if !finished {
                // Slice is non trivial
                slices.push(Slice { ops: slice_ops });
            }
        }

        slices
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MonoidalWiredGraph<O> {
    pub inputs: usize,
    pub slices: Vec<Slice<MonoidalWiredOp<O>>>,
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MonoidalWiredOp<O> {
    Id {
        port: Port,
    },
    Operation {
        addr: NodeIndex,
        inputs: Vec<Port>,
        op_name: O,
    },
    Thunk {
        addr: NodeIndex,
        inputs: Vec<Port>,
        args: usize,
        body: MonoidalWiredGraph<O>,
    },
}

impl<O> InOut for MonoidalWiredOp<O> {
    fn number_of_inputs(&self) -> usize {
        self.input_ports().len()
    }

    fn number_of_outputs(&self) -> usize {
        1
    }
}

impl<O> MonoidalWiredOp<O> {
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
    pub slices: Vec<Slice<MonoidalOp<O>>>,
}

impl<O> MonoidalGraph<O> {
    pub fn selected(&self) -> Option<(Vec<NodeIndex>, HashSet<NodeIndex>)> {
        // Addresses of selected nodes.
        let mut selections = HashSet::default();

        // Internal selection state (prefix and addresses) of the first selected thunk.
        let mut thunk = None;

        for slice in &self.slices {
            for op in &slice.ops {
                match op {
                    MonoidalOp::Operation { addr, selected, .. } => {
                        if *selected {
                            selections.insert(*addr);
                        }
                    }
                    MonoidalOp::Thunk { addr, body, .. } => {
                        if let Some((mut prefix, subselections)) = body.selected() {
                            selections.insert(*addr);

                            if thunk.is_none() {
                                prefix.insert(0, *addr);
                                thunk = Some((prefix, subselections));
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        match (selections.len(), thunk) {
            (0, _) => None,
            (1, Some((prefix, subselections))) => Some((prefix, subselections)),
            _ => Some((vec![], selections)),
        }
    }
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
        addr: NodeIndex,
        inputs: usize,
        op_name: O,
        selected: bool,
    },
    Thunk {
        addr: NodeIndex,
        args: usize,
        body: MonoidalGraph<O>,
        expanded: bool,
    },
    Swap,
}

impl<O> InOut for MonoidalOp<O> {
    fn number_of_inputs(&self) -> usize {
        match self {
            Self::Copy { .. } => 1,
            Self::Operation { inputs, .. } => *inputs,
            Self::Thunk { args, body, .. } => body.inputs - args,
            Self::Swap => 2,
        }
    }

    fn number_of_outputs(&self) -> usize {
        match self {
            Self::Copy { copies } => *copies,
            Self::Operation { .. } => 1,
            Self::Thunk { .. } => 1,
            Self::Swap => 2,
        }
    }
}

impl<O> MonoidalOp<O> {
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

impl<O: Copy + Debug> TryFrom<&HyperGraph<O>> for MonoidalWiredGraph<O> {
    type Error = FromHyperError;

    fn try_from(graph: &HyperGraph<O>) -> Result<Self, Self::Error> {
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

        // Maintain a list of wires we have left to link up
        let mut open_wires: Vec<Port> = output_wires;

        // Build up slices and wirings
        let mut slices: Vec<Slice<MonoidalWiredOp<O>>> = Vec::new();
        let mut wirings: Vec<Wiring> = Vec::new();

        // For each operation in the next layer, we store the following data.
        #[derive(Debug)]
        struct OpData<O> {
            // The corresponding operation in the monoidal wired term, or none if we encounter an input/output node
            op: Option<MonoidalWiredOp<O>>,
            // The output ports of this operation
            outputs: Vec<Port>,
            // The average of the indices of the outputs in 'open_wires'
            weight: Ratio<usize>,
        }

        for r in ranks {
            // We build a list of OpData's for this layer
            let mut ops: Vec<OpData<O>> = Vec::new();

            // Also keep track of the subset of open wires which have a destination operation
            let mut gathered_ports: HashSet<Port> = HashSet::new();

            // Create an OpData for each node
            for &node in r.iter() {
                debug!("Processing node {:?}", node);

                // Compute the weight
                let (sum, count) = open_wires
                    .iter()
                    .enumerate()
                    .filter_map(|(i, Port { node: n, .. })| (node == *n).then_some((i, 1)))
                    .fold((0, 0), |(x, y), (a, b)| (x + a, y + b));
                let weight = if count == 0 {
                    // All outputs of this operation get deleted, so we arbitrarily put it on the right
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

                let op = match &graph[node] {
                    Node::Weight(op) => Some(MonoidalWiredOp::Operation {
                        addr: node,
                        inputs: graph.get_inputs(node)?.collect(),
                        op_name: *op,
                    }),
                    Node::Input | Node::Output => None,
                    Node::Thunk { args, body } => Some(MonoidalWiredOp::Thunk {
                        addr: node,
                        inputs: graph.get_inputs(node)?.collect(),
                        args: *args,
                        body: MonoidalWiredGraph::try_from(body)?,
                    }),
                };
                ops.push(OpData {
                    op,
                    outputs,
                    weight,
                })
            }

            // Wires not in gathered_ports will be used in a later layer
            // Therefore we must introduce an identity to route them through
            for (i, port) in open_wires.iter().copied().enumerate() {
                if !gathered_ports.contains(&port) {
                    gathered_ports.insert(port);
                    ops.push(OpData {
                        op: Some(MonoidalWiredOp::Id { port }),
                        outputs: vec![port],
                        weight: i.into(),
                    });
                }
            }

            // Sort the ops by weight
            ops.sort_by_key(|data| data.weight);

            debug!("Operation layer generated: {:?}", ops);

            // Compute the wiring from the open_wires to the operations
            let out_nodes: HashMap<Port, usize> = ops
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

            // We generate the new set of 'open_wires'.
            // This is just the inputs of the layer we just generated
            open_wires = ops
                .iter()
                .flat_map(|data| {
                    data.op
                        .as_ref()
                        .map(|x| x.input_ports())
                        .unwrap_or_default()
                })
                .collect();

            debug!("Open wires: {:?}", open_wires);

            // If any of the operations were None then this was the input slice
            // Otherwise we should add this slice to the diagram
            if let Some(ops) = ops
                .into_iter()
                .map(|data: OpData<O>| data.op)
                .collect::<Option<Vec<_>>>()
            {
                slices.push(Slice { ops });
            }

            wirings.push(wiring);
        }

        // We worked bottom up, so we reverse the slices and wirings
        slices.reverse();
        wirings.reverse();

        Ok(MonoidalWiredGraph {
            inputs: input_wires,
            slices,
            wirings,
        })
    }
}

impl<O: Copy> From<&MonoidalWiredOp<O>> for MonoidalOp<O> {
    fn from(op: &MonoidalWiredOp<O>) -> Self {
        match op {
            MonoidalWiredOp::Id { .. } => Self::ID,
            MonoidalWiredOp::Operation {
                addr,
                inputs,
                op_name,
            } => Self::Operation {
                addr: *addr,
                inputs: inputs.len(),
                op_name: *op_name,
                selected: false,
            },
            MonoidalWiredOp::Thunk {
                addr, args, body, ..
            } => Self::Thunk {
                addr: *addr,
                args: *args,
                body: body.into(),
                expanded: true,
            },
        }
    }
}

impl<'a, A: InOut, B: InOut + From<&'a A>> From<&'a Slice<A>> for Slice<B> {
    fn from(value: &'a Slice<A>) -> Self {
        Self {
            ops: value.ops.iter().map(B::from).collect(),
        }
    }
}

impl<O: Copy> From<&MonoidalWiredGraph<O>> for MonoidalGraph<O> {
    fn from(graph: &MonoidalWiredGraph<O>) -> Self {
        let wiring_slices = graph.wirings.iter().map(|w| w.to_slices());
        let slices: Vec<Slice<MonoidalOp<O>>> = wiring_slices
            .into_iter()
            .interleave(graph.slices.iter().map(|x| vec![x.into()]))
            .concat();
        Self {
            inputs: graph.inputs,
            slices,
        }
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
    #[case(vec![1,0], vec![Slice { ops: vec![MonoidalOp::Swap]}])]
    #[case(vec![1,2,0], vec![Slice { ops: vec![MonoidalOp::ID, MonoidalOp::Swap]}, Slice { ops: vec![MonoidalOp::Swap, MonoidalOp::ID]}])]
    fn test_permutation(
        #[case] permutation: Vec<usize>,
        #[case] result: Vec<Slice<MonoidalOp<Op>>>,
    ) -> Result<()> {
        assert_eq!(Slice::permutation_to_swaps(permutation), result);
        Ok(())
    }
}
