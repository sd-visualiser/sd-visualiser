use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use crate::{
    common::{FromHyperError, InOut, Slice},
    hypergraph::{HyperGraph, HyperGraphError, Node, NodeIndex, Port, PortIndex},
};
use itertools::Itertools;
use num::rational::Ratio;
use tracing::debug;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Wiring {
    pub(crate) forward: Vec<HashSet<usize>>,
    pub(crate) backward: Vec<usize>,
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

impl<O: Copy + Debug, V: Debug + Clone> TryFrom<&HyperGraph<O, V>> for MonoidalWiredGraph<O> {
    type Error = FromHyperError;

    fn try_from(graph: &HyperGraph<O, V>) -> Result<Self, Self::Error> {
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
                .map(|x| graph.get_inputs(*x).unwrap().map(|x| x.0).collect_vec())
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
                        inputs: graph.get_inputs(node)?.map(|x| x.0).collect(),
                        op_name: *op,
                    }),
                    Node::Input | Node::Output => None,
                    Node::Thunk { args, body } => Some(MonoidalWiredOp::Thunk {
                        addr: node,
                        inputs: graph.get_inputs(node)?.map(|x| x.0).collect(),
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
