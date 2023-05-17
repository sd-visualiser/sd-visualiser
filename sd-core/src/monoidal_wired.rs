use std::{cmp::Reverse, collections::HashMap, fmt::Debug};

use crate::{
    common::{InOut, Slice},
    hypergraph_good::{GraphView, InPort, Node, Operation, OutPort, Thunk},
};
use bimap::BiMap;
use derivative::Derivative;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Wiring {
    size: usize,
    mapping: BiMap<usize, usize>,
}

#[derive(Clone, Debug)]
pub struct MonoidalWiredGraph<V, E> {
    pub inputs: Vec<OutPort<V, E>>,
    pub slices: Vec<Slice<MonoidalWiredOp<V, E>>>,
    pub outputs: Vec<InPort<V, E>>,
}

impl<V, E> Default for MonoidalWiredGraph<V, E> {
    fn default() -> Self {
        MonoidalWiredGraph {
            inputs: vec![],
            slices: vec![],
            outputs: vec![],
        }
    }
}

#[derive(Clone, Debug)]
pub enum MonoidalWiredOp<V, E> {
    Copy {
        addr: OutPort<V, E>,
        copies: usize,
    },
    Operation {
        addr: Operation<V, E>,
    },
    Thunk {
        addr: Thunk<V, E>,
        body: MonoidalWiredGraph<V, E>,
    },
}

impl<V, E> InOut<V, E> for MonoidalWiredOp<V, E> {
    fn number_of_inputs(&self) -> usize {
        match self {
            MonoidalWiredOp::Copy { copies, .. } => *copies,
            MonoidalWiredOp::Operation { addr } => addr.number_of_inputs(),
            MonoidalWiredOp::Thunk { addr, .. } => addr.number_of_inputs(),
        }
    }

    fn number_of_outputs(&self) -> usize {
        match self {
            MonoidalWiredOp::Copy { copies, .. } => *copies,
            MonoidalWiredOp::Operation { addr } => addr.number_of_outputs(),
            MonoidalWiredOp::Thunk { addr, .. } => addr.number_of_outputs(),
        }
    }

    fn inputs<'a>(&'a self) -> Box<dyn Iterator<Item = OutPort<V, E>> + 'a>
    where
        V: 'a,
        E: 'a,
    {
        match self {
            MonoidalWiredOp::Copy { addr, copies } => {
                Box::new(std::iter::repeat(addr.clone()).take(*copies))
            }
            MonoidalWiredOp::Operation { addr } => {
                Box::new(addr.inputs().map(|in_port| in_port.output()))
            }
            MonoidalWiredOp::Thunk { addr, body } => Box::new(
                body.inputs
                    .iter()
                    .map(|port| addr.externalise_input(port).unwrap().output()),
            ),
        }
    }

    fn outputs<'a>(&'a self) -> Box<dyn Iterator<Item = OutPort<V, E>> + 'a>
    where
        V: 'a,
        E: 'a,
    {
        match self {
            MonoidalWiredOp::Copy { addr, .. } => Box::new(std::iter::once(addr.clone())),
            MonoidalWiredOp::Operation { addr } => Box::new(addr.outputs()),
            MonoidalWiredOp::Thunk { addr, body } => Box::new(
                body.outputs
                    .iter()
                    .map(|port| addr.externalise_output(port).unwrap()),
            ),
        }
    }
}

impl<V, E> From<Node<V, E>> for MonoidalWiredOp<V, E> {
    fn from(value: Node<V, E>) -> Self {
        match value {
            Node::Operation(op) => MonoidalWiredOp::Operation { addr: op },
            Node::Thunk(thunk) => MonoidalWiredOp::Thunk {
                body: (&thunk).into(),
                addr: thunk,
            },
        }
    }
}

#[derive(Derivative)]
#[derivative(Default(bound = ""))]
struct MonoidalWiredGraphBuilder<V, E> {
    slices: Vec<Slice<MonoidalWiredOp<V, E>>>,
    open_ports: HashMap<OutPort<V, E>, Vec<usize>>,
}

impl<V, E> MonoidalWiredGraphBuilder<V, E> {
    fn add_op(&mut self, op: MonoidalWiredOp<V, E>, layer: usize) {
        assert!(layer <= self.slices.len());
        match self.slices.get_mut(layer) {
            Some(slice) => slice,
            None => {
                self.slices.push(Default::default());
                self.slices.get_mut(layer).unwrap()
            }
        }
        .ops
        .push(op)
    }

    fn insert_copies_for_port(&mut self, out_port: OutPort<V, E>) -> usize {
        let mut layers = self.open_ports.remove(&out_port).unwrap();
        layers.sort_by_key(|x| Reverse(*x));

        while layers.len() > 1 {
            let first = layers.pop().unwrap();
            let mut copies = 1;
            while layers.last().map(|x| *x == first).unwrap_or_default() {
                layers.pop();
                copies += 1;
            }
            self.add_op(
                MonoidalWiredOp::Copy {
                    addr: out_port.clone(),
                    copies,
                },
                first,
            );
            layers.push(first + 1);
        }

        layers[0]
    }

    fn insert_operation(&mut self, node: Node<V, E>) {
        let non_deleted_output_layers: Vec<_> = node
            .outputs()
            .filter(|x| x.inputs().next().is_some())
            .map(|x| (x.clone(), self.insert_copies_for_port(x)))
            .collect();

        let node_layer = non_deleted_output_layers
            .iter()
            .map(|(_out_port, layer)| *layer)
            .max()
            .unwrap_or_default();

        non_deleted_output_layers
            .into_iter()
            .for_each(|(out_port, layer)| {
                (layer..node_layer).for_each(|x| {
                    self.add_op(
                        MonoidalWiredOp::Copy {
                            addr: out_port.clone(),
                            copies: 1,
                        },
                        x,
                    )
                })
            });

        node.inputs()
            .map(|in_port| InPort::output(&in_port))
            .for_each(|out_port| {
                self.open_ports
                    .entry(out_port)
                    .or_default()
                    .push(node_layer + 1)
            });

        self.add_op(node.into(), node_layer);
    }
}

impl<G, V, E> From<&G> for MonoidalWiredGraph<V, E>
where
    G: GraphView<V, E>,
{
    fn from(graph: &G) -> Self {
        let mut builder = MonoidalWiredGraphBuilder::default();
        let outputs: Vec<InPort<V, E>> = graph.graph_outputs().collect();

        for in_port in outputs.iter() {
            builder
                .open_ports
                .entry(in_port.output())
                .or_default()
                .push(0);
        }

        for node in graph.nodes() {
            // Use topsorted graph here
            builder.insert_operation(node)
        }

        let remaining_ports: Vec<_> = builder.open_ports.keys().cloned().collect();

        let remaining_port_layers: Vec<_> = remaining_ports
            .into_iter()
            .map(|out_port| (out_port.clone(), builder.insert_copies_for_port(out_port)))
            .collect();

        let inputs = remaining_port_layers
            .into_iter()
            .map(|(out_port, layer)| {
                (layer..builder.slices.len()).for_each(|x| {
                    builder.add_op(
                        MonoidalWiredOp::Copy {
                            addr: out_port.clone(),
                            copies: 1,
                        },
                        x,
                    )
                });
                out_port
            })
            .collect();

        builder.slices.reverse();

        MonoidalWiredGraph {
            inputs,
            slices: builder.slices,
            outputs,
        }
    }
}

// #[derive(Default, PartialEq, Eq)]
// struct PortData {
//     ports_open: usize, // Number of open wires going to this port
//     multiplicity: usize, // Number of input ports seen that link to this port
// }

// impl PortData {
//     const ONE: Self = PortData {
//         ports_open: 1,
//         multiplicity: 1,
//     };
// }

// impl AddAssign for PortData {
//     fn add_assign(&mut self, rhs: Self) {
//         self.ports_open += rhs.ports_open;
//         self.multiplicity += rhs.multiplicity;
//     }
// }

// struct LayerIterator<V, E> {
//     nodes: PriorityQueue<Node<V,E>, Reverse<usize>>,
//     ready_ports: HashSet<OutPort<V, E>>,
//     incomplete_ports: HashMap<OutPort<V, E>, PortData>,
// }

// impl<V, E> LayerIterator<V, E> {
//     fn new(graph: &impl GraphView<V, E>) -> Self {
// 	let nodes = graph.nodes().map(|node| {
// 	    let outputs = node.outputs();
// 	    let non_deleted_outputs = outputs.filter(|out_port| out_port.number_of_inputs() != 0 );
// 	    let non_deleted_outputs_len = non_deleted_outputs.count();
// 	    (node, Reverse(non_deleted_outputs_len))
// 	}).collect();

//         let mut layer_iter = Self { nodes, incomplete_ports: Default::default(), ready_ports: Default::default() };

// 	// Add global graph outputs
//         for in_port in graph.graph_outputs() {
// 	    layer_iter.add_incomplete_port(in_port.output());
//         }

// 	layer_iter.complete_ports();
// 	layer_iter
//     }

//     fn add_incomplete_port(&mut self, port: OutPort<V, E>) {
// 	*self.incomplete_ports
//                 .entry(port)
//                 .or_insert(Default::default()) += PortData::ONE;
//     }

//     fn complete_ports(&mut self) {
// 	let ports: Vec<_> = self.incomplete_ports.keys().cloned().collect();
// 	for port in ports {
// 	    if self.incomplete_ports[&port] == (PortData {
// 		ports_open: 1,
// 		multiplicity: port.number_of_inputs(),
// 	    }) {
// 		self.incomplete_ports.remove(&port);
// 		if let Some(node) = port.node() {
// 		    self.nodes.change_priority_by(&node, |Reverse(x)| { *x -= 1; });
// 		}
// 		self.ready_ports.insert(port);
// 	    }
// 	}
//     }

//     fn next_ready(&mut self) -> Option<Node<V, E>> {
// 	if self.nodes.peek()?.1.0 == 0 {
// 	    Some(self.nodes.pop()?.0)
// 	} else {
// 	    None
// 	}
//     }
// }

// impl<V, E> Iterator for LayerIterator<V, E> {
//     type Item = Vec<MonoidalWiredOp<V, E>>;

//     fn next(&mut self) -> Option<Self::Item> {
// 	let mut modified = false; // Check for modification
//         let mut ops: <Self as Iterator>::Item = Default::default();
// 	let mut new_outports: Vec<OutPort<V,E>> = Default::default();

// 	// Process any nodes that are ready
// 	while let Some(node) = self.next_ready() {
// 	    modified = true;
// 	    for out_port in node.outputs() {
// 		self.ready_ports.remove(&out_port);
// 	    }
// 	    new_outports.extend(node.inputs().map(|in_port| in_port.output()));
// 	    ops.push(node.into());
// 	}

// 	// Any remaining ready ports should be passed through
// 	ops.extend(self.ready_ports.iter().map(|port| MonoidalWiredOp::Copy { addr: port.clone(), copies: 1 }));

// 	// Process incomplete ports
// 	for (port, data) in self.incomplete_ports.iter_mut() {
// 	    ops.push(MonoidalWiredOp::Copy { addr: port.clone(), copies: data.ports_open });
// 	    if data.ports_open != 1 {
// 		modified = true;
// 	    }
// 	    data.ports_open = 1;
// 	}

// 	// Add new ports to incomplete ports
// 	for port in new_outports {
// 	    self.add_incomplete_port(port);
// 	}

// 	// Check for completed ports
// 	self.complete_ports();

// 	if modified {
// 	    Some(ops)
// 	} else {
// 	    None
// 	}

//     }
// }

// impl<O: Copy + Debug, V: Debug + Clone> TryFrom<&HyperGraph<O, V>> for MonoidalWiredGraph<O> {
//     type Error = FromHyperError;

//     fn try_from(graph: &HyperGraph<O, V>) -> Result<Self, Self::Error> {
//         debug!("To Process: {:?}", graph);

//         // Separate the nodes into input nodes, output nodes, and other nodes by rank
//         let (ranks, input_wires, output_wires) = {
//             let (inputs, mut r, outputs) = graph.ranks_from_end();

//             debug!("Inputs: {:?}", inputs);
//             debug!("Ranks: {:?}", r);
//             debug!("Outputs: {:?}", outputs);

//             let input_wires = inputs
//                 .iter()
//                 .map(|x| graph.number_of_outputs(*x).unwrap())
//                 .sum();

//             let output_wires = outputs
//                 .iter()
//                 .map(|x| graph.get_inputs(*x).unwrap().map(|x| x.0).collect_vec())
//                 .concat();

//             r.push(inputs);

//             (r, input_wires, output_wires)
//         };

//         debug!("Input wires: {:?}", input_wires);
//         debug!("Output wires: {:?}", output_wires);

//         // Maintain a list of wires we have left to link up
//         let mut open_wires: Vec<Port> = output_wires;

//         // Build up slices and wirings
//         let mut slices: Vec<Slice<MonoidalWiredOp<O>>> = Vec::new();
//         let mut wirings: Vec<Wiring> = Vec::new();

//         // For each operation in the next layer, we store the following data.
//         #[derive(Debug)]
//         struct OpData<O> {
//             // The corresponding operation in the monoidal wired term, or none if we encounter an input/output node
//             op: Option<MonoidalWiredOp<O>>,
//             // The output ports of this operation
//             outputs: Vec<Port>,
//             // The average of the indices of the outputs in 'open_wires'
//             weight: Ratio<usize>,
//         }

//         for r in ranks {
//             // We build a list of OpData's for this layer
//             let mut ops: Vec<OpData<O>> = Vec::new();

//             // Also keep track of the subset of open wires which have a destination operation
//             let mut gathered_ports: HashSet<Port> = HashSet::new();

//             // Create an OpData for each node
//             for &node in r.iter() {
//                 debug!("Processing node {:?}", node);

//                 // Compute the weight
//                 let (sum, count) = open_wires
//                     .iter()
//                     .enumerate()
//                     .filter_map(|(i, Port { node: n, .. })| (node == *n).then_some((i, 1)))
//                     .fold((0, 0), |(x, y), (a, b)| (x + a, y + b));
//                 let weight = if count == 0 {
//                     // All outputs of this operation get deleted, so we arbitrarily put it on the right
//                     usize::MAX.into()
//                 } else {
//                     Ratio::new_raw(sum, count)
//                 };

//                 let outputs = (0..graph.number_of_outputs(node)?)
//                     .map(|index| Port {
//                         node,
//                         index: PortIndex(index),
//                     })
//                     .collect();

//                 gathered_ports.extend(&outputs);

//                 let op = match &graph[node] {
//                     Node::Weight(op) => Some(MonoidalWiredOp::Operation {
//                         addr: node,
//                         inputs: graph.get_inputs(node)?.map(|x| x.0).collect(),
//                         op_name: *op,
//                     }),
//                     Node::Input | Node::Output => None,
//                     Node::Thunk { args, body } => Some(MonoidalWiredOp::Thunk {
//                         addr: node,
//                         inputs: graph.get_inputs(node)?.map(|x| x.0).collect(),
//                         args: *args,
//                         body: MonoidalWiredGraph::try_from(body)?,
//                     }),
//                 };
//                 ops.push(OpData {
//                     op,
//                     outputs,
//                     weight,
//                 })
//             }

//             // Wires not in gathered_ports will be used in a later layer
//             // Therefore we must introduce an identity to route them through
//             for (i, port) in open_wires.iter().copied().enumerate() {
//                 if !gathered_ports.contains(&port) {
//                     gathered_ports.insert(port);
//                     ops.push(OpData {
//                         op: Some(MonoidalWiredOp::Id { port }),
//                         outputs: vec![port],
//                         weight: i.into(),
//                     });
//                 }
//             }

//             // Sort the ops by weight
//             ops.sort_by_key(|data| data.weight);

//             debug!("Operation layer generated: {:?}", ops);

//             // Compute the wiring from the open_wires to the operations
//             let out_nodes: HashMap<Port, usize> = ops
//                 .iter()
//                 .flat_map(|data| data.outputs.iter().copied())
//                 .enumerate()
//                 .map(|(x, y)| (y, x))
//                 .collect();

//             let number_of_out_ports = gathered_ports.len();

//             debug!("Out nodes: {:?}", out_nodes);

//             let mut wiring = Wiring::new(number_of_out_ports);

//             for p in open_wires {
//                 wiring.add_wire(*out_nodes.get(&p).ok_or(HyperGraphError::UnknownPort(p))?);
//             }

//             debug!("Wiring generated");

//             // We generate the new set of 'open_wires'.
//             // This is just the inputs of the layer we just generated
//             open_wires = ops
//                 .iter()
//                 .flat_map(|data| {
//                     data.op
//                         .as_ref()
//                         .map(|x| x.input_ports())
//                         .unwrap_or_default()
//                 })
//                 .collect();

//             debug!("Open wires: {:?}", open_wires);

//             // If any of the operations were None then this was the input slice
//             // Otherwise we should add this slice to the diagram
//             if let Some(ops) = ops
//                 .into_iter()
//                 .map(|data: OpData<O>| data.op)
//                 .collect::<Option<Vec<_>>>()
//             {
//                 slices.push(Slice { ops });
//             }

//             wirings.push(wiring);
//         }

//         // We worked bottom up, so we reverse the slices and wirings
//         slices.reverse();
//         wirings.reverse();

//         Ok(MonoidalWiredGraph {
//             inputs: input_wires,
//             slices,
//             wirings,
//         })
//     }
// }
