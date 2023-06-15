use std::{cmp::Reverse, collections::HashMap, fmt::Debug};

use derivative::Derivative;
use itertools::Itertools;
use tracing::debug;

use crate::{
    common::{Direction, InOut, InOutIter, Link, MonoidalTerm, Slice},
    hypergraph::{Graph, InPort, Node, Operation, OutPort, Thunk},
};

pub type MonoidalWiredGraph<V, E> = MonoidalTerm<(V, E), WiredOp<V, E>>;

#[derive(Clone, Debug, Derivative, Eq)]
#[derivative(PartialEq(bound = ""))]
pub enum WiredOp<V, E> {
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
    Backlink {
        addr: OutPort<V, E>,
    },
}

impl<V, E> InOut for WiredOp<V, E> {
    fn number_of_inputs(&self) -> usize {
        match self {
            WiredOp::Copy { copies, .. } => *copies,
            WiredOp::Operation { addr } => addr.number_of_inputs(),
            WiredOp::Thunk { addr, .. } => addr.number_of_inputs(),
            WiredOp::Backlink { .. } => 1,
        }
    }

    fn number_of_outputs(&self) -> usize {
        match self {
            WiredOp::Copy { copies, .. } => *copies,
            WiredOp::Operation { addr } => addr.number_of_outputs(),
            WiredOp::Thunk { addr, .. } => addr.number_of_outputs(),
            WiredOp::Backlink { .. } => 1,
        }
    }
}

impl<V, E> InOutIter for WiredOp<V, E> {
    type V = V;
    type E = E;

    fn inputs<'a>(&'a self) -> Box<dyn Iterator<Item = Link<V, E>> + 'a> {
        match self {
            WiredOp::Copy { addr, .. } => {
                Box::new(std::iter::once((addr.clone(), Direction::Forward)))
            }
            WiredOp::Operation { addr } => Box::new(Box::new(
                addr.inputs().map(|out_port| (out_port, Direction::Forward)),
            )),
            WiredOp::Thunk { body, .. } => Box::new(
                body.free_inputs
                    .iter()
                    .map(|out_port| (out_port.clone(), Direction::Forward)),
            ),
            WiredOp::Backlink { addr } => {
                Box::new(std::iter::once((addr.clone(), Direction::Backward)))
            }
        }
    }

    fn outputs<'a>(&'a self) -> Box<dyn Iterator<Item = Link<V, E>> + 'a> {
        match self {
            WiredOp::Copy { addr, copies } => {
                Box::new(std::iter::repeat((addr.clone(), Direction::Forward)).take(*copies))
            }
            WiredOp::Operation { addr } => Box::new(
                addr.outputs()
                    .map(|out_port| (out_port, Direction::Forward)),
            ),
            WiredOp::Thunk { addr, body } => Box::new(
                body.outputs
                    .iter()
                    .map(|port| (addr.externalise_output(port).unwrap(), Direction::Forward)),
            ),
            WiredOp::Backlink { addr } => {
                Box::new(std::iter::once((addr.clone(), Direction::Backward)))
            }
        }
    }
}

impl<V: Debug, E: Debug> From<Node<V, E>> for WiredOp<V, E> {
    fn from(value: Node<V, E>) -> Self {
        match value {
            Node::Operation(op) => WiredOp::Operation { addr: op },
            Node::Thunk(thunk) => WiredOp::Thunk {
                body: (&thunk).into(),
                addr: thunk,
            },
        }
    }
}

struct BacklinkData {
    available_layer: usize,
    originating_layer: usize,
}

#[derive(Derivative)]
#[derivative(Default(bound = ""))]
struct MonoidalWiredGraphBuilder<V, E> {
    slices: Vec<Slice<Slice<WiredOp<V, E>>>>,
    open_ports: HashMap<OutPort<V, E>, Vec<usize>>,
    backlinks: HashMap<OutPort<V, E>, BacklinkData>,
}

impl<V: Debug, E: Debug> MonoidalWiredGraphBuilder<V, E> {
    fn add_op(&mut self, op: Slice<WiredOp<V, E>>, layer: usize) {
        while layer >= self.slices.len() {
            self.slices.push(Slice::default());
        }

        self.slices[layer].ops.push(op);
    }

    fn input_layer(&self, out_port: &OutPort<V, E>) -> usize {
        let layers = self.open_ports.get(out_port);
        let max = layers
            .and_then(|x| x.iter().max())
            .copied()
            .unwrap_or_default();

        if out_port.number_of_links() > 1 {
            max + 1
        } else {
            max
        }
    }

    fn prepare_input(&mut self, out_port: &OutPort<V, E>, layer: usize) {
        let mut layers = self.open_ports.remove(out_port).unwrap_or_default();
        layers.sort_by_key(|x| Reverse(*x));
        if let Some(mut current_layer) = layers.pop() {
            while current_layer < layer {
                let mut copies = 1;
                while layers
                    .last()
                    .map(|x| *x == current_layer)
                    .unwrap_or_default()
                {
                    layers.pop();
                    copies += 1;
                }
                self.add_op(
                    Slice {
                        ops: vec![WiredOp::Copy {
                            addr: out_port.clone(),
                            copies,
                        }],
                    },
                    current_layer,
                );
                current_layer += 1;
            }
        }
    }

    fn insert_backlink_on_layer(&mut self, out_port: OutPort<V, E>, layer: usize, is_cap: bool) {
        let ops = &mut self.slices[layer]
            .ops
            .iter_mut()
            .find(|x| {
                if is_cap {
                    x.inputs().map(|x| x.0).contains(&out_port)
                } else {
                    x.outputs().map(|x| x.0).contains(&out_port)
                }
            })
            .unwrap()
            .ops;

        let to_add = WiredOp::Backlink { addr: out_port };

        if !ops.iter().contains(&to_add) {
            ops.push(to_add);
        }
    }

    fn insert_operation(&mut self, node: &Node<V, E>) {
        let node_layer = node
            .outputs()
            .map(|out_port| self.input_layer(&out_port))
            .chain(
                node.inputs()
                    .filter_map(|x| self.backlinks.get(&x).map(|x| x.originating_layer)),
            )
            .max()
            .unwrap_or_default();

        let mut ops = vec![node.clone().into()];

        node.outputs().for_each(|out_port| {
            let open_ports = self
                .open_ports
                .get(&out_port)
                .map(Vec::len)
                .unwrap_or_default();

            if open_ports < out_port.number_of_links() {
                if open_ports == 0 {
                    // Only backlink
                    ops.push(WiredOp::Backlink {
                        addr: out_port.clone(),
                    });
                    self.backlinks.insert(
                        out_port,
                        BacklinkData {
                            available_layer: node_layer + 1,
                            originating_layer: node_layer,
                        },
                    );
                } else {
                    // Backlink and other ports
                    self.open_ports
                        .entry(out_port.clone())
                        .or_default()
                        .push(node_layer - 1);
                    self.prepare_input(&out_port, node_layer);
                    self.insert_backlink_on_layer(out_port.clone(), node_layer - 1, false);
                    self.backlinks.insert(
                        out_port,
                        BacklinkData {
                            available_layer: node_layer,
                            originating_layer: node_layer,
                        },
                    );
                }
            } else {
                // No backlink
                self.prepare_input(&out_port, node_layer);
            }
        });

        node.inputs().for_each(|out_port| {
            self.open_ports
                .entry(out_port)
                .or_default()
                .push(node_layer + 1);
        });

        self.add_op(Slice { ops }, node_layer);
    }
}

impl<G, V: Debug, E: Debug> From<&G> for MonoidalWiredGraph<V, E>
where
    G: Graph<NodeWeight = V, EdgeWeight = E>,
{
    fn from(graph: &G) -> Self {
        let mut builder = MonoidalWiredGraphBuilder::default();
        let outputs: Vec<InPort<V, E>> = graph.graph_outputs().collect();

        for in_port in &outputs {
            builder
                .open_ports
                .entry(in_port.link())
                .or_default()
                .push(0);
        }

        for node in graph.nodes() {
            debug!("Node recieved: {node:#?}");
            // Use topsorted graph here
            builder.insert_operation(&node);
        }

        let remaining_ports: Vec<_> = builder.open_ports.keys().cloned().collect();

        let (backlinked_ports, other_ports): (Vec<_>, Vec<_>) = remaining_ports
            .into_iter()
            .partition(|x| builder.backlinks.get(x).map(|y| (x, y)).is_some());

        for out_port in backlinked_ports {
            let backlink = builder.backlinks.remove(&out_port).unwrap();
            let open_ports = &builder.open_ports[&out_port];

            let layer = if open_ports.len() == 1 {
                let layer = open_ports[0] - 1;
                builder.insert_backlink_on_layer(out_port.clone(), layer, true);
                layer
            } else {
                let layer = *open_ports.iter().max().unwrap();
                builder.prepare_input(&out_port, layer + 1);
                builder.insert_backlink_on_layer(out_port.clone(), layer, true);
                layer
            };

            for x in backlink.available_layer..layer {
                builder.add_op(
                    Slice {
                        ops: vec![WiredOp::Backlink {
                            addr: out_port.clone(),
                        }],
                    },
                    x,
                );
            }
        }

        let final_height = std::cmp::max(
            builder.slices.len(),
            other_ports
                .iter()
                .map(|out_port| builder.input_layer(out_port))
                .max()
                .unwrap_or_default(),
        );

        for out_port in other_ports {
            builder.prepare_input(&out_port, final_height);
        }

        builder.slices.reverse();

        let mut graph = MonoidalTerm::<(V, E), Slice<WiredOp<V, E>>> {
            free_inputs: graph.unbound_graph_inputs().collect(),
            bound_inputs: graph.bound_graph_inputs().collect(),
            slices: builder.slices,
            outputs,
        };

        graph.minimise_swaps();

        graph.flatten_graph()
    }
}
