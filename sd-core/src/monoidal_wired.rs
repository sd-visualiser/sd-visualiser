use std::{cmp::Reverse, collections::HashMap, fmt::Debug};

use crate::{
    common::{Direction, InOut, Link, MonoidalTerm, Slice},
    hypergraph::{GraphView, InPort, Node, Operation, OutPort, Thunk},
};
use derivative::Derivative;

pub type MonoidalWiredGraph<V, E> = MonoidalTerm<WiredOp<V, E>>;

#[derive(Clone, Debug)]
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
    type V = V;
    type E = E;

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

    fn inputs<'a>(&'a self) -> Box<dyn Iterator<Item = Link<V, E>> + 'a> {
        match self {
            WiredOp::Copy { addr, .. } => {
                Box::new(std::iter::once((addr.clone(), Direction::Forward)))
            }
            WiredOp::Operation { addr } => Box::new(
                addr.inputs()
                    .map(|in_port| (in_port.output(), Direction::Forward)),
            ),
            WiredOp::Thunk { addr, body } => {
                Box::new(body.unordered_inputs.iter().filter_map(|port| {
                    addr.externalise_input(port)
                        .map(|x| (x.output(), Direction::Forward))
                }))
            }
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

impl<V, E> From<Node<V, E>> for WiredOp<V, E> {
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

#[derive(Derivative)]
#[derivative(Default(bound = ""))]
struct MonoidalWiredGraphBuilder<V, E> {
    slices: Vec<Slice<Slice<WiredOp<V, E>>>>,
    open_ports: HashMap<OutPort<V, E>, Vec<usize>>,
    backlinks: HashMap<OutPort<V, E>, usize>,
}

impl<V, E> MonoidalWiredGraphBuilder<V, E> {
    fn add_op(&mut self, op: Slice<WiredOp<V, E>>, layer: usize) {
        assert!(layer <= self.slices.len());
        if let Some(slice) = self.slices.get_mut(layer) {
            slice
        } else {
            self.slices.push(Slice::default());
            &mut self.slices[layer]
        }
        .ops
        .push(op);
    }

    fn input_layer(&self, out_port: &OutPort<V, E>) -> usize {
        let layers = self.open_ports.get(out_port);
        let len = layers.map(Vec::len).unwrap_or_default();
        let max = layers
            .and_then(|x| x.iter().max())
            .copied()
            .unwrap_or_default();

        if len > 1 {
            max + 1
        } else {
            max
        }
    }

    fn prepare_input(&mut self, out_port: &OutPort<V, E>, layer: usize) -> bool {
        let mut layers = self.open_ports.remove(out_port).unwrap_or_default();
        let with_backlink =
            layers.len() < out_port.number_of_inputs() || self.backlinks.contains_key(out_port);
        layers.sort_by_key(|x| Reverse(*x));
        if let Some(mut current_layer) = layers.pop() {
            if current_layer == layer {
                return false; // No copies are needed and we are already at the correct level
            }
            while current_layer + 2 <= layer {
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

        if with_backlink {
            self.add_op(
                Slice {
                    ops: vec![
                        WiredOp::Copy {
                            addr: out_port.clone(),
                            copies: layers.len() + 2,
                        },
                        WiredOp::Backlink {
                            addr: out_port.clone(),
                        },
                    ],
                },
                layer - 1,
            );
        } else if out_port.number_of_inputs() != 0 {
            self.add_op(
                Slice {
                    ops: vec![WiredOp::Copy {
                        addr: out_port.clone(),
                        copies: layers.len() + 1,
                    }],
                },
                layer - 1,
            );
        }
        with_backlink
    }

    fn insert_operation(&mut self, node: Node<V, E>) {
        let node_layer = node
            .outputs()
            .map(|out_port| self.input_layer(&out_port))
            .max()
            .unwrap_or_default();

        node.outputs().for_each(|out_port| {
            if self.prepare_input(&out_port, node_layer) {
                self.backlinks.insert(out_port, node_layer);
            }
        });

        node.inputs()
            .map(|in_port| InPort::output(&in_port))
            .for_each(|out_port| {
                self.open_ports
                    .entry(out_port)
                    .or_default()
                    .push(node_layer + 1);
            });

        self.add_op(
            Slice {
                ops: vec![node.into()],
            },
            node_layer,
        );
    }
}

impl<G, V, E> From<&G> for MonoidalWiredGraph<V, E>
where
    G: GraphView<V, E>,
{
    fn from(graph: &G) -> Self {
        let mut builder = MonoidalWiredGraphBuilder::default();
        let outputs: Vec<InPort<V, E>> = graph.graph_outputs().collect();

        for in_port in &outputs {
            builder
                .open_ports
                .entry(in_port.output())
                .or_default()
                .push(0);
        }

        for node in graph.nodes() {
            // Use topsorted graph here
            builder.insert_operation(node);
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
                for op in &mut builder.slices[layer].ops {
                    if op.inputs().any(|in_port| in_port.0 == out_port) {
                        op.ops.push(WiredOp::Backlink {
                            addr: out_port.clone(),
                        });
                    }
                }
                layer
            } else {
                let layer = open_ports.iter().max().unwrap() + 1;
                builder.prepare_input(&out_port, layer);
                layer
            };

            for x in backlink + 1..layer {
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

        let mut graph = MonoidalTerm::<Slice<WiredOp<V, E>>> {
            unordered_inputs: graph.unbound_graph_inputs().collect(),
            ordered_inputs: graph.bound_graph_inputs().collect(),
            slices: builder.slices,
            outputs,
        };

        graph.minimise_swaps();

        graph.flatten_graph()
    }
}
