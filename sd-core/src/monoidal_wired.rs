use std::{cmp::Reverse, collections::HashMap, fmt::Debug};

use crate::{
    common::{generate_permutation, InOut, Slice},
    hypergraph::{GraphView, InPort, Node, Operation, OutPort, Thunk},
};
use bimap::BiMap;
use derivative::Derivative;
use num::rational::Ratio;
use tracing::debug;

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
            MonoidalWiredOp::Copy { addr, .. } => Box::new(std::iter::once(addr.clone())),
            MonoidalWiredOp::Operation { addr } => {
                Box::new(addr.inputs().map(|in_port| in_port.output()))
            }
            MonoidalWiredOp::Thunk { addr, body } => Box::new(
                body.inputs
                    .iter()
                    .filter_map(|port| addr.externalise_input(port).map(|x| x.output())),
            ),
        }
    }

    fn outputs<'a>(&'a self) -> Box<dyn Iterator<Item = OutPort<V, E>> + 'a>
    where
        V: 'a,
        E: 'a,
    {
        match self {
            MonoidalWiredOp::Copy { addr, copies } => {
                Box::new(std::iter::repeat(addr.clone()).take(*copies))
            }
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

        remaining_port_layers
            .into_iter()
            .for_each(|(out_port, layer)| {
                (layer..builder.slices.len()).for_each(|x| {
                    builder.add_op(
                        MonoidalWiredOp::Copy {
                            addr: out_port.clone(),
                            copies: 1,
                        },
                        x,
                    )
                });
            });

        builder.slices.reverse();

        let mut graph = MonoidalWiredGraph {
            inputs: graph.graph_inputs().collect(),
            slices: builder.slices,
            outputs,
        };

        graph.minimise_swaps();

        graph
    }
}

impl<V, E> MonoidalWiredGraph<V, E> {
    pub fn minimise_swaps(&mut self) {
        fn fold_slice<'a, V, E>(
            ports_below: Box<dyn Iterator<Item = OutPort<V, E>> + 'a>,
            slice: &'a mut Slice<MonoidalWiredOp<V, E>>,
        ) -> Box<dyn Iterator<Item = OutPort<V, E>> + 'a> {
            slice.minimise_swaps(ports_below);
            slice.inputs()
        }

        let ports_below = self.slices.iter_mut().rev().fold(
            Box::new(self.outputs.iter().map(|in_port| in_port.output()))
                as Box<dyn Iterator<Item = OutPort<V, E>>>,
            fold_slice::<V, E>,
        );

        let perm_map: HashMap<OutPort<V, E>, Option<usize>> =
            generate_permutation(self.inputs.iter().cloned(), ports_below).collect();

        self.inputs.sort_by_key(|out_port| {
            perm_map
                .get(out_port)
                .copied()
                .flatten()
                .unwrap_or(usize::MAX)
        });
    }
}

impl<V, E> Slice<MonoidalWiredOp<V, E>> {
    pub fn minimise_swaps(&mut self, ports_below: impl Iterator<Item = OutPort<V, E>>) {
        let outputs = self.outputs();

        let perm_map: HashMap<OutPort<V, E>, Option<usize>> =
            generate_permutation(outputs, ports_below).collect();

        debug!("Map: {:#?}", perm_map);

        self.ops.sort_by_cached_key(|op| -> Ratio<usize> {
            match op
                .outputs()
                .filter_map(|out_port| perm_map.get(&out_port).copied().flatten())
                .fold((0, 0), |(a, b), c| (a + c, b + 1))
            {
                (_, 0) => usize::MAX.into(),
                (x, y) => Ratio::new_raw(x, y),
            }
        })
    }
}
