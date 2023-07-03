use std::{cmp::Reverse, collections::HashMap, fmt::Debug};

use derivative::Derivative;
use itertools::Itertools;
use tracing::debug;

use super::{MonoidalTerm, Slice};
use crate::{
    common::{Addr, Direction, InOut, InOutIter, Link},
    hypergraph::{Edge, Graph, Node},
};

/// A `MonoidalWiredGraph` stores the operations of a hypergraph layer by layer
/// It stores the copies of the graph, but does not store deletions, cups, or caps
///
/// The outputs of one slice do not need to exactly line up with the inputs of the next slice
/// and the operations in a monoidal wired graph can be freely permuted without breaking the graph
pub type MonoidalWiredGraph<T> = MonoidalTerm<T, WiredOp<T>>;

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "T::Edge: Debug, T::Thunk: Debug, T::Operation: Debug")
)]
pub enum WiredOp<T: Addr> {
    Copy {
        addr: T::Edge,
        copies: usize,
    },
    Operation {
        addr: T::Operation,
    },
    Thunk {
        addr: T::Thunk,
        body: MonoidalWiredGraph<T>,
    },
    Backlink {
        addr: T::Edge,
    },
}

impl<T: Addr> InOut for WiredOp<T> {
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

impl<T: Addr> InOutIter for WiredOp<T>
where
    T::Operation: InOutIter<T = T>,
    T::Thunk: InOutIter<T = T>,
{
    type T = T;

    fn input_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<T>> + 'a> {
        match self {
            WiredOp::Copy { addr, .. } => {
                Box::new(std::iter::once(Link(addr.clone(), Direction::Forward)))
            }
            WiredOp::Operation { addr } => addr.input_links(),
            WiredOp::Thunk { body, .. } => Box::new(
                body.free_inputs
                    .iter()
                    .map(|edge| Link(edge.clone(), Direction::Forward)),
            ),
            WiredOp::Backlink { addr } => {
                Box::new(std::iter::once(Link(addr.clone(), Direction::Backward)))
            }
        }
    }

    fn output_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<T>> + 'a> {
        match self {
            WiredOp::Copy { addr, copies } => {
                Box::new(std::iter::repeat(Link(addr.clone(), Direction::Forward)).take(*copies))
            }
            WiredOp::Operation { addr } => addr.output_links(),
            WiredOp::Thunk { addr, .. } => addr.output_links(),
            WiredOp::Backlink { addr } => {
                Box::new(std::iter::once(Link(addr.clone(), Direction::Backward)))
            }
        }
    }
}

impl<V: Debug, E: Debug> From<Node<V, E>> for WiredOp<(V, E)> {
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

/// The information we need to store about a "backlinked" edge
/// A edge is backlinked if if it travels upwards through the graph
struct BacklinkData {
    /// The layer from which we must build backlink operations
    available_layer: usize,
    /// The layer on which the edge is outputted to
    originating_layer: usize,
}

/// A structure to help build a monoidal wired graph
#[derive(Derivative)]
#[derivative(Default(bound = ""))]
struct MonoidalWiredGraphBuilder<T: Addr> {
    /// A vector of slices from the bottom up.
    /// Each operation in these slices is itself a slice of operations, allowing operations to be "bundled".
    slices: Vec<Slice<Slice<WiredOp<T>>>>,
    /// Edges that have been connected to an output yet.
    /// Each is mapped to a list of slice indices for where inputs of the edge have been seen.
    open_edges: HashMap<T::Edge, Vec<usize>>,
    /// Edges that have been connected to an output but not all their inputs.
    /// Each is mapped to a `BacklinkData`
    backlinks: HashMap<T::Edge, BacklinkData>,
}

impl<V: Debug, E: Debug> MonoidalWiredGraphBuilder<(V, E)> {
    /// Adds a slice of operations to a layer, creating this layer if it doesn't exist yet.
    /// Does not process any of the inputs or outputs
    fn add_op(&mut self, op: Slice<WiredOp<(V, E)>>, layer: usize) {
        while layer >= self.slices.len() {
            self.slices.push(Slice::default());
        }

        self.slices[layer].ops.push(op);
    }

    /// Determines the minimum layer from which we can output to the given `edge`
    fn edge_layer(&self, edge: &Edge<V, E>) -> usize {
        let layers = self.open_edges.get(edge);

        // Get the maximum layer that the slice is used on
        let max = layers
            .and_then(|x| x.iter().max())
            .copied()
            .unwrap_or_default();

        if edge.number_of_targets() > 1 {
            // If the edge has more than one target then we will need
            // to insert a copy before being able to output to it
            max + 1
        } else {
            max
        }
    }

    /// Insert copies and identities so that `edge` is ready to output to at `layer`
    fn prepare_input(&mut self, edge: &Edge<V, E>, layer: usize) {
        let mut layers = self.open_edges.remove(edge).unwrap_or_default();
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
                            addr: edge.clone(),
                            copies,
                        }],
                    },
                    current_layer,
                );
                current_layer += 1;
            }
        }
    }

    /// Adds a backlink operation for `edge` onto `layer`
    ///
    /// `is_cap` is true if this is the backlink just before a cap operation will need to be inserted
    ///
    /// The added backlink is inserted into the same "compound operation" (slice) as the operation whose
    /// input/output it will form a cap/cup with.
    /// This ensures that caps and cups are not made arbitrarily wide by swap minimisation.
    fn insert_backlink_on_layer(&mut self, edge: Edge<V, E>, layer: usize, is_cap: bool) {
        let ops = &mut self.slices[layer]
            .ops
            .iter_mut()
            .find(|x| {
                if is_cap {
                    x.input_links().map(|x| x.0).contains(&edge)
                } else {
                    x.output_links().map(|x| x.0).contains(&edge)
                }
            })
            .unwrap()
            .ops;

        let to_add = WiredOp::Backlink { addr: edge };

        if !ops.iter().contains(&to_add) {
            ops.push(to_add);
        }
    }

    /// Inserts a node of a hypergraph into the builder
    /// This prepares all the inputs of the node and inserts relevant backlinks
    fn insert_operation(&mut self, node: &Node<V, E>) {
        // The layer we place the node is the max of the layers that the outputs can be prepared
        // and the layers that any backlinked inputs originate.
        let node_layer = node
            .outputs()
            .map(|edge| self.edge_layer(&edge))
            .chain(
                node.inputs()
                    .filter_map(|x| self.backlinks.get(&x).map(|x| x.originating_layer)),
            )
            .max()
            .unwrap_or_default();

        let mut ops = vec![node.clone().into()];

        node.outputs().for_each(|edge| {
            let open_edges = self.open_edges.get(&edge).map(Vec::len).unwrap_or_default();

            if open_edges < edge.number_of_targets() {
                // We need to backlink the edge as it is not done
                if open_edges == 0 {
                    // Only backlink without copying
                    ops.push(WiredOp::Backlink { addr: edge.clone() });
                    self.backlinks.insert(
                        edge,
                        BacklinkData {
                            available_layer: node_layer + 1,
                            originating_layer: node_layer,
                        },
                    );
                } else {
                    // Backlink and other edges
                    self.open_edges
                        .entry(edge.clone())
                        .or_default()
                        .push(node_layer - 1);
                    self.prepare_input(&edge, node_layer);
                    self.insert_backlink_on_layer(edge.clone(), node_layer - 1, false);
                    self.backlinks.insert(
                        edge,
                        BacklinkData {
                            available_layer: node_layer,
                            originating_layer: node_layer,
                        },
                    );
                }
            } else {
                // No backlink
                self.prepare_input(&edge, node_layer);
            }
        });

        // The inputs to the node are now open edges
        node.inputs().for_each(|edge| {
            self.open_edges
                .entry(edge)
                .or_default()
                .push(node_layer + 1);
        });

        self.add_op(Slice { ops }, node_layer);
    }
}

#[allow(clippy::fallible_impl_from)]
impl<G, V: Debug, E: Debug> From<&G> for MonoidalWiredGraph<(V, E)>
where
    G: Graph<NodeWeight = V, EdgeWeight = E>,
{
    fn from(graph: &G) -> Self {
        let mut builder = MonoidalWiredGraphBuilder::default();
        let outputs: Vec<Edge<V, E>> = graph.graph_outputs().collect();

        for edge in &outputs {
            builder.open_edges.entry(edge.clone()).or_default().push(0);
        }

        for node in graph.nodes() {
            debug!("Node recieved: {node:#?}");
            // Use topsorted graph here
            builder.insert_operation(&node);
        }

        let (backlinked_edges, other_edges): (Vec<_>, Vec<_>) = builder
            .open_edges
            .keys()
            .cloned()
            .partition(|x| builder.backlinks.get(x).map(|y| (x, y)).is_some());

        // Connect up backlinks
        for edge in backlinked_edges {
            let backlink = builder.backlinks.remove(&edge).unwrap();
            let open_edges = &builder.open_edges[&edge];

            let layer = if open_edges.len() == 1 {
                let layer = open_edges[0] - 1;
                builder.insert_backlink_on_layer(edge.clone(), layer, true);
                layer
            } else {
                let layer = *open_edges.iter().max().unwrap();
                builder.prepare_input(&edge, layer + 1);
                builder.insert_backlink_on_layer(edge.clone(), layer, true);
                layer
            };

            for x in backlink.available_layer..layer {
                builder.add_op(
                    Slice {
                        ops: vec![WiredOp::Backlink { addr: edge.clone() }],
                    },
                    x,
                );
            }
        }

        let final_height = std::cmp::max(
            builder.slices.len(),
            other_edges
                .iter()
                .map(|edge| builder.edge_layer(edge))
                .max()
                .unwrap_or_default(),
        );

        // Connect up global inputs
        for edge in other_edges {
            builder.prepare_input(&edge, final_height);
        }

        builder.slices.reverse();

        let mut graph = MonoidalTerm::<(V, E), Slice<WiredOp<(V, E)>>> {
            free_inputs: graph.unbound_graph_inputs().collect(),
            bound_inputs: graph.bound_graph_inputs().collect(),
            slices: builder.slices,
            outputs,
        };

        // We can minimise swaps, keeping "compound terms" together
        graph.minimise_swaps();

        // After this we can flatten the "compound terms"
        graph.flatten_graph()
    }
}
