use std::{cmp::Reverse, collections::HashMap};

use derivative::Derivative;
use good_lp::{variable, Expression, Solution, Variable};
use indexmap::IndexMap;
use itertools::Itertools;
use tracing::debug;

use super::{MonoidalTerm, Slice};
use crate::{
    common::{Direction, InOut, InOutIter, Link},
    hypergraph::{
        generic::{Ctx, Edge, Node},
        traits::{Graph, NodeLike},
        utils::{normalised_targets, number_of_normalised_targets},
    },
    lp::LpProblem,
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
    Debug(bound = "")
)]
pub enum WiredOp<T: Ctx> {
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

impl<T: Ctx> InOut for WiredOp<T> {
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

impl<T: Ctx> InOutIter for WiredOp<T> {
    type T = T;

    fn input_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<T>> + 'a> {
        match self {
            WiredOp::Copy { addr, .. } => {
                Box::new(std::iter::once((addr.clone(), Direction::Forward)))
            }
            WiredOp::Operation { addr } => {
                Box::new(addr.inputs().map(|edge| (edge, Direction::Forward)))
            }
            WiredOp::Thunk { body, .. } => Box::new(
                body.free_inputs
                    .iter()
                    .map(|edge| (edge.clone(), Direction::Forward)),
            ),
            WiredOp::Backlink { addr } => {
                Box::new(std::iter::once((addr.clone(), Direction::Backward)))
            }
        }
    }

    fn output_links<'a>(&'a self) -> Box<dyn Iterator<Item = Link<T>> + 'a> {
        match self {
            WiredOp::Copy { addr, copies } => {
                Box::new(std::iter::repeat((addr.clone(), Direction::Forward)).take(*copies))
            }
            WiredOp::Operation { addr } => {
                Box::new(addr.outputs().map(|edge| (edge, Direction::Forward)))
            }
            WiredOp::Thunk { addr, .. } => {
                Box::new(addr.outputs().map(|edge| (edge, Direction::Forward)))
            }
            WiredOp::Backlink { addr } => {
                Box::new(std::iter::once((addr.clone(), Direction::Backward)))
            }
        }
    }
}

/// A structure to help build a monoidal wired graph
#[derive(Derivative)]
#[derivative(Default(bound = ""))]
struct MonoidalWiredGraphBuilder<T: Ctx> {
    /// A vector of slices from the bottom up.
    /// Each operation in these slices is itself a slice of operations, allowing operations to be "bundled".
    slices: Vec<Slice<Slice<WiredOp<T>>>>,
    /// Edges that have been connected to an output yet.
    /// Each is mapped to a list of slice indices for where inputs of the edge have been seen.
    open_edges: IndexMap<T::Edge, Vec<usize>>,
    /// Edges that have been connected to an output but not all their inputs.
    /// Each is mapped to a `BacklinkData`
    backlinks: HashMap<T::Edge, usize>,
}

impl<T: Ctx> MonoidalWiredGraphBuilder<T> {
    /// Adds a slice of operations to a layer, creating this layer if it doesn't exist yet.
    /// Does not process any of the inputs or outputs
    fn add_op(&mut self, op: Slice<WiredOp<T>>, layer: usize) {
        while layer >= self.slices.len() {
            self.slices.push(Slice::default());
        }

        self.slices[layer].ops.push(op);
    }

    /// Insert copies and identities so that `edge` is ready to output to at `layer`
    fn prepare_input(&mut self, edge: &T::Edge, layer: usize) {
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
    fn insert_backlink_on_layer(&mut self, edge: T::Edge, layer: usize, is_cap: bool) {
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
    fn insert_operation(&mut self, node: &Node<T>, node_layer: usize) {
        let wired_op = match node {
            Node::Operation(op) => WiredOp::Operation { addr: op.clone() },
            Node::Thunk(thunk) => WiredOp::Thunk {
                body: MonoidalWiredGraph::from(thunk),
                addr: thunk.clone(),
            },
        };
        let mut ops = vec![wired_op];

        node.outputs().for_each(|edge| {
            let open_edges = self.open_edges.get(&edge).map(Vec::len).unwrap_or_default();

            if open_edges < number_of_normalised_targets::<T>(&edge) {
                // We need to backlink the edge as it is not done
                if open_edges == 0 {
                    // Only backlink without copying
                    ops.push(WiredOp::Backlink { addr: edge.clone() });
                    self.backlinks.insert(edge, node_layer + 1);
                } else {
                    // Backlink and other edges
                    self.open_edges
                        .entry(edge.clone())
                        .or_default()
                        .push(node_layer - 1);
                    self.prepare_input(&edge, node_layer);
                    self.insert_backlink_on_layer(edge.clone(), node_layer - 1, false);
                    self.backlinks.insert(edge, node_layer);
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
impl<G: Graph> From<&G> for MonoidalWiredGraph<G::Ctx> {
    #[allow(clippy::too_many_lines)]
    #[allow(clippy::cast_possible_truncation)]
    #[allow(clippy::cast_sign_loss)]
    fn from(graph: &G) -> Self {
        let mut problem = LpProblem::default();
        let max = problem.add_variable(variable().min(0.0));
        let nodes: IndexMap<Node<G::Ctx>, Variable> = graph
            .nodes()
            .map(|x| (x, problem.add_variable(variable().min(0.0))))
            .collect();

        for (i, (node, var)) in nodes.iter().enumerate() {
            problem.add_constraint(Expression::leq((*var).into(), max));
            for edge in node.outputs() {
                let targets = normalised_targets::<G::Ctx>(&edge, &node.backlink());
                let bottom = problem.add_variable(variable().min(0.0));
                let offset = if targets.len() > 1 { 1.0 } else { 0.0 };
                problem.add_constraint(Expression::leq(bottom + offset, var));
                let top = problem.add_variable(variable().min(0.0));
                problem.add_constraint(Expression::leq((*var).into(), top));
                problem.add_objective(top - bottom);

                for target in targets {
                    if let Some(target_node) = target {
                        let (j, _, var_target) = nodes.get_full(&target_node).unwrap();
                        problem.add_constraint(Expression::leq(bottom.into(), var_target));
                        problem.add_constraint(Expression::geq(top.into(), *var_target));
                        match j.cmp(&i) {
                            std::cmp::Ordering::Less => {
                                problem.add_constraint((*var_target + offset + 1.0).leq(*var));
                            }
                            std::cmp::Ordering::Equal => {}
                            std::cmp::Ordering::Greater => {
                                problem.add_constraint(Expression::leq((*var).into(), var_target));
                            }
                        };
                    } else {
                        problem.add_constraint(Expression::eq(bottom.into(), 0.0));
                    }
                }
            }
        }

        for edge in graph.graph_inputs() {
            let targets = normalised_targets::<G::Ctx>(&edge, &graph.graph_backlink());
            let bottom = problem.add_variable(variable().min(0.0));
            let offset = if targets.len() > 1 { 1.0 } else { 0.0 };
            problem.add_constraint(Expression::leq(bottom + offset, max));
            problem.add_objective(max - bottom);
            for target in targets {
                if let Some(target_node) = target {
                    let var_target = nodes.get(&target_node).unwrap();
                    problem.add_constraint(Expression::leq(bottom.into(), *var_target));
                    problem.add_constraint((*var_target + offset).leq(max));
                } else {
                    problem.add_constraint(Expression::eq(bottom.into(), 0.0));
                }
            }
        }
        problem.add_objective(max);

        let soln = problem.minimise(good_lp::default_solver).unwrap();

        let mut builder = MonoidalWiredGraphBuilder::<G::Ctx>::default();
        let outputs: Vec<Edge<G::Ctx>> = graph.graph_outputs().collect();

        for edge in &outputs {
            builder.open_edges.entry(edge.clone()).or_default().push(0);
        }

        for (node, var) in nodes {
            debug!("Node recieved: {node:?}");
            // Use topsorted graph here
            builder.insert_operation(&node, soln.value(var).floor() as usize);
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

            for x in backlink..layer {
                builder.add_op(
                    Slice {
                        ops: vec![WiredOp::Backlink { addr: edge.clone() }],
                    },
                    x,
                );
            }
        }

        let final_height = soln.value(max).floor() as usize + 1;

        // Connect up global inputs
        for edge in other_edges {
            builder.prepare_input(&edge, final_height);
        }

        builder.slices.reverse();

        let mut graph = MonoidalTerm::<G::Ctx, Slice<WiredOp<G::Ctx>>> {
            free_inputs: graph.free_graph_inputs().collect(),
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
