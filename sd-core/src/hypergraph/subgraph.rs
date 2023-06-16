use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use super::{fragment::Fragment, Graph, HyperGraph, InPort, Node, Operation, OutPort};
use crate::{
    common::InOut,
    graph::Name,
    language::{chil, spartan, Language},
};

pub trait Free {
    fn is_var(&self) -> bool;
    fn generate_free(number: usize) -> Self;
}

impl Free for chil::Variable {
    fn is_var(&self) -> bool {
        true
    }

    fn generate_free(number: usize) -> Self {
        Self {
            name: None,
            addr: chil::Addr('?', number),
        }
    }
}

impl Free for spartan::Variable {
    fn is_var(&self) -> bool {
        true
    }

    fn generate_free(number: usize) -> Self {
        Self(format!("?{number}"))
    }
}

impl<T> Free for Name<T>
where
    T: Language,
    T::Var: Free,
{
    fn is_var(&self) -> bool {
        self.to_var().is_some()
    }

    fn generate_free(number: usize) -> Self {
        Self::FreeVar(T::Var::generate_free(number))
    }
}

fn normalise_graph<G: Graph>(
    graph_view: &G,
    selection: &HashSet<Operation<G::NodeWeight, G::EdgeWeight>>,
) -> HashSet<Node<G::NodeWeight, G::EdgeWeight>> {
    let selected_nodes: HashSet<Node<G::NodeWeight, G::EdgeWeight>> = graph_view
        .nodes()
        .filter(|node| match node {
            Node::Operation(op) => selection.contains(op),
            Node::Thunk(thunk) => contains_selection(thunk, selection),
        })
        .collect();

    if selected_nodes.len() == 1 {
        if let Node::Thunk(thunk) = selected_nodes.iter().next().unwrap() {
            return normalise_graph(thunk, selection);
        }
    }

    selected_nodes
}

fn contains_selection<G: Graph>(
    graph_view: &G,
    selection: &HashSet<Operation<G::NodeWeight, G::EdgeWeight>>,
) -> bool {
    graph_view.operations().any(|op| selection.contains(&op))
        || graph_view
            .thunks()
            .any(|thunk| contains_selection(&thunk, selection))
}

impl<V, E> HyperGraph<V, E>
where
    V: Debug + Send + Sync + Clone,
    E: Debug + Send + Sync + Clone + Free,
{
    #[must_use]
    pub fn normalise_selection(&self, selection: &HashSet<Operation<V, E>>) -> HashSet<Node<V, E>> {
        normalise_graph(self, selection)
    }

    #[must_use]
    pub fn generate_subgraph(&self, selection: &HashSet<Node<V, E>>) -> HyperGraph<V, E> {
        let global_inputs: HashSet<OutPort<V, E>> = selection
            .iter()
            .flat_map(|node| node.inputs().map(|in_port| in_port.link()))
            .filter(|out_port| {
                out_port
                    .node()
                    .filter(|node| selection.contains(node))
                    .is_none()
            })
            .collect();

        let global_outputs: HashSet<InPort<V, E>> = selection
            .iter()
            .flat_map(Node::outputs)
            .flat_map(|out_port| out_port.links())
            .filter(|in_port| {
                in_port
                    .node()
                    .filter(|node| selection.contains(node))
                    .is_none()
            })
            .collect();

        let mut input_weights: Vec<E> =
            global_inputs.iter().map(OutPort::weight).cloned().collect();

        let mut count = 0;

        for name in &mut input_weights {
            if !name.is_var() {
                *name = E::generate_free(count);
                count += 1;
            }
        }

        let mut builder = HyperGraph::new(input_weights, global_outputs.len());

        // Mapping from in_ports of the subgraph to in_ports of the original graph
        let mut in_port_map: HashMap<InPort<V, E, false>, InPort<V, E>> =
            builder.graph_outputs().zip(global_outputs).collect();

        // Mapping from out_ports of the original graph to out_ports of the subgraph
        let mut out_port_map: HashMap<OutPort<V, E>, OutPort<V, E, false>> = global_inputs
            .into_iter()
            .zip(builder.graph_inputs())
            .collect();

        for node in self.nodes().filter(|node| selection.contains(node)) {
            let new_node = match &node {
                Node::Operation(op) => {
                    let input_len = op.number_of_inputs();
                    let output_weights: Vec<_> = op
                        .outputs()
                        .map(|out_port| out_port.weight().clone())
                        .collect();
                    let weight: V = op.weight().clone();

                    Node::Operation(builder.add_operation(input_len, output_weights, weight))
                }
                Node::Thunk(thunk) => Node::Thunk(builder.clone_thunk(thunk)),
            };

            in_port_map.extend(new_node.inputs().zip(node.inputs()));

            out_port_map.extend(node.outputs().zip(new_node.outputs()));
        }

        // Link up ports

        for (x, y) in in_port_map {
            builder.link(out_port_map[&y.link()].clone(), x).unwrap();
        }

        builder.build().unwrap()
    }
}
