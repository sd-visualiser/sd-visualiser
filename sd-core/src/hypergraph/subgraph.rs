use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use indexmap::IndexSet;

use super::{
    builder::{fragment::Fragment, HyperGraphBuilder, InPort, OutPort},
    Edge, Graph, HyperGraph, Node, Operation,
};
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
) -> IndexSet<Node<G::NodeWeight, G::EdgeWeight>> {
    let selected_nodes: IndexSet<Node<G::NodeWeight, G::EdgeWeight>> = graph_view
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
    pub fn normalise_selection(
        &self,
        selection: &HashSet<Operation<V, E>>,
    ) -> IndexSet<Node<V, E>> {
        normalise_graph(self, selection)
    }

    #[must_use]
    pub fn generate_subgraph(&self, selection: &IndexSet<Node<V, E>>) -> HyperGraph<V, E> {
        let global_inputs: HashSet<Edge<V, E>> = selection
            .iter()
            .flat_map(Node::inputs)
            .filter(|edge| {
                edge.node()
                    .filter(|node| selection.contains(node))
                    .is_none()
            })
            .collect();

        let global_outputs: HashSet<Edge<V, E>> = selection
            .iter()
            .flat_map(Node::outputs)
            .filter(|edge| {
                edge.targets()
                    .any(|node| node.map_or(true, |node| !selection.contains(&node)))
            })
            .collect();

        let mut input_weights: Vec<E> = global_inputs.iter().map(Edge::weight).cloned().collect();

        let mut count = 0;

        for name in &mut input_weights {
            if !name.is_var() {
                *name = E::generate_free(count);
                count += 1;
            }
        }

        let mut builder = HyperGraphBuilder::new(input_weights, global_outputs.len());

        // Mapping from in_ports of the subgraph to edges of the original graph
        let mut in_port_map: HashMap<InPort<V, E>, Edge<V, E>> =
            builder.graph_outputs().zip(global_outputs).collect();

        // Mapping from edges of the original graph to out_ports of the subgraph
        let mut out_port_map: HashMap<Edge<V, E>, OutPort<V, E>> = global_inputs
            .into_iter()
            .zip(builder.graph_inputs())
            .collect();

        for node in selection {
            match &node {
                Node::Operation(op) => {
                    let input_len = op.number_of_inputs();
                    let output_weights: Vec<_> = op
                        .outputs()
                        .map(|out_port| out_port.weight().clone())
                        .collect();
                    let weight: V = op.weight().clone();

                    let op = builder.add_operation(input_len, output_weights, weight);

                    in_port_map.extend(op.inputs().zip(node.inputs()));

                    out_port_map.extend(node.outputs().zip(op.outputs()));
                }
                Node::Thunk(thunk) => {
                    thunk.clone_thunk(&mut builder, &mut in_port_map, &mut out_port_map);
                }
            };
        }

        // Link up ports
        for (in_port, edge) in in_port_map {
            builder.link(out_port_map[&edge].clone(), in_port).unwrap();
        }

        builder.build().unwrap()
    }
}
