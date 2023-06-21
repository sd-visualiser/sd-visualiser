use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use derivative::Derivative;
use indexmap::{IndexMap, IndexSet};

use super::{
    builder::{fragment::Fragment, HyperGraphBuilder, InPort, OutPort},
    Edge, HyperGraph, Node, Operation, Thunk,
};
use crate::{
    common::{Addr, InOut},
    graph::Name,
    language::{chil, spartan, Language},
    weak_map::WeakMap,
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

/// Finds the ancestor of given node which is contained in containing, returning none if no such ancestor exists
fn find_ancestor<V, E>(
    containing: &Option<Thunk<V, E>>,
    mut node: Node<V, E>,
) -> Option<Node<V, E>> {
    while &node.backlink() != containing {
        node = Node::Thunk(node.backlink()?);
    }
    Some(node)
}

#[must_use]
pub fn normalise_selection<V, E>(selection: &IndexSet<Operation<V, E>>) -> IndexSet<Node<V, E>> {
    if let Some(op) = selection.first() {
        let mut containing = op.backlink();
        for op in selection {
            while find_ancestor(&containing, Node::Operation(op.clone())).is_none() {
                containing = containing.unwrap().backlink();
            }
        }

        selection
            .iter()
            .cloned()
            .map(|op| find_ancestor(&containing, Node::Operation(op)).unwrap())
            .collect()
    } else {
        IndexSet::new()
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = "T::Edge: Clone, T::Thunk: Clone"),
    Hash(bound = ""),
    Default(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = "")
)]
pub struct Mapping<T: Addr> {
    pub edge_mapping: WeakMap<T::Edge, T::Edge>,
    pub thunk_mapping: WeakMap<T::Thunk, T::Thunk>,
}

pub struct Subgraph<V, E> {
    pub graph: HyperGraph<V, E>,
    pub mapping: Option<Mapping<(V, E)>>,
}

impl<V, E> Subgraph<V, E>
where
    V: Debug + Send + Sync + Clone,
    E: Debug + Send + Sync + Clone + Free,
{
    #[must_use]
    pub fn generate_subgraph(selection: &IndexSet<Node<V, E>>) -> Self {
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
        let mut out_port_map: IndexMap<Edge<V, E>, OutPort<V, E>> = global_inputs
            .into_iter()
            .zip(builder.graph_inputs())
            .collect();

        let mut thunk_mapping: IndexMap<Thunk<V, E>, Thunk<V, E>> = IndexMap::new();

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
                    thunk.clone_thunk(
                        &mut builder,
                        &mut in_port_map,
                        &mut out_port_map,
                        &mut thunk_mapping,
                    );
                }
            };
        }

        // Link up ports
        for (in_port, edge) in in_port_map {
            builder.link(out_port_map[&edge].clone(), in_port).unwrap();
        }

        let mapping = Some(Mapping {
            edge_mapping: out_port_map
                .into_iter()
                .map(|(k, v)| (v.into_edge_unchecked(), k))
                .collect::<IndexMap<_, _>>()
                .into(),
            thunk_mapping: thunk_mapping.into(),
        });

        Subgraph {
            graph: builder.build().unwrap(),
            mapping,
        }
    }
}
