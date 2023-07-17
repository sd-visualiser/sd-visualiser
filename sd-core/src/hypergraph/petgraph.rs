use std::collections::HashMap;

use petgraph::graph::NodeIndex;
#[cfg(test)]
use serde::Serialize;

use super::traits::{EdgeLike, Graph, NodeLike, WithWeight};
use crate::common::Addr;

pub type PetGraph<V, E> = petgraph::Graph<PetNode<V, E>, usize>;

#[cfg_attr(test, derive(Serialize))]
pub enum PetNode<V, E> {
    Edge(E),
    Operation(V),
    Thunk(PetGraph<V, E>),
}

pub fn to_pet<G>(hypergraph: &G) -> PetGraph<G::NodeWeight, G::EdgeWeight>
where
    G: Graph,
    G::NodeWeight: Clone,
    G::EdgeWeight: Clone,
    <G::T as Addr>::Node: NodeLike<T = G::T>,
    <G::T as Addr>::Edge: EdgeLike<T = G::T> + WithWeight<Weight = G::EdgeWeight>,
    <G::T as Addr>::Operation: NodeLike<T = G::T> + WithWeight<Weight = G::NodeWeight>,
    <G::T as Addr>::Thunk: NodeLike<T = G::T>
        + Graph<T = G::T, NodeWeight = G::NodeWeight, EdgeWeight = G::EdgeWeight>,
{
    let mut graph = petgraph::Graph::new();

    // Maps hyperedges to petgraph nodes.
    let mut edge_map: HashMap<<G::T as Addr>::Edge, NodeIndex> = HashMap::new();

    // Need to do this in case there are any inputs that are immediately discarded.
    for edge in hypergraph.graph_inputs() {
        edge_map.insert(
            edge.clone(),
            graph.add_node(PetNode::Edge(edge.weight().clone())),
        );
    }

    for node in hypergraph.nodes() {
        let pet_node = if let Ok(op) = <G::T as Addr>::Operation::try_from(node.clone()) {
            graph.add_node(PetNode::Operation(op.weight().clone()))
        } else if let Ok(thunk) = <G::T as Addr>::Thunk::try_from(node.clone()) {
            graph.add_node(PetNode::Thunk(to_pet(&thunk)))
        } else {
            unreachable!()
        };

        for (i, edge) in node.inputs().enumerate() {
            let pet_edge = edge_map
                .entry(edge)
                .or_insert_with_key(|edge| graph.add_node(PetNode::Edge(edge.weight().clone())));
            graph.add_edge(*pet_edge, pet_node, i);
        }
        for (i, edge) in node.outputs().enumerate() {
            let pet_edge = edge_map
                .entry(edge)
                .or_insert_with_key(|edge| graph.add_node(PetNode::Edge(edge.weight().clone())));
            graph.add_edge(pet_node, *pet_edge, i);
        }
    }

    graph
}
