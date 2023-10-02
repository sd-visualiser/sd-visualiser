use std::collections::HashSet;

use super::{
    generic::{Ctx, Endpoint, Node},
    traits::{EdgeLike, NodeLike},
};

/// Finds the ancestor of given node which is contained in containing, returning none if no such ancestor exists
pub fn find_ancestor<T: Ctx>(containing: &Option<T::Thunk>, mut node: Node<T>) -> Option<Node<T>> {
    while &node.backlink() != containing {
        node = Node::Thunk(node.backlink()?);
    }
    Some(node)
}

pub fn normalised_targets<T: Ctx>(
    edge: &T::Edge,
    containing: &Option<T::Thunk>,
) -> Vec<Endpoint<T>> {
    let targets = edge
        .targets()
        .filter_map(|x| match x {
            Endpoint::Node(node) => find_ancestor(containing, node).map(Endpoint::Node),
            Endpoint::Boundary(graph) if &graph == containing => Some(Endpoint::Boundary(graph)),
            Endpoint::Boundary(Some(thunk)) => {
                find_ancestor(containing, Node::Thunk(thunk)).map(Endpoint::Node)
            }
            Endpoint::Boundary(_) => None,
        })
        .collect::<Vec<_>>();

    let mut non_dupe_outputs = HashSet::new();
    let mut outputs = Vec::new();
    for x in targets {
        match x {
            Endpoint::Node(Node::Thunk(t)) => {
                non_dupe_outputs.insert(t);
            }
            _ => {
                outputs.push(x);
            }
        }
    }
    outputs.extend(
        non_dupe_outputs
            .into_iter()
            .map(|x| Endpoint::Node(Node::Thunk(x))),
    );
    outputs
}

pub fn number_of_normalised_targets<T: Ctx>(edge: &T::Edge) -> usize {
    let containing = match edge.source() {
        Endpoint::Node(node) => node.backlink(),
        Endpoint::Boundary(graph) => graph,
    };
    let targets = edge
        .targets()
        .filter_map(|x| match x {
            Endpoint::Node(node) => find_ancestor(&containing, node).map(Endpoint::Node),
            Endpoint::Boundary(graph) if graph == containing => Some(Endpoint::Boundary(graph)),
            Endpoint::Boundary(Some(thunk)) => {
                find_ancestor(&containing, Node::Thunk(thunk)).map(Endpoint::Node)
            }
            Endpoint::Boundary(_) => None,
        })
        .collect::<Vec<_>>();

    let mut non_dupe_outputs = HashSet::new();
    let mut other_outputs = 0;
    for x in targets {
        match x {
            Endpoint::Node(Node::Thunk(t)) => {
                non_dupe_outputs.insert(t);
            }
            _ => {
                other_outputs += 1;
            }
        }
    }
    other_outputs + non_dupe_outputs.len()
}
