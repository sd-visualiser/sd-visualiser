use std::collections::HashSet;

use super::{
    generic::{Ctx, Node},
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
) -> Vec<Option<Node<T>>> {
    let targets = edge
        .targets()
        .map(|x| x.and_then(|y| find_ancestor::<T>(containing, y)))
        .collect::<Vec<_>>();

    let mut non_dupe_outputs = HashSet::new();
    let mut outputs = Vec::new();
    for x in targets {
        match x {
            Some(Node::Thunk(t)) => {
                non_dupe_outputs.insert(t);
            }
            _ => {
                outputs.push(x);
            }
        }
    }
    outputs.extend(non_dupe_outputs.into_iter().map(|x| Some(Node::Thunk(x))));
    outputs
}

pub fn number_of_normalised_targets<T: Ctx>(edge: &T::Edge) -> usize {
    let containing = edge.source().and_then(|x| x.backlink());
    let targets = edge
        .targets()
        .map(|x| x.and_then(|y| find_ancestor::<T>(&containing, y)))
        .collect::<Vec<_>>();

    let mut non_dupe_outputs = HashSet::new();
    let mut other_outputs = 0;
    for x in targets {
        match x {
            Some(Node::Thunk(t)) => {
                non_dupe_outputs.insert(t);
            }
            _ => {
                other_outputs += 1;
            }
        }
    }
    other_outputs + non_dupe_outputs.len()
}
