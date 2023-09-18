use std::collections::HashSet;

use indexmap::IndexMap;

use super::{
    generic::{Ctx, Edge, Node, Thunk},
    traits::{EdgeLike, Graph, NodeLike},
};
use crate::{selection::SelectionMap, weak_map::WeakMap};

pub fn create_expanded<G: Graph>(graph: &G) -> WeakMap<Thunk<G::Ctx>, bool> {
    fn helper<T: Ctx>(set: &mut IndexMap<T::Thunk, bool>, thunk: T::Thunk) {
        for t in thunk.thunks() {
            helper::<T>(set, t);
        }
        set.insert(thunk, true);
    }

    let mut set = IndexMap::new();

    for thunk in graph.thunks() {
        helper::<G::Ctx>(&mut set, thunk);
    }

    WeakMap::from(set)
}

#[must_use]
pub fn create_selected<G: Graph>(graph: &G) -> SelectionMap<G::Ctx> {
    fn helper<T: Ctx>(set: &mut IndexMap<Node<T>, bool>, thunk: &T::Thunk) {
        for node in thunk.nodes() {
            if let Node::Thunk(thunk) = &node {
                helper::<T>(set, thunk);
            }
            set.insert(node, false);
        }
    }

    let mut set = IndexMap::new();

    for node in graph.nodes() {
        if let Node::Thunk(thunk) = &node {
            helper::<G::Ctx>(&mut set, thunk);
        }
        set.insert(node, false);
    }

    SelectionMap::from(set)
}

pub fn create_cut_edges<G: Graph>(graph: &G) -> WeakMap<Edge<G::Ctx>, bool> {
    fn helper<G: Graph>(set: &mut IndexMap<Edge<G::Ctx>, bool>, graph: &G) {
        for edge in graph.graph_outputs() {
            set.insert(edge, false);
        }
        for node in graph.nodes() {
            for edge in node.outputs() {
                set.insert(edge, false);
            }
            for edge in node.inputs() {
                set.insert(edge, false);
            }
            if let Node::Thunk(thunk) = &node {
                helper(set, thunk);
            }
        }
        for edge in graph.graph_inputs() {
            set.insert(edge, false);
        }
    }

    let mut set = IndexMap::new();
    helper(&mut set, graph);
    WeakMap::from(set)
}

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
