use indexmap::IndexMap;

use super::{
    generic::{Ctx, Edge, Key, Node, Thunk},
    traits::{Graph, Keyable, NodeLike},
};
use crate::weak_map::WeakMap;

pub type EdgeMap<T, V> = WeakMap<Key<Edge<T>>, V>;
pub type ThunkMap<T, V> = WeakMap<Key<Thunk<T>>, V>;

pub fn edge_map<G: Graph, V: Copy>(graph: &G, default: V) -> EdgeMap<G::Ctx, V> {
    fn helper<T: Ctx, V: Copy>(
        map: &mut IndexMap<Key<Edge<T>>, V>,
        graph: &impl Graph<Ctx = T>,
        default: V,
    ) {
        for node in graph.nodes() {
            if let Node::Thunk(thunk) = &node {
                helper(map, thunk, default);
            }
            for edge in node.inputs() {
                map.insert(edge.key(), default);
            }
            for edge in node.outputs() {
                map.insert(edge.key(), default);
            }
        }
        for edge in graph.graph_inputs() {
            map.insert(edge.key(), default);
        }
        for edge in graph.graph_outputs() {
            map.insert(edge.key(), default);
        }
    }

    let mut set = IndexMap::new();
    helper(&mut set, graph, default);
    WeakMap::from(set)
}

pub fn thunk_map<G: Graph, V: Copy>(graph: &G, default: V) -> ThunkMap<G::Ctx, V> {
    fn helper<T: Ctx, V: Copy>(
        map: &mut IndexMap<Key<Thunk<T>>, V>,
        graph: &impl Graph<Ctx = T>,
        default: V,
    ) {
        for thunk in graph.thunks() {
            helper(map, &thunk, default);
            map.insert(thunk.key(), default);
        }
    }

    let mut map = IndexMap::new();
    helper(&mut map, graph, default);
    WeakMap::from(map)
}
