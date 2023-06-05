use std::collections::HashSet;

use crate::hypergraph::{HyperGraph, Operation, BackPointer};

struct NormalSelection<V, E> {
    selection: HashSet<Node<V, E>>,
    containing: BackPointer<V, E>,
}

fn normalise_selection<V, E>(selection: HashSet<Operation<V, E>>) -> NormalSelection<V, E> {

}

impl<V, E> HyperGraph<V, E> {
    fn generate_subgraph(&self, selection: HashSet<Operation<V, E>>) -> HyperGraph<V, E> {

    }


}
