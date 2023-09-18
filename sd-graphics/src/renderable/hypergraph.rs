use sd_core::hypergraph::{Edge, Hypergraph, Node, Operation, Thunk, Weight};

use super::RenderableGraph;

impl<W: Weight> RenderableGraph for Hypergraph<W> {
    // Always false
    fn selected(&self, _node: Node<W>) -> bool {
        false
    }

    // Noop
    fn clicked_edge(&mut self, _edge: Edge<W>) {}

    // Noop
    fn clicked_operation(&mut self, _op: Operation<W>, _primary: bool) {}

    // Noop
    fn clicked_thunk(&mut self, _thunk: Thunk<W>, _primary: bool) {}

    // Noop
    fn extend(&mut self, _nodes: impl Iterator<Item = Node<W>>) {}
}
