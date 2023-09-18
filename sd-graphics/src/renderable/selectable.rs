use sd_core::hypergraph::{
    adapter::selectable::SelectableGraph,
    generic::{Edge, Node, Operation, Thunk},
};

use super::RenderableGraph;

impl<G: RenderableGraph> RenderableGraph for SelectableGraph<G> {
    // Get the selection.
    fn selected(&self, node: Node<G::Ctx>) -> bool {
        *self.selected(&node)
    }

    // Delegate to inner graph.
    fn clicked_edge(&mut self, edge: Edge<G::Ctx>) {
        self.inner_mut().clicked_edge(edge);
    }

    // Toggle the selection.
    fn clicked_operation(&mut self, op: Operation<G::Ctx>, _primary: bool) {
        *self.selected_mut(&Node::Operation(op)) ^= true;
    }

    // Toggle the selection.
    fn clicked_thunk(&mut self, thunk: Thunk<G::Ctx>, _primary: bool) {
        *self.selected_mut(&Node::Thunk(thunk)) ^= true;
    }

    // Delegate to inner graph.
    fn extend(&mut self, nodes: impl Iterator<Item = Node<G::Ctx>>) {
        self.inner_mut().extend(nodes);
    }
}
