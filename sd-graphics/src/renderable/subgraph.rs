use sd_core::hypergraph::{
    generic::{Ctx, Node},
    subgraph::{SubEdge, SubNode, SubOperation, SubThunk, Subgraph},
};

use super::RenderableGraph;

impl<T: Ctx> RenderableGraph for Subgraph<T> {
    // Always false
    fn selected(&self, _node: SubNode<T>) -> bool {
        false
    }

    // Noop
    fn clicked_edge(&mut self, _edge: SubEdge<T>) {}

    // Remove from the subgraph.
    fn clicked_operation(&mut self, op: SubOperation<T>, _primary: bool) {
        self.remove(&Node::Operation(op.into_inner()));
    }

    // Remove from the subgraph.
    fn clicked_thunk(&mut self, thunk: SubThunk<T>, _primary: bool) {
        self.remove(&Node::Thunk(thunk.into_inner()));
    }

    // Delegate to inner graph.
    fn extend(&mut self, nodes: impl Iterator<Item = SubNode<T>>) {
        self.extend(nodes.map(SubNode::into_inner));
    }
}
