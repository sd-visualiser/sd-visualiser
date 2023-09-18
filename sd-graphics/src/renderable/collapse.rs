use sd_core::hypergraph::{
    adapter::collapse::{
        CollapseEdge, CollapseGraph, CollapseNode, CollapseOperation, CollapseThunk,
    },
    generic::Node,
};

use super::RenderableGraph;

impl<G: RenderableGraph> RenderableGraph for CollapseGraph<G> {
    // Delegate to inner graph.
    fn selected(&self, node: CollapseNode<G>) -> bool {
        self.inner().selected(node.into_inner())
    }

    // Delegate to inner graph.
    fn clicked_edge(&mut self, edge: CollapseEdge<G>) {
        self.inner_mut().clicked_edge(edge.into_inner());
    }

    // Inner operations: delegate to inner graph.
    // Collapsed thunks: either expand the thunk [click] or delegate to inner graph [right-click].
    fn clicked_operation(&mut self, op: CollapseOperation<G>, primary: bool) {
        match op.into_inner() {
            Node::Operation(op) => {
                self.inner_mut().clicked_operation(op, primary);
            }
            Node::Thunk(thunk) => {
                if primary {
                    self.toggle(&thunk);
                } else {
                    self.inner_mut().clicked_thunk(thunk, false);
                }
            }
        }
    }

    // Either collapse the thunk [click] or delegate to inner graph [right-click].
    fn clicked_thunk(&mut self, thunk: CollapseThunk<G>, primary: bool) {
        if primary {
            self.toggle(thunk.inner());
        } else {
            self.inner_mut().clicked_thunk(thunk.into_inner(), false);
        }
    }

    // Delegate to inner graph.
    fn extend(&mut self, nodes: impl Iterator<Item = CollapseNode<G>>) {
        self.inner_mut().extend(nodes.map(CollapseNode::into_inner));
    }
}
