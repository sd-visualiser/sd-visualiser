use sd_core::{
    hypergraph::generic::{Ctx, Edge, Node, Operation, Thunk},
    interactive::{InteractiveGraph, InteractiveSubgraph},
};

use super::RenderableGraph;

impl<G: RenderableGraph> RenderableGraph for InteractiveGraph<G> {
    fn selected(&self, node: Node<Self::Ctx>) -> bool {
        self.0.selected(node)
    }

    fn clicked_edge(&mut self, edge: Edge<Self::Ctx>) {
        self.0.clicked_edge(edge);
    }

    fn clicked_operation(&mut self, op: Operation<Self::Ctx>, primary: bool) {
        self.0.clicked_operation(op, primary);
    }

    fn clicked_thunk(&mut self, thunk: Thunk<Self::Ctx>, primary: bool) {
        self.0.clicked_thunk(thunk, primary);
    }

    fn extend(&mut self, nodes: impl Iterator<Item = Node<Self::Ctx>>) {
        self.0.extend(nodes);
    }
}

impl<T: Ctx> RenderableGraph for InteractiveSubgraph<T> {
    fn selected(&self, node: Node<Self::Ctx>) -> bool {
        self.0.selected(node)
    }

    fn clicked_edge(&mut self, edge: Edge<Self::Ctx>) {
        self.0.clicked_edge(edge);
    }

    fn clicked_operation(&mut self, op: Operation<Self::Ctx>, primary: bool) {
        self.0.clicked_operation(op, primary);
    }

    fn clicked_thunk(&mut self, thunk: Thunk<Self::Ctx>, primary: bool) {
        self.0.clicked_thunk(thunk, primary);
    }

    fn extend(&mut self, nodes: impl Iterator<Item = Node<Self::Ctx>>) {
        self.0.extend(nodes);
    }
}
