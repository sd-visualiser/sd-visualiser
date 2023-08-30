use itertools::Either;
use sd_core::{
    hypergraph::{
        generic::{Ctx, Edge, Node, Operation, Thunk},
        subgraph::Subgraph,
        traits::Graph,
    },
    interactive::InteractiveGraph,
};

/// Abstraction over a graph that can be rendered.
pub trait RenderableGraph: Graph {
    fn selected(&self, node: Node<Self::Ctx>) -> bool;
    fn toggle(&mut self, node: Node<Self::Ctx>);
    fn cut(&mut self, edge: Edge<Self::Ctx>);
    fn extend(&mut self, nodes: impl Iterator<Item = Node<Self::Ctx>>);

    // This is a bit of a hack.
    type InnerCtx: Ctx;
    fn inner_edge(&self, edge: &Edge<Self::Ctx>) -> Edge<Self::InnerCtx>;
    fn inner_operation(
        &self,
        op: &Operation<Self::Ctx>,
    ) -> Either<Operation<Self::InnerCtx>, Edge<Self::InnerCtx>>;
    fn inner_thunk(&self, thunk: &Thunk<Self::Ctx>) -> Thunk<Self::InnerCtx>;
}

/// For interactive graphs, we just delegate to the selection map.
impl<G: Graph> RenderableGraph for InteractiveGraph<G> {
    fn selected(&self, node: Node<Self::Ctx>) -> bool {
        node.inner()
            .either(|node| self.selection[&node], |_edge| false)
    }

    fn toggle(&mut self, node: Node<Self::Ctx>) {
        node.inner().either(
            |node| self.selection[&node] ^= true,
            |edge| self.graph.set(edge, false),
        );
    }

    fn cut(&mut self, edge: Edge<Self::Ctx>) {
        self.graph.set(edge.inner(), true);
    }

    fn extend(&mut self, _nodes: impl Iterator<Item = Node<Self::Ctx>>) {
        panic!("Graph cannot be extended")
    }

    type InnerCtx = G::Ctx;

    fn inner_edge(&self, edge: &Edge<Self::Ctx>) -> Edge<Self::InnerCtx> {
        edge.inner().clone()
    }

    fn inner_operation(
        &self,
        op: &Operation<Self::Ctx>,
    ) -> Either<Operation<Self::InnerCtx>, Edge<Self::InnerCtx>> {
        op.inner().map_either(Clone::clone, Clone::clone)
    }

    fn inner_thunk(&self, thunk: &Thunk<Self::Ctx>) -> Thunk<Self::InnerCtx> {
        thunk.inner().clone()
    }
}

/// For subgraphs, nodes are never selected and toggling them actually removes them.
impl<T: Ctx> RenderableGraph for Subgraph<T> {
    fn selected(&self, _node: Node<Self::Ctx>) -> bool {
        false
    }

    fn toggle(&mut self, node: Node<Self::Ctx>) {
        self.remove(node);
    }

    fn cut(&mut self, _edge: Edge<Self::Ctx>) {}

    fn extend(&mut self, nodes: impl Iterator<Item = Node<Self::Ctx>>) {
        self.extend(nodes);
    }

    type InnerCtx = Self::Ctx;

    fn inner_edge(&self, edge: &Edge<Self::Ctx>) -> Edge<Self::InnerCtx> {
        edge.clone()
    }

    fn inner_operation(
        &self,
        op: &Operation<Self::Ctx>,
    ) -> Either<Operation<Self::InnerCtx>, Edge<Self::InnerCtx>> {
        Either::Left(op.clone())
    }

    fn inner_thunk(&self, thunk: &Thunk<Self::Ctx>) -> Thunk<Self::InnerCtx> {
        thunk.clone()
    }
}
