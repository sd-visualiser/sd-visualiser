use itertools::Either;
use sd_core::{
    hypergraph::{
        generic::{Ctx, Edge, Node, Operation, Thunk},
        subgraph::{SubNode, Subgraph},
        traits::Graph,
    },
    interactive::InteractiveGraph,
};

/// Abstraction over a graph that can be rendered.
pub trait RenderableGraph: Graph {
    fn selected(&self, node: Node<Self::Ctx>) -> bool;
    fn clicked_edge(&mut self, edge: Edge<Self::Ctx>);
    fn clicked_operation(&mut self, op: Operation<Self::Ctx>);
    fn clicked_thunk(&mut self, thunk: Thunk<Self::Ctx>);
    fn extend(&mut self, nodes: impl Iterator<Item = Node<Self::Ctx>>);
}

impl<G: Graph> RenderableGraph for InteractiveGraph<G> {
    fn selected(&self, node: Node<Self::Ctx>) -> bool {
        node.into_inner()
            .either(|node| self.selection[&node], |_edge| false)
    }

    fn clicked_edge(&mut self, edge: Edge<Self::Ctx>) {
        self.graph.toggle(edge.inner());
    }

    fn clicked_operation(&mut self, op: Operation<Self::Ctx>) {
        match op.into_inner() {
            Either::Left(op) => {
                self.selection[&Node::Operation(op)] ^= true;
            }
            Either::Right(edge) => {
                self.graph.toggle(&edge);
            }
        }
    }

    fn clicked_thunk(&mut self, thunk: Thunk<Self::Ctx>) {
        let thunk = thunk.into_inner();
        self.selection[&Node::Thunk(thunk)] ^= true;
    }

    fn extend(&mut self, _nodes: impl Iterator<Item = Node<Self::Ctx>>) {
        panic!("Graph cannot be extended")
    }
}

impl<T: Ctx> RenderableGraph for Subgraph<T> {
    fn selected(&self, _node: Node<Self::Ctx>) -> bool {
        false
    }

    fn clicked_edge(&mut self, _edge: Edge<Self::Ctx>) {}

    fn clicked_operation(&mut self, op: Operation<Self::Ctx>) {
        self.remove(&Node::Operation(op.into_inner()));
    }

    fn clicked_thunk(&mut self, thunk: Thunk<Self::Ctx>) {
        self.remove(&Node::Thunk(thunk.into_inner()));
    }

    fn extend(&mut self, nodes: impl Iterator<Item = Node<Self::Ctx>>) {
        self.extend(nodes.map(SubNode::into_inner));
    }
}
