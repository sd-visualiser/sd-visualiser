use itertools::Either;
use sd_core::{
    hypergraph::{
        generic::{Ctx, Edge, Node, Operation, Thunk},
        traits::Graph,
    },
    interactive::{InteractiveGraph, InteractiveSubgraph},
};

/// Abstraction over a graph that can be rendered.
pub trait RenderableGraph: Graph {
    fn selected(&self, node: Node<Self::Ctx>) -> bool;
    fn clicked_edge(&mut self, edge: Edge<Self::Ctx>);
    fn clicked_operation(&mut self, op: Operation<Self::Ctx>, primary: bool);
    fn clicked_thunk(&mut self, thunk: Thunk<Self::Ctx>, primary: bool);
    fn extend(&mut self, nodes: impl Iterator<Item = Node<Self::Ctx>>);
}

impl<G: Graph> RenderableGraph for InteractiveGraph<G> {
    fn selected(&self, node: Node<Self::Ctx>) -> bool {
        node.into_inner()
            .either(|node| self.selection[&node.into_inner()], |_edge| false)
    }

    fn clicked_edge(&mut self, edge: Edge<Self::Ctx>) {
        self.graph.toggle(edge.inner());
    }

    fn clicked_operation(&mut self, op: Operation<Self::Ctx>, primary: bool) {
        match op.into_inner() {
            Either::Left(op) => match op.into_inner() {
                Node::Thunk(thunk) if primary => {
                    self.graph.inner_mut().toggle(&thunk);
                }
                node => {
                    self.selection[&node] ^= true;
                }
            },
            Either::Right(edge) => {
                self.graph.toggle(&edge);
            }
        }
    }

    fn clicked_thunk(&mut self, thunk: Thunk<Self::Ctx>, primary: bool) {
        let thunk = thunk.into_inner().into_inner();
        if primary {
            self.graph.inner_mut().toggle(&thunk);
        } else {
            self.selection[&Node::Thunk(thunk)] ^= true;
        }
    }

    fn extend(&mut self, _nodes: impl Iterator<Item = Node<Self::Ctx>>) {
        panic!("Graph cannot be extended")
    }
}

impl<T: Ctx> RenderableGraph for InteractiveSubgraph<T> {
    fn selected(&self, _node: Node<Self::Ctx>) -> bool {
        false
    }

    fn clicked_edge(&mut self, _edge: Edge<Self::Ctx>) {}

    fn clicked_operation(&mut self, op: Operation<Self::Ctx>, primary: bool) {
        match op.into_inner() {
            Node::Thunk(thunk) if primary => {
                self.0.toggle(&thunk);
            }
            node => {
                self.0.inner_mut().remove(&node.into_inner());
            }
        }
    }

    fn clicked_thunk(&mut self, thunk: Thunk<Self::Ctx>, primary: bool) {
        let thunk = thunk.into_inner();
        if primary {
            self.0.toggle(&thunk);
        } else {
            self.0.inner_mut().remove(&Node::Thunk(thunk.into_inner()));
        }
    }

    fn extend(&mut self, nodes: impl Iterator<Item = Node<Self::Ctx>>) {
        self.0
            .inner_mut()
            .extend(nodes.map(|node| node.into_inner().into_inner()));
    }
}
