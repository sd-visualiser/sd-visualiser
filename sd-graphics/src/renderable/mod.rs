use sd_core::hypergraph::{
    generic::{Edge, Node, Operation, Thunk},
    traits::Graph,
};

mod collapse;
mod cut;
mod hypergraph;
mod interactive;
mod selectable;
mod subgraph;

/// Abstraction over a graph that can be rendered.
pub trait RenderableGraph: Graph {
    fn selected(&self, node: Node<Self::Ctx>) -> bool;
    fn clicked_edge(&mut self, edge: Edge<Self::Ctx>);
    fn clicked_operation(&mut self, op: Operation<Self::Ctx>, primary: bool);
    fn clicked_thunk(&mut self, thunk: Thunk<Self::Ctx>, primary: bool);
    fn extend(&mut self, nodes: impl Iterator<Item = Node<Self::Ctx>>);
}
