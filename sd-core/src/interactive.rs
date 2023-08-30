#![allow(clippy::type_repetition_in_bounds)]

use derivative::Derivative;

use crate::{
    hypergraph::{
        generic::{Edge, Node, Thunk},
        traits::Graph,
        utils::create_selected,
    },
    selection::SelectionMap,
};

/// A pair of a graph and a selection map that can be used as a graph.
#[allow(clippy::partial_pub_fields)]
#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub struct InteractiveGraph<G: Graph> {
    graph: G,
    #[derivative(PartialEq = "ignore", Hash = "ignore")]
    pub selection: SelectionMap<G::Ctx>,
}

impl<G: Graph> InteractiveGraph<G> {
    pub fn new(graph: G) -> Self {
        let selection = create_selected(&graph);
        Self { graph, selection }
    }
}

impl<G: Graph> Graph for InteractiveGraph<G> {
    type Ctx = G::Ctx;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        self.graph.free_graph_inputs()
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        self.graph.bound_graph_inputs()
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        self.graph.graph_outputs()
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<Self::Ctx>> + '_> {
        self.graph.nodes()
    }

    fn graph_backlink(&self) -> Option<Thunk<Self::Ctx>> {
        self.graph.graph_backlink()
    }
}
