#![allow(clippy::inline_always)]

use delegate::delegate;
use derivative::Derivative;

use crate::{
    codeable::{Code, Codeable},
    common::Direction,
    hypergraph::{
        adapter::{collapse::CollapseGraph, cut::CutGraph},
        generic::{Ctx, Edge, Node, Thunk},
        subgraph::Subgraph,
        traits::Graph,
        utils::{create_cut_edges, create_expanded},
    },
    selection::SelectionMap,
};

/// An interactive graph is a graph with cut edges, collapsible thunks, and selectable nodes.
#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub struct InteractiveGraph<G: Graph> {
    pub graph: CutGraph<CollapseGraph<G>>,
    #[derivative(PartialEq = "ignore", Hash = "ignore")]
    pub selection: SelectionMap<G::Ctx>,
}

impl<G: Graph> InteractiveGraph<G> {
    pub fn new(graph: G) -> Self {
        let selection = SelectionMap::new(&graph);

        let expanded = create_expanded(&graph);
        let graph = CollapseGraph::new(graph, expanded);

        let cut_edges = create_cut_edges(&graph);
        let graph = CutGraph::new(graph, cut_edges);

        Self { graph, selection }
    }

    delegate! {
        to self.selection {
            pub fn is_empty(&self) -> bool;
            pub fn clear_selection(&mut self);
            pub fn extend_selection(&mut self, direction: Option<(Direction, usize)>);
        }
    }

    delegate! {
        to self.graph.inner_mut() {
            #[call(set_all)]
            pub fn set_expanded_all(&mut self, value: bool);
        }
    }

    pub fn to_subgraph(&self) -> InteractiveSubgraph<G::Ctx> {
        let subgraph = Subgraph::new(self.selection.clone());

        // Reindex the thunk mapping.
        let expanded = self
            .graph
            .inner()
            .expanded()
            .iter()
            .map(|(thunk, value)| (subgraph.convert_thunk(thunk.clone()), *value))
            .collect();

        InteractiveSubgraph(CollapseGraph::new(subgraph, expanded))
    }
}

impl<G: Graph> Graph for InteractiveGraph<G> {
    type Ctx = CutGraph<CollapseGraph<G>>;

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

/// An interactive subgraph is a subgraph with collapsible thunks.
#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub struct InteractiveSubgraph<T: Ctx>(pub CollapseGraph<Subgraph<T>>);

impl<T: Ctx> Graph for InteractiveSubgraph<T> {
    type Ctx = CollapseGraph<Subgraph<T>>;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        self.0.free_graph_inputs()
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        self.0.bound_graph_inputs()
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        self.0.graph_outputs()
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<Self::Ctx>> + '_> {
        self.0.nodes()
    }

    fn graph_backlink(&self) -> Option<Thunk<Self::Ctx>> {
        self.0.graph_backlink()
    }
}

impl<T: Ctx> Codeable for InteractiveSubgraph<T>
where
    Subgraph<T>: Codeable,
{
    type Code = Code<CollapseGraph<Subgraph<T>>>;

    fn code(&self) -> Self::Code {
        self.0.code()
    }
}
