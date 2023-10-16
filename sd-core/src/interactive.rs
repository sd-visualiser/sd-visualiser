#![allow(clippy::inline_always)]

use delegate::delegate;
use derivative::Derivative;

use crate::{
    codeable::{Code, Codeable},
    common::Direction,
    hypergraph::{
        adapter::{collapse::CollapseGraph, cut::CutGraph, selectable::SelectableGraph},
        generic::{Ctx, Edge, Key, Node, Thunk},
        mapping::{edge_map, thunk_map},
        subgraph::Subgraph,
        traits::{Graph, Keyable},
    },
};

/// An interactive graph is a graph with cut edges, collapsible thunks, and selectable nodes.
#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub struct InteractiveGraph<G: Graph>(pub CutGraph<CollapseGraph<SelectableGraph<G>>>);

impl<G: Graph> InteractiveGraph<G> {
    pub fn new(graph: G) -> Self {
        let graph = SelectableGraph::new(graph);

        let expanded = thunk_map(&graph, true);
        let graph = CollapseGraph::new(graph, expanded);

        let cut_edges = edge_map(&graph, false);
        let graph = CutGraph::new(graph, cut_edges);

        Self(graph)
    }

    delegate! {
        to self.0.inner().inner() {
            pub fn is_empty(&self) -> bool;
        }

        to self.0.inner_mut().inner_mut() {
            pub fn clear_selection(&mut self);
            pub fn extend_selection(&mut self, direction: Option<(Direction, usize)>);
        }
    }

    delegate! {
        to self.0.inner_mut() {
            #[call(set_all)]
            pub fn set_expanded_all(&mut self, value: bool);
        }
    }

    pub fn to_subgraph(&self) -> InteractiveSubgraph<G::Ctx> {
        let subgraph = self.0.inner().inner().to_subgraph();
        let expanded = self.0.inner().expanded().clone();
        InteractiveSubgraph(CollapseGraph::new(subgraph, expanded))
    }
}

impl<G: Graph> Graph for InteractiveGraph<G> {
    type Ctx = CutGraph<CollapseGraph<SelectableGraph<G>>>;

    delegate! {
        to self.0 {
            fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_>;
            fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_>;
            fn free_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_>;
            fn bound_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_>;
            fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<Self::Ctx>> + '_>;
            fn graph_backlink(&self) -> Option<Thunk<Self::Ctx>>;
            fn number_of_free_graph_inputs(&self) -> usize;
            fn number_of_bound_graph_inputs(&self) -> usize;
            fn number_of_free_graph_outputs(&self) -> usize;
            fn number_of_bound_graph_outputs(&self) -> usize;
        }
    }
}

impl<G: Graph + Codeable> Codeable for InteractiveGraph<G> {
    type Code = Code<CutGraph<CollapseGraph<SelectableGraph<G>>>>;

    fn code(&self) -> Self::Code {
        self.0.code()
    }
}

impl<G: Graph> Keyable for InteractiveGraph<G> {
    type Key = Key<CutGraph<CollapseGraph<SelectableGraph<G>>>>;

    fn key(&self) -> Self::Key {
        self.0.key()
    }
}

/// An interactive subgraph is a subgraph with collapsible thunks.
#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub struct InteractiveSubgraph<T: Ctx>(pub CollapseGraph<Subgraph<T>>);

impl<T: Ctx> Graph for InteractiveSubgraph<T> {
    type Ctx = CollapseGraph<Subgraph<T>>;

    delegate! {
        to self.0 {
            fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_>;
            fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_>;
            fn free_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_>;
            fn bound_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_>;
            fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<Self::Ctx>> + '_>;
            fn graph_backlink(&self) -> Option<Thunk<Self::Ctx>>;
            fn number_of_free_graph_inputs(&self) -> usize;
            fn number_of_bound_graph_inputs(&self) -> usize;
            fn number_of_free_graph_outputs(&self) -> usize;
            fn number_of_bound_graph_outputs(&self) -> usize;
        }
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

impl<T: Ctx> Keyable for InteractiveSubgraph<T> {
    type Key = Key<CollapseGraph<Subgraph<T>>>;

    fn key(&self) -> Self::Key {
        self.0.key()
    }
}
