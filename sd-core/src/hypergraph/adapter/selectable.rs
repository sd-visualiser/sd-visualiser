#![allow(clippy::inline_always)]

use std::ops::{Index, IndexMut};

use delegate::delegate;
use derivative::Derivative;

use crate::{
    codeable::{Code, Codeable},
    common::Direction,
    hypergraph::{
        generic::{Edge, Key, Node, Thunk},
        subgraph::Subgraph,
        traits::{Graph, Keyable},
    },
    selection::SelectionMap,
};

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub struct SelectableGraph<G: Graph> {
    graph: G,
    selection: SelectionMap<G::Ctx>,
}

impl<G: Graph> SelectableGraph<G> {
    pub fn new(graph: G) -> Self {
        Self {
            selection: SelectionMap::new(&graph),
            graph,
        }
    }

    pub fn inner(&self) -> &G {
        &self.graph
    }

    pub fn inner_mut(&mut self) -> &mut G {
        &mut self.graph
    }

    pub fn to_subgraph(&self) -> Subgraph<G::Ctx> {
        Subgraph::new(self.selection.clone())
    }

    delegate! {
        to self.selection {
            #[call(index)]
            pub fn selected(&self, node: &Node<G::Ctx>) -> &bool;

            #[call(index_mut)]
            pub fn selected_mut(&mut self, node: &Node<G::Ctx>) -> &mut bool;

            pub fn is_empty(&self) -> bool;
            pub fn clear_selection(&mut self);
            pub fn extend_selection(&mut self, direction: Option<(Direction, usize)>);
        }
    }
}

impl<G: Graph> Graph for SelectableGraph<G> {
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

impl<G: Graph> Keyable for SelectableGraph<G> {
    type Key = Key<G>;

    fn key(&self) -> Self::Key {
        self.graph.key()
    }
}

impl<G: Graph + Codeable> Codeable for SelectableGraph<G> {
    type Code = Code<G>;

    fn code(&self) -> Self::Code {
        self.graph.code()
    }
}
