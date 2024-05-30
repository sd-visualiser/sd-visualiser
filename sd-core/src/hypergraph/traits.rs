use std::{fmt::Debug, hash::Hash};

use super::generic::{Ctx, Edge, Endpoint, Node, Operation, Thunk};

pub trait Keyable {
    type Key: Clone + Eq + Hash + Debug + Send + Sync;
    fn key(&self) -> Self::Key;
}

pub trait WithWeight {
    type Weight;
    fn weight(&self) -> Self::Weight;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WireType {
    Data,
    ControlFlow,
    SymName,
}

pub trait WithType {
    fn get_type(&self) -> WireType;
}

pub trait NodeLike: Clone + Eq + Hash + Debug + Send + Sync {
    type Ctx: Ctx;
    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_>;
    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_>;
    fn backlink(&self) -> Option<Thunk<Self::Ctx>>;

    fn number_of_inputs(&self) -> usize;
    fn number_of_outputs(&self) -> usize;
}

pub trait EdgeLike: Clone + Eq + Hash + Debug + Send + Sync {
    type Ctx: Ctx;
    fn source(&self) -> Endpoint<Self::Ctx>;
    fn targets(&self) -> Box<dyn DoubleEndedIterator<Item = Endpoint<Self::Ctx>> + '_>;
}

pub trait Graph: Clone + Debug + Send + Sync + Keyable {
    type Ctx: Ctx;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_>;
    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_>;

    fn graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.free_graph_inputs().chain(self.bound_graph_inputs()))
    }

    fn free_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_>;
    fn bound_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_>;

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.free_graph_outputs().chain(self.bound_graph_outputs()))
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<Self::Ctx>> + '_>;

    fn operations<'a>(&'a self) -> Box<dyn DoubleEndedIterator<Item = Operation<Self::Ctx>> + 'a> {
        Box::new(self.nodes().filter_map(Node::into_operation))
    }

    fn thunks<'a>(&'a self) -> Box<dyn DoubleEndedIterator<Item = Thunk<Self::Ctx>> + 'a> {
        Box::new(self.nodes().filter_map(Node::into_thunk))
    }

    fn graph_backlink(&self) -> Option<Thunk<Self::Ctx>>;

    fn number_of_free_graph_inputs(&self) -> usize;
    fn number_of_bound_graph_inputs(&self) -> usize;

    fn number_of_graph_inputs(&self) -> usize {
        self.number_of_free_graph_inputs() + self.number_of_bound_graph_inputs()
    }

    fn number_of_free_graph_outputs(&self) -> usize;
    fn number_of_bound_graph_outputs(&self) -> usize;

    fn number_of_graph_outputs(&self) -> usize {
        self.number_of_free_graph_outputs() + self.number_of_bound_graph_outputs()
    }
}
