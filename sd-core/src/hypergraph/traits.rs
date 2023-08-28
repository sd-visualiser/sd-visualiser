use std::hash::Hash;

use super::generic::{Ctx, Edge, Node, Operation, Thunk};

pub trait WithWeight {
    type Weight: Clone;
    fn weight(&self) -> Self::Weight;
}

pub trait NodeLike: Clone + Eq + PartialEq + Hash {
    type Ctx: Ctx;
    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_>;
    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_>;
    fn backlink(&self) -> Option<Thunk<Self::Ctx>>;

    fn number_of_inputs(&self) -> usize {
        self.inputs().count()
    }

    fn number_of_outputs(&self) -> usize {
        self.outputs().count()
    }
}

pub trait EdgeLike: Clone + Eq + PartialEq + Hash {
    type Ctx: Ctx;
    fn source(&self) -> Option<Node<Self::Ctx>>;
    fn targets(&self) -> Box<dyn DoubleEndedIterator<Item = Option<Node<Self::Ctx>>> + '_>;
}

pub trait Graph: Clone + Eq + PartialEq + Hash {
    type Ctx: Ctx;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_>;

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_>;

    fn graph_inputs<'a>(&'a self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + 'a>
    where
        Edge<Self::Ctx>: 'a,
    {
        Box::new(self.free_graph_inputs().chain(self.bound_graph_inputs()))
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_>;

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<Self::Ctx>> + '_>;

    fn operations<'a>(&'a self) -> Box<dyn DoubleEndedIterator<Item = Operation<Self::Ctx>> + 'a> {
        Box::new(self.nodes().filter_map(Node::into_operation))
    }

    fn thunks<'a>(&'a self) -> Box<dyn DoubleEndedIterator<Item = Thunk<Self::Ctx>> + 'a> {
        Box::new(self.nodes().filter_map(Node::into_thunk))
    }

    fn graph_backlink(&self) -> Option<Thunk<Self::Ctx>>;
}
