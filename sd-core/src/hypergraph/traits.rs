use std::hash::Hash;

use super::generic::Node;
use crate::common::Addr;

pub trait WithWeight {
    type Weight;
    fn weight(&self) -> &Self::Weight;
}

pub trait NodeLike {
    type T: Addr;
    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_>;
    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_>;
    fn backlink(&self) -> Option<<Self::T as Addr>::Thunk>;

    fn number_of_inputs(&self) -> usize {
        self.inputs().count()
    }

    fn number_of_outputs(&self) -> usize {
        self.outputs().count()
    }
}

pub trait EdgeLike {
    type T: Addr;
    fn source(&self) -> Option<Node<Self::T>>;
    fn targets(&self) -> Box<dyn DoubleEndedIterator<Item = Option<Node<Self::T>>> + '_>;
}

pub trait Graph: Clone + Eq + PartialEq + Hash {
    type T: Addr;

    fn free_graph_inputs(
        &self,
    ) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_>;

    fn bound_graph_inputs(
        &self,
    ) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_>;

    fn graph_inputs<'a>(
        &'a self,
    ) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + 'a>
    where
        <Self::T as Addr>::Edge: 'a,
    {
        Box::new(self.free_graph_inputs().chain(self.bound_graph_inputs()))
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_>;

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<Self::T>> + '_>;

    fn operations<'a>(
        &'a self,
    ) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Operation> + 'a> {
        Box::new(self.nodes().filter_map(Node::into_operation))
    }

    fn thunks<'a>(&'a self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Thunk> + 'a> {
        Box::new(self.nodes().filter_map(Node::into_thunk))
    }

    fn graph_backlink(&self) -> Option<<Self::T as Addr>::Thunk>;
}
