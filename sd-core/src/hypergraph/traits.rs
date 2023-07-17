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
}

pub trait EdgeLike {
    type T: Addr;
    fn source(&self) -> Option<<Self::T as Addr>::Node>;
    fn targets(&self) -> Box<dyn DoubleEndedIterator<Item = Option<<Self::T as Addr>::Node>> + '_>;
    fn number_of_targets(&self) -> usize;
}

pub trait Graph {
    type T: Addr;
    type NodeWeight;
    type EdgeWeight;

    fn bound_graph_inputs(
        &self,
    ) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_>;

    fn unbound_graph_inputs(
        &self,
    ) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_>;

    fn graph_inputs<'a>(
        &'a self,
    ) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + 'a>
    where
        <Self::T as Addr>::Edge: 'a,
    {
        Box::new(self.unbound_graph_inputs().chain(self.bound_graph_inputs()))
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Edge> + '_>;

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Node> + '_>;

    fn operations<'a>(
        &'a self,
    ) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Operation> + 'a>
    where
        <Self::T as Addr>::Node: 'a,
    {
        Box::new(self.nodes().filter_map(|node| node.try_into().ok()))
    }

    fn thunks<'a>(&'a self) -> Box<dyn DoubleEndedIterator<Item = <Self::T as Addr>::Thunk> + 'a>
    where
        <Self::T as Addr>::Node: 'a,
    {
        Box::new(self.nodes().filter_map(|node| node.try_into().ok()))
    }
}
