use derivative::Derivative;
use thiserror::Error;

use crate::{
    graph::Op,
    hypergraph_good::{HyperGraphError, OutPort},
};

/// Specifies an operation which has inputs and outputs.
pub trait InOut<V, E> {
    fn number_of_inputs(&self) -> usize;
    fn number_of_outputs(&self) -> usize;
    fn inputs<'a>(&'a self) -> Box<dyn Iterator<Item = OutPort<V, E>> + 'a>
    where
        V: 'a,
        E: 'a;
    fn outputs<'a>(&'a self) -> Box<dyn Iterator<Item = OutPort<V, E>> + 'a>
    where
        V: 'a,
        E: 'a;
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Derivative)]
#[derivative(Default(bound = ""))]
pub struct Slice<O> {
    pub ops: Vec<O>,
}

impl<V, E, O: InOut<V, E>> InOut<V, E> for Slice<O> {
    fn number_of_inputs(&self) -> usize {
        self.ops.iter().map(|op| op.number_of_inputs()).sum()
    }

    fn number_of_outputs(&self) -> usize {
        self.ops.iter().map(|op| op.number_of_outputs()).sum()
    }

    fn inputs<'a>(&'a self) -> Box<dyn Iterator<Item = OutPort<V, E>> + 'a>
    where
        V: 'a,
        E: 'a,
    {
        Box::new(self.ops.iter().flat_map(InOut::inputs))
    }

    fn outputs<'a>(&'a self) -> Box<dyn Iterator<Item = OutPort<V, E>> + 'a>
    where
        V: 'a,
        E: 'a,
    {
        Box::new(self.ops.iter().flat_map(InOut::outputs))
    }
}

impl<'a, A, B> From<&'a Slice<A>> for Slice<B>
where
    B: From<&'a A>,
{
    fn from(value: &'a Slice<A>) -> Self {
        Self {
            ops: value.ops.iter().map(B::from).collect(),
        }
    }
}

#[derive(Debug, Error, Clone)]
pub enum FromHyperError {
    #[error("Hypergraph contains no nodes")]
    EmptyGraph,

    #[error("Hypergraph error")]
    HyperGraphError(#[from] HyperGraphError<Op, ()>),
}
