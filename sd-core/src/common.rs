use thiserror::Error;

use crate::{graph::Op, hypergraph_good::HyperGraphError};

/// Specifies an operation which has inputs and outputs.
pub trait InOut {
    fn number_of_inputs(&self) -> usize;
    fn number_of_outputs(&self) -> usize;
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Slice<O: InOut> {
    pub ops: Vec<O>,
}

impl<O: InOut> InOut for Slice<O> {
    fn number_of_inputs(&self) -> usize {
        self.ops.iter().map(|op| op.number_of_inputs()).sum()
    }

    fn number_of_outputs(&self) -> usize {
        self.ops.iter().map(|op| op.number_of_outputs()).sum()
    }
}

#[derive(Debug, Error, Clone)]
pub enum FromHyperError {
    #[error("Hypergraph contains no nodes")]
    EmptyGraph,

    #[error("Hypergraph error")]
    HyperGraphError(#[from] HyperGraphError<Op, ()>),
}
