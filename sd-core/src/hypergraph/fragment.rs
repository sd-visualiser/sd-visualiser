use std::{fmt::Debug, sync::Arc};

use by_address::ByThinAddress;
use tracing::Level;

use super::{
    weakbyaddress::WeakByAddress, Graph, HyperGraph, HyperGraphError, InPort, NodeInternal,
    Operation, OperationInternal, OutPort, Result, Thunk, ThunkCursor, ThunkInternal,
};

pub trait Fragment: Graph<false> {
    fn add_operation(
        &mut self,
        input_len: usize,
        output_weights: impl IntoIterator<Item = Self::EdgeWeight>,
        weight: Self::NodeWeight,
    ) -> Operation<Self::NodeWeight, Self::EdgeWeight, false>;

    fn add_thunk(
        &mut self,
        free_variables: impl IntoIterator<Item = Self::EdgeWeight>,
        bound_variables: impl IntoIterator<Item = Self::EdgeWeight>,
        output_weights: impl IntoIterator<Item = Self::EdgeWeight>,
    ) -> Thunk<Self::NodeWeight, Self::EdgeWeight, false>;

    #[tracing::instrument(level=Level::DEBUG, skip(self), err, ret)]
    fn link(
        &mut self,
        out_port: OutPort<Self::NodeWeight, Self::EdgeWeight, false>,
        in_port: InPort<Self::NodeWeight, Self::EdgeWeight, false>,
    ) -> Result<(), Self::NodeWeight, Self::EdgeWeight>
    where
        Self::NodeWeight: Debug,
        Self::EdgeWeight: Debug,
    {
        let mut out = in_port.0.link.try_write().expect("Lock unexpectedly taken");
        if let Some(existing) = out.upgrade() {
            return Err(HyperGraphError::OutputLinkError(OutPort(ByThinAddress(
                existing,
            ))));
        }
        *out = Arc::downgrade(&out_port.0);
        out_port
            .0
            .links
            .try_write()
            .expect("Lock unexpectedly taken")
            .insert(WeakByAddress(Arc::downgrade(&in_port.0)));
        Ok(())
    }

    fn in_thunk<T>(
        &mut self,
        thunk: Thunk<Self::NodeWeight, Self::EdgeWeight, false>,
        f: impl FnOnce(ThunkCursor<Self::NodeWeight, Self::EdgeWeight>) -> T,
    ) -> T
    where
        Self::NodeWeight: Debug,
        Self::EdgeWeight: Debug,
    {
        f(ThunkCursor(thunk))
    }
}

impl<V, E> Fragment for HyperGraph<V, E, false>
where
    V: Debug + Send + Sync,
    E: Debug + Send + Sync,
{
    fn add_operation(
        &mut self,
        input_len: usize,
        output_weights: impl IntoIterator<Item = E>,
        weight: V,
    ) -> Operation<V, E, false> {
        let op = OperationInternal::new(input_len, output_weights, weight);
        self.0.nodes.push(NodeInternal::Operation(op.clone()));
        Operation(ByThinAddress(op))
    }

    fn add_thunk(
        &mut self,
        free_variables: impl IntoIterator<Item = E>,
        bound_variables: impl IntoIterator<Item = E>,
        output_weights: impl IntoIterator<Item = E>,
    ) -> Thunk<V, E, false> {
        let thunk = ThunkInternal::new(free_variables, bound_variables, output_weights);
        self.0.nodes.push(NodeInternal::Thunk(thunk.clone()));
        Thunk(ByThinAddress(thunk))
    }
}

impl<V, E> Fragment for ThunkCursor<V, E>
where
    V: Debug + Send + Sync + 'static,
    E: Debug + Send + Sync + 'static,
{
    fn add_operation(
        &mut self,
        input_len: usize,
        output_weights: impl IntoIterator<Item = E>,
        weight: V,
    ) -> Operation<V, E, false> {
        let op = OperationInternal::new(input_len, output_weights, weight);
        self.0
             .0
            .nodes
            .try_write()
            .expect("Lock unexpectedly taken")
            .push(NodeInternal::Operation(op.clone()));
        Operation(ByThinAddress(op))
    }

    fn add_thunk(
        &mut self,
        free_variables: impl IntoIterator<Item = E>,
        bound_variables: impl IntoIterator<Item = E>,
        output_weights: impl IntoIterator<Item = E>,
    ) -> Thunk<V, E, false> {
        let thunk = ThunkInternal::new(free_variables, bound_variables, output_weights);
        self.0
             .0
            .nodes
            .try_write()
            .expect("Lock unexpectedly taken")
            .push(NodeInternal::Thunk(thunk.clone()));
        Thunk(ByThinAddress(thunk))
    }
}
