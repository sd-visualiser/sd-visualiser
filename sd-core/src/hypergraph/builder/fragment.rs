use std::sync::Arc;

use by_address::ByThinAddress;
use tracing::Level;

use crate::hypergraph::{
    builder::{
        HypergraphBuilder, HypergraphError, InPort, OperationBuilder, OutPort, Result, ThunkBuilder,
    },
    weakbyaddress::WeakByAddress,
    NodeInternal, OperationInternal, ThunkInternal, Weight,
};

pub trait Fragment {
    type Weight: Weight;

    fn add_operation(
        &mut self,
        input_len: usize,
        outputs: impl IntoIterator<Item = <Self::Weight as Weight>::EdgeWeight>,
        weight: <Self::Weight as Weight>::NodeWeight,
    ) -> OperationBuilder<Self::Weight>;

    fn add_thunk(
        &mut self,
        bound_inputs: impl IntoIterator<Item = <Self::Weight as Weight>::EdgeWeight>,
        inner_output_len: usize,
        outer_outputs: impl IntoIterator<Item = <Self::Weight as Weight>::EdgeWeight>,
        weight: <Self::Weight as Weight>::NodeWeight,
    ) -> ThunkBuilder<Self::Weight>;

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = InPort<Self::Weight>> + '_>;

    #[tracing::instrument(level=Level::DEBUG, skip(self), err, ret)]
    fn link(
        &mut self,
        out_port: OutPort<Self::Weight>,
        in_port: InPort<Self::Weight>,
    ) -> Result<(), Self::Weight> {
        let mut out = in_port.0.link.try_write().expect("Lock unexpectedly taken");

        if let Some(existing) = out.upgrade() {
            return Err(HypergraphError::OutputLinkError(OutPort(ByThinAddress(
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
        thunk: ThunkBuilder<Self::Weight>,
        f: impl FnOnce(ThunkCursor<Self::Weight>) -> T,
    ) -> T {
        f(ThunkCursor(thunk))
    }
}

impl<W: Weight> Fragment for HypergraphBuilder<W> {
    type Weight = W;

    fn add_operation(
        &mut self,
        input_len: usize,
        outputs: impl IntoIterator<Item = W::EdgeWeight>,
        weight: W::NodeWeight,
    ) -> OperationBuilder<W> {
        let op = OperationInternal::new(input_len, outputs, weight, None);
        self.0
            .nodes
            .push(NodeInternal::Operation(ByThinAddress(op.clone())));
        OperationBuilder(ByThinAddress(op))
    }

    fn add_thunk(
        &mut self,
        bound_inputs: impl IntoIterator<Item = W::EdgeWeight>,
        inner_output_len: usize,
        outer_outputs: impl IntoIterator<Item = W::EdgeWeight>,
        weight: W::NodeWeight,
    ) -> ThunkBuilder<W> {
        let thunk = ThunkInternal::new(bound_inputs, inner_output_len, outer_outputs, weight, None);
        self.0
            .nodes
            .push(NodeInternal::Thunk(ByThinAddress(thunk.clone())));
        ThunkBuilder(ByThinAddress(thunk))
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = InPort<W>> + '_> {
        Box::new(
            self.0
                .graph_outputs
                .iter()
                .map(|in_port| InPort(in_port.clone())),
        )
    }
}

pub struct ThunkCursor<W: Weight>(ThunkBuilder<W>);

impl<W: Weight> Fragment for ThunkCursor<W> {
    type Weight = W;

    fn add_operation(
        &mut self,
        input_len: usize,
        outputs: impl IntoIterator<Item = W::EdgeWeight>,
        weight: W::NodeWeight,
    ) -> OperationBuilder<W> {
        let op = OperationInternal::new(
            input_len,
            outputs,
            weight,
            Some(Arc::downgrade(&self.0 .0 .0)),
        );
        self.0
             .0
            .nodes
            .try_write()
            .expect("Lock unexpectedly taken")
            .push(NodeInternal::Operation(ByThinAddress(op.clone())));
        OperationBuilder(ByThinAddress(op))
    }

    fn add_thunk(
        &mut self,
        bound_inputs: impl IntoIterator<Item = W::EdgeWeight>,
        inner_output_len: usize,
        outer_outputs: impl IntoIterator<Item = W::EdgeWeight>,
        weight: W::NodeWeight,
    ) -> ThunkBuilder<W> {
        let thunk = ThunkInternal::new(
            bound_inputs,
            inner_output_len,
            outer_outputs,
            weight,
            Some(Arc::downgrade(&self.0 .0 .0)),
        );
        self.0
             .0
            .nodes
            .try_write()
            .expect("Lock unexpectedly taken")
            .push(NodeInternal::Thunk(ByThinAddress(thunk.clone())));
        ThunkBuilder(ByThinAddress(thunk))
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = InPort<W>> + '_> {
        Box::new(self.0.graph_outputs())
    }
}
