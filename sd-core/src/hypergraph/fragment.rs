use std::{collections::HashMap, fmt::Debug, sync::Arc};

use by_address::ByThinAddress;
use tracing::Level;

use super::{
    weakbyaddress::WeakByAddress, Edge, Graph, HyperGraphBuilder, HyperGraphError, InPort,
    NodeInternal, OperationBuilder, OperationInternal, OutPort, Result, Thunk, ThunkBuilder,
    ThunkCursor, ThunkInternal,
};
use crate::common::InOut;

pub trait Fragment {
    type NodeWeight;
    type EdgeWeight;

    fn add_operation(
        &mut self,
        input_len: usize,
        output_weights: impl IntoIterator<Item = Self::EdgeWeight>,
        weight: Self::NodeWeight,
    ) -> OperationBuilder<Self::NodeWeight, Self::EdgeWeight>;

    fn add_thunk(
        &mut self,
        bound_variables: impl IntoIterator<Item = Self::EdgeWeight>,
        output_weights: impl IntoIterator<Item = Self::EdgeWeight>,
    ) -> ThunkBuilder<Self::NodeWeight, Self::EdgeWeight>;

    fn graph_outputs(
        &self,
    ) -> Box<dyn DoubleEndedIterator<Item = InPort<Self::NodeWeight, Self::EdgeWeight>> + '_>;

    #[tracing::instrument(level=Level::DEBUG, skip(self), err, ret)]
    fn link(
        &mut self,
        out_port: OutPort<Self::NodeWeight, Self::EdgeWeight>,
        in_port: InPort<Self::NodeWeight, Self::EdgeWeight>,
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
        thunk: ThunkBuilder<Self::NodeWeight, Self::EdgeWeight>,
        f: impl FnOnce(ThunkCursor<Self::NodeWeight, Self::EdgeWeight>) -> T,
    ) -> T {
        f(ThunkCursor(thunk))
    }
}

impl<V, E> Fragment for HyperGraphBuilder<V, E> {
    type NodeWeight = V;
    type EdgeWeight = E;

    fn add_operation(
        &mut self,
        input_len: usize,
        output_weights: impl IntoIterator<Item = E>,
        weight: V,
    ) -> OperationBuilder<V, E> {
        let op = OperationInternal::new(input_len, output_weights, weight, None);
        self.0.nodes.push(NodeInternal::Operation(op.clone()));
        OperationBuilder(ByThinAddress(op))
    }

    fn add_thunk(
        &mut self,
        bound_variables: impl IntoIterator<Item = E>,
        output_weights: impl IntoIterator<Item = E>,
    ) -> ThunkBuilder<V, E> {
        let thunk = ThunkInternal::new(bound_variables, output_weights, None);
        self.0.nodes.push(NodeInternal::Thunk(thunk.clone()));
        ThunkBuilder(ByThinAddress(thunk))
    }

    fn graph_outputs(
        &self,
    ) -> Box<dyn DoubleEndedIterator<Item = InPort<Self::NodeWeight, Self::EdgeWeight>> + '_> {
        Box::new(
            self.0
                .graph_outputs
                .iter()
                .map(|in_port| InPort(ByThinAddress(in_port.clone()))),
        )
    }
}

impl<V, E> Fragment for ThunkCursor<V, E> {
    type NodeWeight = V;
    type EdgeWeight = E;

    fn add_operation(
        &mut self,
        input_len: usize,
        output_weights: impl IntoIterator<Item = E>,
        weight: V,
    ) -> OperationBuilder<V, E> {
        let op = OperationInternal::new(
            input_len,
            output_weights,
            weight,
            Some(Arc::downgrade(&self.0 .0 .0)),
        );
        self.0
             .0
            .nodes
            .try_write()
            .expect("Lock unexpectedly taken")
            .push(NodeInternal::Operation(op.clone()));
        OperationBuilder(ByThinAddress(op))
    }

    fn add_thunk(
        &mut self,
        bound_variables: impl IntoIterator<Item = E>,
        output_weights: impl IntoIterator<Item = E>,
    ) -> ThunkBuilder<V, E> {
        let thunk = ThunkInternal::new(
            bound_variables,
            output_weights,
            Some(Arc::downgrade(&self.0 .0 .0)),
        );
        self.0
             .0
            .nodes
            .try_write()
            .expect("Lock unexpectedly taken")
            .push(NodeInternal::Thunk(thunk.clone()));
        ThunkBuilder(ByThinAddress(thunk))
    }

    fn graph_outputs(
        &self,
    ) -> Box<dyn DoubleEndedIterator<Item = InPort<Self::NodeWeight, Self::EdgeWeight>> + '_> {
        Box::new(
            self.0
                 .0
                .outputs
                .keys()
                .map(|in_port| InPort(in_port.clone())),
        )
    }
}

impl<V: Clone, E: Clone> Thunk<V, E> {
    /// Clone a thunk into the fragment, maintaining a map from `in_ports` and `out_ports`
    /// in the generated graph to edges in the original thunk.
    pub(super) fn clone_thunk<F: Fragment<NodeWeight = V, EdgeWeight = E>>(
        &self,
        fragment: &mut F,
        in_port_map: &mut HashMap<InPort<V, E>, Edge<V, E>>,
        out_port_map: &mut HashMap<Edge<V, E>, OutPort<V, E>>,
    ) {
        let bound_variables = self.bound_graph_inputs().map(|edge| edge.weight().clone());

        let output_weights = self.outputs().map(|edge| edge.weight().clone());

        let thunk = fragment.add_thunk(bound_variables, output_weights);

        out_port_map.extend(self.bound_graph_inputs().zip(thunk.bound_inputs()));
        out_port_map.extend(self.outputs().zip(thunk.outputs()));

        fragment.in_thunk(thunk, |mut inner_fragment| {
            in_port_map.extend(inner_fragment.graph_outputs().zip(self.graph_outputs()));

            for node in self.nodes() {
                match &node {
                    super::Node::Operation(op) => {
                        let input_len = op.number_of_inputs();
                        let output_weights: Vec<_> = op
                            .outputs()
                            .map(|out_port| out_port.weight().clone())
                            .collect();
                        let weight: V = op.weight().clone();

                        let op = inner_fragment.add_operation(input_len, output_weights, weight);

                        in_port_map.extend(op.inputs().zip(node.inputs()));

                        out_port_map.extend(node.outputs().zip(op.outputs()));
                    }
                    super::Node::Thunk(thunk) => {
                        thunk.clone_thunk(&mut inner_fragment, in_port_map, out_port_map);
                    }
                }
            }
        });
    }
}
