use std::{fmt::Debug, sync::Arc};

use by_address::ByThinAddress;

use super::{Edge, Graph, InPort, Operation, OutPort, Thunk};

impl<V, E> Debug for InPort<V, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut x = f.debug_struct("InPort");
        if let Some(outport) = self
            .0
             .0
            .link
            .try_read()
            .expect("lock unexpectedly taken")
            .upgrade()
        {
            x.field("output", &OutPort::<V, E>(ByThinAddress(outport)));
        }
        x.finish()
    }
}

impl<V: Debug, E> Debug for Operation<V, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Operation")
            .field("weight", self.weight())
            .field("inputs", &self.inputs().collect::<Vec<_>>())
            .field("outputs", &self.outputs().collect::<Vec<_>>())
            .finish()
    }
}

impl<V: Debug, E: Debug> Debug for Thunk<V, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Thunk")
            .field(
                "free_vars",
                &self.unbound_graph_inputs().collect::<Vec<_>>(),
            )
            .field(
                "bound_inputs",
                &self.bound_graph_inputs().collect::<Vec<_>>(),
            )
            .field("nodes", &self.nodes().collect::<Vec<_>>())
            .field(
                "outputs",
                &self.graph_outputs().zip(self.outputs()).collect::<Vec<_>>(),
            )
            .finish()
    }
}

impl<V, E> Debug for OutPort<V, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("OutPort")
            .field(&Arc::as_ptr(&self.0))
            .finish()
    }
}

impl<V, E> Debug for Edge<V, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Edge").field(&Arc::as_ptr(&self.0)).finish()
    }
}
