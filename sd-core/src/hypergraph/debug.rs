use std::{fmt::Debug, sync::Arc};

use by_address::ByThinAddress;

use super::{Graph, InPort, Operation, OutPort, Thunk};

impl<V, E, const BUILT: bool> Debug for InPort<V, E, BUILT> {
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
            x.field("output", &OutPort::<V, E, BUILT>(ByThinAddress(outport)));
        }
        x.finish()
    }
}

impl<V: Debug, E, const BUILT: bool> Debug for Operation<V, E, BUILT> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Operation")
            .field("weight", self.weight())
            // .field("inputs", &self.inputs().collect::<Vec<_>>())
            .field("outputs", &self.outputs().collect::<Vec<_>>())
            .finish()
    }
}

impl<V: Debug, E: Debug, const BUILT: bool> Debug for Thunk<V, E, BUILT> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Thunk")
            // .field(
            //     "free_vars",
            //     &self
            //         .unbound_graph_inputs()
            //         .collect::<Vec<_>>(),
            // )
            .field(
                "bound_inputs",
                &self.bound_graph_inputs().collect::<Vec<_>>(),
            )
            .field("nodes", &self.nodes().collect::<Vec<_>>())
            .field(
                "outputs",
                &self
                    .graph_outputs()
                    .map(|in_port| {
                        let x = self.externalise_output(&in_port);
                        (in_port, x)
                    })
                    .collect::<Vec<_>>(),
            )
            .finish()
    }
}

impl<V, E, const BUILT: bool> Debug for OutPort<V, E, BUILT> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("OutPort")
            .field(&Arc::as_ptr(&self.0))
            .finish()
    }
}
