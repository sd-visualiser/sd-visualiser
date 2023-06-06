use std::{
    fmt::Debug,
    sync::{Arc, RwLock, Weak},
};

use bimap::BiMap;
use by_address::ByThinAddress;
use derivative::Derivative;
use indexmap::IndexSet;

use super::{weakbyaddress::WeakByAddress, Thunk};

#[derive(Derivative, Debug)]
#[derivative(Clone(bound = ""))]
pub(super) enum WeakNodeInternal<V, E> {
    Operation(Weak<OperationInternal<V, E>>),
    Thunk(Weak<ThunkInternal<V, E>>),
}

#[derive(Debug)]
pub(super) struct InPortInternal<V, E> {
    pub(super) node: Option<WeakNodeInternal<V, E>>,
    pub(super) link: RwLock<Weak<OutPortInternal<V, E>>>,
}

impl<V, E> InPortInternal<V, E>
where
    V: Debug,
    E: Debug,
{
    pub(super) fn new_boundary() -> Self {
        Self {
            node: None,
            link: RwLock::new(Weak::default()),
        }
    }
}

#[derive(Debug)]
pub(super) struct OutPortInternal<V, E> {
    pub(super) node: Option<WeakNodeInternal<V, E>>,
    pub(super) links: RwLock<IndexSet<WeakByAddress<InPortInternal<V, E>>>>,
    pub(super) weight: E,
}

impl<V, E> OutPortInternal<V, E>
where
    V: Debug,
    E: Debug,
{
    pub(super) fn new_boundary(weight: E) -> Self {
        Self {
            node: None,
            links: RwLock::new(IndexSet::default()),
            weight,
        }
    }
}

#[derive(Debug, Derivative)]
#[derivative(Clone(bound = ""), Default(bound = ""))]
pub(super) struct HyperGraphInternal<V, E> {
    pub(super) nodes: Vec<NodeInternal<V, E>>,
    pub(super) graph_inputs: Vec<Arc<OutPortInternal<V, E>>>,
    pub(super) graph_outputs: Vec<Arc<InPortInternal<V, E>>>,
}

#[derive(Derivative, Debug)]
#[derivative(Clone(bound = ""))]
pub(super) enum NodeInternal<V, E> {
    Operation(Arc<OperationInternal<V, E>>),
    Thunk(Arc<ThunkInternal<V, E>>),
}

#[derive(Debug)]
pub(super) struct OperationInternal<V, E> {
    pub(super) weight: V,
    pub(super) inputs: Vec<Arc<InPortInternal<V, E>>>,
    pub(super) outputs: Vec<Arc<OutPortInternal<V, E>>>,
}

impl<V, E> OperationInternal<V, E>
where
    V: Debug + Send + Sync,
    E: Debug + Send + Sync,
{
    pub(super) fn new(
        input_len: usize,
        output_weights: impl IntoIterator<Item = E>,
        weight: V,
    ) -> Arc<Self> {
        Arc::new_cyclic(|weak: &Weak<Self>| {
            let inputs = (0..input_len)
                .map(|_| {
                    Arc::new(InPortInternal {
                        node: Some(WeakNodeInternal::Operation(weak.clone())),
                        link: RwLock::new(Weak::new()),
                    })
                })
                .collect();
            let outputs = output_weights
                .into_iter()
                .map(|weight| {
                    Arc::new(OutPortInternal {
                        node: Some(WeakNodeInternal::Operation(weak.clone())),
                        links: RwLock::new(IndexSet::default()),
                        weight,
                    })
                })
                .collect();
            OperationInternal {
                weight,
                inputs,
                outputs,
            }
        })
    }
}

type InOutMap<V, E> =
    BiMap<ByThinAddress<Arc<InPortInternal<V, E>>>, ByThinAddress<Arc<OutPortInternal<V, E>>>>;

#[derive(Debug)]
pub(super) struct ThunkInternal<V, E> {
    #[allow(clippy::type_complexity)]
    pub(super) nodes: RwLock<Vec<NodeInternal<V, E>>>,
    pub(super) free_variable_inputs: InOutMap<V, E>,
    pub(super) input_order: Vec<ByThinAddress<Arc<OutPortInternal<V, E>>>>,
    pub(super) bound_variables: Vec<Arc<OutPortInternal<V, E>>>,
    pub(super) outputs: InOutMap<V, E>,
    pub(super) output_order: Vec<ByThinAddress<Arc<InPortInternal<V, E>>>>,
}

impl<V, E> ThunkInternal<V, E>
where
    V: Debug + Send + Sync,
    E: Debug + Send + Sync,
{
    pub(super) fn new(
        free_variables: impl IntoIterator<Item = E>,
        bound_variables: impl IntoIterator<Item = E>,
        output_weights: impl IntoIterator<Item = E>,
    ) -> Arc<Self> {
        Arc::new_cyclic(|weak: &Weak<Self>| {
            let input_order: Vec<_> = free_variables
                .into_iter()
                .map(|fv| ByThinAddress(Arc::new(OutPortInternal::new_boundary(fv))))
                .collect();

            let free_variable_inputs = input_order
                .iter()
                .map(|_| {
                    ByThinAddress(Arc::new(InPortInternal {
                        node: Some(WeakNodeInternal::Thunk(weak.clone())),
                        link: RwLock::new(Weak::new()),
                    }))
                })
                .zip(input_order.iter().cloned())
                .collect();

            let bound_variables = bound_variables
                .into_iter()
                .map(|bv| Arc::new(OutPortInternal::new_boundary(bv)))
                .collect();

            let (output_order, outputs): (Vec<_>, BiMap<_, _>) = output_weights
                .into_iter()
                .map(|weight| {
                    let inner_output = ByThinAddress(Arc::new(InPortInternal::new_boundary()));
                    let outer_output = ByThinAddress(Arc::new(OutPortInternal {
                        node: Some(WeakNodeInternal::Thunk(weak.clone())),
                        links: RwLock::new(IndexSet::default()),
                        weight,
                    }));
                    (inner_output.clone(), (inner_output, outer_output))
                })
                .unzip();

            ThunkInternal {
                nodes: RwLock::new(Vec::default()),
                free_variable_inputs,
                input_order,
                bound_variables,
                outputs,
                output_order,
            }
        })
    }

    pub(super) fn from_existing<const BUILT: bool>(thunk: &Thunk<V, E, BUILT>) -> Arc<Self>
    where
        E: Clone,
    {
        Arc::new_cyclic(|weak: &Weak<Self>| {
            let internal = &thunk.0 .0;
            let free_variable_inputs = thunk
                .inputs()
                .map(|_| {
                    ByThinAddress(Arc::new(InPortInternal {
                        node: Some(WeakNodeInternal::Thunk(weak.clone())),
                        link: RwLock::new(Weak::new()),
                    }))
                })
                .zip(internal.input_order.iter().cloned())
                .collect();

            let outputs = internal
                .output_order
                .iter()
                .cloned()
                .zip(thunk.outputs().map(|out_port| {
                    ByThinAddress(Arc::new(OutPortInternal {
                        node: Some(WeakNodeInternal::Thunk(weak.clone())),
                        links: RwLock::new(IndexSet::default()),
                        weight: out_port.weight().clone(),
                    }))
                }))
                .collect();

            ThunkInternal {
                nodes: RwLock::new(internal.nodes.read().expect("Could not unlock").clone()),
                free_variable_inputs,
                input_order: internal.input_order.clone(),
                bound_variables: internal.bound_variables.clone(),
                outputs,
                output_order: internal.output_order.clone(),
            }
        })
    }
}
