use std::{
    fmt::Debug,
    sync::{Arc, OnceLock, RwLock, Weak},
};

use by_address::ByThinAddress;
use derivative::Derivative;
use indexmap::{IndexMap, IndexSet};

use super::weakbyaddress::WeakByAddress;

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

impl<V, E> InPortInternal<V, E> {
    pub(super) fn new(node: Option<WeakNodeInternal<V, E>>) -> Self {
        Self {
            node,
            link: RwLock::new(Weak::default()),
        }
    }

    pub(super) fn link(&self) -> Arc<OutPortInternal<V, E>> {
        self.link.read().unwrap().upgrade().unwrap()
    }
}

#[derive(Debug)]
pub(super) struct OutPortInternal<V, E> {
    pub(super) node: Option<WeakNodeInternal<V, E>>,
    pub(super) links: RwLock<IndexSet<WeakByAddress<InPortInternal<V, E>>>>,
    pub(super) weight: E,
}

impl<V, E> OutPortInternal<V, E> {
    pub(super) fn new(node: Option<WeakNodeInternal<V, E>>, weight: E) -> Self {
        Self {
            node,
            links: RwLock::new(IndexSet::default()),
            weight,
        }
    }
}

#[derive(Derivative, Debug)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = "")
)]
pub(super) enum NodeInternal<V, E> {
    Operation(ByThinAddress<Arc<OperationInternal<V, E>>>),
    Thunk(ByThinAddress<Arc<ThunkInternal<V, E>>>),
}

#[derive(Debug)]
pub(super) struct OperationInternal<V, E> {
    pub(super) weight: V,
    pub(super) inputs: Vec<Arc<InPortInternal<V, E>>>,
    pub(super) outputs: Vec<Arc<OutPortInternal<V, E>>>,
    pub(super) backlink: Option<Weak<ThunkInternal<V, E>>>,
}

impl<V, E> OperationInternal<V, E> {
    pub(super) fn new(
        input_len: usize,
        output_weights: impl IntoIterator<Item = E>,
        weight: V,
        backlink: Option<Weak<ThunkInternal<V, E>>>,
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
                backlink,
            }
        })
    }
}

#[derive(Debug)]
pub(super) struct ThunkInternal<V, E> {
    pub(super) nodes: RwLock<Vec<NodeInternal<V, E>>>,
    #[allow(clippy::type_complexity)]
    pub(super) free_variable_edges: OnceLock<IndexSet<ByThinAddress<Arc<OutPortInternal<V, E>>>>>,
    pub(super) bound_variables: Vec<ByThinAddress<Arc<OutPortInternal<V, E>>>>,
    #[allow(clippy::type_complexity)]
    pub(super) outputs:
        IndexMap<ByThinAddress<Arc<InPortInternal<V, E>>>, Arc<OutPortInternal<V, E>>>,
    pub(super) backlink: Option<Weak<ThunkInternal<V, E>>>,
}

impl<V, E> ThunkInternal<V, E> {
    pub(super) fn new(
        bound_variables: impl IntoIterator<Item = E>,
        output_weights: impl IntoIterator<Item = E>,
        backlink: Option<Weak<ThunkInternal<V, E>>>,
    ) -> Arc<Self> {
        Arc::new_cyclic(|weak: &Weak<Self>| {
            let bound_variables = bound_variables
                .into_iter()
                .map(|bv| ByThinAddress(Arc::new(OutPortInternal::new(None, bv))))
                .collect();

            let outputs = output_weights
                .into_iter()
                .map(|weight| {
                    let inner_output = ByThinAddress(Arc::new(InPortInternal::new(Some(
                        WeakNodeInternal::Thunk(weak.clone()),
                    ))));
                    let outer_output = Arc::new(OutPortInternal::new(
                        Some(WeakNodeInternal::Thunk(weak.clone())),
                        weight,
                    ));
                    (inner_output, outer_output)
                })
                .collect();

            ThunkInternal {
                nodes: RwLock::new(Vec::default()),
                free_variable_edges: OnceLock::new(),
                bound_variables,
                outputs,
                backlink,
            }
        })
    }
}
