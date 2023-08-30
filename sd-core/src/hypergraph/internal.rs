use std::{
    fmt::Debug,
    sync::{Arc, OnceLock, RwLock, Weak},
};

use by_address::ByThinAddress;
use derivative::Derivative;
use indexmap::IndexSet;

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
        outputs: impl IntoIterator<Item = E>,
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
            let outputs = outputs
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
    pub(super) weight: V,
    pub(super) nodes: RwLock<Vec<NodeInternal<V, E>>>,
    #[allow(clippy::type_complexity)]
    pub(super) free_inputs: OnceLock<IndexSet<ByThinAddress<Arc<OutPortInternal<V, E>>>>>,
    pub(super) bound_inputs: Vec<Arc<OutPortInternal<V, E>>>,
    pub(super) inner_outputs: Vec<Arc<InPortInternal<V, E>>>,
    pub(super) outer_outputs: Vec<Arc<OutPortInternal<V, E>>>,
    pub(super) backlink: Option<Weak<ThunkInternal<V, E>>>,
}

impl<V, E> ThunkInternal<V, E> {
    pub(super) fn new(
        bound_inputs: impl IntoIterator<Item = E>,
        inner_output_len: usize,
        outer_outputs: impl IntoIterator<Item = E>,
        weight: V,
        backlink: Option<Weak<ThunkInternal<V, E>>>,
    ) -> Arc<Self> {
        Arc::new_cyclic(|weak: &Weak<Self>| {
            let bound_inputs = bound_inputs
                .into_iter()
                .map(|weight| Arc::new(OutPortInternal::new(None, weight)))
                .collect();

            let inner_outputs = (0..inner_output_len)
                .map(|_| {
                    Arc::new(InPortInternal::new(Some(WeakNodeInternal::Thunk(
                        weak.clone(),
                    ))))
                })
                .collect();

            let outer_outputs = outer_outputs
                .into_iter()
                .map(|weight| {
                    Arc::new(OutPortInternal::new(
                        Some(WeakNodeInternal::Thunk(weak.clone())),
                        weight,
                    ))
                })
                .collect();

            ThunkInternal {
                weight,
                nodes: RwLock::new(Vec::default()),
                free_inputs: OnceLock::new(),
                bound_inputs,
                inner_outputs,
                outer_outputs,
                backlink,
            }
        })
    }
}
