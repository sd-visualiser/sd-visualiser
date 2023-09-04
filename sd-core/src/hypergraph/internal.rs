use std::sync::{Arc, OnceLock, RwLock, Weak};

use by_address::ByThinAddress;
use derivative::Derivative;
use indexmap::IndexSet;

use super::{weakbyaddress::WeakByAddress, Weight};

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub(super) enum EndPointInternal<W: Weight> {
    Operation(Weak<OperationInternal<W>>),
    Thunk(Weak<ThunkInternal<W>>),
    GraphBoundary(Option<Weak<ThunkInternal<W>>>),
}

impl<W: Weight> EndPointInternal<W> {
    pub(super) fn is_boundary(&self) -> bool {
        matches!(self, EndPointInternal::GraphBoundary(_))
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub(super) struct InPortInternal<W: Weight> {
    pub(super) endpoint: EndPointInternal<W>,
    pub(super) link: RwLock<Weak<OutPortInternal<W>>>,
}

impl<W: Weight> InPortInternal<W> {
    pub(super) fn new(endpoint: EndPointInternal<W>) -> Self {
        Self {
            endpoint,
            link: RwLock::new(Weak::default()),
        }
    }

    pub(super) fn link(&self) -> Arc<OutPortInternal<W>> {
        self.link.read().unwrap().upgrade().unwrap()
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub(super) struct OutPortInternal<W: Weight> {
    pub(super) endpoint: EndPointInternal<W>,
    pub(super) links: RwLock<IndexSet<WeakByAddress<InPortInternal<W>>>>,
    pub(super) weight: W::EdgeWeight,
}

impl<W: Weight> OutPortInternal<W> {
    pub(super) fn new(endpoint: EndPointInternal<W>, weight: W::EdgeWeight) -> Self {
        Self {
            endpoint,
            links: RwLock::new(IndexSet::default()),
            weight,
        }
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub(super) enum NodeInternal<W: Weight> {
    Operation(ByThinAddress<Arc<OperationInternal<W>>>),
    Thunk(ByThinAddress<Arc<ThunkInternal<W>>>),
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub(super) struct OperationInternal<W: Weight> {
    pub(super) weight: W::OperationWeight,
    pub(super) inputs: Vec<Arc<InPortInternal<W>>>,
    pub(super) outputs: Vec<Arc<OutPortInternal<W>>>,
    pub(super) backlink: Option<Weak<ThunkInternal<W>>>,
}

impl<W: Weight> OperationInternal<W> {
    pub(super) fn new(
        input_len: usize,
        outputs: impl IntoIterator<Item = W::EdgeWeight>,
        weight: W::OperationWeight,
        backlink: Option<Weak<ThunkInternal<W>>>,
    ) -> Arc<Self> {
        Arc::new_cyclic(|weak: &Weak<Self>| {
            let inputs = (0..input_len)
                .map(|_| {
                    Arc::new(InPortInternal {
                        endpoint: EndPointInternal::Operation(weak.clone()),
                        link: RwLock::new(Weak::new()),
                    })
                })
                .collect();
            let outputs = outputs
                .into_iter()
                .map(|weight| {
                    Arc::new(OutPortInternal {
                        endpoint: EndPointInternal::Operation(weak.clone()),
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

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub(super) struct ThunkInternal<W: Weight> {
    pub(super) weight: W::ThunkWeight,
    pub(super) nodes: RwLock<Vec<NodeInternal<W>>>,
    #[allow(clippy::type_complexity)]
    pub(super) free_inputs: OnceLock<IndexSet<ByThinAddress<Arc<OutPortInternal<W>>>>>,
    #[allow(clippy::type_complexity)]
    pub(super) free_outputs: OnceLock<IndexSet<ByThinAddress<Arc<OutPortInternal<W>>>>>,
    pub(super) bound_inputs: Vec<Arc<OutPortInternal<W>>>,
    pub(super) bound_outputs: Vec<Arc<InPortInternal<W>>>,
    pub(super) inputs: Vec<Arc<InPortInternal<W>>>,
    pub(super) outputs: Vec<Arc<OutPortInternal<W>>>,
    pub(super) backlink: Option<Weak<ThunkInternal<W>>>,
}

impl<W: Weight> ThunkInternal<W> {
    pub(super) fn new(
        weight: W::ThunkWeight,
        inputs_len: usize,
        bound_inputs: impl IntoIterator<Item = W::EdgeWeight>,
        bound_output_len: usize,
        outputs: impl IntoIterator<Item = W::EdgeWeight>,
        backlink: Option<Weak<ThunkInternal<W>>>,
    ) -> Arc<Self> {
        Arc::new_cyclic(|weak: &Weak<Self>| {
            let bound_inputs = bound_inputs
                .into_iter()
                .map(|weight| {
                    Arc::new(OutPortInternal::new(
                        EndPointInternal::GraphBoundary(Some(weak.clone())),
                        weight,
                    ))
                })
                .collect();

            let bound_outputs = (0..bound_output_len)
                .map(|_| {
                    Arc::new(InPortInternal::new(EndPointInternal::GraphBoundary(Some(
                        weak.clone(),
                    ))))
                })
                .collect();

            let inputs = (0..inputs_len)
                .map(|_| Arc::new(InPortInternal::new(EndPointInternal::Thunk(weak.clone()))))
                .collect();

            let outputs = outputs
                .into_iter()
                .map(|weight| {
                    Arc::new(OutPortInternal::new(
                        EndPointInternal::Thunk(weak.clone()),
                        weight,
                    ))
                })
                .collect();

            ThunkInternal {
                weight,
                nodes: RwLock::new(Vec::default()),
                free_inputs: OnceLock::new(),
                free_outputs: OnceLock::new(),
                bound_inputs,
                bound_outputs,
                outputs,
                backlink,
                inputs,
            }
        })
    }
}
