use std::sync::{Arc, OnceLock, RwLock, Weak};

use by_address::ByThinAddress;
use derivative::Derivative;
use indexmap::IndexSet;

use super::{weakbyaddress::WeakByAddress, Weight};

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub(super) enum WeakNodeInternal<W: Weight> {
    Operation(Weak<OperationInternal<W>>),
    Thunk(Weak<ThunkInternal<W>>),
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub(super) struct InPortInternal<W: Weight> {
    pub(super) node: Option<WeakNodeInternal<W>>,
    pub(super) link: RwLock<Weak<OutPortInternal<W>>>,
}

impl<W: Weight> InPortInternal<W> {
    pub(super) fn new(node: Option<WeakNodeInternal<W>>) -> Self {
        Self {
            node,
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
    pub(super) node: Option<WeakNodeInternal<W>>,
    pub(super) links: RwLock<IndexSet<WeakByAddress<InPortInternal<W>>>>,
    pub(super) weight: W::EdgeWeight,
}

impl<W: Weight> OutPortInternal<W> {
    pub(super) fn new(node: Option<WeakNodeInternal<W>>, weight: W::EdgeWeight) -> Self {
        Self {
            node,
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

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub(super) struct ThunkInternal<W: Weight> {
    pub(super) weight: W::ThunkWeight,
    pub(super) nodes: RwLock<Vec<NodeInternal<W>>>,
    pub(super) free_inputs: OnceLock<IndexSet<ByThinAddress<Arc<OutPortInternal<W>>>>>,
    pub(super) bound_inputs: Vec<Arc<OutPortInternal<W>>>,
    pub(super) inner_outputs: Vec<Arc<InPortInternal<W>>>,
    pub(super) outer_outputs: Vec<Arc<OutPortInternal<W>>>,
    pub(super) backlink: Option<Weak<ThunkInternal<W>>>,
}

impl<W: Weight> ThunkInternal<W> {
    pub(super) fn new(
        bound_inputs: impl IntoIterator<Item = W::EdgeWeight>,
        inner_output_len: usize,
        outer_outputs: impl IntoIterator<Item = W::EdgeWeight>,
        weight: W::ThunkWeight,
        backlink: Option<Weak<ThunkInternal<W>>>,
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
