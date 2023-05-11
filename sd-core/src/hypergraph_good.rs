use std::hash::Hash;
use std::ops::{Deref, DerefMut};
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    sync::{Arc, Weak},
};

use bimap::BiMap;
use by_address::ByThinAddress;
use derivative::Derivative;
use getset::Getters;

use qcell::{QCell, QCellOwner};
#[cfg(test)]
use serde::Serialize;
use thiserror::Error;

use self::owner::HasOwner;

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(Serialize))]
pub enum EdgeStrength {
    Strong,
    Weak,
}

#[derive(Clone)]
pub struct Owned<'a, T: ?Sized> {
    inner: Arc<T>,
    owner: &'a QCellOwner,
}

pub struct OwnedMut<'a, T: ?Sized> {
    inner: Arc<T>,
    owner: &'a mut QCellOwner,
}

pub type InPort<'hyper, V, E> = Owned<'hyper, InPortBuilder<V, E>>;
pub type OutPort<'hyper, V, E> = Owned<'hyper, OutPortBuilder<V, E>>;
pub type Operation<'hyper, V, E> = Owned<'hyper, OperationBuilder<V, E>>;
pub type Thunk<'hyper, V, E> = Owned<'hyper, ThunkBuilder<V, E>>;

#[derive(Debug, Error, Clone)]
pub enum HyperGraphError<V, E>
where
    V: Debug,
    E: Debug,
{
    #[error("No input port at index `{0:#?}`")]
    UnknownInput(Arc<InPortBuilder<V, E>>),
    #[error("No output port at index `{0:#?}`")]
    UnknownOutput(Arc<OutPortBuilder<V, E>>),
    #[error("Input port not linked: {0:#?}")]
    UnlinkedInput(Arc<InPortBuilder<V, E>>),
    #[error("Unlinked port")]
    UnlinkedPort,
    #[error("Input port already linked: {0:#?}")]
    InputLinkError(Arc<InPortBuilder<V, E>>),
    #[error("Output port already linked to specified input: {0:#?}")]
    OutputLinkError(Arc<OutPortBuilder<V, E>>),
}

type Result<T, V, E> = core::result::Result<T, HyperGraphError<V, E>>;

#[derive(Derivative, Debug)]
#[derivative(Clone(bound = ""))]
enum WeakNodeBuilder<V, E> {
    Operation(Weak<OperationBuilder<V, E>>),
    Thunk(Weak<ThunkBuilder<V, E>>),
}

#[derive(Derivative, Getters)]
#[derivative(Debug)]
pub struct InPortBuilder<V, E> {
    node: Option<WeakNodeBuilder<V, E>>,
    #[derivative(Debug = "ignore")]
    output: QCell<Weak<OutPortBuilder<V, E>>>,
}

impl<V, E> InPortBuilder<V, E>
where
    V: Debug,
    E: Debug,
{
    fn new_boundary(owner: &QCellOwner) -> Self {
        Self {
            node: None,
            output: QCell::new(owner, Default::default()),
        }
    }
}

pub enum Node<'hyper, V, E> {
    Operation(Operation<'hyper, V, E>),
    Thunk(Thunk<'hyper, V, E>),
}

macro_rules! get_node {
    () => {
        pub fn node(&self) -> Option<Node<V, E>> {
            Some(match self.inner.node.as_ref()? {
                WeakNodeBuilder::Operation(weak_op) => Node::Operation(Owned {
                    inner: weak_op
                        .upgrade()
                        .expect("got dangling reference to operation"),
                    owner: &self.owner,
                }),
                WeakNodeBuilder::Thunk(weak_thunk) => Node::Thunk(Owned {
                    inner: weak_thunk
                        .upgrade()
                        .expect("got dangling reference to thunk"),
                    owner: &self.owner,
                }),
            })
        }
    };
}

impl<V, E> InPort<'_, V, E> {
    get_node! {}

    pub fn output(&self) -> OutPort<V, E> {
        Owned {
            inner: self
                .inner
                .output
                .ro(self.owner)
                .upgrade()
                .expect("got dangling reference to outport"),
            owner: self.owner,
        }
    }
}

#[derive(Derivative, Getters)]
#[derivative(Debug)]
pub struct OutPortBuilder<V, E> {
    node: Option<WeakNodeBuilder<V, E>>,
    #[derivative(Debug = "ignore")]
    inputs: QCell<HashMap<WeakByAddress<InPortBuilder<V, E>>, EdgeStrength>>,
    #[get = "pub"]
    weight: E,
}

struct WeakByAddress<T>(Weak<T>);

impl<V, E> PartialEq for WeakByAddress<InPortBuilder<V, E>> {
    fn eq(&self, other: &Self) -> bool {
        self.0.ptr_eq(&other.0)
    }
}

impl<V, E> Eq for WeakByAddress<InPortBuilder<V, E>> {}

impl<V, E> Hash for WeakByAddress<InPortBuilder<V, E>> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.0.as_ptr(), state)
    }
}

impl<T> Deref for WeakByAddress<T> {
    type Target = Weak<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for WeakByAddress<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// impl_partialeq_eq_hash_by_ptr!(&OutputPort<'hyper, V, E>);

impl<V, E> OutPortBuilder<V, E>
where
    V: Debug,
    E: Debug,
{
    fn new_boundary(owner: &QCellOwner, weight: E) -> Self {
        Self {
            node: None,
            inputs: QCell::new(owner, Default::default()),
            weight,
        }
    }
}

impl<V, E> OutPort<'_, V, E> {
    get_node! {}

    pub fn inputs(&self) -> impl Iterator<Item = (InPort<V, E>, EdgeStrength)> {
        self.inner
            .inputs
            .ro(self.owner)
            .iter()
            .map(|(WeakByAddress(inport), &strength)| {
                let inport = inport.upgrade().expect("got dangling reference to inport");
                (
                    Owned {
                        inner: inport,
                        owner: self.owner,
                    },
                    strength,
                )
            })
    }
}

pub struct HyperGraphBuilder<V, E>
where
    V: 'static,
    E: 'static,
{
    operations: HashSet<ByThinAddress<Arc<OperationBuilder<V, E>>>>,
    thunks: HashSet<ByThinAddress<Arc<ThunkBuilder<V, E>>>>,
    graph_inputs: HashSet<ByThinAddress<Arc<OutPortBuilder<V, E>>>>,
    graph_outputs: HashSet<ByThinAddress<Arc<InPortBuilder<V, E>>>>,
    token: QCellOwner,
}

pub struct HyperGraph<V, E>(HyperGraphBuilder<V, E>)
where
    V: Debug + 'static,
    E: Debug + 'static;

impl<V, E> HyperGraphBuilder<V, E>
where
    V: Debug,
    E: Debug,
{
    pub fn new(input_weights: Vec<E>, number_of_outputs: usize) -> Self {
        let token = QCellOwner::new();

        let graph_inputs = input_weights
            .into_iter()
            .map(|weight| ByThinAddress(Arc::new(OutPortBuilder::new_boundary(&token, weight))))
            .collect();

        let graph_outputs = (0..number_of_outputs)
            .into_iter()
            .map(|_| ByThinAddress(Arc::new(InPortBuilder::new_boundary(&token))))
            .collect();

        HyperGraphBuilder {
            operations: Default::default(),
            thunks: Default::default(),
            graph_inputs,
            graph_outputs,
            token,
        }
    }

    pub fn build(self) -> Result<HyperGraph<V, E>, V, E> {
        // check validity of hypergraph:
        // no unlinked ports
        // no strong cycles
        // todo!();

        Ok(HyperGraph(self))
    }
}

impl<V, E> HyperGraph<V, E>
where
    V: Debug,
    E: Debug,
{
    pub fn operations(&self) -> impl Iterator<Item = Operation<V, E>> {
        self.0.operations.iter().cloned().map(|node| Owned {
            inner: node.0,
            owner: &self.0.token,
        })
    }

    pub fn thunks(&self) -> impl Iterator<Item = Thunk<V, E>> {
        self.0.thunks.iter().cloned().map(|node| Owned {
            inner: node.0,
            owner: &self.0.token,
        })
    }

    pub fn graph_inputs(&self) -> impl Iterator<Item = OutPort<V, E>> {
        self.0
            .graph_inputs
            .iter()
            .map(|ByThinAddress(inner)| Owned {
                inner: inner.clone(),
                owner: &self.0.token,
            })
    }

    pub fn graph_outputs(&self) -> impl Iterator<Item = InPort<V, E>> {
        self.0
            .graph_outputs
            .iter()
            .map(|ByThinAddress(inner)| Owned {
                inner: inner.clone(),
                owner: &self.0.token,
            })
    }
}

mod owner {
    pub trait HasOwner {
        fn owner(&mut self) -> &mut qcell::QCellOwner;
    }

    impl<V, E> HasOwner for super::HyperGraphBuilder<V, E> {
        fn owner(&mut self) -> &mut qcell::QCellOwner {
            &mut self.token
        }
    }

    impl<V, E> HasOwner for super::OwnedMut<'_, super::ThunkBuilder<V, E>> {
        fn owner(&mut self) -> &mut qcell::QCellOwner {
            self.owner
        }
    }
}

pub trait Fragment<V: 'static, E: 'static>: owner::HasOwner {
    fn add_operation(
        &mut self,
        input_len: usize,
        output_weights: Vec<E>,
        weight: V,
    ) -> Arc<OperationBuilder<V, E>>;

    fn add_thunk(
        &mut self,
        free_variables: Vec<E>,
        bound_variables: Vec<E>,
        output_weights: Vec<E>,
    ) -> Arc<ThunkBuilder<V, E>>;

    fn link(
        &mut self,
        out_port: Arc<OutPortBuilder<V, E>>,
        in_port: Arc<InPortBuilder<V, E>>,
        strength: EdgeStrength,
    ) -> Result<(), V, E>
    where
        V: Debug,
        E: Debug,
    {
        let out = in_port.output.rw(self.owner());
        if let Some(existing) = out.upgrade() {
            return Err(HyperGraphError::OutputLinkError(existing));
        } else {
            *out = Arc::downgrade(&out_port);
        }
        out_port
            .inputs
            .rw(self.owner())
            .insert(WeakByAddress(Arc::downgrade(&in_port)), strength);
        Ok(())
    }

    fn borrow(&mut self, thunk: Arc<ThunkBuilder<V, E>>) -> OwnedMut<ThunkBuilder<V, E>> {
        OwnedMut {
            inner: thunk,
            owner: self.owner(),
        }
    }

    fn in_thunk<T>(
        &mut self,
        thunk: Arc<ThunkBuilder<V, E>>,
        f: impl FnOnce(OwnedMut<ThunkBuilder<V, E>>) -> T,
    ) -> T
    where
        V: Debug,
        E: Debug,
    {
        let owned = OwnedMut {
            inner: thunk,
            owner: self.owner(),
        };
        f(owned)
    }

    fn graph_inputs(&self) -> Box<dyn Iterator<Item = Arc<OutPortBuilder<V, E>>> + '_>;
    fn graph_outputs(&self) -> Box<dyn Iterator<Item = Arc<InPortBuilder<V, E>>> + '_>;
}

impl<V, E> Fragment<V, E> for HyperGraphBuilder<V, E>
where
    V: Debug + Send + Sync,
    E: Debug + Send + Sync,
{
    fn add_operation(
        &mut self,
        input_len: usize,
        output_weights: Vec<E>,
        weight: V,
    ) -> Arc<OperationBuilder<V, E>> {
        let op = OperationBuilder::new(self.owner(), input_len, output_weights, weight);
        self.operations.insert(ByThinAddress(op.clone()));
        op
    }

    fn add_thunk(
        &mut self,
        free_variables: Vec<E>,
        bound_variables: Vec<E>,
        output_weights: Vec<E>,
    ) -> Arc<ThunkBuilder<V, E>> {
        let thunk = ThunkBuilder::new(
            self.owner(),
            free_variables,
            bound_variables,
            output_weights,
        );
        self.thunks.insert(ByThinAddress(thunk.clone()));
        thunk
    }

    fn graph_inputs(&self) -> Box<dyn Iterator<Item = Arc<OutPortBuilder<V, E>>> + '_> {
        Box::new(
            self.graph_inputs
                .iter()
                .map(|ByThinAddress(out_port)| out_port.clone()),
        )
    }

    fn graph_outputs(&self) -> Box<dyn Iterator<Item = Arc<InPortBuilder<V, E>>> + '_> {
        Box::new(
            self.graph_outputs
                .iter()
                .map(|ByThinAddress(in_port)| in_port.clone()),
        )
    }
}

impl<V, E> Fragment<V, E> for OwnedMut<'_, ThunkBuilder<V, E>>
where
    V: Debug + Send + Sync + 'static,
    E: Debug + Send + Sync + 'static,
{
    fn add_operation(
        &mut self,
        input_len: usize,
        output_weights: Vec<E>,
        weight: V,
    ) -> Arc<OperationBuilder<V, E>> {
        let op = OperationBuilder::new(self.owner(), input_len, output_weights, weight);
        let operations = self.inner.operations.rw(self.owner);
        operations.insert(ByThinAddress(op.clone()));
        op
    }

    fn add_thunk(
        &mut self,
        free_variables: Vec<E>,
        bound_variables: Vec<E>,
        output_weights: Vec<E>,
    ) -> Arc<ThunkBuilder<V, E>> {
        let thunk = ThunkBuilder::new(
            self.owner(),
            free_variables,
            bound_variables,
            output_weights,
        );
        let thunks = self.inner.thunks.rw(self.owner);
        thunks.insert(ByThinAddress(thunk.clone()));
        thunk
    }

    fn graph_inputs(&self) -> Box<dyn Iterator<Item = Arc<OutPortBuilder<V, E>>> + '_> {
        Box::new(self.inner.free_inputs().chain(self.inner.bound_inputs()))
    }

    fn graph_outputs(&self) -> Box<dyn Iterator<Item = Arc<InPortBuilder<V, E>>> + '_> {
        Box::new(
            self.inner
                .outputs
                .left_values()
                .map(|ByThinAddress(inport)| inport.clone()),
        )
    }
}

// pub type Node<'hyper, V, E> = Owned<'hyper, dyn NodeBuilder<V, E>>;

// impl<V, E> Node<'_, V, E> {
//     pub fn inputs(&self) -> impl Iterator<Item = InPort<V, E>> + '_ {
//         self.inner.inputs().map(|inner| Owned {
//             inner,
//             owner: self.owner,
//         })
//     }

//     pub fn outputs(&self) -> impl Iterator<Item = OutPort<V, E>> + '_ {
//         self.inner.outputs().map(|inner| Owned {
//             inner,
//             owner: self.owner,
//         })
//     }
// }

#[derive(Derivative)]
#[derivative(Debug)]
pub struct OperationBuilder<V, E> {
    weight: V,
    inputs: Vec<Arc<InPortBuilder<V, E>>>,
    outputs: Vec<Arc<OutPortBuilder<V, E>>>,
}

impl<V, E> OperationBuilder<V, E>
where
    V: Debug + Send + Sync + 'static,
    E: Debug + Send + Sync + 'static,
{
    fn new(owner: &QCellOwner, input_len: usize, output_weights: Vec<E>, weight: V) -> Arc<Self> {
        Arc::new_cyclic(|weak: &Weak<Self>| {
            let inputs = (0..input_len)
                .map(|_| {
                    Arc::new(InPortBuilder {
                        node: Some(WeakNodeBuilder::Operation(weak.clone())),
                        output: QCell::new(owner, Weak::new()),
                    })
                })
                .collect();
            let outputs = output_weights
                .into_iter()
                .map(|weight| {
                    Arc::new(OutPortBuilder {
                        node: Some(WeakNodeBuilder::Operation(weak.clone())),
                        inputs: QCell::new(owner, Default::default()),
                        weight,
                    })
                })
                .collect();
            OperationBuilder {
                weight,
                inputs,
                outputs,
            }
        })
    }

    pub fn inputs(&self) -> impl Iterator<Item = Arc<InPortBuilder<V, E>>> + '_ {
        self.inputs.iter().cloned()
    }

    pub fn outputs(&self) -> impl Iterator<Item = Arc<OutPortBuilder<V, E>>> + '_ {
        self.outputs.iter().cloned()
    }
}

type InOutMap<V, E> =
    BiMap<ByThinAddress<Arc<InPortBuilder<V, E>>>, ByThinAddress<Arc<OutPortBuilder<V, E>>>>;

#[derive(Derivative)]
#[derivative(Debug)]
pub struct ThunkBuilder<V, E> {
    #[derivative(Debug = "ignore")]
    operations: QCell<HashSet<ByThinAddress<Arc<OperationBuilder<V, E>>>>>,
    #[derivative(Debug = "ignore")]
    thunks: QCell<HashSet<ByThinAddress<Arc<ThunkBuilder<V, E>>>>>,
    free_variable_inputs: InOutMap<V, E>,
    bound_variables: Vec<Arc<OutPortBuilder<V, E>>>,
    outputs: InOutMap<V, E>,
}

impl<V, E> ThunkBuilder<V, E>
where
    V: Debug + Send + Sync + 'static,
    E: Debug + Send + Sync + 'static,
{
    fn new(
        owner: &QCellOwner,
        free_variables: Vec<E>,
        bound_variables: Vec<E>,
        output_weights: Vec<E>,
    ) -> Arc<Self> {
        Arc::new_cyclic(|weak: &Weak<Self>| {
            let free_variable_inputs = free_variables
                .into_iter()
                .map(|fv| {
                    let inner_output = Arc::new(OutPortBuilder::new_boundary(owner, fv));
                    let outer_input = Arc::new(InPortBuilder {
                        node: Some(WeakNodeBuilder::Thunk(weak.clone())),
                        output: QCell::new(owner, Weak::new()),
                    });
                    (ByThinAddress(outer_input), ByThinAddress(inner_output))
                })
                .collect();
            let bound_variables = bound_variables
                .into_iter()
                .map(|bv| Arc::new(OutPortBuilder::new_boundary(owner, bv)))
                .collect();
            let outputs = output_weights
                .into_iter()
                .map(|weight| {
                    let inner_input = Arc::new(InPortBuilder::new_boundary(owner));
                    let outer_output = Arc::new(OutPortBuilder {
                        node: Some(WeakNodeBuilder::Thunk(weak.clone())),
                        inputs: QCell::new(owner, Default::default()),
                        weight,
                    });
                    (ByThinAddress(inner_input), ByThinAddress(outer_output))
                })
                .collect();
            ThunkBuilder {
                operations: QCell::new(owner, Default::default()),
                thunks: QCell::new(owner, Default::default()),
                free_variable_inputs,
                bound_variables,
                outputs,
            }
        })
    }
}

impl<V, E> ThunkBuilder<V, E> {
    pub fn bound_inputs(&self) -> impl Iterator<Item = Arc<OutPortBuilder<V, E>>> + '_ {
        self.bound_variables.iter().cloned()
    }

    pub fn free_inputs(&self) -> impl Iterator<Item = Arc<OutPortBuilder<V, E>>> + '_ {
        self.free_variable_inputs
            .right_values()
            .map(|ByThinAddress(outport)| outport)
            .cloned()
    }

    pub fn inputs(&self) -> impl Iterator<Item = Arc<InPortBuilder<V, E>>> + '_ {
        self.free_variable_inputs
            .left_values()
            .cloned()
            .map(|x| x.0)
    }

    pub fn outputs(&self) -> impl Iterator<Item = Arc<OutPortBuilder<V, E>>> + '_ {
        self.outputs.right_values().cloned().map(|x| x.0)
    }
}

// impl<V, E> NodeBuilder<V, E> for ThunkBuilder<V, E>
// where
//     V: Debug + Send + Sync,
//     E: Debug + Send + Sync,

// }
