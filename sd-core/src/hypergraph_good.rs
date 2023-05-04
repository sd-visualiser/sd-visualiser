use std::hash::Hash;
use std::ops::{Deref, DerefMut};
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    rc::{Rc, Weak},
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

// macro_rules! impl_partialeq_eq_hash_by_ptr {
//     (& $t:ident < $($l:lifetime),+ , $($g:ident),+ >) => {
//         impl<$($l),+ , $($g),+> PartialEq for &$t < $($l),+ , $($g),+ > {
//             fn eq(&self, other: &Self) -> bool {
//                 std::ptr::eq(*self, *other)
//             }
//         }
//
//         impl < $($l),+ , $($g),+ > Eq for &$t < $($l),+ , $($g),+ > {}
//
//         impl < $($l),+ , $($g),+ > std::hash::Hash for &$t < $($l),+ , $($g),+ > {
//             fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//                 std::ptr::hash(*self, state)
//             }
//         }
//     };
// }

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(Serialize))]
pub enum EdgeStrength {
    Strong,
    Weak,
}

#[derive(Clone)]
pub struct Owned<'a, T: ?Sized> {
    inner: Rc<T>,
    owner: &'a QCellOwner,
}

pub struct OwnedMut<'a, T: ?Sized> {
    inner: Rc<T>,
    owner: &'a mut QCellOwner,
}

type InPort<'hyper, V, E> = Owned<'hyper, InPortBuilder<V, E>>;
type OutPort<'hyper, V, E> = Owned<'hyper, OutPortBuilder<V, E>>;

#[derive(Debug, Error, Clone)]
pub enum HyperGraphError<V, E>
where
    V: Debug,
    E: Debug,
{
    #[error("No input port at index `{0:#?}`")]
    UnknownInput(Rc<InPortBuilder<V, E>>),
    #[error("No output port at index `{0:#?}`")]
    UnknownOutput(Rc<OutPortBuilder<V, E>>),
    #[error("Input port not linked: {0:#?}")]
    UnlinkedInput(Rc<InPortBuilder<V, E>>),
    #[error("Unlinked port")]
    UnlinkedPort,
    #[error("Input port already linked: {0:#?}")]
    InputLinkError(Rc<InPortBuilder<V, E>>),
    #[error("Output port already linked to specified input: {0:#?}")]
    OutputLinkError(Rc<OutPortBuilder<V, E>>),
}

type Result<T, V, E> = core::result::Result<T, HyperGraphError<V, E>>;

#[derive(Derivative, Getters)]
#[derivative(Debug)]
pub struct InPortBuilder<V, E> {
    node: Option<Weak<dyn NodeBuilder<V, E>>>,
    #[derivative(Debug = "ignore")]
    output: QCell<Weak<OutPortBuilder<V, E>>>,
}

// impl_partialeq_eq_hash_by_ptr!(&InputPort<'hyper, V, E>);

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

macro_rules! get_node {
    () => {
        pub fn node(&self) -> Option<Node<V, E>> {
            self.inner.node.clone().map(|weak| Owned {
                inner: weak.upgrade().expect("got dangling reference to node"),
                owner: self.owner,
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
    node: Option<Weak<dyn NodeBuilder<V, E>>>,
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
    nodes: HashSet<ByThinAddress<Rc<dyn NodeBuilder<V, E>>>>,
    graph_inputs: HashSet<ByThinAddress<Rc<OutPortBuilder<V, E>>>>,
    graph_outputs: HashSet<ByThinAddress<Rc<InPortBuilder<V, E>>>>,
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
    pub fn build(self) -> Result<HyperGraph<V, E>, V, E> {
        // check validity of hypergraph:
        // no unlinked ports
        todo!();
        // no strong cycles
        todo!();

        Ok(HyperGraph(self))
    }
}

impl<V, E> HyperGraph<V, E>
where
    V: Debug,
    E: Debug,
{
    pub fn nodes(&self) -> impl Iterator<Item = Node<V, E>> {
        self.0.nodes.iter().cloned().map(|node| Owned {
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

pub trait Fragment<V, E>: owner::HasOwner {
    fn add_operation(
        &mut self,
        input_len: usize,
        output_weights: Vec<E>,
        weight: V,
    ) -> Rc<Operation<V, E>>;

    fn add_thunk(
        &mut self,
        free_variables: Vec<E>,
        bound_variables: Vec<E>,
        output_weights: Vec<E>,
    ) -> Rc<ThunkBuilder<V, E>>;

    fn link(
        &mut self,
        input: Rc<InPortBuilder<V, E>>,
        output: Rc<OutPortBuilder<V, E>>,
        strength: EdgeStrength,
    ) -> Result<(), V, E>
    where
        V: Debug,
        E: Debug,
    {
        let out = input.output.rw(self.owner());
        if let Some(existing) = out.upgrade() {
            return Err(HyperGraphError::OutputLinkError(existing));
        } else {
            *out = Rc::downgrade(&output);
        }
        output
            .inputs
            .rw(self.owner())
            .insert(WeakByAddress(Rc::downgrade(&input)), strength);
        Ok(())
    }
}

impl<V, E> Fragment<V, E> for HyperGraphBuilder<V, E>
where
    V: Debug,
    E: Debug,
{
    fn add_operation(
        &mut self,
        input_len: usize,
        output_weights: Vec<E>,
        weight: V,
    ) -> Rc<Operation<V, E>> {
        let op = Operation::new(self.owner(), input_len, output_weights, weight);
        self.nodes.insert(ByThinAddress(op.clone()));
        op
    }

    fn add_thunk(
        &mut self,
        free_variables: Vec<E>,
        bound_variables: Vec<E>,
        output_weights: Vec<E>,
    ) -> Rc<ThunkBuilder<V, E>> {
        let thunk = ThunkBuilder::new(
            self.owner(),
            free_variables,
            bound_variables,
            output_weights,
        );
        self.nodes.insert(ByThinAddress(thunk.clone()));
        thunk
    }
}

impl<V, E> Fragment<V, E> for OwnedMut<'_, ThunkBuilder<V, E>>
where
    V: Debug + 'static,
    E: Debug + 'static,
{
    fn add_operation(
        &mut self,
        input_len: usize,
        output_weights: Vec<E>,
        weight: V,
    ) -> Rc<Operation<V, E>> {
        let op = Operation::new(self.owner(), input_len, output_weights, weight);
        let nodes = self.inner.nodes.rw(self.owner);
        nodes.insert(ByThinAddress(op.clone()));
        op
    }

    fn add_thunk(
        &mut self,
        free_variables: Vec<E>,
        bound_variables: Vec<E>,
        output_weights: Vec<E>,
    ) -> Rc<ThunkBuilder<V, E>> {
        let thunk = ThunkBuilder::new(
            self.owner(),
            free_variables,
            bound_variables,
            output_weights,
        );
        let nodes = self.inner.nodes.rw(self.owner);
        nodes.insert(ByThinAddress(thunk.clone()));
        thunk
    }
}

pub trait NodeBuilder<V, E>: Debug {
    fn inputs(&self) -> Box<dyn Iterator<Item = Rc<InPortBuilder<V, E>>> + '_>;
    fn outputs(&self) -> Box<dyn Iterator<Item = Rc<OutPortBuilder<V, E>>> + '_>;
}

pub type Node<'hyper, V, E> = Owned<'hyper, dyn NodeBuilder<V, E>>;

impl<V, E> Node<'_, V, E> {
    pub fn inputs(&self) -> impl Iterator<Item = InPort<V, E>> + '_ {
        self.inner.inputs().map(|inner| Owned {
            inner,
            owner: self.owner,
        })
    }

    pub fn outputs(&self) -> impl Iterator<Item = OutPort<V, E>> + '_ {
        self.inner.outputs().map(|inner| Owned {
            inner,
            owner: self.owner,
        })
    }
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Operation<V, E> {
    weight: V,
    inputs: Vec<Rc<InPortBuilder<V, E>>>,
    outputs: Vec<Rc<OutPortBuilder<V, E>>>,
}

impl<V, E> Operation<V, E>
where
    V: 'static,
    E: 'static,
{
    fn new(owner: &QCellOwner, input_len: usize, output_weights: Vec<E>, weight: V) -> Rc<Self>
    where
        V: Debug,
        E: Debug,
    {
        Rc::new_cyclic(|weak: &Weak<Self>| {
            let inputs = (0..input_len)
                .map(|_| {
                    Rc::new(InPortBuilder {
                        node: Some(weak.clone()),
                        output: QCell::new(owner, Weak::new()),
                    })
                })
                .collect();
            let outputs = output_weights
                .into_iter()
                .map(|weight| {
                    Rc::new(OutPortBuilder {
                        node: Some(weak.clone()),
                        inputs: QCell::new(owner, Default::default()),
                        weight,
                    })
                })
                .collect();
            Operation {
                weight,
                inputs,
                outputs,
            }
        })
    }
}

impl<V, E> NodeBuilder<V, E> for Operation<V, E>
where
    V: Debug,
    E: Debug,
{
    fn inputs(&self) -> Box<dyn Iterator<Item = Rc<InPortBuilder<V, E>>> + '_> {
        Box::new(self.inputs.iter().cloned())
    }

    fn outputs(&self) -> Box<dyn Iterator<Item = Rc<OutPortBuilder<V, E>>> + '_> {
        Box::new(self.outputs.iter().cloned())
    }
}

type InOutMap<V, E> =
    BiMap<ByThinAddress<Rc<InPortBuilder<V, E>>>, ByThinAddress<Rc<OutPortBuilder<V, E>>>>;

#[derive(Derivative)]
#[derivative(Debug)]
pub struct ThunkBuilder<V, E> {
    #[allow(clippy::type_complexity)]
    #[derivative(Debug = "ignore")]
    nodes: QCell<HashSet<ByThinAddress<Rc<dyn NodeBuilder<V, E>>>>>,
    free_variable_inputs: InOutMap<V, E>,
    bound_variables: Vec<Rc<OutPortBuilder<V, E>>>,
    outputs: InOutMap<V, E>,
}

impl<V, E> ThunkBuilder<V, E>
where
    V: Debug + 'static,
    E: Debug + 'static,
{
    fn new(
        owner: &QCellOwner,
        free_variables: Vec<E>,
        bound_variables: Vec<E>,
        output_weights: Vec<E>,
    ) -> Rc<Self> {
        Rc::new_cyclic(|weak: &Weak<Self>| {
            let free_variable_inputs = free_variables
                .into_iter()
                .map(|fv| {
                    let inner_output = Rc::new(OutPortBuilder::new_boundary(owner, fv));
                    let outer_input = Rc::new(InPortBuilder {
                        node: Some(weak.clone()),
                        output: QCell::new(owner, Weak::new()),
                    });
                    (ByThinAddress(outer_input), ByThinAddress(inner_output))
                })
                .collect();
            let bound_variables = bound_variables
                .into_iter()
                .map(|bv| Rc::new(OutPortBuilder::new_boundary(owner, bv)))
                .collect();
            let outputs = output_weights
                .into_iter()
                .map(|weight| {
                    let inner_input = Rc::new(InPortBuilder::new_boundary(owner));
                    let outer_output = Rc::new(OutPortBuilder {
                        node: Some(weak.clone()),
                        inputs: QCell::new(owner, Default::default()),
                        weight,
                    });
                    (ByThinAddress(inner_input), ByThinAddress(outer_output))
                })
                .collect();
            ThunkBuilder {
                nodes: QCell::new(owner, Default::default()),
                free_variable_inputs,
                bound_variables,
                outputs,
            }
        })
    }
}

impl<V, E> ThunkBuilder<V, E> {
    pub fn bound_inputs(&self) -> impl Iterator<Item = Rc<OutPortBuilder<V, E>>> + '_ {
        self.bound_variables.iter().cloned()
    }

    pub fn free_inputs(&self) -> impl Iterator<Item = Rc<OutPortBuilder<V, E>>> + '_ {
        self.free_variable_inputs
            .right_values()
            .map(|ByThinAddress(outport)| outport)
            .cloned()
    }

    pub fn graph_outputs(&self) -> impl Iterator<Item = Rc<InPortBuilder<V, E>>> + '_ {
        self.outputs
            .left_values()
            .map(|ByThinAddress(inport)| inport)
            .cloned()
    }
}

impl<V, E> NodeBuilder<V, E> for ThunkBuilder<V, E>
where
    V: Debug,
    E: Debug,
{
    fn inputs(&self) -> Box<dyn Iterator<Item = Rc<InPortBuilder<V, E>>> + '_> {
        Box::new(
            self.free_variable_inputs
                .left_values()
                .cloned()
                .map(|x| x.0),
        )
    }

    fn outputs(&self) -> Box<dyn Iterator<Item = Rc<OutPortBuilder<V, E>>> + '_> {
        Box::new(self.outputs.right_values().cloned().map(|x| x.0))
    }
}
