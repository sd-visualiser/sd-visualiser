use std::hash::Hash;
use std::ops::{Deref, DerefMut};
use std::sync::RwLock;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    sync::{Arc, Weak},
};

use bimap::BiMap;
use by_address::ByThinAddress;
use derivative::Derivative;
use getset::Getters;

#[cfg(test)]
use serde::Serialize;
use thiserror::Error;

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(Serialize))]
pub enum EdgeStrength {
    Strong,
    Weak,
}

pub struct InPort<V, E>(Arc<InPortBuilder<V, E>>);
pub struct OutPort<V, E>(Arc<OutPortBuilder<V, E>>);
pub struct Operation<V, E>(Arc<OperationBuilder<V, E>>);
pub struct Thunk<V, E>(Arc<ThunkBuilder<V, E>>);

#[derive(Debug, Error, Clone)]
pub enum HyperGraphError<V, E>
where
    V: Debug,
    E: Debug,
{
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
    output: RwLock<Weak<OutPortBuilder<V, E>>>,
}

impl<V, E> InPortBuilder<V, E>
where
    V: Debug,
    E: Debug,
{
    fn new_boundary() -> Self {
        Self {
            node: None,
            output: RwLock::new(Default::default()),
        }
    }
}

pub enum Node<V, E> {
    Operation(Operation<V, E>),
    Thunk(Thunk<V, E>),
}

macro_rules! get_node {
    () => {
        pub fn node(&self) -> Option<Node<V, E>> {
            Some(match self.0.node.as_ref()? {
                WeakNodeBuilder::Operation(weak_op) => Node::Operation(Operation(weak_op
                        .upgrade()
                        .expect("got dangling reference to operation"))),
                WeakNodeBuilder::Thunk(weak_thunk) => Node::Thunk(Thunk(
                    weak_thunk
                        .upgrade()
                        .expect("got dangling reference to thunk"))),
            })
        }
    };
}

impl<V, E> InPort<V, E> {
    get_node! {}

    pub fn output(&self) -> OutPort<V, E> {
        OutPort(self.0.output.try_read().expect("Lock unexpectedly taken").upgrade()
                .expect("got dangling reference to outport"))
    }
}

#[derive(Derivative, Getters)]
#[derivative(Debug)]
pub struct OutPortBuilder<V, E> {
    node: Option<WeakNodeBuilder<V, E>>,
    #[derivative(Debug = "ignore")]
    inputs: RwLock<HashMap<WeakByAddress<InPortBuilder<V, E>>, EdgeStrength>>,
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

impl<V, E> OutPortBuilder<V, E>
where
    V: Debug,
    E: Debug,
{
    fn new_boundary(weight: E) -> Self {
        Self {
            node: None,
            inputs: RwLock::new(Default::default()),
            weight,
        }
    }
}

impl<V, E> OutPort<V, E> {
    get_node! {}

    pub fn inputs(&self) -> impl Iterator<Item = (InPort<V, E>, EdgeStrength)> {
        self.0
            .inputs
            .try_read()
            .expect("Lock unexpectly taken")
            .iter()
            .map(|(WeakByAddress(inport), &strength)| {
                let inport = inport.upgrade().expect("got dangling reference to inport");
                (InPort(inport.clone()),strength)
            }).collect::<Vec<_>>().into_iter()
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
        let graph_inputs = input_weights
            .into_iter()
            .map(|weight| ByThinAddress(Arc::new(OutPortBuilder::new_boundary(weight))))
            .collect();

        let graph_outputs = (0..number_of_outputs)
            .into_iter()
            .map(|_| ByThinAddress(Arc::new(InPortBuilder::new_boundary())))
            .collect();

        HyperGraphBuilder {
            operations: Default::default(),
            thunks: Default::default(),
            graph_inputs,
            graph_outputs,
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
    pub fn operations(&self) -> impl Iterator<Item = Operation<V, E>> + '_ {
        self.0.operations.iter().cloned().map(|ByThinAddress(op)| Operation(op))
    }

    pub fn thunks(&self) -> impl Iterator<Item = Thunk<V, E>> + '_ {
        self.0.thunks.iter().cloned().map(|ByThinAddress(thunk)| Thunk(thunk))
    }

    pub fn graph_inputs(&self) -> impl Iterator<Item = OutPort<V, E>> + '_ {
        self.0
            .graph_inputs
            .iter()
            .cloned()
            .map(|ByThinAddress(out_port)| OutPort(out_port))
    }

    pub fn graph_outputs(&self) -> impl Iterator<Item = InPort<V, E>> + '_ {
        self.0
            .graph_outputs
            .iter()
            .cloned()
            .map(|ByThinAddress(in_port)| InPort(in_port))
    }
}

pub struct ThunkCursor<V, E>(Arc<ThunkBuilder<V, E>>);

pub trait Fragment<V: 'static, E: 'static> {
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
        let mut out = in_port.output.try_write().expect("Lock unexpectedly taken");
        if let Some(existing) = out.upgrade() {
            return Err(HyperGraphError::OutputLinkError(existing));
        } else {
            *out = Arc::downgrade(&out_port);
        }
        out_port
            .inputs
            .try_write()
            .expect("Lock unexpectedly taken")
            .insert(WeakByAddress(Arc::downgrade(&in_port)), strength);
        Ok(())
    }

    fn in_thunk<T>(
        &mut self,
        thunk: Arc<ThunkBuilder<V, E>>,
        f: impl FnOnce(ThunkCursor<V, E>) -> T,
    ) -> T
    where
        V: Debug,
        E: Debug,
    {
        f(ThunkCursor(thunk))
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
        let op = OperationBuilder::new(input_len, output_weights, weight);
        self.operations.insert(ByThinAddress(op.clone()));
        op
    }

    fn add_thunk(
        &mut self,
        free_variables: Vec<E>,
        bound_variables: Vec<E>,
        output_weights: Vec<E>,
    ) -> Arc<ThunkBuilder<V, E>> {
        let thunk = ThunkBuilder::new(free_variables, bound_variables, output_weights);
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

impl<V, E> Fragment<V, E> for ThunkCursor<V, E>
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
        let op = OperationBuilder::new(input_len, output_weights, weight);
        self.0
            .operations
            .try_write()
            .expect("Lock unexpectedly taken")
            .insert(ByThinAddress(op.clone()));
        op
    }

    fn add_thunk(
        &mut self,
        free_variables: Vec<E>,
        bound_variables: Vec<E>,
        output_weights: Vec<E>,
    ) -> Arc<ThunkBuilder<V, E>> {
        let thunk = ThunkBuilder::new(free_variables, bound_variables, output_weights);
        self.0
            .thunks
            .try_write()
            .expect("Lock unexpectedly taken")
            .insert(ByThinAddress(thunk.clone()));
        thunk
    }

    fn graph_inputs(&self) -> Box<dyn Iterator<Item = Arc<OutPortBuilder<V, E>>> + '_> {
        Box::new(self.0.free_inputs().chain(self.0.bound_inputs()))
    }

    fn graph_outputs(&self) -> Box<dyn Iterator<Item = Arc<InPortBuilder<V, E>>> + '_> {
        Box::new(
            self.0
                .outputs
                .left_values()
                .map(|ByThinAddress(inport)| inport.clone()),
        )
    }
}

// impl<V, E> Node<V, E> {
//     pub fn inputs(&self) -> impl Iterator<Item = InPort<V, E>> + '_ {
// 	match self {
// 	    Node::Operation(op) => op,
// 	    Node::Thunk(thunk) => todo!(),
// }
//         // self.inner.inputs().map(|inner| Owned {
//         //     inner,
//         //     owner: self.owner,
//         // })
//     }

//     // pub fn outputs(&self) -> impl Iterator<Item = OutPort<V, E>> + '_ {
//     //     self.inner.outputs().map(|inner| Owned {
//     //         inner,
//     //         owner: self.owner,
//     //     })
//     // }
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
    fn new(input_len: usize, output_weights: Vec<E>, weight: V) -> Arc<Self> {
        Arc::new_cyclic(|weak: &Weak<Self>| {
            let inputs = (0..input_len)
                .map(|_| {
                    Arc::new(InPortBuilder {
                        node: Some(WeakNodeBuilder::Operation(weak.clone())),
                        output: RwLock::new(Weak::new()),
                    })
                })
                .collect();
            let outputs = output_weights
                .into_iter()
                .map(|weight| {
                    Arc::new(OutPortBuilder {
                        node: Some(WeakNodeBuilder::Operation(weak.clone())),
                        inputs: RwLock::new(Default::default()),
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
    operations: RwLock<HashSet<ByThinAddress<Arc<OperationBuilder<V, E>>>>>,
    #[derivative(Debug = "ignore")]
    thunks: RwLock<HashSet<ByThinAddress<Arc<ThunkBuilder<V, E>>>>>,
    free_variable_inputs: InOutMap<V, E>,
    bound_variables: Vec<Arc<OutPortBuilder<V, E>>>,
    outputs: InOutMap<V, E>,
}

impl<V, E> ThunkBuilder<V, E>
where
    V: Debug + Send + Sync + 'static,
    E: Debug + Send + Sync + 'static,
{
    fn new(free_variables: Vec<E>, bound_variables: Vec<E>, output_weights: Vec<E>) -> Arc<Self> {
        Arc::new_cyclic(|weak: &Weak<Self>| {
            let free_variable_inputs = free_variables
                .into_iter()
                .map(|fv| {
                    let inner_output = Arc::new(OutPortBuilder::new_boundary(fv));
                    let outer_input = Arc::new(InPortBuilder {
                        node: Some(WeakNodeBuilder::Thunk(weak.clone())),
                        output: RwLock::new(Weak::new()),
                    });
                    (ByThinAddress(outer_input), ByThinAddress(inner_output))
                })
                .collect();
            let bound_variables = bound_variables
                .into_iter()
                .map(|bv| Arc::new(OutPortBuilder::new_boundary(bv)))
                .collect();
            let outputs = output_weights
                .into_iter()
                .map(|weight| {
                    let inner_input = Arc::new(InPortBuilder::new_boundary());
                    let outer_output = Arc::new(OutPortBuilder {
                        node: Some(WeakNodeBuilder::Thunk(weak.clone())),
                        inputs: RwLock::new(Default::default()),
                        weight,
                    });
                    (ByThinAddress(inner_input), ByThinAddress(outer_output))
                })
                .collect();
            ThunkBuilder {
                operations: RwLock::new(Default::default()),
                thunks: RwLock::new(Default::default()),
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
