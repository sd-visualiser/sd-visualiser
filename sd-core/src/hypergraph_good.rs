use std::hash::Hash;
use std::sync::RwLock;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    sync::{Arc, Weak},
};

use bimap::BiMap;
use by_address::ByThinAddress;
use delegate::delegate;
use derivative::Derivative;

#[cfg(test)]
use serde::Serialize;
use thiserror::Error;

// use self::visitor::Visitor;
mod weakbyaddress;
use self::weakbyaddress::WeakByAddress;

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(Serialize))]
pub enum EdgeStrength {
    Strong,
    Weak,
}

#[derive(Debug, Error, Clone)]
pub enum HyperGraphError<V, E>
where
    V: Debug,
    E: Debug,
{
    #[error("Output port already linked to specified input: {0:#?}")]
    OutputLinkError(OutPort<V, E, false>),
}

type Result<T, V, E> = core::result::Result<T, HyperGraphError<V, E>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InPort<V, E, const BUILT: bool = true>(ByThinAddress<Arc<InPortInternal<V, E>>>);
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OutPort<V, E, const BUILT: bool = true>(ByThinAddress<Arc<OutPortInternal<V, E>>>);
#[derive(Debug, Clone)]
pub struct HyperGraph<V, E, const BUILT: bool = true>(HyperGraphInternal<V, E>);
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Operation<V, E, const BUILT: bool = true>(ByThinAddress<Arc<OperationInternal<V, E>>>);
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Thunk<V, E, const BUILT: bool = true>(ByThinAddress<Arc<ThunkInternal<V, E>>>);

#[derive(Debug, Clone)]
pub enum Node<V, E, const BUILT: bool = true> {
    Operation(Operation<V, E, BUILT>),
    Thunk(Thunk<V, E, BUILT>),
}

#[derive(Derivative, Debug)]
#[derivative(Clone(bound = ""))]
enum WeakNodeInternal<V, E> {
    Operation(Weak<OperationInternal<V, E>>),
    Thunk(Weak<ThunkInternal<V, E>>),
}

#[derive(Debug)]
struct InPortInternal<V, E> {
    node: Option<WeakNodeInternal<V, E>>,
    output: RwLock<Weak<OutPortInternal<V, E>>>,
}

impl<V, E> InPortInternal<V, E>
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
macro_rules! get_node {
    () => {
        pub fn node(&self) -> Option<Node<V, E>> {
            Some(match self.0.node.as_ref()? {
                WeakNodeInternal::Operation(weak_op) => Node::Operation(Operation(ByThinAddress(
                    weak_op
                        .upgrade()
                        .expect("got dangling reference to operation"),
                ))),
                WeakNodeInternal::Thunk(weak_thunk) => Node::Thunk(Thunk(ByThinAddress(
                    weak_thunk
                        .upgrade()
                        .expect("got dangling reference to thunk"),
                ))),
            })
        }
    };
}

impl<V, E> InPort<V, E> {
    get_node! {}

    pub fn output(&self) -> OutPort<V, E> {
        OutPort(ByThinAddress(
            self.0
                .output
                .try_read()
                .expect("Lock unexpectedly taken")
                .upgrade()
                .expect("got dangling reference to outport"),
        ))
    }
}

#[derive(Debug)]
struct OutPortInternal<V, E> {
    node: Option<WeakNodeInternal<V, E>>,
    inputs: RwLock<HashMap<WeakByAddress<InPortInternal<V, E>>, EdgeStrength>>,
    weight: E,
}

impl<V, E> OutPortInternal<V, E>
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
            .expect("Lock unexpectedly taken")
            .iter()
            .map(|(WeakByAddress(inport), &strength)| {
                let inport = inport.upgrade().expect("got dangling reference to inport");
                (InPort(ByThinAddress(inport)), strength)
            })
            .collect::<Vec<_>>()
            .into_iter()
    }

    pub fn weight(&self) -> &E {
        &self.0.weight
    }
}

#[derive(Debug, Clone)]
struct HyperGraphInternal<V, E> {
    operations: HashSet<ByThinAddress<Arc<OperationInternal<V, E>>>>,
    thunks: HashSet<ByThinAddress<Arc<ThunkInternal<V, E>>>>,
    graph_inputs: HashSet<ByThinAddress<Arc<OutPortInternal<V, E>>>>,
    graph_outputs: HashSet<ByThinAddress<Arc<InPortInternal<V, E>>>>,
}

impl<V, E> HyperGraph<V, E, false>
where
    V: Debug,
    E: Debug,
{
    pub fn new(input_weights: Vec<E>, number_of_outputs: usize) -> Self {
        let graph_inputs = input_weights
            .into_iter()
            .map(|weight| ByThinAddress(Arc::new(OutPortInternal::new_boundary(weight))))
            .collect();

        let graph_outputs = (0..number_of_outputs)
            .map(|_| ByThinAddress(Arc::new(InPortInternal::new_boundary())))
            .collect();

        HyperGraph(HyperGraphInternal {
            operations: Default::default(),
            thunks: Default::default(),
            graph_inputs,
            graph_outputs,
        })
    }

    pub fn build(self) -> Result<HyperGraph<V, E, true>, V, E> {
        // check validity of hypergraph:
        // all inports linked to exactly one outport
        // self.graph_outputs.iter().all(|ByThinAddress(inport)| {
        //     let output = inport.output.ro(&self.token);
        //     output.strong_count() > 0
        //     // check dependents
        // });
        // no strong cycles
        // todo!();

        Ok(HyperGraph(self.0))
    }
}

pub struct ThunkCursor<V, E>(Thunk<V, E, false>);

pub trait Fragment<V, E> {
    fn add_operation(
        &mut self,
        input_len: usize,
        output_weights: Vec<E>,
        weight: V,
    ) -> Operation<V, E, false>;

    fn add_thunk(
        &mut self,
        free_variables: Vec<E>,
        bound_variables: Vec<E>,
        output_weights: Vec<E>,
    ) -> Thunk<V, E, false>;

    fn link(
        &mut self,
        out_port: OutPort<V, E, false>,
        in_port: InPort<V, E, false>,
        strength: EdgeStrength,
    ) -> Result<(), V, E>
    where
        V: Debug,
        E: Debug,
    {
        let mut out = in_port
            .0
            .output
            .try_write()
            .expect("Lock unexpectedly taken");
        if let Some(existing) = out.upgrade() {
            return Err(HyperGraphError::OutputLinkError(OutPort(ByThinAddress(
                existing,
            ))));
        } else {
            *out = Arc::downgrade(&out_port.0);
        }
        out_port
            .0
            .inputs
            .try_write()
            .expect("Lock unexpectedly taken")
            .insert(WeakByAddress(Arc::downgrade(&in_port.0)), strength);
        Ok(())
    }

    fn in_thunk<T>(
        &mut self,
        thunk: Thunk<V, E, false>,
        f: impl FnOnce(ThunkCursor<V, E>) -> T,
    ) -> T
    where
        V: Debug,
        E: Debug,
    {
        f(ThunkCursor(thunk))
    }
}

impl<V, E> Fragment<V, E> for HyperGraph<V, E, false>
where
    V: Debug + Send + Sync,
    E: Debug + Send + Sync,
{
    fn add_operation(
        &mut self,
        input_len: usize,
        output_weights: Vec<E>,
        weight: V,
    ) -> Operation<V, E, false> {
        let op = ByThinAddress(OperationInternal::new(input_len, output_weights, weight));
        self.0.operations.insert(op.clone());
        Operation(op)
    }

    fn add_thunk(
        &mut self,
        free_variables: Vec<E>,
        bound_variables: Vec<E>,
        output_weights: Vec<E>,
    ) -> Thunk<V, E, false> {
        let thunk = ByThinAddress(ThunkInternal::new(
            free_variables,
            bound_variables,
            output_weights,
        ));
        self.0.thunks.insert(thunk.clone());
        Thunk(thunk)
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
    ) -> Operation<V, E, false> {
        let op = ByThinAddress(OperationInternal::new(input_len, output_weights, weight));
        self.0
             .0
            .operations
            .try_write()
            .expect("Lock unexpectedly taken")
            .insert(op.clone());
        Operation(op)
    }

    fn add_thunk(
        &mut self,
        free_variables: Vec<E>,
        bound_variables: Vec<E>,
        output_weights: Vec<E>,
    ) -> Thunk<V, E, false> {
        let thunk = ByThinAddress(ThunkInternal::new(
            free_variables,
            bound_variables,
            output_weights,
        ));
        self.0
             .0
            .thunks
            .try_write()
            .expect("Lock unexpectedly taken")
            .insert(thunk.clone());
        Thunk(thunk)
    }
}

pub trait Graph<V, E, const BUILT: bool> {
    fn graph_inputs(&self) -> Box<dyn Iterator<Item = OutPort<V, E, BUILT>> + '_>;
    fn graph_outputs(&self) -> Box<dyn Iterator<Item = InPort<V, E, BUILT>> + '_>;
}

pub trait GraphView<V, E>: Graph<V, E, true> {
    fn operations(&self) -> Box<dyn Iterator<Item = Operation<V, E>> + '_>;
    fn thunks(&self) -> Box<dyn Iterator<Item = Thunk<V, E>> + '_>;
    fn nodes<'a>(&'a self) -> Box<dyn Iterator<Item = Node<V, E>> + 'a>
    where
        V: 'a,
        E: 'a,
    {
        Box::new(
            self.thunks()
                .map(Node::Thunk)
                .chain(self.operations().map(Node::Operation)),
        )
    }
}

impl<V, E, const BUILT: bool> Graph<V, E, BUILT> for HyperGraph<V, E, BUILT> {
    fn graph_inputs(&self) -> Box<dyn Iterator<Item = OutPort<V, E, BUILT>> + '_> {
        Box::new(self.0.graph_inputs.iter().cloned().map(OutPort))
    }

    fn graph_outputs(&self) -> Box<dyn Iterator<Item = InPort<V, E, BUILT>> + '_> {
        Box::new(self.0.graph_outputs.iter().cloned().map(InPort))
    }
}

impl<V, E> GraphView<V, E> for HyperGraph<V, E> {
    fn operations(&self) -> Box<dyn Iterator<Item = Operation<V, E>> + '_> {
        Box::new(self.0.operations.iter().cloned().map(Operation))
    }

    fn thunks(&self) -> Box<dyn Iterator<Item = Thunk<V, E>> + '_> {
        Box::new(self.0.thunks.iter().cloned().map(Thunk))
    }
}

impl<V, E, const BUILT: bool> Graph<V, E, BUILT> for Thunk<V, E, BUILT> {
    fn graph_inputs(&self) -> Box<dyn Iterator<Item = OutPort<V, E, BUILT>> + '_> {
        Box::new(self.free_inputs().chain(self.bound_inputs()))
    }

    fn graph_outputs(&self) -> Box<dyn Iterator<Item = InPort<V, E, BUILT>> + '_> {
        Box::new(self.0.outputs.left_values().cloned().map(InPort))
    }
}

impl<V, E> GraphView<V, E> for Thunk<V, E> {
    fn operations(&self) -> Box<dyn Iterator<Item = Operation<V, E>> + '_> {
        Box::new(
            self.0
                .operations
                .read()
                .expect("Lock unexpectedly taken")
                .iter()
                .cloned()
                .map(Operation)
                .collect::<Vec<_>>()
                .into_iter(),
        )
    }

    fn thunks(&self) -> Box<dyn Iterator<Item = Thunk<V, E>> + '_> {
        Box::new(
            self.0
                .thunks
                .read()
                .expect("Lock unexpectedly taken")
                .iter()
                .cloned()
                .map(Thunk)
                .collect::<Vec<_>>()
                .into_iter(),
        )
    }
}

impl<V, E> Graph<V, E, false> for ThunkCursor<V, E>
where
    V: 'static,
    E: 'static,
{
    delegate! {
        to self.0 {
            fn graph_inputs(&self) -> Box<dyn Iterator<Item = OutPort<V, E, false>> + '_>;
            fn graph_outputs(&self) -> Box<dyn Iterator<Item = InPort<V, E, false>> + '_>;
        }
    }
}

#[derive(Debug)]
struct OperationInternal<V, E> {
    weight: V,
    inputs: Vec<Arc<InPortInternal<V, E>>>,
    outputs: Vec<Arc<OutPortInternal<V, E>>>,
}

impl<V, E, const BUILT: bool> Operation<V, E, BUILT> {
    pub fn inputs(&self) -> impl Iterator<Item = InPort<V, E, BUILT>> + '_ {
        self.0
            .inputs
            .iter()
            .cloned()
            .map(|i| InPort(ByThinAddress(i)))
    }

    pub fn outputs(&self) -> impl Iterator<Item = OutPort<V, E, BUILT>> + '_ {
        self.0
            .outputs
            .iter()
            .cloned()
            .map(|o| OutPort(ByThinAddress(o)))
    }

    pub fn number_of_inputs(&self) -> usize {
        self.0.inputs.len()
    }

    pub fn number_of_outputs(&self) -> usize {
        self.0.outputs.len()
    }

    pub fn weight(&self) -> &V {
        &self.0.weight
    }
}

impl<V, E> OperationInternal<V, E>
where
    V: Debug + Send + Sync,
    E: Debug + Send + Sync,
{
    fn new(input_len: usize, output_weights: Vec<E>, weight: V) -> Arc<Self> {
        Arc::new_cyclic(|weak: &Weak<Self>| {
            let inputs = (0..input_len)
                .map(|_| {
                    Arc::new(InPortInternal {
                        node: Some(WeakNodeInternal::Operation(weak.clone())),
                        output: RwLock::new(Weak::new()),
                    })
                })
                .collect();
            let outputs = output_weights
                .into_iter()
                .map(|weight| {
                    Arc::new(OutPortInternal {
                        node: Some(WeakNodeInternal::Operation(weak.clone())),
                        inputs: RwLock::new(Default::default()),
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
struct ThunkInternal<V, E> {
    #[allow(clippy::type_complexity)]
    operations: RwLock<HashSet<ByThinAddress<Arc<OperationInternal<V, E>>>>>,
    #[allow(clippy::type_complexity)]
    thunks: RwLock<HashSet<ByThinAddress<Arc<ThunkInternal<V, E>>>>>,
    free_variable_inputs: InOutMap<V, E>,
    bound_variables: Vec<Arc<OutPortInternal<V, E>>>,
    outputs: InOutMap<V, E>,
}

impl<V, E, const BUILT: bool> Thunk<V, E, BUILT> {
    pub fn bound_inputs(&self) -> impl Iterator<Item = OutPort<V, E, BUILT>> + '_ {
        self.0
            .bound_variables
            .iter()
            .cloned()
            .map(|o| OutPort(ByThinAddress(o)))
    }

    pub fn free_inputs(&self) -> impl Iterator<Item = OutPort<V, E, BUILT>> + '_ {
        self.0
            .free_variable_inputs
            .right_values()
            .cloned()
            .map(OutPort)
    }

    pub fn inputs(&self) -> impl Iterator<Item = InPort<V, E, BUILT>> + '_ {
        self.0
            .free_variable_inputs
            .left_values()
            .cloned()
            .map(InPort)
    }

    pub fn outputs(&self) -> impl Iterator<Item = OutPort<V, E, BUILT>> + '_ {
        self.0.outputs.right_values().cloned().map(OutPort)
    }

    pub fn number_of_inputs(&self) -> usize {
        self.0.free_variable_inputs.len()
    }

    pub fn number_of_outputs(&self) -> usize {
        self.0.outputs.len()
    }
}

impl<V, E> ThunkInternal<V, E>
where
    V: Debug + Send + Sync,
    E: Debug + Send + Sync,
{
    fn new(free_variables: Vec<E>, bound_variables: Vec<E>, output_weights: Vec<E>) -> Arc<Self> {
        Arc::new_cyclic(|weak: &Weak<Self>| {
            let free_variable_inputs = free_variables
                .into_iter()
                .map(|fv| {
                    let inner_output = Arc::new(OutPortInternal::new_boundary(fv));
                    let outer_input = Arc::new(InPortInternal {
                        node: Some(WeakNodeInternal::Thunk(weak.clone())),
                        output: RwLock::new(Weak::new()),
                    });
                    (ByThinAddress(outer_input), ByThinAddress(inner_output))
                })
                .collect();
            let bound_variables = bound_variables
                .into_iter()
                .map(|bv| Arc::new(OutPortInternal::new_boundary(bv)))
                .collect();
            let outputs = output_weights
                .into_iter()
                .map(|weight| {
                    let inner_input = Arc::new(InPortInternal::new_boundary());
                    let outer_output = Arc::new(OutPortInternal {
                        node: Some(WeakNodeInternal::Thunk(weak.clone())),
                        inputs: RwLock::new(Default::default()),
                        weight,
                    });
                    (ByThinAddress(inner_input), ByThinAddress(outer_output))
                })
                .collect();
            ThunkInternal {
                operations: RwLock::new(Default::default()),
                thunks: RwLock::new(Default::default()),
                free_variable_inputs,
                bound_variables,
                outputs,
            }
        })
    }
}

impl<V, E, const BUILT: bool> Node<V, E, BUILT> {
    pub fn inputs(&self) -> Box<dyn Iterator<Item = InPort<V, E, BUILT>> + '_> {
        match self {
            Node::Operation(op) => Box::new(op.inputs()),
            Node::Thunk(thunk) => Box::new(thunk.inputs()),
        }
    }

    pub fn outputs(&self) -> Box<dyn Iterator<Item = OutPort<V, E, BUILT>> + '_> {
        match self {
            Node::Operation(op) => Box::new(op.outputs()),
            Node::Thunk(thunk) => Box::new(thunk.outputs()),
        }
    }
}
