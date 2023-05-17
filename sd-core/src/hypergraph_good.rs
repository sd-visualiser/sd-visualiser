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
    #[error("Building hypergraph failed: {0:#?}")]
    BuildError(HyperGraphBuildError<V, E>),
}

#[derive(Debug, Error, Clone)]
pub enum HyperGraphBuildError<V, E>
where
    V: Debug,
    E: Debug,
{
    #[error("InPort has uninitialised OutPort: {0:#?}")]
    UninitializedInPort(InPort<V, E, false>),
    #[error("OutPort has uninitialised InPort: {0:#?}")]
    UninitializedOutPort(OutPort<V, E, false>),
    #[error("Strong cycle of linked ports detected")]
    StrongCycle,
}

type Result<T, V, E> = core::result::Result<T, HyperGraphError<V, E>>;

#[derive(Debug, Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct InPort<V, E, const BUILT: bool = true>(ByThinAddress<Arc<InPortInternal<V, E>>>);
#[derive(Debug, Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct OutPort<V, E, const BUILT: bool = true>(ByThinAddress<Arc<OutPortInternal<V, E>>>);
#[derive(Debug, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct HyperGraph<V, E, const BUILT: bool = true>(HyperGraphInternal<V, E>);
#[derive(Debug, Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct Operation<V, E, const BUILT: bool = true>(ByThinAddress<Arc<OperationInternal<V, E>>>);
#[derive(Debug, Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct Thunk<V, E, const BUILT: bool = true>(ByThinAddress<Arc<ThunkInternal<V, E>>>);

#[derive(Derivative, Debug)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
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

impl<V, E> WeakNodeInternal<V, E> {
    fn upgrade(&self) -> Option<NodeInternal<V, E>> {
        match self {
            WeakNodeInternal::Operation(weak_operation) => weak_operation
                .upgrade()
                .map(|operation| NodeInternal::Operation(operation)),
            WeakNodeInternal::Thunk(weak_thunk) => {
                weak_thunk.upgrade().map(|thunk| NodeInternal::Thunk(thunk))
            }
        }
    }
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
    ($port:ident) => {
        impl<V, E, const BUILT: bool> $port<V, E, BUILT> {
            pub fn node(&self) -> Option<Node<V, E, BUILT>> {
                Some(match self.0.node.as_ref()? {
                    WeakNodeInternal::Operation(weak_op) => {
                        Node::Operation(Operation(ByThinAddress(
                            weak_op
                                .upgrade()
                                .expect("got dangling reference to operation"),
                        )))
                    }
                    WeakNodeInternal::Thunk(weak_thunk) => Node::Thunk(Thunk(ByThinAddress(
                        weak_thunk
                            .upgrade()
                            .expect("got dangling reference to thunk"),
                    ))),
                })
            }
        }
    };
}

get_node!(InPort);
get_node!(OutPort);

impl<V, E> InPort<V, E> {
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

    pub fn number_of_inputs(&self) -> usize {
        self.0
            .inputs
            .try_read()
            .expect("Lock unexpectedly taken")
            .len()
    }

    pub fn weight(&self) -> &E {
        &self.0.weight
    }
}

#[derive(Debug, Derivative)]
#[derivative(Clone(bound = ""))]
struct HyperGraphInternal<V, E> {
    nodes: Vec<NodeInternal<V, E>>,
    graph_inputs: HashSet<ByThinAddress<Arc<OutPortInternal<V, E>>>>,
    graph_outputs: Vec<Arc<InPortInternal<V, E>>>,
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
            .map(|_| Arc::new(InPortInternal::new_boundary()))
            .collect();

        HyperGraph(HyperGraphInternal {
            nodes: Default::default(),
            graph_inputs,
            graph_outputs,
        })
    }

    pub fn build(self) -> Result<HyperGraph<V, E, true>, V, E> {
        // check validity of hypergraph:
        // all inports linked to exactly one outport
        {
            fn check_inports_initialized<V, E>(
                outport: OutPort<V, E, false>,
            ) -> std::result::Result<(), HyperGraphBuildError<V, E>>
            where
                V: Debug,
                E: Debug,
            {
                outport
                    .0
                    .inputs
                    .try_read()
                    .expect("failed to lock outport inputs {outport.0.inputs:#?}")
                    .iter()
                    .all(|(weak_inport, _strength)| weak_inport.strong_count() > 0)
                    .then_some(())
                    .ok_or_else(|| HyperGraphBuildError::UninitializedOutPort(outport.clone()))
            }

            fn check_outport_initialized<V, E>(
                inport: InPort<V, E, false>,
            ) -> std::result::Result<(), HyperGraphBuildError<V, E>>
            where
                V: Debug,
                E: Debug,
            {
                (inport
                    .0
                    .output
                    .try_read()
                    .expect("failed to lock inport output {inport.0.output:#?}")
                    .strong_count()
                    > 0)
                .then_some(())
                .ok_or_else(|| HyperGraphBuildError::UninitializedInPort(inport.clone()))
            }

            for outport in self.graph_inputs() {
                // check associated with hypergraph
                assert!(&outport.0.node.is_none());
                // check inputs initialised
                check_inports_initialized(outport).map_err(HyperGraphError::BuildError)?;
            }

            for inport in self.graph_outputs() {
                // check associated with hypergraph
                assert!(&inport.0.node.is_none());
                // check output initialised
                check_outport_initialized(inport).map_err(HyperGraphError::BuildError)?;
            }
            for node in self.nodes() {
                node.fold(&mut (), |no_state, n| {
                    for inport in n.inputs() {
                        check_outport_initialized(inport).map_err(HyperGraphError::BuildError)?;
                    }
                    for outport in n.outputs() {
                        check_inports_initialized(outport).map_err(HyperGraphError::BuildError)?;
                    }

                    Ok(no_state)
                })?;
            }
        }

        let mut new = HyperGraph(self.0);

        // topologically sort nodes
        fn next_strong<V, E>(node: &Node<V, E>) -> impl Iterator<Item = Node<V, E>>
        where
            V: Debug,
            E: Debug,
        {
            node.outputs()
                .flat_map(|outport| {
                    outport.inputs().filter_map(|(inport, strength)| {
                        (strength == EdgeStrength::Strong)
                            .then(|| inport.node())
                            .flatten()
                    })
                })
                .collect::<HashSet<_>>()
                .into_iter()
        }

        fn topsort<V, E, T, TS>(xs: &mut Vec<T>, next: impl Fn(&T) -> TS) -> Result<(), V, E>
        where
            V: Debug,
            E: Debug,
            T: Clone + Hash + Eq,
            TS: Iterator<Item = T>,
        {
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            enum Mark {
                Temporary,
                Permanent,
            }
            let mut state: HashMap<_, _> = xs.drain(..).map(|x| (x, None)).collect();
            while let Some((root, _)) = state.iter().find(|(_, seen)| seen.is_none()) {
                // invariant: each x will appear in stack twice
                // initially with state[x] = None, then with state[x] = Some(Mark::Temporary)
                let mut stack = vec![root.clone()];
                while let Some(cur) = stack.pop() {
                    let mark = state.get_mut(&cur).unwrap();
                    match mark {
                        Some(Mark::Temporary) => {
                            *mark = Some(Mark::Permanent); // backtracking from stack
                            xs.push(cur);
                        }
                        Some(Mark::Permanent) => unreachable!(),
                        None => {
                            *mark = Some(Mark::Temporary);
                            stack.push(cur.clone());
                            for n in next(&cur) {
                                match state[&n] {
                                    Some(Mark::Temporary) => {
                                        return Err(HyperGraphError::BuildError(
                                            HyperGraphBuildError::StrongCycle,
                                        ));
                                    }
                                    Some(Mark::Permanent) => { /* don't visit n */ }
                                    None => {
                                        stack.push(n);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Ok(())
        }

        // proxy from NodeInternal to Node to get Hash impl
        fn topsort_node_internals<V, E>(internals: &mut Vec<NodeInternal<V, E>>) -> Result<(), V, E>
        where
            V: Debug,
            E: Debug,
        {
            let mut nodes: Vec<_> = internals
                .iter()
                .map(|ni| match ni {
                    NodeInternal::Operation(operation) => {
                        Node::Operation(Operation(ByThinAddress(operation.clone())))
                    }
                    NodeInternal::Thunk(thunk) => Node::Thunk(Thunk(ByThinAddress(thunk.clone()))),
                })
                .collect();
            topsort::<V, E, _, _>(&mut nodes, next_strong)?;
            *internals = nodes
                .into_iter()
                .map(|node| match node {
                    Node::Operation(Operation(ByThinAddress(operation))) => {
                        NodeInternal::Operation(operation)
                    }
                    Node::Thunk(Thunk(ByThinAddress(thunk))) => NodeInternal::Thunk(thunk),
                })
                .collect();
            Ok(())
        }

        topsort_node_internals(&mut new.0.nodes)?;

        for node in new.nodes() {
            node.fold(&mut (), |no_state, n| {
                match n {
                    Node::Operation(_) => { /* do nothing */ }
                    Node::Thunk(thunk) => {
                        let mut nodes = thunk
                            .0
                            .nodes
                            .try_write()
                            .expect("failed to lock thunk nodes");
                        topsort_node_internals(&mut nodes)?;
                    }
                }
                Ok(no_state)
            })?;
        }

        Ok(new)
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
        let op = OperationInternal::new(input_len, output_weights, weight);
        self.0.nodes.push(NodeInternal::Operation(op.clone()));
        Operation(ByThinAddress(op))
    }

    fn add_thunk(
        &mut self,
        free_variables: Vec<E>,
        bound_variables: Vec<E>,
        output_weights: Vec<E>,
    ) -> Thunk<V, E, false> {
        let thunk = ThunkInternal::new(free_variables, bound_variables, output_weights);
        self.0.nodes.push(NodeInternal::Thunk(thunk.clone()));
        Thunk(ByThinAddress(thunk))
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
        let op = OperationInternal::new(input_len, output_weights, weight);
        self.0
             .0
            .nodes
            .try_write()
            .expect("Lock unexpectedly taken")
            .push(NodeInternal::Operation(op.clone()));
        Operation(ByThinAddress(op))
    }

    fn add_thunk(
        &mut self,
        free_variables: Vec<E>,
        bound_variables: Vec<E>,
        output_weights: Vec<E>,
    ) -> Thunk<V, E, false> {
        let thunk = ThunkInternal::new(free_variables, bound_variables, output_weights);
        self.0
             .0
            .nodes
            .try_write()
            .expect("Lock unexpectedly taken")
            .push(NodeInternal::Thunk(thunk.clone()));
        Thunk(ByThinAddress(thunk))
    }
}

pub trait Graph<V, E, const BUILT: bool> {
    fn graph_inputs(&self) -> Box<dyn Iterator<Item = OutPort<V, E, BUILT>> + '_>;
    fn graph_outputs(&self) -> Box<dyn Iterator<Item = InPort<V, E, BUILT>> + '_>;
}

pub trait GraphView<V, E, const BUILT: bool>: Graph<V, E, BUILT> {
    fn nodes(&self) -> Box<dyn Iterator<Item = Node<V, E, BUILT>> + '_>;
    fn operations<'a>(&'a self) -> Box<dyn Iterator<Item = Operation<V, E, BUILT>> + 'a>
    where
        V: 'a,
        E: 'a,
    {
        Box::new(self.nodes().filter_map(|node| match node {
            Node::Operation(operation) => Some(operation),
            Node::Thunk(_) => None,
        }))
    }
    fn thunks<'a>(&'a self) -> Box<dyn Iterator<Item = Thunk<V, E, BUILT>> + 'a>
    where
        V: 'a,
        E: 'a,
    {
        Box::new(self.nodes().filter_map(|node| match node {
            Node::Operation(_) => None,
            Node::Thunk(thunk) => Some(thunk),
        }))
    }
}

impl<V, E, const BUILT: bool> Graph<V, E, BUILT> for HyperGraph<V, E, BUILT> {
    fn graph_inputs(&self) -> Box<dyn Iterator<Item = OutPort<V, E, BUILT>> + '_> {
        Box::new(self.0.graph_inputs.iter().cloned().map(OutPort))
    }

    fn graph_outputs(&self) -> Box<dyn Iterator<Item = InPort<V, E, BUILT>> + '_> {
        Box::new(
            self.0
                .graph_outputs
                .iter()
                .cloned()
                .map(|inport| InPort(ByThinAddress(inport))),
        )
    }
}

impl<V, E, const BUILT: bool> GraphView<V, E, BUILT> for HyperGraph<V, E, BUILT> {
    fn nodes(&self) -> Box<dyn Iterator<Item = Node<V, E, BUILT>> + '_> {
        Box::new(self.0.nodes.iter().cloned().map(|node| match node {
            NodeInternal::Operation(operation) => {
                Node::Operation(Operation(ByThinAddress(operation)))
            }
            NodeInternal::Thunk(thunk) => Node::Thunk(Thunk(ByThinAddress(thunk))),
        }))
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

impl<V, E, const BUILT: bool> GraphView<V, E, BUILT> for Thunk<V, E, BUILT> {
    fn nodes(&self) -> Box<dyn Iterator<Item = Node<V, E, BUILT>> + '_> {
        Box::new(
            self.0
                .nodes
                .try_read()
                .expect("nodes lock unexpectedly taken")
                .iter()
                .cloned()
                .map(|node| match node {
                    NodeInternal::Operation(operation) => {
                        Node::Operation(Operation(ByThinAddress(operation)))
                    }
                    NodeInternal::Thunk(thunk) => Node::Thunk(Thunk(ByThinAddress(thunk))),
                })
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

#[derive(Derivative, Debug)]
#[derivative(Clone(bound = ""))]
enum NodeInternal<V, E> {
    Operation(Arc<OperationInternal<V, E>>),
    Thunk(Arc<ThunkInternal<V, E>>),
}

impl<V, E> NodeInternal<V, E>
where
    V: Debug + Send + Sync,
    E: Debug + Send + Sync,
{
    delegate! {
        to match self {
            NodeInternal::Operation(operation) => operation,
            NodeInternal::Thunk(thunk) => thunk,
        } {
            fn inputs(&self) -> Box<dyn Iterator<Item = Arc<InPortInternal<V, E>>> + '_>;
            fn outputs(&self) -> Box<dyn Iterator<Item = Arc<OutPortInternal<V, E>>> + '_>;
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
    fn inputs(&self) -> Box<dyn Iterator<Item = Arc<InPortInternal<V, E>>> + '_> {
        Box::new(self.inputs.iter().cloned())
    }

    fn outputs(&self) -> Box<dyn Iterator<Item = Arc<OutPortInternal<V, E>>> + '_> {
        Box::new(self.outputs.iter().cloned())
    }

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
    nodes: RwLock<Vec<NodeInternal<V, E>>>,
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
    fn inputs(&self) -> Box<dyn Iterator<Item = Arc<InPortInternal<V, E>>> + '_> {
        Box::new(
            self.free_variable_inputs
                .left_values()
                .map(|ByThinAddress(x)| x)
                .cloned(),
        )
    }

    fn outputs(&self) -> Box<dyn Iterator<Item = Arc<OutPortInternal<V, E>>> + '_> {
        Box::new(
            self.outputs
                .right_values()
                .map(|ByThinAddress(x)| x)
                .cloned(),
        )
    }

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
                nodes: RwLock::new(Default::default()),
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

    fn fold<T, ERR>(
        self,
        init: &mut T,
        combine: impl Fn(&mut T, Self) -> std::result::Result<&mut T, ERR>,
    ) -> std::result::Result<&mut T, ERR> {
        let mut frontier = vec![self];
        let mut acc = init;
        while let Some(cur) = frontier.pop() {
            match &cur {
                Node::Operation(_) => { /* no children */ }
                Node::Thunk(thunk) => frontier.extend(thunk.nodes()),
            }
            acc = combine(acc, cur)?;
        }
        Ok(acc)
    }
}
