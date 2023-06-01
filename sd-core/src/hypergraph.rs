use std::{
    cmp::min,
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
    sync::{Arc, RwLock, Weak},
};

use bimap::BiMap;
use by_address::ByThinAddress;
use delegate::delegate;
use derivative::Derivative;
use indexmap::IndexSet;
#[cfg(test)]
use serde::Serialize;
use thiserror::Error;
use tracing::Level;

use crate::common::InOut;

mod weakbyaddress;
use self::weakbyaddress::WeakByAddress;

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

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct InPort<V, E, const BUILT: bool = true>(ByThinAddress<Arc<InPortInternal<V, E>>>);
#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct OutPort<V, E, const BUILT: bool = true>(ByThinAddress<Arc<OutPortInternal<V, E>>>);
#[derive(Debug, Derivative)]
#[derivative(Clone(bound = ""), Default(bound = ""))]
pub struct HyperGraph<V, E, const BUILT: bool = true>(HyperGraphInternal<V, E>);
#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct Operation<V, E, const BUILT: bool = true>(ByThinAddress<Arc<OperationInternal<V, E>>>);
#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct Thunk<V, E, const BUILT: bool = true>(ByThinAddress<Arc<ThunkInternal<V, E>>>);

#[derive(Debug, Derivative)]
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

impl<V, E, const BUILT: bool> Debug for InPort<V, E, BUILT> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut x = f.debug_struct("InPort");
        if BUILT {
            x.field("output", &self.output());
        }
        x.finish()
    }
}

impl<V: Debug, E, const BUILT: bool> Debug for Operation<V, E, BUILT> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Operation")
            .field("weight", self.weight())
            .field("inputs", &self.inputs().collect::<Vec<_>>())
            .field("outputs", &self.outputs().collect::<Vec<_>>())
            .finish()
    }
}

impl<V: Debug, E: Debug, const BUILT: bool> Debug for Thunk<V, E, BUILT> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Thunk")
            .field(
                "free_vars",
                &self
                    .free_inputs()
                    .map(|out_port| (self.externalise_input(&out_port), out_port))
                    .collect::<Vec<_>>(),
            )
            .field(
                "bound_inputs",
                &self.bound_graph_inputs().collect::<Vec<_>>(),
            )
            .field("nodes", &self.nodes().collect::<Vec<_>>())
            .field(
                "outputs",
                &self
                    .graph_outputs()
                    .map(|in_port| {
                        let x = self.externalise_output(&in_port);
                        (in_port, x)
                    })
                    .collect::<Vec<_>>(),
            )
            .finish()
    }
}

impl<V, E, const BUILT: bool> Debug for OutPort<V, E, BUILT> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("OutPort")
            .field(&Arc::as_ptr(&self.0))
            .finish()
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
            output: RwLock::new(Weak::default()),
        }
    }
}
macro_rules! get_node {
    ($port:ident) => {
        impl<V, E, const BUILT: bool> $port<V, E, BUILT> {
            #[must_use]
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

impl<V, E, const BUILT: bool> InPort<V, E, BUILT> {
    #[must_use]
    pub fn output(&self) -> OutPort<V, E, BUILT> {
        assert!(BUILT);
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
    inputs: RwLock<IndexSet<WeakByAddress<InPortInternal<V, E>>>>,
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
            inputs: RwLock::new(IndexSet::default()),
            weight,
        }
    }
}

impl<V, E, const BUILT: bool> OutPort<V, E, BUILT> {
    pub fn inputs(&self) -> impl Iterator<Item = InPort<V, E, BUILT>> {
        assert!(BUILT);
        self.0
            .inputs
            .try_read()
            .expect("Lock unexpectedly taken")
            .iter()
            .map(|WeakByAddress(inport)| {
                let inport = inport.upgrade().expect("got dangling reference to inport");
                InPort(ByThinAddress(inport))
            })
            .collect::<Vec<_>>()
            .into_iter()
    }

    #[must_use]
    pub fn number_of_inputs(&self) -> usize {
        self.0
            .inputs
            .try_read()
            .expect("Lock unexpectedly taken")
            .len()
    }

    #[must_use]
    pub fn weight(&self) -> &E {
        &self.0.weight
    }
}

#[derive(Debug, Derivative)]
#[derivative(Clone(bound = ""), Default(bound = ""))]
struct HyperGraphInternal<V, E> {
    nodes: Vec<NodeInternal<V, E>>,
    graph_inputs: Vec<Arc<OutPortInternal<V, E>>>,
    graph_outputs: Vec<Arc<InPortInternal<V, E>>>,
}

impl<V, E> HyperGraph<V, E, false>
where
    V: Debug,
    E: Debug,
{
    #[must_use]
    #[tracing::instrument]
    pub fn new(input_weights: Vec<E>, number_of_outputs: usize) -> Self {
        let graph_inputs = input_weights
            .into_iter()
            .map(|weight| Arc::new(OutPortInternal::new_boundary(weight)))
            .collect();

        let graph_outputs = (0..number_of_outputs)
            .map(|_| Arc::new(InPortInternal::new_boundary()))
            .collect();

        HyperGraph(HyperGraphInternal {
            nodes: Vec::default(),
            graph_inputs,
            graph_outputs,
        })
    }

    #[allow(clippy::too_many_lines)]
    pub fn build(self) -> Result<HyperGraph<V, E, true>, V, E> {
        // check validity of hypergraph:
        // all inports linked to exactly one outport
        fn check_inports_initialized<V, E>(
            outport: &OutPort<V, E, false>,
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
                .all(|weak_inport| weak_inport.strong_count() > 0)
                .then_some(())
                .ok_or_else(|| HyperGraphBuildError::UninitializedOutPort(outport.clone()))
        }

        fn check_outport_initialized<V, E>(
            inport: &InPort<V, E, false>,
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

        // topologically sort nodes
        fn next_strong<V, E>(node: &Node<V, E>) -> impl Iterator<Item = Node<V, E>>
        where
            V: Debug,
            E: Debug,
        {
            node.outputs()
                .flat_map(|outport| outport.inputs().filter_map(|inport| inport.node()))
                .collect::<IndexSet<_>>()
                .into_iter()
        }

        fn strongconnect<T, TS>(
            next: impl (Fn(&T) -> TS) + Copy,
            stack: &mut IndexSet<T>,
            visited: &mut HashMap<T, usize>,
            output: &mut Vec<Vec<T>>,
            node: &T,
        ) where
            T: Clone + Hash + Eq,
            TS: Iterator<Item = T>,
        {
            let index = stack.insert_full(node.clone()).0;
            visited.insert(node.clone(), index);

            for n in next(node) {
                if !visited.contains_key(&n) {
                    strongconnect(next, stack, visited, output, &n);
                    let y = visited[&n];
                    let x = visited.get_mut(node).unwrap();
                    *x = min(*x, y);
                } else if let Some(index) = stack.get_index_of(&n) {
                    let x = visited.get_mut(node).unwrap();
                    *x = min(*x, index);
                }
            }

            if Some(visited[node]) == stack.get_index_of(node) {
                let component = stack.split_off(visited[node]).into_iter().collect();
                output.push(component);
            }
        }

        fn tarjans<T, TS>(xs: Vec<T>, next: impl Fn(&T) -> TS) -> Vec<T>
        where
            T: Clone + Hash + Eq,
            TS: Iterator<Item = T>,
        {
            let original_ord: IndexSet<T> = xs.into_iter().collect();
            let mut output: Vec<Vec<T>> = Vec::default();

            let mut stack: IndexSet<T> = IndexSet::default();

            let mut visited: HashMap<T, usize> = HashMap::default();

            for x in &original_ord {
                if !visited.contains_key(x) {
                    strongconnect(&next, &mut stack, &mut visited, &mut output, x);
                }
            }

            output
                .into_iter()
                .flat_map(|mut xs| {
                    xs.sort_by_key(|x| original_ord.get_index_of(x));
                    xs
                })
                .collect()
        }

        // proxy from NodeInternal to Node to get Hash impl
        fn topsort_node_internals<V, E>(internals: &mut Vec<NodeInternal<V, E>>)
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
            // topsort::<V, E, _, _>(&mut nodes, next_strong)?;
            nodes = tarjans(nodes, next_strong);
            *internals = nodes
                .into_iter()
                .map(|node| match node {
                    Node::Operation(Operation(ByThinAddress(operation))) => {
                        NodeInternal::Operation(operation)
                    }
                    Node::Thunk(Thunk(ByThinAddress(thunk))) => NodeInternal::Thunk(thunk),
                })
                .collect();
        }

        for outport in self.graph_inputs() {
            // check associated with hypergraph
            assert!(&outport.0.node.is_none());
            // check inputs initialised
            check_inports_initialized(&outport).map_err(HyperGraphError::BuildError)?;
        }

        for inport in self.graph_outputs() {
            // check associated with hypergraph
            assert!(&inport.0.node.is_none());
            // check output initialised
            check_outport_initialized(&inport).map_err(HyperGraphError::BuildError)?;
        }
        for node in self.nodes() {
            node.fold(&mut (), |no_state, n| {
                for inport in n.inputs() {
                    check_outport_initialized(&inport).map_err(HyperGraphError::BuildError)?;
                }
                for outport in n.outputs() {
                    check_inports_initialized(&outport).map_err(HyperGraphError::BuildError)?;
                }

                Ok(no_state)
            })?;
        }

        let mut new = HyperGraph(self.0);
        topsort_node_internals(&mut new.0.nodes);

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
                        topsort_node_internals(&mut nodes);
                    }
                }
                Ok(no_state)
            })?;
        }

        Ok(new)
    }
}

pub struct ThunkCursor<V, E>(Thunk<V, E, false>);

pub trait Fragment: Graph<false> {
    fn add_operation(
        &mut self,
        input_len: usize,
        output_weights: impl IntoIterator<Item = Self::EdgeWeight>,
        weight: Self::NodeWeight,
    ) -> Operation<Self::NodeWeight, Self::EdgeWeight, false>;

    fn add_thunk(
        &mut self,
        free_variables: impl IntoIterator<Item = Self::EdgeWeight>,
        bound_variables: impl IntoIterator<Item = Self::EdgeWeight>,
        output_weights: impl IntoIterator<Item = Self::EdgeWeight>,
    ) -> Thunk<Self::NodeWeight, Self::EdgeWeight, false>;

    #[tracing::instrument(level=Level::DEBUG, skip(self), err, ret)]
    fn link(
        &mut self,
        out_port: OutPort<Self::NodeWeight, Self::EdgeWeight, false>,
        in_port: InPort<Self::NodeWeight, Self::EdgeWeight, false>,
    ) -> Result<(), Self::NodeWeight, Self::EdgeWeight>
    where
        Self::NodeWeight: Debug,
        Self::EdgeWeight: Debug,
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
        }
        *out = Arc::downgrade(&out_port.0);
        out_port
            .0
            .inputs
            .try_write()
            .expect("Lock unexpectedly taken")
            .insert(WeakByAddress(Arc::downgrade(&in_port.0)));
        Ok(())
    }

    fn in_thunk<T>(
        &mut self,
        thunk: Thunk<Self::NodeWeight, Self::EdgeWeight, false>,
        f: impl FnOnce(ThunkCursor<Self::NodeWeight, Self::EdgeWeight>) -> T,
    ) -> T
    where
        Self::NodeWeight: Debug,
        Self::EdgeWeight: Debug,
    {
        f(ThunkCursor(thunk))
    }
}

impl<V, E> Fragment for HyperGraph<V, E, false>
where
    V: Debug + Send + Sync,
    E: Debug + Send + Sync,
{
    fn add_operation(
        &mut self,
        input_len: usize,
        output_weights: impl IntoIterator<Item = E>,
        weight: V,
    ) -> Operation<V, E, false> {
        let op = OperationInternal::new(
            input_len,
            output_weights,
            weight,
            0,
            BackPointerInternal::Graph,
        );
        self.0.nodes.push(NodeInternal::Operation(op.clone()));
        Operation(ByThinAddress(op))
    }

    fn add_thunk(
        &mut self,
        free_variables: impl IntoIterator<Item = E>,
        bound_variables: impl IntoIterator<Item = E>,
        output_weights: impl IntoIterator<Item = E>,
    ) -> Thunk<V, E, false> {
        let thunk = ThunkInternal::new(
            free_variables,
            bound_variables,
            output_weights,
            0,
            BackPointerInternal::Graph,
        );
        self.0.nodes.push(NodeInternal::Thunk(thunk.clone()));
        Thunk(ByThinAddress(thunk))
    }
}

impl<V, E> Fragment for ThunkCursor<V, E>
where
    V: Debug + Send + Sync + 'static,
    E: Debug + Send + Sync + 'static,
{
    fn add_operation(
        &mut self,
        input_len: usize,
        output_weights: impl IntoIterator<Item = E>,
        weight: V,
    ) -> Operation<V, E, false> {
        let op = OperationInternal::new(
            input_len,
            output_weights,
            weight,
            self.0 .0.depth + 1,
            BackPointerInternal::Thunk(Arc::downgrade(&self.0 .0 .0)),
        );
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
        free_variables: impl IntoIterator<Item = E>,
        bound_variables: impl IntoIterator<Item = E>,
        output_weights: impl IntoIterator<Item = E>,
    ) -> Thunk<V, E, false> {
        let thunk = ThunkInternal::new(
            free_variables,
            bound_variables,
            output_weights,
            self.0 .0.depth + 1,
            BackPointerInternal::Thunk(Arc::downgrade(&self.0 .0 .0)),
        );
        self.0
             .0
            .nodes
            .try_write()
            .expect("Lock unexpectedly taken")
            .push(NodeInternal::Thunk(thunk.clone()));
        Thunk(ByThinAddress(thunk))
    }
}

pub trait Graph<const BUILT: bool> {
    type NodeWeight;
    type EdgeWeight;
    fn bound_graph_inputs(
        &self,
    ) -> Box<dyn Iterator<Item = OutPort<Self::NodeWeight, Self::EdgeWeight, BUILT>> + '_>;
    fn unbound_graph_inputs(
        &self,
    ) -> Box<dyn Iterator<Item = OutPort<Self::NodeWeight, Self::EdgeWeight, BUILT>> + '_>;
    fn graph_inputs<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = OutPort<Self::NodeWeight, Self::EdgeWeight, BUILT>> + 'a>
    where
        Self::EdgeWeight: 'a,
        Self::NodeWeight: 'a,
    {
        Box::new(self.unbound_graph_inputs().chain(self.bound_graph_inputs()))
    }
    fn graph_outputs(
        &self,
    ) -> Box<dyn Iterator<Item = InPort<Self::NodeWeight, Self::EdgeWeight, BUILT>> + '_>;
}

pub trait GraphView<const BUILT: bool = true>: Graph<BUILT> {
    fn nodes(
        &self,
    ) -> Box<dyn Iterator<Item = Node<Self::NodeWeight, Self::EdgeWeight, BUILT>> + '_>;
    fn operations<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = Operation<Self::NodeWeight, Self::EdgeWeight, BUILT>> + 'a>
    where
        Self::NodeWeight: 'a,
        Self::EdgeWeight: 'a,
    {
        Box::new(self.nodes().filter_map(|node| match node {
            Node::Operation(operation) => Some(operation),
            Node::Thunk(_) => None,
        }))
    }
    fn thunks<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = Thunk<Self::NodeWeight, Self::EdgeWeight, BUILT>> + 'a>
    where
        Self::NodeWeight: 'a,
        Self::EdgeWeight: 'a,
    {
        Box::new(self.nodes().filter_map(|node| match node {
            Node::Operation(_) => None,
            Node::Thunk(thunk) => Some(thunk),
        }))
    }
}

impl<V, E, const BUILT: bool> Graph<BUILT> for HyperGraph<V, E, BUILT> {
    type NodeWeight = V;
    type EdgeWeight = E;
    fn unbound_graph_inputs(&self) -> Box<dyn Iterator<Item = OutPort<V, E, BUILT>> + '_> {
        Box::new(
            self.0
                .graph_inputs
                .iter()
                .cloned()
                .map(|o| OutPort(ByThinAddress(o))),
        )
    }

    fn bound_graph_inputs(&self) -> Box<dyn Iterator<Item = OutPort<V, E, BUILT>> + '_> {
        Box::new(std::iter::empty())
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

impl<V, E, const BUILT: bool> GraphView<BUILT> for HyperGraph<V, E, BUILT> {
    fn nodes(&self) -> Box<dyn Iterator<Item = Node<V, E, BUILT>> + '_> {
        Box::new(self.0.nodes.iter().cloned().map(|node| match node {
            NodeInternal::Operation(operation) => {
                Node::Operation(Operation(ByThinAddress(operation)))
            }
            NodeInternal::Thunk(thunk) => Node::Thunk(Thunk(ByThinAddress(thunk))),
        }))
    }
}

impl<V, E, const BUILT: bool> Graph<BUILT> for Thunk<V, E, BUILT> {
    type NodeWeight = V;
    type EdgeWeight = E;
    fn unbound_graph_inputs(&self) -> Box<dyn Iterator<Item = OutPort<V, E, BUILT>> + '_> {
        Box::new(self.free_inputs())
    }

    fn bound_graph_inputs(&self) -> Box<dyn Iterator<Item = OutPort<V, E, BUILT>> + '_> {
        Box::new(self.bound_inputs())
    }

    fn graph_outputs(&self) -> Box<dyn Iterator<Item = InPort<V, E, BUILT>> + '_> {
        Box::new(self.0.output_order.iter().cloned().map(InPort))
    }
}

impl<V, E, const BUILT: bool> GraphView<BUILT> for Thunk<V, E, BUILT> {
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

#[allow(clippy::inline_always)]
impl<V, E> Graph<false> for ThunkCursor<V, E>
where
    V: 'static,
    E: 'static,
{
    type NodeWeight = V;
    type EdgeWeight = E;
    delegate! {
        to self.0 {
            fn graph_inputs<'a>(&'a self) -> Box<dyn Iterator<Item = OutPort<V, E, false>> + 'a> where V: 'a, E: 'a;
            fn unbound_graph_inputs(&self) -> Box<dyn Iterator<Item = OutPort<V, E, false>> + '_>;
            fn bound_graph_inputs(&self) -> Box<dyn Iterator<Item = OutPort<V, E, false>> + '_>;
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

#[derive(Debug)]
enum BackPointerInternal<V, E> {
    Graph,
    Thunk(Weak<ThunkInternal<V, E>>),
}

#[derive(Debug)]
pub enum BackPointer<V, E, const BUILT: bool = true> {
    Graph,
    Thunk(Thunk<V, E, BUILT>),
}

#[derive(Debug)]
struct OperationInternal<V, E> {
    weight: V,
    inputs: Vec<Arc<InPortInternal<V, E>>>,
    outputs: Vec<Arc<OutPortInternal<V, E>>>,
    depth: usize,
    back_pointer: BackPointerInternal<V, E>,
}

impl<V, E, const BUILT: bool> InOut for Operation<V, E, BUILT> {
    #[must_use]
    fn number_of_inputs(&self) -> usize {
        self.0.inputs.len()
    }

    #[must_use]
    fn number_of_outputs(&self) -> usize {
        self.0.outputs.len()
    }
}

impl<V, E, const BUILT: bool> Operation<V, E, BUILT> {
    #[must_use]
    pub fn inputs(&self) -> impl DoubleEndedIterator<Item = InPort<V, E, BUILT>> + '_ {
        self.0
            .inputs
            .iter()
            .cloned()
            .map(|i| InPort(ByThinAddress(i)))
    }

    #[must_use]
    pub fn outputs(&self) -> impl DoubleEndedIterator<Item = OutPort<V, E, BUILT>> + '_ {
        self.0
            .outputs
            .iter()
            .cloned()
            .map(|o| OutPort(ByThinAddress(o)))
    }

    #[must_use]
    pub fn weight(&self) -> &V {
        &self.0.weight
    }

    #[must_use]
    pub fn depth(&self) -> usize {
        self.0.depth
    }

    #[must_use]
    pub fn back_pointer(&self) -> BackPointer<V, E, BUILT> {
        match &self.0.back_pointer {
            BackPointerInternal::Graph => BackPointer::Graph,
            BackPointerInternal::Thunk(a) => BackPointer::Thunk(Thunk(ByThinAddress(
                a.upgrade().expect("back pointer unexpectedly dropped"),
            ))),
        }
    }
}

impl<V, E> OperationInternal<V, E>
where
    V: Debug + Send + Sync,
    E: Debug + Send + Sync,
{
    fn new(
        input_len: usize,
        output_weights: impl IntoIterator<Item = E>,
        weight: V,
        depth: usize,
        back_pointer: BackPointerInternal<V, E>,
    ) -> Arc<Self> {
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
                        inputs: RwLock::new(IndexSet::default()),
                        weight,
                    })
                })
                .collect();
            OperationInternal {
                weight,
                inputs,
                outputs,
                depth,
                back_pointer,
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
    input_order: Vec<ByThinAddress<Arc<OutPortInternal<V, E>>>>,
    bound_variables: Vec<Arc<OutPortInternal<V, E>>>,
    outputs: InOutMap<V, E>,
    output_order: Vec<ByThinAddress<Arc<InPortInternal<V, E>>>>,
    depth: usize,
    back_pointer: BackPointerInternal<V, E>,
}

impl<V, E, const BUILT: bool> InOut for Thunk<V, E, BUILT> {
    #[must_use]
    fn number_of_inputs(&self) -> usize {
        self.0.free_variable_inputs.len()
    }

    #[must_use]
    fn number_of_outputs(&self) -> usize {
        self.0.outputs.len()
    }
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
        self.0.input_order.iter().cloned().map(OutPort)
    }

    pub fn inputs(&self) -> impl Iterator<Item = InPort<V, E, BUILT>> + '_ {
        self.0.input_order.iter().map(|out_port| {
            InPort(
                self.0
                    .free_variable_inputs
                    .get_by_right(out_port)
                    .unwrap()
                    .clone(),
            )
        })
    }

    pub fn outputs(&self) -> impl Iterator<Item = OutPort<V, E, BUILT>> + '_ {
        self.0
            .output_order
            .iter()
            .map(|in_port| OutPort(self.0.outputs.get_by_left(in_port).unwrap().clone()))
    }

    #[must_use]
    pub fn externalise_input(&self, port: &OutPort<V, E, BUILT>) -> Option<InPort<V, E, BUILT>> {
        self.0
            .free_variable_inputs
            .get_by_right(&port.0)
            .map(|in_port| InPort(in_port.clone()))
    }

    #[must_use]
    pub fn externalise_output(&self, port: &InPort<V, E, BUILT>) -> Option<OutPort<V, E, BUILT>> {
        self.0
            .outputs
            .get_by_left(&port.0)
            .map(|out_port| OutPort(out_port.clone()))
    }

    #[must_use]
    pub fn depth(&self) -> usize {
        self.0.depth
    }

    #[must_use]
    pub fn back_pointer(&self) -> BackPointer<V, E, BUILT> {
        match &self.0.back_pointer {
            BackPointerInternal::Graph => BackPointer::Graph,
            BackPointerInternal::Thunk(a) => BackPointer::Thunk(Thunk(ByThinAddress(
                a.upgrade().expect("back pointer unexpectedly dropped"),
            ))),
        }
    }
}

impl<V, E> ThunkInternal<V, E>
where
    V: Debug + Send + Sync,
    E: Debug + Send + Sync,
{
    fn new(
        free_variables: impl IntoIterator<Item = E>,
        bound_variables: impl IntoIterator<Item = E>,
        output_weights: impl IntoIterator<Item = E>,
        depth: usize,
        back_pointer: BackPointerInternal<V, E>,
    ) -> Arc<Self> {
        Arc::new_cyclic(|weak: &Weak<Self>| {
            let input_order: Vec<_> = free_variables
                .into_iter()
                .map(|fv| ByThinAddress(Arc::new(OutPortInternal::new_boundary(fv))))
                .collect();
            let free_variable_inputs = input_order
                .iter()
                .cloned()
                .map(|inner_output| {
                    let outer_input = Arc::new(InPortInternal {
                        node: Some(WeakNodeInternal::Thunk(weak.clone())),
                        output: RwLock::new(Weak::new()),
                    });
                    (ByThinAddress(outer_input), inner_output)
                })
                .collect();
            let bound_variables = bound_variables
                .into_iter()
                .map(|bv| Arc::new(OutPortInternal::new_boundary(bv)))
                .collect();

            let outputs: BiMap<_, _> = output_weights
                .into_iter()
                .zip(std::iter::repeat(ByThinAddress(Arc::new(
                    InPortInternal::new_boundary(),
                ))))
                .map(|(weight, inner_input)| {
                    let outer_output = Arc::new(OutPortInternal {
                        node: Some(WeakNodeInternal::Thunk(weak.clone())),
                        inputs: RwLock::new(IndexSet::default()),
                        weight,
                    });
                    (inner_input, ByThinAddress(outer_output))
                })
                .collect();
            let output_order = outputs.left_values().cloned().collect();
            ThunkInternal {
                nodes: RwLock::new(Vec::default()),
                free_variable_inputs,
                input_order,
                bound_variables,
                outputs,
                output_order,
                depth,
                back_pointer,
            }
        })
    }
}

impl<V, E, const BUILT: bool> InOut for Node<V, E, BUILT> {
    #[must_use]
    fn number_of_inputs(&self) -> usize {
        match self {
            Node::Operation(op) => op.number_of_inputs(),
            Node::Thunk(thunk) => thunk.number_of_inputs(),
        }
    }

    #[must_use]
    fn number_of_outputs(&self) -> usize {
        match self {
            Node::Operation(op) => op.number_of_outputs(),
            Node::Thunk(thunk) => thunk.number_of_outputs(),
        }
    }
}

impl<V, E, const BUILT: bool> Node<V, E, BUILT> {
    #[must_use]
    pub fn inputs(&self) -> Box<dyn Iterator<Item = InPort<V, E, BUILT>> + '_> {
        match self {
            Node::Operation(op) => Box::new(op.inputs()),
            Node::Thunk(thunk) => Box::new(thunk.inputs()),
        }
    }

    #[must_use]
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

    #[must_use]
    pub fn depth(&self) -> usize {
        match self {
            Node::Operation(op) => op.depth(),
            Node::Thunk(thunk) => thunk.depth(),
        }
    }

    #[must_use]
    pub fn back_pointer(&self) -> BackPointer<V, E, BUILT> {
        match self {
            Node::Operation(op) => op.back_pointer(),
            Node::Thunk(thunk) => thunk.back_pointer(),
        }
    }
}
