use std::{
    cmp::min,
    collections::{HashMap, HashSet},
    fmt::Debug,
    sync::{Arc, Weak},
};

use by_address::ByThinAddress;
use derivative::Derivative;
use indexmap::IndexSet;
use itertools::Itertools;
#[cfg(test)]
use serde::Serialize;
use thiserror::Error;

use crate::common::InOut;

mod debug;
pub mod fragment;
mod internal;
mod weakbyaddress;
use fragment::Fragment;

use self::{
    internal::{
        HyperGraphInternal, InPortInternal, NodeInternal, OperationInternal, OutPortInternal,
        ThunkInternal, WeakNodeInternal,
    },
    weakbyaddress::WeakByAddress,
};

pub mod reachability;
// pub mod subgraph;

#[derive(Debug, Error, Clone)]
pub enum HyperGraphError<V, E>
where
    V: Debug,
    E: Debug,
{
    #[error("Output port already linked to specified input: {0:#?}")]
    OutputLinkError(OutPort<V, E>),
    #[error("Tried to link {0:#?} to {1:#?} which does not live in the same thunk")]
    ThunkLinkError(OutPort<V, E>, InPort<V, E>),
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
    UninitializedInPort(InPort<V, E>),
    #[error("OutPort has uninitialised InPort: {0:#?}")]
    UninitializedOutPort(OutPort<V, E>),
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
pub struct InPort<V, E>(ByThinAddress<Arc<InPortInternal<V, E>>>);

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct OutPort<V, E>(ByThinAddress<Arc<OutPortInternal<V, E>>>);

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct Edge<V, E>(ByThinAddress<Arc<OutPortInternal<V, E>>>);

#[derive(Debug, Derivative)]
#[derivative(Clone(bound = ""), Default(bound = ""))]
pub struct HyperGraphBuilder<V, E>(HyperGraphInternal<V, E>);

#[derive(Debug, Derivative)]
#[derivative(Clone(bound = ""), Default(bound = ""))]
pub struct HyperGraph<V, E>(HyperGraphInternal<V, E>);

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct OperationBuilder<V, E>(ByThinAddress<Arc<OperationInternal<V, E>>>);

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct Operation<V, E>(ByThinAddress<Arc<OperationInternal<V, E>>>);

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct ThunkBuilder<V, E>(ByThinAddress<Arc<ThunkInternal<V, E>>>);

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct Thunk<V, E>(ByThinAddress<Arc<ThunkInternal<V, E>>>);

#[derive(Debug, Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub enum Node<V, E> {
    Operation(Operation<V, E>),
    Thunk(Thunk<V, E>),
}

impl<V, E> WeakNodeInternal<V, E> {
    pub(super) fn unwrap_node(&self) -> Node<V, E> {
        match self {
            WeakNodeInternal::Operation(op_weak) => {
                Node::Operation(Operation(ByThinAddress(op_weak.upgrade().unwrap())))
            }
            WeakNodeInternal::Thunk(thunk_weak) => {
                Node::Thunk(Thunk(ByThinAddress(thunk_weak.upgrade().unwrap())))
            }
        }
    }
}

impl<V, E> Edge<V, E> {
    #[must_use]
    pub fn weight(&self) -> &E {
        &self.0.weight
    }

    #[must_use]
    pub fn node(&self) -> Option<Node<V, E>> {
        self.0.node.as_ref().map(WeakNodeInternal::unwrap_node)
    }

    pub fn targets(&self) -> impl Iterator<Item = Option<Node<V, E>>> {
        self.0
            .links
            .try_read()
            .expect("Lock unexpectedly taken")
            .iter()
            .map(|WeakByAddress(in_port)| {
                let in_port = in_port.upgrade().expect("got dangling reference to inport");
                in_port.node.as_ref().map(WeakNodeInternal::unwrap_node)
            })
            .collect::<Vec<_>>()
            .into_iter()
    }

    #[must_use]
    pub fn number_of_links(&self) -> usize {
        self.0
            .links
            .try_read()
            .expect("Lock unexpectedly taken")
            .len()
    }
}

impl<V, E> HyperGraphBuilder<V, E>
where
    V: Debug,
    E: Debug,
{
    #[must_use]
    #[tracing::instrument]
    pub fn new(input_weights: Vec<E>, number_of_outputs: usize) -> Self {
        let graph_inputs = input_weights
            .into_iter()
            .map(|weight| Arc::new(OutPortInternal::new(None, weight)))
            .collect();

        let graph_outputs = (0..number_of_outputs)
            .map(|_| Arc::new(InPortInternal::new(None)))
            .collect();

        HyperGraphBuilder(HyperGraphInternal {
            nodes: Vec::default(),
            graph_inputs,
            graph_outputs,
        })
    }

    #[must_use]
    pub fn graph_inputs(&self) -> impl DoubleEndedIterator<Item = OutPort<V, E>> + '_ {
        self.0
            .graph_inputs
            .iter()
            .map(|out_port| OutPort(ByThinAddress(out_port.clone())))
    }

    fn fold<Err>(
        &self,
        on_operation: impl Fn(OperationBuilder<V, E>) -> std::result::Result<(), Err>,
        on_thunk: impl Fn(ThunkBuilder<V, E>) -> std::result::Result<(), Err>,
    ) -> std::result::Result<(), Err> {
        for node in &self.0.nodes {
            match node {
                NodeInternal::Operation(op) => {
                    on_operation(OperationBuilder(ByThinAddress(op.clone())))?;
                }
                NodeInternal::Thunk(thunk) => {
                    let thunk = ThunkBuilder(ByThinAddress(thunk.clone()));
                    thunk.fold(&on_operation, &on_thunk)?;
                    on_thunk(thunk)?;
                }
            }
        }
        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    pub fn build(mut self) -> Result<HyperGraph<V, E>, V, E> {
        // check validity of hypergraph:
        // all inports linked to exactly one outport
        fn check_inports_initialized<V, E>(
            outport: &OutPort<V, E>,
        ) -> std::result::Result<(), HyperGraphBuildError<V, E>>
        where
            V: Debug,
            E: Debug,
        {
            outport
                .0
                .links
                .try_read()
                .expect("failed to lock outport inputs {outport.0.inputs:#?}")
                .iter()
                .all(|weak_inport| weak_inport.strong_count() > 0)
                .then_some(())
                .ok_or_else(|| HyperGraphBuildError::UninitializedOutPort(outport.clone()))
        }

        fn check_outport_initialized<V, E>(
            inport: &InPort<V, E>,
        ) -> std::result::Result<(), HyperGraphBuildError<V, E>>
        where
            V: Debug,
            E: Debug,
        {
            (inport
                .0
                .link
                .try_read()
                .expect("failed to lock inport output {inport.0.output:#?}")
                .strong_count()
                > 0)
            .then_some(())
            .ok_or_else(|| HyperGraphBuildError::UninitializedInPort(inport.clone()))
        }

        fn build_thunk_inputs<V, E>(thunk: Thunk<V, E>)
        where
            V: Debug,
            E: Debug,
        {
            let built_nodes: HashSet<Node<V, E>> = thunk.nodes().collect();

            let mut inputs: Vec<ByThinAddress<Arc<OutPortInternal<V, E>>>> = vec![];

            let thunk = Thunk(thunk.0);

            for edge in built_nodes
                .iter()
                .flat_map(Node::inputs)
                .chain(thunk.graph_outputs())
            {
                match edge.node() {
                    Some(node) => {
                        if !built_nodes.contains(&node) {
                            inputs.push(edge.0);
                        }
                    }
                    None => {
                        if !thunk.bound_graph_inputs().contains(&edge) {
                            inputs.push(edge.0);
                        }
                    }
                }
            }

            thunk.0.free_variable_edges.set(inputs).unwrap();
        }

        fn strongconnect<V, E>(
            stack: &mut IndexSet<Node<V, E>>,
            visited: &mut HashMap<Node<V, E>, usize>,
            output: &mut Vec<Vec<Node<V, E>>>,
            node: &Node<V, E>,
        ) where
            V: Debug,
            E: Debug,
        {
            let index = stack.insert_full(node.clone()).0;
            visited.insert(node.clone(), index);

            for n in node.flat_successors() {
                if !visited.contains_key(&n) {
                    strongconnect(stack, visited, output, &n);
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

        fn tarjans<V, E>(xs: Vec<Node<V, E>>) -> Vec<Node<V, E>>
        where
            V: Debug,
            E: Debug,
        {
            let original_ord: IndexSet<Node<V, E>> = xs.into_iter().collect();
            let mut output: Vec<Vec<Node<V, E>>> = Vec::default();

            let mut stack: IndexSet<Node<V, E>> = IndexSet::default();

            let mut visited: HashMap<Node<V, E>, usize> = HashMap::default();

            for x in &original_ord {
                if !visited.contains_key(x) {
                    strongconnect(&mut stack, &mut visited, &mut output, x);
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
            nodes = tarjans(nodes);
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

        self.fold(
            |op| {
                for in_port in op.inputs() {
                    check_outport_initialized(&in_port).map_err(HyperGraphError::BuildError)?;
                }
                for out_port in op.outputs() {
                    check_inports_initialized(&out_port).map_err(HyperGraphError::BuildError)?;
                }
                Ok(())
            },
            |thunk| {
                for in_port in ThunkCursor(thunk.clone()).graph_outputs() {
                    check_outport_initialized(&in_port).map_err(HyperGraphError::BuildError)?;
                }
                for out_port in thunk.outputs() {
                    check_inports_initialized(&out_port).map_err(HyperGraphError::BuildError)?;
                }
                Ok(())
            },
        )?;

        self.fold(
            |_| Ok(()),
            |thunk| {
                build_thunk_inputs(Thunk(thunk.0));
                Ok(())
            },
        )?;

        topsort_node_internals(&mut self.0.nodes);

        self.fold(
            |_| Ok(()),
            |thunk| {
                let mut nodes = thunk.0.nodes.try_write().unwrap();
                topsort_node_internals(&mut nodes);
                Ok(())
            },
        )?;

        Ok(HyperGraph(self.0))
    }
}

pub struct ThunkCursor<V, E>(ThunkBuilder<V, E>);

pub trait Graph {
    type NodeWeight;
    type EdgeWeight;
    fn bound_graph_inputs(
        &self,
    ) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::NodeWeight, Self::EdgeWeight>> + '_>;
    fn unbound_graph_inputs(
        &self,
    ) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::NodeWeight, Self::EdgeWeight>> + '_>;
    fn graph_inputs<'a>(
        &'a self,
    ) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::NodeWeight, Self::EdgeWeight>> + 'a>
    where
        Self::EdgeWeight: 'a,
        Self::NodeWeight: 'a,
    {
        Box::new(self.unbound_graph_inputs().chain(self.bound_graph_inputs()))
    }
    fn graph_outputs(
        &self,
    ) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::NodeWeight, Self::EdgeWeight>> + '_>;
    fn nodes(
        &self,
    ) -> Box<dyn DoubleEndedIterator<Item = Node<Self::NodeWeight, Self::EdgeWeight>> + '_>;
    fn operations<'a>(
        &'a self,
    ) -> Box<dyn DoubleEndedIterator<Item = Operation<Self::NodeWeight, Self::EdgeWeight>> + 'a>
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
    ) -> Box<dyn DoubleEndedIterator<Item = Thunk<Self::NodeWeight, Self::EdgeWeight>> + 'a>
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

impl<V, E> Graph for HyperGraph<V, E> {
    type NodeWeight = V;
    type EdgeWeight = E;
    fn unbound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(
            self.0
                .graph_inputs
                .iter()
                .cloned()
                .map(|o| Edge(ByThinAddress(o))),
        )
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(std::iter::empty())
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(
            self.0
                .graph_outputs
                .iter()
                .cloned()
                .map(|in_port| Edge(ByThinAddress(in_port.link()))),
        )
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<V, E>> + '_> {
        Box::new(self.0.nodes.iter().cloned().map(|node| match node {
            NodeInternal::Operation(operation) => {
                Node::Operation(Operation(ByThinAddress(operation)))
            }
            NodeInternal::Thunk(thunk) => Node::Thunk(Thunk(ByThinAddress(thunk))),
        }))
    }
}

impl<V, E> Graph for Thunk<V, E> {
    type NodeWeight = V;
    type EdgeWeight = E;
    fn unbound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(
            self.0
                .free_variable_edges
                .get()
                .expect("Could not lock")
                .clone()
                .into_iter()
                .map(Edge),
        )
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(self.0.bound_variables.iter().cloned().map(|o| Edge(o)))
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<V, E>> + '_> {
        Box::new(
            self.0
                .outputs
                .keys()
                .map(|in_port| Edge(ByThinAddress(in_port.link()))),
        )
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<V, E>> + '_> {
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

impl<V, E> OperationBuilder<V, E> {
    #[must_use]
    pub fn inputs(&self) -> impl DoubleEndedIterator<Item = InPort<V, E>> + '_ {
        self.0
            .inputs
            .iter()
            .cloned()
            .map(|i| InPort(ByThinAddress(i)))
    }

    #[must_use]
    pub fn outputs(&self) -> impl DoubleEndedIterator<Item = OutPort<V, E>> + '_ {
        self.0
            .outputs
            .iter()
            .cloned()
            .map(|o| OutPort(ByThinAddress(o)))
    }
}

impl<V, E> Operation<V, E> {
    #[must_use]
    pub fn inputs(&self) -> impl DoubleEndedIterator<Item = Edge<V, E>> + '_ {
        self.0
            .inputs
            .iter()
            .map(|i| Edge(ByThinAddress(i.link.read().unwrap().upgrade().unwrap())))
    }

    #[must_use]
    pub fn outputs(&self) -> impl DoubleEndedIterator<Item = Edge<V, E>> + '_ {
        self.0
            .outputs
            .iter()
            .cloned()
            .map(|o| Edge(ByThinAddress(o)))
    }

    #[must_use]
    pub fn weight(&self) -> &V {
        &self.0.weight
    }

    #[must_use]
    pub fn backlink(&self) -> Option<Thunk<V, E>> {
        self.0
            .backlink
            .as_ref()
            .and_then(Weak::upgrade)
            .map(|thunk_internal| Thunk(ByThinAddress(thunk_internal)))
    }
}

impl<V, E> ThunkBuilder<V, E> {
    pub fn bound_inputs(&self) -> impl Iterator<Item = OutPort<V, E>> + '_ {
        self.0
            .bound_variables
            .iter()
            .map(|out_port| OutPort(out_port.clone()))
    }

    pub fn outputs(&self) -> impl Iterator<Item = OutPort<V, E>> + '_ {
        self.0
            .outputs
            .values()
            .map(|out_port| OutPort(ByThinAddress(out_port.clone())))
    }

    fn fold<Err>(
        &self,
        on_operation: impl Fn(OperationBuilder<V, E>) -> std::result::Result<(), Err> + Copy,
        on_thunk: impl Fn(ThunkBuilder<V, E>) -> std::result::Result<(), Err> + Copy,
    ) -> std::result::Result<(), Err> {
        let nodes: &Vec<NodeInternal<V, E>> = &self.0.nodes.read().unwrap();
        for node in nodes {
            match node {
                NodeInternal::Operation(op) => {
                    on_operation(OperationBuilder(ByThinAddress(op.clone())))?;
                }
                NodeInternal::Thunk(thunk) => {
                    let thunk = ThunkBuilder(ByThinAddress(thunk.clone()));
                    thunk.fold(on_operation, on_thunk)?;
                    on_thunk(thunk)?;
                }
            }
        }
        Ok(())
    }
}

impl<V, E> Thunk<V, E> {
    #[must_use]
    pub fn inputs(&self) -> impl DoubleEndedIterator<Item = Edge<V, E>> + '_ {
        self.0
            .free_variable_edges
            .get()
            .expect("Failed to unlock")
            .clone()
            .into_iter()
            .map(|out_port| Edge(out_port))
    }

    #[must_use]
    pub fn outputs(&self) -> impl DoubleEndedIterator<Item = Edge<V, E>> + '_ {
        self.0
            .outputs
            .values()
            .map(|out_port| Edge(ByThinAddress(out_port.clone())))
    }

    // #[must_use]
    // pub fn externalise_output(&self, port: &InPort<V, E, BUILT>) -> Option<OutPort<V, E, BUILT>> {
    //     self.0
    //         .outputs
    //         .get(&port.0)
    //         .map(|out_port| OutPort(ByThinAddress(out_port.clone())))
    // }

    #[must_use]
    pub fn backlink(&self) -> Option<Thunk<V, E>> {
        self.0
            .backlink
            .as_ref()
            .and_then(Weak::upgrade)
            .map(|thunk_internal| Thunk(ByThinAddress(thunk_internal)))
    }
}

impl<V, E> Node<V, E> {
    #[must_use]
    pub fn inputs(&self) -> Box<dyn Iterator<Item = Edge<V, E>> + '_> {
        match self {
            Node::Operation(op) => Box::new(op.inputs()),
            Node::Thunk(thunk) => Box::new(thunk.inputs()),
        }
    }

    #[must_use]
    pub fn outputs(&self) -> Box<dyn Iterator<Item = Edge<V, E>> + '_> {
        match self {
            Node::Operation(op) => Box::new(op.outputs()),
            Node::Thunk(thunk) => Box::new(thunk.outputs()),
        }
    }

    #[must_use]
    pub fn backlink(&self) -> Option<Thunk<V, E>> {
        match self {
            Node::Operation(op) => op.backlink(),
            Node::Thunk(thunk) => thunk.backlink(),
        }
    }
}

impl<V, E> InOut for Operation<V, E> {
    #[must_use]
    fn number_of_inputs(&self) -> usize {
        self.0.inputs.len()
    }

    #[must_use]
    fn number_of_outputs(&self) -> usize {
        self.0.outputs.len()
    }
}

impl<V, E> InOut for Thunk<V, E> {
    #[must_use]
    fn number_of_inputs(&self) -> usize {
        self.0
            .free_variable_edges
            .get()
            .expect("Failed to unlock")
            .len()
    }

    #[must_use]
    fn number_of_outputs(&self) -> usize {
        self.0.outputs.len()
    }
}

impl<V, E> InOut for Node<V, E> {
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
