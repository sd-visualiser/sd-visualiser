use std::{cmp::min, collections::HashMap, fmt::Debug, sync::Arc};

use by_address::ByThinAddress;
use derivative::Derivative;
use indexmap::IndexSet;
use itertools::Itertools;
use thiserror::Error;

use super::{
    internal::{InPortInternal, NodeInternal, OperationInternal, OutPortInternal, ThunkInternal},
    traits::{EdgeLike, Graph, NodeLike},
    Hypergraph, Node, Operation, Thunk,
};

pub mod fragment;
pub use self::fragment::Fragment;

pub(super) type Result<T, V, E> = core::result::Result<T, HypergraphError<V, E>>;

#[derive(Debug, Error, Clone)]
pub enum HypergraphError<V, E>
where
    V: Debug,
    E: Debug,
{
    #[error("Output port already linked to specified input: {0:#?}")]
    OutputLinkError(OutPort<V, E>),
    #[error("Tried to link {0:#?} to {1:#?} which does not live in the same thunk")]
    ThunkLinkError(OutPort<V, E>, InPort<V, E>),
    #[error("Building hypergraph failed: {0:#?}")]
    BuildError(HypergraphBuildError<V, E>),
}

#[derive(Debug, Error, Clone)]
pub enum HypergraphBuildError<V, E>
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

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct InPort<V, E>(ByThinAddress<Arc<InPortInternal<V, E>>>);

impl<V, E> Debug for InPort<V, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut x = f.debug_struct("InPort");
        if let Some(out_port) = self
            .0
             .0
            .link
            .try_read()
            .expect("lock unexpectedly taken")
            .upgrade()
        {
            x.field("output", &OutPort::<V, E>(ByThinAddress(out_port)));
        }
        x.field("ptr", &Arc::as_ptr(&self.0)).finish()
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct OutPort<V, E>(ByThinAddress<Arc<OutPortInternal<V, E>>>);

impl<V, E> Debug for OutPort<V, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("OutPort")
            .field(&Arc::as_ptr(&self.0))
            .finish()
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct OperationBuilder<V, E>(ByThinAddress<Arc<OperationInternal<V, E>>>);

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

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct ThunkBuilder<V, E>(ByThinAddress<Arc<ThunkInternal<V, E>>>);

impl<V, E> ThunkBuilder<V, E> {
    #[must_use]
    pub fn bound_inputs(&self) -> impl DoubleEndedIterator<Item = OutPort<V, E>> + '_ {
        self.0
            .bound_inputs
            .iter()
            .map(|out_port| OutPort(ByThinAddress(out_port.clone())))
    }

    #[must_use]
    pub fn outputs(&self) -> impl DoubleEndedIterator<Item = OutPort<V, E>> + '_ {
        self.0
            .outer_outputs
            .iter()
            .map(|out_port| OutPort(ByThinAddress(out_port.clone())))
    }

    #[must_use]
    pub fn graph_outputs(&self) -> impl DoubleEndedIterator<Item = InPort<V, E>> + '_ {
        self.0
            .inner_outputs
            .iter()
            .map(|in_port| InPort(ByThinAddress(in_port.clone())))
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
                    on_operation(OperationBuilder(op.clone()))?;
                }
                NodeInternal::Thunk(thunk) => {
                    let thunk = ThunkBuilder(thunk.clone());
                    thunk.fold(on_operation, on_thunk)?;
                    on_thunk(thunk)?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug, Derivative)]
#[derivative(Clone(bound = ""), Default(bound = ""))]
pub struct HypergraphBuilder<V, E>(Hypergraph<V, E>);

impl<V, E> HypergraphBuilder<V, E>
where
    V: Debug,
    E: Debug,
{
    #[must_use]
    #[tracing::instrument]
    pub fn new(input_weights: Vec<E>, number_of_outputs: usize) -> Self {
        let graph_inputs = input_weights
            .into_iter()
            .map(|weight| ByThinAddress(Arc::new(OutPortInternal::new(None, weight))))
            .collect();

        let graph_outputs = (0..number_of_outputs)
            .map(|_| ByThinAddress(Arc::new(InPortInternal::new(None))))
            .collect();

        HypergraphBuilder(Hypergraph {
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
            .map(|out_port| OutPort(out_port.clone()))
    }

    fn fold<Err>(
        &self,
        on_operation: impl Fn(OperationBuilder<V, E>) -> std::result::Result<(), Err>,
        on_thunk: impl Fn(ThunkBuilder<V, E>) -> std::result::Result<(), Err>,
    ) -> std::result::Result<(), Err> {
        for node in &self.0.nodes {
            match node {
                NodeInternal::Operation(op) => {
                    on_operation(OperationBuilder(op.clone()))?;
                }
                NodeInternal::Thunk(thunk) => {
                    let thunk = ThunkBuilder(thunk.clone());
                    thunk.fold(&on_operation, &on_thunk)?;
                    on_thunk(thunk)?;
                }
            }
        }
        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    pub fn build(mut self) -> Result<Hypergraph<V, E>, V, E> {
        // check validity of hypergraph:
        // all in_ports linked to exactly one out_port
        fn check_in_ports_initialized<V, E>(
            out_port: &OutPort<V, E>,
        ) -> std::result::Result<(), HypergraphBuildError<V, E>>
        where
            V: Debug,
            E: Debug,
        {
            out_port
                .0
                .links
                .try_read()
                .expect("failed to lock out_port inputs {out_port.0.inputs:#?}")
                .iter()
                .all(|weak_in_port| weak_in_port.strong_count() > 0)
                .then_some(())
                .ok_or_else(|| HypergraphBuildError::UninitializedOutPort(out_port.clone()))
        }

        fn check_out_port_initialized<V, E>(
            in_port: &InPort<V, E>,
        ) -> std::result::Result<(), HypergraphBuildError<V, E>>
        where
            V: Debug,
            E: Debug,
        {
            (in_port
                .0
                .link
                .try_read()
                .expect("failed to lock in_port output {in_port.0.output:#?}")
                .strong_count()
                > 0)
            .then_some(())
            .ok_or_else(|| HypergraphBuildError::UninitializedInPort(in_port.clone()))
        }

        fn build_thunk_inputs<V, E>(thunk: Thunk<V, E>)
        where
            V: Debug,
            E: Debug,
        {
            let built_nodes: IndexSet<Node<V, E>> = thunk.nodes().collect();

            let mut inputs: IndexSet<ByThinAddress<Arc<OutPortInternal<V, E>>>> =
                IndexSet::default();

            let thunk = Thunk(thunk.0);

            for edge in built_nodes
                .iter()
                .flat_map(Node::inputs)
                .chain(thunk.graph_outputs())
            {
                match edge.source() {
                    Some(node) => {
                        if !built_nodes.contains(&node) {
                            inputs.insert(edge.0);
                        }
                    }
                    None => {
                        if !thunk.bound_graph_inputs().contains(&edge) {
                            inputs.insert(edge.0);
                        }
                    }
                }
            }

            thunk.0.free_inputs.set(inputs).unwrap();
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
                        Node::Operation(Operation(operation.clone()))
                    }
                    NodeInternal::Thunk(thunk) => Node::Thunk(Thunk(thunk.clone())),
                })
                .collect();
            nodes = tarjans(nodes);
            *internals = nodes
                .into_iter()
                .map(|node| match node {
                    Node::Operation(Operation(operation)) => NodeInternal::Operation(operation),
                    Node::Thunk(Thunk(thunk)) => NodeInternal::Thunk(thunk),
                })
                .collect();
        }

        for out_port in self.graph_inputs() {
            // check associated with hypergraph
            assert!(&out_port.0.node.is_none());
            // check inputs initialised
            check_in_ports_initialized(&out_port).map_err(HypergraphError::BuildError)?;
        }

        for in_port in self.graph_outputs() {
            // check associated with hypergraph
            assert!(&in_port.0.node.is_none());
            // check output initialised
            check_out_port_initialized(&in_port).map_err(HypergraphError::BuildError)?;
        }

        self.fold(
            |op| {
                for in_port in op.inputs() {
                    check_out_port_initialized(&in_port).map_err(HypergraphError::BuildError)?;
                }
                for out_port in op.outputs() {
                    check_in_ports_initialized(&out_port).map_err(HypergraphError::BuildError)?;
                }
                Ok(())
            },
            |thunk| {
                for in_port in thunk.graph_outputs() {
                    check_out_port_initialized(&in_port).map_err(HypergraphError::BuildError)?;
                }
                for out_port in thunk.outputs() {
                    check_in_ports_initialized(&out_port).map_err(HypergraphError::BuildError)?;
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

        Ok(self.0)
    }
}
