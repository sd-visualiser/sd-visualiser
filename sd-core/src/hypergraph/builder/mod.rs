use std::{cmp::min, collections::HashMap, fmt::Debug, sync::Arc};

use by_address::ByThinAddress;
use derivative::Derivative;
use indexmap::IndexSet;
use itertools::Itertools;
use thiserror::Error;

use super::{
    internal::{
        EndPointInternal, InPortInternal, NodeInternal, OperationInternal, OutPortInternal,
        ThunkInternal,
    },
    traits::{EdgeLike, Graph, NodeLike},
    Hypergraph, Node, Operation, Thunk, Weight,
};
use crate::hypergraph::EndPoint;

pub mod fragment;
pub use self::fragment::Fragment;

pub(super) type Result<T, W> = core::result::Result<T, HypergraphError<W>>;

#[derive(Derivative, Error)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub enum HypergraphError<W: Weight> {
    #[error("Output port already linked to specified input: {0:#?}")]
    OutputLinkError(OutPort<W>),
    #[error("Tried to link {0:#?} to {1:#?} which does not live in the same thunk")]
    ThunkLinkError(OutPort<W>, InPort<W>),
    #[error("Building hypergraph failed: {0:#?}")]
    BuildError(HypergraphBuildError<W>),
}

#[derive(Derivative, Error)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub enum HypergraphBuildError<W: Weight> {
    #[error("InPort has uninitialised OutPort: {0:#?}")]
    UninitializedInPort(InPort<W>),
    #[error("OutPort has uninitialised InPort: {0:#?}")]
    UninitializedOutPort(OutPort<W>),
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
pub struct InPort<W: Weight>(ByThinAddress<Arc<InPortInternal<W>>>);

impl<W: Weight> Debug for InPort<W> {
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
            x.field("output", &OutPort::<W>(ByThinAddress(out_port)));
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
pub struct OutPort<W: Weight>(ByThinAddress<Arc<OutPortInternal<W>>>);

impl<W: Weight> Debug for OutPort<W> {
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
pub struct OperationBuilder<W: Weight>(ByThinAddress<Arc<OperationInternal<W>>>);

impl<W: Weight> OperationBuilder<W> {
    #[must_use]
    pub fn inputs(&self) -> impl DoubleEndedIterator<Item = InPort<W>> + '_ {
        self.0
            .inputs
            .iter()
            .cloned()
            .map(|i| InPort(ByThinAddress(i)))
    }

    #[must_use]
    pub fn outputs(&self) -> impl DoubleEndedIterator<Item = OutPort<W>> + '_ {
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
pub struct ThunkBuilder<W: Weight>(ByThinAddress<Arc<ThunkInternal<W>>>);

impl<W: Weight> ThunkBuilder<W> {
    #[must_use]
    pub fn bound_inputs(&self) -> impl DoubleEndedIterator<Item = OutPort<W>> + '_ {
        self.0
            .bound_inputs
            .iter()
            .map(|out_port| OutPort(ByThinAddress(out_port.clone())))
    }

    #[must_use]
    pub fn outputs(&self) -> impl DoubleEndedIterator<Item = OutPort<W>> + '_ {
        self.0
            .outer_outputs
            .iter()
            .map(|out_port| OutPort(ByThinAddress(out_port.clone())))
    }

    #[must_use]
    pub fn graph_outputs(&self) -> impl DoubleEndedIterator<Item = InPort<W>> + '_ {
        self.0
            .inner_outputs
            .iter()
            .map(|in_port| InPort(ByThinAddress(in_port.clone())))
    }

    fn fold<Err>(
        &self,
        on_operation: impl Fn(OperationBuilder<W>) -> std::result::Result<(), Err> + Copy,
        on_thunk: impl Fn(ThunkBuilder<W>) -> std::result::Result<(), Err> + Copy,
    ) -> std::result::Result<(), Err> {
        let nodes: &Vec<NodeInternal<W>> = &self.0.nodes.read().unwrap();
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

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""), Default(bound = ""))]
pub struct HypergraphBuilder<W: Weight>(Hypergraph<W>);

impl<W: Weight> HypergraphBuilder<W> {
    #[must_use]
    #[tracing::instrument]
    pub fn new(input_weights: Vec<W::EdgeWeight>, number_of_outputs: usize) -> Self {
        let graph_inputs = input_weights
            .into_iter()
            .map(|weight| {
                ByThinAddress(Arc::new(OutPortInternal::new(
                    EndPointInternal::GraphBoundary(None),
                    weight,
                )))
            })
            .collect();

        let graph_outputs = (0..number_of_outputs)
            .map(|_| {
                ByThinAddress(Arc::new(InPortInternal::new(
                    EndPointInternal::GraphBoundary(None),
                )))
            })
            .collect();

        HypergraphBuilder(Hypergraph {
            nodes: Vec::default(),
            graph_inputs,
            graph_outputs,
        })
    }

    #[must_use]
    pub fn graph_inputs(&self) -> impl DoubleEndedIterator<Item = OutPort<W>> + '_ {
        self.0
            .graph_inputs
            .iter()
            .map(|out_port| OutPort(out_port.clone()))
    }

    fn fold<Err>(
        &self,
        on_operation: impl Fn(OperationBuilder<W>) -> std::result::Result<(), Err>,
        on_thunk: impl Fn(ThunkBuilder<W>) -> std::result::Result<(), Err>,
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
    pub fn build(mut self) -> Result<Hypergraph<W>, W> {
        // check validity of hypergraph:
        // all in_ports linked to exactly one out_port
        fn check_in_ports_initialized<W: Weight>(
            out_port: &OutPort<W>,
        ) -> std::result::Result<(), HypergraphBuildError<W>> {
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

        fn check_out_port_initialized<W: Weight>(
            in_port: &InPort<W>,
        ) -> std::result::Result<(), HypergraphBuildError<W>> {
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

        fn build_thunk_inputs<W: Weight>(thunk: Thunk<W>) {
            let built_nodes: IndexSet<Node<W>> = thunk.nodes().collect();

            let mut inputs: IndexSet<ByThinAddress<Arc<OutPortInternal<W>>>> = IndexSet::default();

            let thunk = Thunk(thunk.0);

            for edge in built_nodes
                .iter()
                .flat_map(Node::inputs)
                .chain(thunk.graph_outputs())
            {
                match edge.source() {
                    EndPoint::Node(node) => {
                        if !built_nodes.contains(&node) {
                            inputs.insert(edge.0);
                        }
                    }
                    EndPoint::Boundary(_) => {
                        if !thunk.bound_graph_inputs().contains(&edge) {
                            inputs.insert(edge.0);
                        }
                    }
                }
            }

            thunk.0.free_inputs.set(inputs).unwrap();
        }

        fn strongconnect<W: Weight>(
            stack: &mut IndexSet<Node<W>>,
            visited: &mut HashMap<Node<W>, usize>,
            output: &mut Vec<Vec<Node<W>>>,
            node: &Node<W>,
        ) {
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

        fn tarjans<W: Weight>(xs: Vec<Node<W>>) -> Vec<Node<W>> {
            let original_ord: IndexSet<Node<W>> = xs.into_iter().collect();
            let mut output: Vec<Vec<Node<W>>> = Vec::default();

            let mut stack: IndexSet<Node<W>> = IndexSet::default();

            let mut visited: HashMap<Node<W>, usize> = HashMap::default();

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
        fn topsort_node_internals<W: Weight>(internals: &mut Vec<NodeInternal<W>>) {
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
            assert!(&out_port.0.endpoint.is_boundary());
            // check inputs initialised
            check_in_ports_initialized(&out_port).map_err(HypergraphError::BuildError)?;
        }

        for in_port in self.graph_outputs() {
            // check associated with hypergraph
            assert!(&in_port.0.endpoint.is_boundary());
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
