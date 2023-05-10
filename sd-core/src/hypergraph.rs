use std::collections::{HashMap, HashSet};

use bimap::BiMap;
use elsa::FrozenVec;
use qcell::{QCell, QCellOwner};
#[cfg(test)]
use serde::Serialize;
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum HyperGraphError {
    #[error("Output port already linked")]
    OutputLinkError,
}

type Result<T> = std::result::Result<T, HyperGraphError>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct InputIndex(usize);
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct OutputIndex(usize);
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct NodeIndex(usize);

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(Serialize))]
pub enum EdgeStrength {
    Strong,
    Weak,
}

pub struct InputPortB {
    node: Option<NodeIndex>,
    output: QCell<Option<OutputIndex>>,
    index: InputIndex,
}

pub struct OutputPortB<E> {
    node: Option<NodeIndex>,
    inputs: QCell<HashMap<InputIndex, EdgeStrength>>,
    weight: E,
    index: OutputIndex,
}

pub struct HyperGraphB<V, E> {
    node_store: FrozenVec<Box<NodeB<V>>>,
    input_ports: FrozenVec<Box<InputPortB>>,
    output_ports: FrozenVec<Box<OutputPortB<E>>>,
    inputs: HashSet<OutputIndex>,
    outputs: HashSet<InputIndex>,
    nodes: HashSet<NodeIndex>,
    token: QCellOwner,
}

pub type Position<'a> = Option<&'a ThunkB>;
pub type PositionAndWeight<'a, E> = Option<(&'a ThunkB, E)>;

impl<V, E> Default for HyperGraphB<V, E> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V, E> HyperGraphB<V, E> {
    pub fn new() -> Self {
        HyperGraphB::<V, E> {
            node_store: Default::default(),
            input_ports: Default::default(),
            output_ports: Default::default(),
            inputs: Default::default(),
            outputs: Default::default(),
            nodes: Default::default(),
            token: Default::default(),
        }
    }

    fn node(&self, idx: NodeIndex) -> &NodeB<V> {
        self.node_store.get(idx.0).unwrap()
    }

    fn input(&self, idx: InputIndex) -> &InputPortB {
        self.input_ports.get(idx.0).unwrap()
    }

    fn output(&self, idx: OutputIndex) -> &OutputPortB<E> {
        self.output_ports.get(idx.0).unwrap()
    }

    fn next_node(&self) -> NodeIndex {
        NodeIndex(self.node_store.len())
    }

    fn next_input(&self) -> InputIndex {
        InputIndex(self.input_ports.len())
    }

    fn next_output(&self) -> OutputIndex {
        OutputIndex(self.output_ports.len())
    }

    pub fn add_operation(
        &mut self,
        pos: Position,
        input_len: usize,
        output_weights: Vec<E>,
        weight: V,
    ) -> &OperationB<V> {
        let next_index = self.next_node();

        let inputs = (0..input_len)
            .map(|_| {
                let index = self.next_input();
                let port = InputPortB {
                    node: Some(next_index),
                    output: self.token.cell(None),
                    index,
                };
                self.input_ports.push(Box::new(port));
                index
            })
            .collect();

        let outputs = output_weights
            .into_iter()
            .map(|weight| {
                let index = self.next_output();
                let port = OutputPortB {
                    node: Some(next_index),
                    inputs: self.token.cell(Default::default()),
                    index,
                    weight,
                };
                self.output_ports.push(Box::new(port));
                index
            })
            .collect();
        let op = OperationB {
            weight,
            inputs,
            outputs,
            index: next_index,
        };
        match pos {
            Some(thunk) => self.token.rw(&thunk.nodes).insert(next_index),
            None => self.nodes.insert(next_index),
        };
        if let NodeB::Operation(op) = self.node_store.push_get(Box::new(NodeB::Operation(op))) {
            op
        } else {
            unreachable!()
        }
    }

    pub fn add_thunk(
        &mut self,
        pos: Position,
        bound_weights: Vec<E>,
    ) -> (&ThunkB, Vec<&OutputPortB<E>>) {
        let next_index = self.next_node();

        let bound_variables: Vec<OutputIndex> = bound_weights
            .into_iter()
            .map(|weight| {
                let index = self.next_output();
                let port = OutputPortB {
                    node: None,
                    inputs: self.token.cell(Default::default()),
                    index,
                    weight,
                };
                self.output_ports.push(Box::new(port));
                index
            })
            .collect();

        let thunk = ThunkB {
            nodes: self.token.cell(Default::default()),
            free_variable_inputs: self.token.cell(Default::default()),
            bound_variables: bound_variables.clone(),
            outputs: self.token.cell(Default::default()),
            index: next_index,
        };

        match pos {
            Some(thunk) => self.token.rw(&thunk.nodes).insert(next_index),
            None => self.nodes.insert(next_index),
        };
        if let NodeB::Thunk(thunk) = self
            .node_store
            .push_get(Box::new(NodeB::Thunk(Box::new(thunk))))
        {
            let bound_var_data = bound_variables
                .iter()
                .map(|&idx| self.output(idx))
                .collect();
            (thunk, bound_var_data)
        } else {
            unreachable!()
        }
    }

    pub fn add_input(&mut self, pos: Position, weight: E) -> &OutputPortB<E> {
        let out_port_idx = self.next_output();

        let port = OutputPortB {
            node: None,
            inputs: self.token.cell(Default::default()),
            index: out_port_idx,
            weight,
        };

        let out_port = self.output_ports.push_get(Box::new(port));

        match pos {
            Some(thunk) => {
                let in_port_idx = self.next_input();

                let port = InputPortB {
                    node: Some(thunk.index),
                    output: self.token.cell(Default::default()),
                    index: in_port_idx,
                };

                self.input_ports.push(Box::new(port));

                self.token
                    .rw(&thunk.free_variable_inputs)
                    .insert(in_port_idx, out_port_idx);
            }
            None => {
                self.inputs.insert(out_port_idx);
            }
        };

        out_port
    }

    pub fn add_output(&mut self, pos: PositionAndWeight<E>) -> &InputPortB {
        let in_port_idx = self.next_input();

        let port = InputPortB {
            node: None,
            output: self.token.cell(Default::default()),
            index: in_port_idx,
        };

        let in_port = self.input_ports.push_get(Box::new(port));

        match pos {
            Some((thunk, weight)) => {
                let out_port_idx = self.next_output();

                let port = OutputPortB {
                    node: Some(thunk.index),
                    inputs: self.token.cell(Default::default()),
                    index: out_port_idx,
                    weight,
                };

                self.output_ports.push(Box::new(port));

                self.token
                    .rw(&thunk.outputs)
                    .insert(in_port_idx, out_port_idx);
            }
            None => {
                self.outputs.insert(in_port_idx);
            }
        };

        in_port
    }

    pub fn link(
        &mut self,
        output: &OutputPortB<E>,
        input: &InputPortB,
        strength: EdgeStrength,
    ) -> Result<()> {
        let output_field = self.token.rw(&input.output);

        if output_field.is_some() {
            return Err(HyperGraphError::OutputLinkError);
        }

        *output_field = Some(output.index);

        let inputs_field = self.token.rw(&output.inputs);

        inputs_field.insert(input.index, strength);

        Ok(())
    }

    pub fn build(self) -> Result<HyperGraph<V, E>> {
        //TODO perform check here
        Ok(HyperGraph(self))
    }
}

pub enum NodeB<V> {
    Operation(OperationB<V>),
    Thunk(Box<ThunkB>),
}

pub struct OperationB<V> {
    weight: V,
    inputs: Vec<InputIndex>,
    outputs: Vec<OutputIndex>,
    #[allow(dead_code)] // TODO: Remove this if not necessary I guess
    index: NodeIndex,
}

pub struct ThunkB {
    nodes: QCell<HashSet<NodeIndex>>,
    free_variable_inputs: QCell<BiMap<InputIndex, OutputIndex>>,
    bound_variables: Vec<OutputIndex>,
    outputs: QCell<BiMap<InputIndex, OutputIndex>>,
    index: NodeIndex,
}

pub struct HyperGraph<V, E>(HyperGraphB<V, E>);

pub struct WithGraph<'a, T, V, E> {
    inner: &'a T,
    graph: &'a HyperGraph<V, E>,
}

pub type Thunk<'a, V, E> = WithGraph<'a, ThunkB, V, E>;
pub type Operation<'a, V, E> = WithGraph<'a, OperationB<V>, V, E>;
pub type InputPort<'a, V, E> = WithGraph<'a, InputPortB, V, E>;
pub type OutputPort<'a, V, E> = WithGraph<'a, OutputPortB<E>, V, E>;

pub enum Node<'a, V, E> {
    Operation(Operation<'a, V, E>),
    Thunk(Thunk<'a, V, E>),
}

impl<'a, V, E> Node<'a, V, E> {
    fn build(node: &'a NodeB<V>, graph: &'a HyperGraph<V, E>) -> Self {
        match node {
            NodeB::Operation(op) => Node::Operation(WithGraph { inner: op, graph }),
            NodeB::Thunk(thunk) => Node::Thunk(WithGraph {
                inner: thunk,
                graph,
            }),
        }
    }
}

impl<V, E> HyperGraph<V, E> {
    fn node(&self, idx: NodeIndex) -> Node<V, E> {
        Node::build(self.0.node(idx), self)
    }

    fn input(&self, idx: InputIndex) -> InputPort<V, E> {
        InputPort {
            inner: self.0.input(idx),
            graph: self,
        }
    }

    fn output(&self, idx: OutputIndex) -> OutputPort<V, E> {
        OutputPort {
            inner: self.0.output(idx),
            graph: self,
        }
    }

    pub fn nodes(&self) -> impl Iterator<Item = Node<V, E>> {
        self.0.nodes.iter().map(|&idx| self.node(idx))
    }

    pub fn inputs(&self) -> impl Iterator<Item = OutputPort<V, E>> {
        self.0.inputs.iter().map(|&idx| self.output(idx))
    }

    pub fn outputs(&self) -> impl Iterator<Item = InputPort<V, E>> {
        self.0.outputs.iter().map(|&idx| self.input(idx))
    }
}

impl<'a, V, E> Thunk<'a, V, E> {
    pub fn nodes(&self) -> impl Iterator<Item = Node<V, E>> {
        self.graph
            .0
            .token
            .ro(&self.inner.nodes)
            .iter()
            .map(|&idx| self.graph.node(idx))
    }

    pub fn bound_variables(&self) -> impl Iterator<Item = OutputPort<V, E>> {
        self.inner
            .bound_variables
            .iter()
            .map(|&idx| self.graph.output(idx))
    }
}

impl<'a, V, E> Operation<'a, V, E> {
    pub fn weight(&self) -> &V {
        &self.inner.weight
    }

    pub fn inputs(&self) -> impl Iterator<Item = InputPort<V, E>> {
        self.inner.inputs.iter().map(|&idx| self.graph.input(idx))
    }

    pub fn outputs(&self) -> impl Iterator<Item = OutputPort<V, E>> {
        self.inner.outputs.iter().map(|&idx| self.graph.output(idx))
    }
}

impl<'a, V, E> InputPort<'a, V, E> {
    pub fn node(&self) -> Option<Node<V, E>> {
        self.inner.node.map(|idx| self.graph.node(idx))
    }

    pub fn output(&self) -> OutputPort<V, E> {
        self.graph
            .output(self.graph.0.token.ro(&self.inner.output).unwrap())
    }
}

impl<'a, V, E> OutputPort<'a, V, E> {
    pub fn node(&self) -> Option<Node<V, E>> {
        self.inner.node.map(|idx| self.graph.node(idx))
    }

    pub fn inputs(&self) -> impl Iterator<Item = (InputPort<V, E>, EdgeStrength)> {
        self.graph
            .0
            .token
            .ro(&self.inner.inputs)
            .iter()
            .map(|(&idx, &s)| (self.graph.input(idx), s))
    }

    pub fn weight(&self) -> &E {
        &self.inner.weight
    }
}
