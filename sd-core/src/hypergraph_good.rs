use std::collections::{HashMap, HashSet};

use bimap::BiMap;
use delegate::delegate;
use getset::Getters;
use id_arena::{Arena, Id};

use once_cell::unsync::OnceCell;
#[cfg(test)]
use serde::Serialize;
use thiserror::Error;

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(Serialize))]
pub enum EdgeStrength {
    Strong,
    Weak,
}

#[derive(Getters)]
pub struct InputPort<V, E> {
    #[get = "pub"]
    node: Option<Id<Node<V, E>>>,
    output: OnceCell<Id<OutputPort<V, E>>>,
}

impl<V, E> InputPort<V, E> {
    pub fn output(&self) -> Result<Id<OutputPort<V, E>>, V, E> {
        self.output
            .get()
            .copied()
            .ok_or(HyperGraphError::UnlinkedPort)
    }
}

#[derive(Getters)]
pub struct OutputPort<V, E> {
    #[get = "pub"]
    node: Option<Id<Node<V, E>>>,
    #[get = "pub"]
    inputs: HashMap<Id<InputPort<V, E>>, EdgeStrength>,
    #[get = "pub"]
    weight: E,
}

#[derive(Debug, Error, Clone)]
pub enum HyperGraphError<V, E> {
    #[error("No node at index `{0:#?}`")]
    UnknownNode(Id<Node<V, E>>),
    #[error("No input port at index `{0:#?}`")]
    UnknownInput(Id<InputPort<V, E>>),
    #[error("No output port at index `{0:#?}`")]
    UnknownOutput(Id<OutputPort<V, E>>),
    #[error("Input port not linked: {0:#?}")]
    UnlinkedInput(Id<InputPort<V, E>>),
    #[error("Unlinked port")]
    UnlinkedPort,
    #[error("Input port already linked: {0:#?}")]
    InputLinkError(Id<InputPort<V, E>>),
    #[error("Output port already linked to specified input: {0:#?}")]
    OutputLinkError(Id<OutputPort<V, E>>),
}

type Result<T, V, E> = core::result::Result<T, HyperGraphError<V, E>>;

struct Data<V, E> {
    nodes: Arena<Node<V, E>>,
    input_ports: Arena<InputPort<V, E>>,
    output_ports: Arena<OutputPort<V, E>>,
}

impl<V, E> Default for Data<V, E> {
    fn default() -> Self {
        Self {
            nodes: Default::default(),
            input_ports: Default::default(),
            output_ports: Default::default(),
        }
    }
}

impl<V, E> Data<V, E> {
    fn add_input(&mut self, weight: E) -> Id<OutputPort<V, E>> {
        self.output_ports.alloc(OutputPort {
            node: None,
            inputs: Default::default(),
            weight,
        })
    }

    fn add_output(&mut self) -> Id<InputPort<V, E>> {
        self.input_ports.alloc(InputPort {
            node: None,
            output: Default::default(),
        })
    }

    fn add_operation(
        &mut self,
        input_len: usize,
        output_weights: Vec<E>,
        weight: V,
    ) -> Id<Node<V, E>> {
        self.nodes.alloc_with_id(|node| {
            let inputs = (0..input_len)
                .map(|_| {
                    self.input_ports.alloc(InputPort {
                        node: Some(node),
                        output: Default::default(),
                    })
                })
                .collect();
            let outputs = output_weights
                .into_iter()
                .map(|weight| {
                    self.output_ports.alloc(OutputPort {
                        node: Some(node),
                        inputs: Default::default(),
                        weight,
                    })
                })
                .collect();
            Node::Operation(Operation {
                weight,
                inputs,
                outputs,
            })
        })
    }

    fn add_thunk(
        &mut self,
        free_variables: Vec<E>,
        bound_variables: Vec<E>,
        output_weights: Vec<E>,
    ) -> Id<Node<V, E>> {
        self.nodes.alloc_with_id(|node| {
            let mut data: Data<V, E> = Default::default();
            let free_variable_inputs = free_variables
                .into_iter()
                .map(|fv| {
                    let inner_output = data.add_input(fv);
                    let outer_input = self.input_ports.alloc(InputPort {
                        node: Some(node),
                        output: Default::default(),
                    });
                    (outer_input, inner_output)
                })
                .collect();
            let bound_variables = bound_variables
                .into_iter()
                .map(|bv| data.add_input(bv))
                .collect();
            let outputs = output_weights
                .into_iter()
                .map(|weight| {
                    let inner_input = data.add_output();
                    let outer_output = self.output_ports.alloc(OutputPort {
                        node: Some(node),
                        inputs: Default::default(),
                        weight,
                    });
                    (inner_input, outer_output)
                })
                .collect();
            Node::Thunk(Thunk {
                data,
                free_variable_inputs,
                bound_variables,
                outputs,
            })
        })
    }

    fn link(
        &mut self,
        input: Id<InputPort<V, E>>,
        output: Id<OutputPort<V, E>>,
        strength: EdgeStrength,
    ) -> Result<(), V, E> {
        let i = &mut self.input_ports[input];
        let o = &mut self.output_ports[output];
        i.output
            .set(output)
            .map_err(|_| HyperGraphError::InputLinkError(input))?;
        o.inputs
            .insert(input, strength)
            .ok_or(HyperGraphError::OutputLinkError(output))?;
        Ok(())
    }

    fn input_port(&self, port: Id<InputPort<V, E>>) -> Result<&InputPort<V, E>, V, E> {
        self.input_ports
            .get(port)
            .ok_or(HyperGraphError::UnknownInput(port))
    }

    fn output_port(&self, port: Id<OutputPort<V, E>>) -> Result<&OutputPort<V, E>, V, E> {
        self.output_ports
            .get(port)
            .ok_or(HyperGraphError::UnknownOutput(port))
    }

    #[allow(clippy::type_complexity)]
    fn node_inputs(
        &self,
        node: Id<Node<V, E>>,
    ) -> Result<Box<dyn Iterator<Item = Id<InputPort<V, E>>> + '_>, V, E> {
        Ok(
            match self
                .nodes
                .get(node)
                .ok_or(HyperGraphError::UnknownNode(node))?
            {
                Node::Operation(Operation { inputs, .. }) => Box::new(inputs.iter().copied()),
                Node::Thunk(Thunk {
                    free_variable_inputs,
                    ..
                }) => Box::new(free_variable_inputs.left_values().copied()),
            },
        )
    }

    #[allow(clippy::type_complexity)]
    fn node_outputs(
        &self,
        node: Id<Node<V, E>>,
    ) -> Result<Box<dyn Iterator<Item = Id<OutputPort<V, E>>> + '_>, V, E> {
        Ok(
            match self
                .nodes
                .get(node)
                .ok_or(HyperGraphError::UnknownNode(node))?
            {
                Node::Operation(Operation { outputs, .. }) => Box::new(outputs.iter().copied()),
                Node::Thunk(Thunk { outputs, .. }) => Box::new(outputs.right_values().copied()),
            },
        )
    }
}

#[derive(Getters)]
pub struct HyperGraph<V, E> {
    data: Data<V, E>,
    #[get = "pub"]
    inputs: HashSet<Id<OutputPort<V, E>>>,
    #[get = "pub"]
    outputs: HashSet<Id<InputPort<V, E>>>,
}

macro_rules! delegate_data_methods {
    () => {
        delegate! {
            to self.data {
                pub fn add_operation(&mut self, input_len: usize, output_weights: Vec<E>, weight: V) -> Id<Node<V, E>>;
                pub fn add_thunk(&mut self, free_variables: Vec<E>, bound_variables: Vec<E>, output_weights: Vec<E>) -> Id<Node<V, E>>;
                pub fn link(&mut self, input: Id<InputPort<V, E>>, output: Id<OutputPort<V, E>>, strength: EdgeStrength) -> Result<(), V, E>;
                pub fn input_port(&self, port: Id<InputPort<V, E>>) -> Result<&InputPort<V, E>, V, E>;
                pub fn output_port(&self, port: Id<OutputPort<V, E>>) -> Result<&OutputPort<V, E>, V, E>;
                pub fn node_inputs(&self, node: Id<Node<V, E>>) -> Result<Box<dyn Iterator<Item = Id<InputPort<V, E>>> + '_>, V, E>;
                pub fn node_outputs(&self, node: Id<Node<V, E>>) -> Result<Box<dyn Iterator<Item = Id<OutputPort<V, E>>> + '_>, V, E>;
            }
        }
    };
}

impl<V, E> HyperGraph<V, E> {
    pub fn new(inputs: Vec<E>, output_len: usize) -> Self {
        let mut data: Data<V, E> = Default::default();
        let inputs = inputs.into_iter().map(|i| data.add_input(i)).collect();
        let outputs = (0..output_len).map(|_| data.add_output()).collect();
        HyperGraph {
            data,
            inputs,
            outputs,
        }
    }

    delegate_data_methods! {}
}

pub enum Node<V, E> {
    Operation(Operation<V, E>),
    Thunk(Thunk<V, E>),
}

pub struct Operation<V, E> {
    weight: V,
    inputs: Vec<Id<InputPort<V, E>>>,
    outputs: Vec<Id<OutputPort<V, E>>>,
}

type InOutMap<V, E> = BiMap<Id<InputPort<V, E>>, Id<OutputPort<V, E>>>;

pub struct Thunk<V, E> {
    data: Data<V, E>,
    free_variable_inputs: InOutMap<V, E>,
    bound_variables: Vec<Id<OutputPort<V, E>>>,
    outputs: InOutMap<V, E>,
}

impl<V, E> Thunk<V, E> {
    delegate_data_methods! {}
}
