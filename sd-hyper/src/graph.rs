use slab::Slab;
use std::{collections::BTreeSet, fmt::Debug};
use thiserror::Error;

use crate::concat_iter::concat_iter;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Port {
    pub node: usize,
    pub index: usize,
}

#[derive(Debug, Error)]
pub enum HyperGraphError {
    #[error("No node at index `{0}`")]
    UnknownNode(usize),
    #[error("Node `{}` has no port `{}`", .0.node, .0.index)]
    UnknownPort(Port),
}

/// HyperGraph with hyperedges/nodes with weights E and vertices/wires
#[derive(Clone)]
pub struct Graph<E> {
    nodes: Slab<NodeInfo<E>>,
}

#[derive(Debug, Clone)]
pub struct NodeInfo<E> {
    data: E,
    inputs: Vec<Port>,
    outputs: Vec<BTreeSet<Port>>,
}

impl<E> Graph<E> {
    /// Generate a new graph
    pub fn new() -> Self {
        Graph { nodes: Slab::new() }
    }

    /// Adds a new node to a graph with specified node data, a list of ports to obtain inputs from
    pub fn add_node(
        &mut self,
        data: E,
        inputs: Vec<Port>,
        output_ports: usize,
    ) -> Result<usize, HyperGraphError> {
        let next_node = self.nodes.vacant_key();

        for (i, port @ Port { node, index }) in inputs.iter().enumerate() {
            let input = self
                .nodes
                .get_mut(*node)
                .ok_or(HyperGraphError::UnknownNode(*node))?;
            let port_set = input
                .outputs
                .get_mut(*index)
                .ok_or(HyperGraphError::UnknownPort(*port))?;
            port_set.insert(Port {
                node: next_node,
                index: i,
            });
        }

        let outputs = vec![BTreeSet::new(); output_ports];

        let info = NodeInfo {
            data,
            inputs,
            outputs,
        };

        let idx = self.nodes.insert(info);

        Ok(idx)
    }

    fn get_info(&self, key: usize) -> Result<&NodeInfo<E>, HyperGraphError> {
        self.nodes.get(key).ok_or(HyperGraphError::UnknownNode(key))
    }

    pub fn get(&self, key: usize) -> Result<&E, HyperGraphError> {
        let info = self.get_info(key)?;
        Ok(&info.data)
    }

    pub fn input_ports(
        &self,
        key: usize,
    ) -> Result<impl Iterator<Item = Port> + '_, HyperGraphError> {
        let info = self.get_info(key)?;
        Ok(info.inputs.iter().copied())
    }

    pub fn nodes(&self) -> impl Iterator<Item = (usize, &E)> {
        self.nodes.iter().map(|(x, d)| (x, &d.data))
    }

    pub fn edges(&self) -> impl Iterator<Item = (Port, &BTreeSet<Port>)> {
        concat_iter(self.nodes.iter().map(|(node, d)| {
            d.outputs
                .iter()
                .enumerate()
                .map(move |(index, targets)| (Port { node, index }, targets))
        }))
    }
}

impl<E: Debug> Debug for Graph<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Hypergraph")
            .field("nodes", &self.nodes().collect::<Vec<_>>())
            .field("edges", &self.edges().collect::<Vec<_>>())
            .finish()
    }
}
