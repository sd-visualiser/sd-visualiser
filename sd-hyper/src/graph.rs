use slab::Slab;
use std::collections::BTreeSet;
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Port {
    node: usize,
    index: usize,
}

#[derive(Debug, Error)]
pub enum HyperGraphError {
    #[error("No node at index `{0}`")]
    UnknownNode(usize),
    #[error("Node `{}` has no port `{}`", .0.node, .0.index)]
    UnknownPort(Port),
}

/// HyperGraph with hyperedges/nodes with weights E and vertices/links with weights V
#[derive(Debug, Clone)]
pub struct Graph<E, V> {
    nodes: Slab<NodeInfo<E, V>>,
}

#[derive(Debug, Clone)]
pub struct NodeInfo<E, V> {
    data: E,
    inputs: Vec<Port>,
    outputs: Vec<(BTreeSet<Port>, V)>,
}

impl<E, V> Graph<E, V> {
    /// Generate a new graph
    pub fn new() -> Self {
        Graph { nodes: Slab::new() }
    }

    /// Adds a new node to a graph with specified node data, a list of ports to obtain inputs from
    pub fn add_node(
        &mut self,
        data: E,
        inputs: Vec<Port>,
        output_ports: Vec<V>,
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
            port_set.0.insert(Port {
                node: next_node,
                index: i,
            });
        }

        let outputs = output_ports
            .into_iter()
            .map(|x| (BTreeSet::new(), x))
            .collect();

        let info = NodeInfo {
            data,
            inputs,
            outputs,
        };

        let idx = self.nodes.insert(info);

        Ok(idx)
    }

    fn get_info(&self, key: usize) -> Result<&NodeInfo<E, V>, HyperGraphError> {
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
}
