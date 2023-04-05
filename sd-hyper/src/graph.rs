use slab::Slab;
use std::{collections::BTreeSet, fmt::Debug};
use thiserror::Error;

use crate::concat_iter::concat_iter;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NodeIndex(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct PortIndex(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Port {
    pub node: NodeIndex,
    pub index: PortIndex,
}

impl From<(usize, usize)> for Port {
    fn from(value: (usize, usize)) -> Self {
        Port {
            node: NodeIndex(value.0),
            index: PortIndex(value.1),
        }
    }
}

#[derive(Debug, Error)]
pub enum HyperGraphError {
    #[error("No node at index `{0:?}`")]
    UnknownNode(NodeIndex),
    #[error("Node `{:?}` has no port `{:?}`", .0.node, .0.index)]
    UnknownPort(Port),
}

/// HyperGraph with hyperedges/nodes with weights E and vertices/wires
#[derive(Clone, Default)]
pub struct Graph<E> {
    nodes: Slab<NodeInfo<E>>,
}

#[derive(Debug, Clone)]
pub enum GraphNode<E> {
    Weight(E),
    Input,
    Output,
    Thunk { args: usize, body: Box<Graph<E>> },
}

impl<E> GraphNode<E> {
    pub fn w<F: Into<E>>(weight: F) -> Self {
        GraphNode::Weight(weight.into())
    }

    pub fn is_input(&self) -> bool {
        matches!(self, GraphNode::Input)
    }

    pub fn is_output(&self) -> bool {
        matches!(self, GraphNode::Output)
    }
}

#[derive(Debug, Clone)]
pub struct NodeInfo<E> {
    data: GraphNode<E>,
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
        data: GraphNode<E>,
        inputs: Vec<Port>,
        output_ports: usize,
    ) -> Result<NodeIndex, HyperGraphError> {
        let next_node = self.nodes.vacant_key();

        for (i, port @ Port { node, index }) in inputs.iter().enumerate() {
            let input = self
                .nodes
                .get_mut(node.0)
                .ok_or(HyperGraphError::UnknownNode(*node))?;

            let port_set = input
                .outputs
                .get_mut(index.0)
                .ok_or(HyperGraphError::UnknownPort(*port))?;
            port_set.insert((next_node, i).into());
        }

        let outputs = vec![BTreeSet::new(); output_ports];

        let info = NodeInfo {
            data,
            inputs,
            outputs,
        };

        let idx = self.nodes.insert(info);

        Ok(NodeIndex(idx))
    }

    fn get_info(&self, key: NodeIndex) -> Result<&NodeInfo<E>, HyperGraphError> {
        self.nodes
            .get(key.0)
            .ok_or(HyperGraphError::UnknownNode(key))
    }

    pub fn get(&self, key: NodeIndex) -> Result<&GraphNode<E>, HyperGraphError> {
        let info = self.get_info(key)?;
        Ok(&info.data)
    }

    pub fn get_outputs(
        &self,
        key: NodeIndex,
    ) -> Result<impl Iterator<Item = &BTreeSet<Port>>, HyperGraphError> {
        let info = self.get_info(key)?;
        Ok(info.outputs.iter())
    }

    pub fn number_of_outputs(&self, key: NodeIndex) -> Result<usize, HyperGraphError> {
        let info = self.get_info(key)?;
        Ok(info.outputs.len())
    }

    pub fn input_ports(
        &self,
        key: NodeIndex,
    ) -> Result<impl Iterator<Item = Port> + '_, HyperGraphError> {
        let info = self.get_info(key)?;
        Ok(info.inputs.iter().copied())
    }

    pub fn nodes(&self) -> impl Iterator<Item = (NodeIndex, &GraphNode<E>)> {
        self.nodes.iter().map(|(x, d)| (NodeIndex(x), &d.data))
    }

    pub fn edges(&self) -> impl Iterator<Item = (Port, &BTreeSet<Port>)> {
        concat_iter(self.nodes.iter().map(|(node, d)| {
            d.outputs
                .iter()
                .enumerate()
                .map(move |(index, targets)| ((node, index).into(), targets))
        }))
    }

    pub fn ranks_from_end(&self) -> Vec<BTreeSet<NodeIndex>> {
        let mut nodes: BTreeSet<(NodeIndex, &Vec<BTreeSet<Port>>)> = self
            .nodes
            .iter()
            .map(|(x, d)| (NodeIndex(x), &d.outputs))
            .collect();
        let mut ranks: Vec<BTreeSet<NodeIndex>> = vec![];
        let mut collected: BTreeSet<NodeIndex> = BTreeSet::new();

        while !nodes.is_empty() {
            let mut next_rank = BTreeSet::new();
            for (n, o) in nodes.iter() {
                if concat_iter(o.iter().map(|x| x.iter().map(|y| y.node)))
                    .all(|z| collected.contains(&z))
                {
                    collected.insert(*n);
                    next_rank.insert(*n);
                }
            }
            nodes.retain(|x| !next_rank.contains(&x.0));

            ranks.push(next_rank);
        }

        ranks
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
