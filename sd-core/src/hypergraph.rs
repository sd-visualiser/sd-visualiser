use slab::Slab;
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Debug,
    ops::Index,
};
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeIndex(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PortIndex(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Debug, Error, Clone)]
pub enum HyperGraphError {
    #[error("No node at index `{0:?}`")]
    UnknownNode(NodeIndex),
    #[error("Node `{:?}` has no port `{:?}`", .0.node, .0.index)]
    UnknownPort(Port),
}

/// Hypergraph with hyperedges/nodes with weights E and hypervertices/wires
#[derive(Clone)]
pub struct HyperGraph<E> {
    nodes: Slab<NodeInfo<E>>,
}

impl<E> Index<NodeIndex> for HyperGraph<E> {
    type Output = Node<E>;
    fn index(&self, index: NodeIndex) -> &Self::Output {
        &self.nodes[index.0].data
    }
}

impl<E> Default for HyperGraph<E> {
    fn default() -> Self {
        let mut g = Self::new();
        g.add_node(Node::Input, vec![], 0).unwrap();
        g.add_node(Node::Output, vec![], 0).unwrap();
        g
    }
}

#[derive(Debug, Clone)]
pub enum Node<E> {
    Weight(E),
    Input,
    Output,
    Thunk { args: usize, body: HyperGraph<E> },
}

impl<E> Node<E> {
    pub fn w<F: Into<E>>(weight: F) -> Self {
        Node::Weight(weight.into())
    }

    pub fn is_input(&self) -> bool {
        matches!(self, Node::Input)
    }

    pub fn is_output(&self) -> bool {
        matches!(self, Node::Output)
    }
}

#[derive(Debug, Clone)]
pub struct NodeInfo<E> {
    data: Node<E>,
    inputs: Vec<Port>,
    outputs: Vec<BTreeSet<Port>>,
}

impl<E> HyperGraph<E> {
    /// Generate a new graph
    pub fn new() -> Self {
        HyperGraph { nodes: Slab::new() }
    }

    /// Adds a new node to a graph with specified node data, a list of ports to obtain inputs from
    pub fn add_node(
        &mut self,
        data: Node<E>,
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

    pub fn get(&self, key: NodeIndex) -> Result<&Node<E>, HyperGraphError> {
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

    pub fn get_inputs(
        &self,
        key: NodeIndex,
    ) -> Result<impl Iterator<Item = Port> + '_, HyperGraphError> {
        let info = self.get_info(key)?;
        Ok(info.inputs.iter().copied())
    }

    pub fn number_of_inputs(&self, key: NodeIndex) -> Result<usize, HyperGraphError> {
        let info = self.get_info(key)?;
        Ok(info.inputs.len())
    }

    pub fn nodes(&self) -> impl Iterator<Item = (NodeIndex, &Node<E>)> {
        self.nodes.iter().map(|(x, d)| (NodeIndex(x), &d.data))
    }

    pub fn edges(&self) -> impl Iterator<Item = (Port, &BTreeSet<Port>)> {
        self.nodes.iter().flat_map(|(node, d)| {
            d.outputs
                .iter()
                .enumerate()
                .map(move |(index, targets)| ((node, index).into(), targets))
        })
    }

    pub fn ranks_from_end(
        &self,
    ) -> (
        BTreeSet<NodeIndex>,
        Vec<BTreeSet<NodeIndex>>,
        BTreeSet<NodeIndex>,
    ) {
        let mut nodes: BTreeMap<NodeIndex, &NodeInfo<E>> =
            self.nodes.iter().map(|(x, d)| (NodeIndex(x), d)).collect();

        let inputs: BTreeSet<NodeIndex> = nodes
            .iter()
            .filter_map(|(x, d)| if d.data.is_input() { Some(*x) } else { None })
            .collect();
        let outputs: BTreeSet<NodeIndex> = nodes
            .iter()
            .filter_map(|(x, d)| if d.data.is_output() { Some(*x) } else { None })
            .collect();

        nodes.retain(|x, _| !inputs.contains(x) && !outputs.contains(x));

        let mut ranks: Vec<BTreeSet<NodeIndex>> = vec![];
        let mut collected: BTreeSet<NodeIndex> = outputs.clone();

        while !nodes.is_empty() {
            let mut next_rank = BTreeSet::new();
            for (n, o) in nodes.iter() {
                if o.outputs
                    .iter()
                    .flat_map(|x| x.iter().map(|y| y.node))
                    .all(|z| collected.contains(&z))
                {
                    collected.insert(*n);
                    next_rank.insert(*n);
                }
            }
            nodes.retain(|x, _| !next_rank.contains(x));

            ranks.push(next_rank);
        }

        (inputs, ranks, outputs)
    }
}

impl<E: Debug> Debug for HyperGraph<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Hypergraph")
            .field("nodes", &self.nodes().collect::<Vec<_>>())
            .field("edges", &self.edges().collect::<Vec<_>>())
            .finish()
    }
}
