use std::collections::{HashSet, VecDeque};

use itertools::Itertools;

use super::{Node, OutPort};

impl<V, E> Node<V, E> {
    #[must_use]
    pub fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = OutPort<V, E>> + '_> {
        match self {
            Node::Operation(op) => Box::new(op.inputs()),
            Node::Thunk(thunk) => Box::new(thunk.inputs()),
        }
    }

    pub fn successors(&self) -> impl Iterator<Item = Self> + '_ {
        self.outputs()
            .flat_map(|outport| outport.links().filter_map(|inport| inport.node()))
            .unique()
    }

    // Returns successors at the same thunk depth
    pub fn flat_successors(&self) -> impl Iterator<Item = Self> + '_ {
        let backlink = self.backlink();
        self.successors()
            .filter_map(move |node| {
                let mut last = node;
                let mut next = last.backlink();
                while next != backlink {
                    last = Node::Thunk(next?);
                    next = last.backlink();
                }
                Some(last)
            })
            .unique()
    }

    pub fn predecessors(&self) -> impl Iterator<Item = Self> + '_ {
        self.inputs()
            .filter_map(|out_port| out_port.node())
            .unique()
    }

    #[inline]
    pub fn forward_reachable(&self) -> impl Iterator<Item = Self> {
        self.forward_reachable_n(usize::MAX)
    }

    pub fn forward_reachable_n(&self, depth_limit: usize) -> impl Iterator<Item = Self> {
        NReachable {
            depth_limit,
            seen: HashSet::default(),
            frontier: [(0, self.clone())].into_iter().collect(),
            next_nodes: Self::boxed_sucessors,
        }
    }

    #[inline]
    pub fn backward_reachable(&self) -> impl Iterator<Item = Self> {
        self.backward_reachable_n(usize::MAX)
    }

    pub fn backward_reachable_n(&self, depth_limit: usize) -> impl Iterator<Item = Self> {
        NReachable {
            depth_limit,
            seen: HashSet::default(),
            frontier: [(0, self.clone())].into_iter().collect(),
            next_nodes: Self::boxed_predecessors,
        }
    }

    #[inline]
    #[must_use]
    pub fn bidirectional_reachable(&self) -> HashSet<Self> {
        self.bidirectional_reachable_n(usize::MAX)
    }

    #[must_use]
    pub fn bidirectional_reachable_n(&self, depth_limit: usize) -> HashSet<Self> {
        let forward: HashSet<_> = self.forward_reachable_n(depth_limit).collect();
        let backward: HashSet<_> = self.backward_reachable_n(depth_limit).collect();
        forward.intersection(&backward).cloned().collect()
    }

    #[inline]
    fn boxed_sucessors(node: &Node<V, E>) -> Box<dyn Iterator<Item = Node<V, E>> + '_> {
        Box::new(node.successors())
    }

    #[inline]
    fn boxed_predecessors(node: &Node<V, E>) -> Box<dyn Iterator<Item = Node<V, E>> + '_> {
        Box::new(node.predecessors())
    }
}

type NextFn<T> = fn(&T) -> Box<dyn Iterator<Item = T> + '_>;
pub struct NReachable<V, E> {
    depth_limit: usize,
    seen: HashSet<Node<V, E>>,
    // invariant: for each (i, node) in frontier, i increases monotonically
    frontier: VecDeque<(usize, Node<V, E>)>,
    next_nodes: NextFn<Node<V, E>>,
}

impl<V, E> NReachable<V, E> {
    #[inline]
    pub fn forward_from(nodes: impl IntoIterator<Item = Node<V, E>>) -> Self {
        Self::forward_from_n(nodes, usize::MAX)
    }

    pub fn forward_from_n(nodes: impl IntoIterator<Item = Node<V, E>>, depth_limit: usize) -> Self {
        Self {
            depth_limit,
            seen: HashSet::default(),
            frontier: nodes.into_iter().map(|node| (0, node)).collect(),
            next_nodes: Node::boxed_sucessors,
        }
    }

    #[inline]
    pub fn backward_from(nodes: impl IntoIterator<Item = Node<V, E>>) -> Self {
        Self::backward_from_n(nodes, usize::MAX)
    }

    pub fn backward_from_n(
        nodes: impl IntoIterator<Item = Node<V, E>>,
        depth_limit: usize,
    ) -> Self {
        Self {
            depth_limit,
            seen: HashSet::default(),
            frontier: nodes.into_iter().map(|node| (0, node)).collect(),
            next_nodes: Node::boxed_predecessors,
        }
    }

    #[inline]
    #[must_use]
    pub fn bidirectional_from(
        nodes: impl IntoIterator<Item = Node<V, E>> + Clone,
    ) -> HashSet<Node<V, E>> {
        Self::bidirectional_from_n(nodes, usize::MAX)
    }

    #[must_use]
    pub fn bidirectional_from_n(
        nodes: impl IntoIterator<Item = Node<V, E>> + Clone,
        depth_limit: usize,
    ) -> HashSet<Node<V, E>> {
        let forward: HashSet<_> = Self::forward_from_n(nodes.clone(), depth_limit).collect();
        let backward: HashSet<_> = Self::backward_from_n(nodes, depth_limit).collect();
        forward.intersection(&backward).cloned().collect()
    }

    #[inline]
    pub fn bump_depth_limit(&mut self) {
        self.increase_depth_limit(1);
    }

    pub fn increase_depth_limit(&mut self, n: usize) {
        self.depth_limit += n;
    }
}

impl<V, E> Iterator for NReachable<V, E> {
    type Item = Node<V, E>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.frontier.pop_front() {
            None => None,
            Some((d, node)) if d > self.depth_limit => {
                self.frontier.push_front((d, node));
                None
            }
            Some((d, node)) => {
                self.seen.insert(node.clone());
                self.frontier.extend(
                    (self.next_nodes)(&node)
                        .filter_map(|s| (!self.seen.contains(&s)).then_some((d + 1, s))),
                );
                Some(node)
            }
        }
    }
}
