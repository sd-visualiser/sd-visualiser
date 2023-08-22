use std::collections::{HashSet, VecDeque};

use indexmap::IndexSet;
use itertools::Itertools;

use super::{
    generic::{Ctx, Node},
    traits::{EdgeLike, NodeLike},
};

impl<T: Ctx> Node<T> {
    pub fn successors(&self) -> impl Iterator<Item = Self> + '_ {
        self.outputs()
            .flat_map(|edge| edge.targets().flatten().collect::<Vec<_>>())
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
        self.inputs().filter_map(|edge| edge.source()).unique()
    }

    #[inline]
    pub fn forward_reachable(&self) -> impl Iterator<Item = Self> {
        self.forward_reachable_n(usize::MAX)
    }

    pub fn forward_reachable_n(&self, depth_limit: usize) -> impl Iterator<Item = Self> {
        NReachable {
            depth_limit,
            seen: HashSet::default(),
            frontier: std::iter::once((0, self.clone())).collect(),
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
            frontier: std::iter::once((0, self.clone())).collect(),
            next_nodes: Self::boxed_predecessors,
        }
    }

    #[inline]
    #[must_use]
    pub fn bidirectional_reachable(&self) -> IndexSet<Self> {
        self.bidirectional_reachable_n(usize::MAX)
    }

    #[must_use]
    pub fn bidirectional_reachable_n(&self, depth_limit: usize) -> IndexSet<Self> {
        let forward: IndexSet<_> = self.forward_reachable_n(depth_limit).collect();
        let backward: IndexSet<_> = self.backward_reachable_n(depth_limit).collect();
        forward.intersection(&backward).cloned().collect()
    }

    #[inline]
    fn boxed_sucessors(&self) -> Box<dyn Iterator<Item = Self> + '_> {
        Box::new(self.successors())
    }

    #[inline]
    fn boxed_predecessors(&self) -> Box<dyn Iterator<Item = Self> + '_> {
        Box::new(self.predecessors())
    }
}

type NextFn<T> = fn(&T) -> Box<dyn Iterator<Item = T> + '_>;
pub struct NReachable<T: Ctx> {
    depth_limit: usize,
    seen: HashSet<Node<T>>,
    // invariant: for each (i, node) in frontier, i increases monotonically
    frontier: VecDeque<(usize, Node<T>)>,
    next_nodes: NextFn<Node<T>>,
}

impl<T: Ctx> NReachable<T> {
    #[inline]
    pub fn forward_from(nodes: impl IntoIterator<Item = Node<T>>) -> Self {
        Self::forward_from_n(nodes, usize::MAX)
    }

    pub fn forward_from_n(nodes: impl IntoIterator<Item = Node<T>>, depth_limit: usize) -> Self {
        Self {
            depth_limit,
            seen: HashSet::default(),
            frontier: nodes.into_iter().map(|node| (0, node)).collect(),
            next_nodes: Node::boxed_sucessors,
        }
    }

    #[inline]
    pub fn backward_from(nodes: impl IntoIterator<Item = Node<T>>) -> Self {
        Self::backward_from_n(nodes, usize::MAX)
    }

    pub fn backward_from_n(nodes: impl IntoIterator<Item = Node<T>>, depth_limit: usize) -> Self {
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
        nodes: impl IntoIterator<Item = Node<T>> + Clone,
    ) -> IndexSet<Node<T>> {
        Self::bidirectional_from_n(nodes, usize::MAX)
    }

    #[must_use]
    pub fn bidirectional_from_n(
        nodes: impl IntoIterator<Item = Node<T>> + Clone,
        depth_limit: usize,
    ) -> IndexSet<Node<T>> {
        let forward: IndexSet<_> = Self::forward_from_n(nodes.clone(), depth_limit).collect();
        let backward: IndexSet<_> = Self::backward_from_n(nodes, depth_limit).collect();
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

impl<T: Ctx> Iterator for NReachable<T> {
    type Item = Node<T>;

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
