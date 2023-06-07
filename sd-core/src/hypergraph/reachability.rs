use std::collections::{HashSet, VecDeque};

use itertools::Itertools;

use super::Node;

impl<V, E> Node<V, E> {
    pub fn successors(&self) -> impl Iterator<Item = Self> + '_ {
        self.outputs()
            .flat_map(|outport| outport.links().filter_map(|inport| inport.node()))
            .unique()
    }

    pub fn predecessors(&self) -> impl Iterator<Item = Self> + '_ {
        self.inputs()
            .filter_map(|inport| inport.link().node())
            .unique()
    }

    pub fn forward_reachable(&self) -> impl Iterator<Item = Self> {
        self.forward_reachable_n(usize::MAX)
    }

    pub fn forward_reachable_n(&self, depth_limit: usize) -> impl Iterator<Item = Self> {
        fn boxed_sucessors<V, E>(node: &Node<V, E>) -> Box<dyn Iterator<Item = Node<V, E>> + '_> {
            Box::new(node.successors())
        }
        NReachable {
            depth_limit,
            seen: HashSet::default(),
            frontier: [(0, self.clone())].into_iter().collect(),
            next_nodes: boxed_sucessors,
        }
    }

    pub fn backward_reachable(&self) -> impl Iterator<Item = Self> {
        self.backward_reachable_n(usize::MAX)
    }

    pub fn backward_reachable_n(&self, depth_limit: usize) -> impl Iterator<Item = Self> {
        fn boxed_predecessors<V, E>(
            node: &Node<V, E>,
        ) -> Box<dyn Iterator<Item = Node<V, E>> + '_> {
            Box::new(node.predecessors())
        }
        NReachable {
            depth_limit,
            seen: HashSet::default(),
            frontier: [(0, self.clone())].into_iter().collect(),
            next_nodes: boxed_predecessors,
        }
    }
}

type NextFn<T> = fn(&T) -> Box<dyn Iterator<Item = T> + '_>;
struct NReachable<V, E, const BUILT: bool> {
    depth_limit: usize,
    seen: HashSet<Node<V, E, BUILT>>,
    frontier: VecDeque<(usize, Node<V, E, BUILT>)>,
    next_nodes: NextFn<Node<V, E, BUILT>>,
}

impl<V, E, const BUILT: bool> Iterator for NReachable<V, E, BUILT> {
    type Item = Node<V, E, BUILT>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.frontier.pop_front() {
            None => None,
            Some((d, node)) => {
                self.seen.insert(node.clone());
                if d < self.depth_limit {
                    self.frontier.extend(
                        (self.next_nodes)(&node)
                            .filter_map(|s| (!self.seen.contains(&s)).then_some((d + 1, s)))
                            .collect::<Vec<_>>(),
                    );
                }
                Some(node)
            }
        }
    }
}
