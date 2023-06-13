use std::{
    hash::{Hash, Hasher},
    ops::{Index, IndexMut},
};

use derivative::Derivative;
use indexmap::IndexMap;
use sd_core::hypergraph::{GraphView, HyperGraph, Thunk};

#[derive(Clone, Debug, Derivative)]
#[derivative(Default(bound = ""))]
pub struct Expanded<T>(IndexMap<T, bool>);

impl<T: PartialEq> PartialEq for Expanded<T> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }

        self.0.iter().zip(other.0.iter()).all(|(x, y)| x == y)
    }
}

impl<T: PartialEq + Eq> Eq for Expanded<T> {}

impl<T: PartialEq + Eq + Hash> Index<&T> for Expanded<T> {
    type Output = bool;

    fn index(&self, index: &T) -> &Self::Output {
        &self.0[index]
    }
}

impl<T: PartialEq + Eq + Hash> IndexMut<&T> for Expanded<T> {
    fn index_mut(&mut self, index: &T) -> &mut Self::Output {
        &mut self.0[index]
    }
}

impl<T: Hash> Hash for Expanded<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for x in self.0.iter() {
            x.hash(state);
        }
    }
}

impl<V, E> Expanded<Thunk<V, E>> {
    #[must_use]
    pub fn from_graph(graph: &HyperGraph<V, E>) -> Self {
        fn helper<V, E>(set: &mut IndexMap<Thunk<V, E>, bool>, thunk: Thunk<V, E>) {
            for t in thunk.thunks() {
                helper(set, t);
            }
            set.insert(thunk, true);
        }

        let mut set = IndexMap::new();

        for thunk in graph.thunks() {
            helper(&mut set, thunk);
        }

        Expanded(set)
    }
}
