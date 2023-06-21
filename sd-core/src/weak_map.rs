use std::{
    fmt::Debug,
    hash::{Hash, Hasher},
    ops::{Index, IndexMut},
};

use derivative::Derivative;
use indexmap::IndexMap;

#[derive(Clone, Debug, Derivative)]
#[derivative(Default(bound = ""))]
pub struct WeakMap<S, T>(pub(crate) IndexMap<S, T>);

impl<S, T> From<IndexMap<S, T>> for WeakMap<S, T> {
    fn from(value: IndexMap<S, T>) -> Self {
        WeakMap(value)
    }
}

impl<S: PartialEq, T: PartialEq> PartialEq for WeakMap<S, T> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }

        self.0.iter().zip(other.0.iter()).all(|(x, y)| x == y)
    }
}

impl<S: PartialEq + Eq, T: PartialEq + Eq> Eq for WeakMap<S, T> {}

impl<S: PartialEq + Eq + Hash, T> Index<&S> for WeakMap<S, T> {
    type Output = T;

    fn index(&self, index: &S) -> &Self::Output {
        &self.0[index]
    }
}

impl<S: PartialEq + Eq + Hash, T> IndexMut<&S> for WeakMap<S, T> {
    fn index_mut(&mut self, index: &S) -> &mut Self::Output {
        &mut self.0[index]
    }
}

impl<S: Hash, T: Hash> Hash for WeakMap<S, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for x in self.0.iter() {
            x.hash(state);
        }
    }
}
