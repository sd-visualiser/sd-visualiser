use std::{
    fmt::Debug,
    hash::{Hash, Hasher},
};

use derivative::Derivative;
use indexmap::IndexMap;

#[derive(Clone, Debug, Derivative)]
#[derivative(Default(bound = ""))]
pub struct WeakMap<S, T>(pub(crate) IndexMap<S, T>);

impl<S, T> std::ops::Deref for WeakMap<S, T> {
    type Target = IndexMap<S, T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<S, T> std::ops::DerefMut for WeakMap<S, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<S, T: Clone> WeakMap<S, T> {
    pub fn set_all(&mut self, value: T) {
        for x in self.0.values_mut() {
            *x = value.clone();
        }
    }
}

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

impl<S: Hash, T: Hash> Hash for WeakMap<S, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for x in self.0.iter() {
            x.hash(state);
        }
    }
}
