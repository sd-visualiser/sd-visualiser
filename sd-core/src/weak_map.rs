use std::hash::{Hash, Hasher};

use derivative::Derivative;
use indexmap::IndexMap;

/// A wrapper around [`IndexMap`] that implements [`Eq`] and [`Hash`] using the insertion order.
#[derive(Clone, Debug, Derivative)]
#[derivative(Default(bound = ""))]
pub struct WeakMap<K, V>(pub(crate) IndexMap<K, V>);

impl<K, V> std::ops::Deref for WeakMap<K, V> {
    type Target = IndexMap<K, V>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<K, V> std::ops::DerefMut for WeakMap<K, V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<K, V> From<IndexMap<K, V>> for WeakMap<K, V> {
    fn from(map: IndexMap<K, V>) -> Self {
        WeakMap(map)
    }
}

impl<K: PartialEq, V: PartialEq> PartialEq for WeakMap<K, V> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }

        self.0.iter().zip(other.0.iter()).all(|(x, y)| x == y)
    }
}

impl<K: Eq, V: Eq> Eq for WeakMap<K, V> {}

impl<K: Hash, V: Hash> Hash for WeakMap<K, V> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for x in self.0.iter() {
            x.hash(state);
        }
    }
}
