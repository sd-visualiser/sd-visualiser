use std::hash::{Hash, Hasher};

use delegate::delegate;
use indexmap::IndexMap;

/// A wrapper around [`IndexMap`] that implements [`Eq`] and [`Hash`] using the insertion order.
#[derive(Clone, Debug)]
pub struct WeakMap<K, V>(IndexMap<K, V>);

impl<K, V> std::ops::Index<&K> for WeakMap<K, V>
where
    K: Hash + Eq,
{
    type Output = V;

    fn index(&self, key: &K) -> &Self::Output {
        &self.0[key]
    }
}

impl<K, V> std::ops::IndexMut<&K> for WeakMap<K, V>
where
    K: Hash + Eq,
{
    fn index_mut(&mut self, key: &K) -> &mut Self::Output {
        &mut self.0[key]
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
        for x in &self.0 {
            x.hash(state);
        }
    }
}

#[allow(clippy::inline_always)]
#[allow(clippy::must_use_candidate)]
impl<K, V> WeakMap<K, V> {
    delegate! {
        to self.0 {
            pub fn iter(&self) -> indexmap::map::Iter<'_, K, V>;
            pub fn iter_mut(&mut self) -> indexmap::map::IterMut<'_, K, V>;
            pub fn keys(&self) -> indexmap::map::Keys<'_, K, V>;
            pub fn values(&self) -> indexmap::map::Values<'_, K, V>;
            pub fn values_mut(&mut self) -> indexmap::map::ValuesMut<'_, K, V>;
        }
    }
}

impl<K, V> FromIterator<(K, V)> for WeakMap<K, V>
where
    K: Eq + Hash,
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        WeakMap(IndexMap::from_iter(iter))
    }
}
