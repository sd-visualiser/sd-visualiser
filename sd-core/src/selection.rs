use std::ops::{Index, IndexMut};

use derivative::Derivative;
use indexmap::IndexMap;

use crate::{
    common::{Addr, Direction},
    hypergraph::reachability::NReachable,
    weak_map::WeakMap,
};

#[derive(Derivative)]
#[derivative(
    Clone(bound = "T::Edge: Clone, T::Thunk: Clone"),
    Hash(bound = ""),
    Default(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = "")
)]
pub struct SelectionMap<T: Addr>(WeakMap<T::Node, bool>);

impl<T> From<IndexMap<T::Node, bool>> for SelectionMap<T>
where
    T: Addr,
{
    fn from(map: IndexMap<T::Node, bool>) -> Self {
        Self(WeakMap(map))
    }
}

impl<T> Index<&T::Node> for SelectionMap<T>
where
    T: Addr,
{
    type Output = bool;

    fn index(&self, index: &T::Node) -> &Self::Output {
        &self.0[index]
    }
}

impl<T> IndexMut<&T::Node> for SelectionMap<T>
where
    T: Addr,
{
    fn index_mut(&mut self, index: &T::Node) -> &mut Self::Output {
        &mut self.0[index]
    }
}

impl<T> SelectionMap<T>
where
    T: Addr,
{
    /// Unselect all nodes.
    pub fn clear_selection(&mut self) {
        self.0
             .0
            .values_mut()
            .for_each(|selected| *selected = false);
    }

    /// Iterator of selected nodes.
    pub fn iter(&self) -> impl Iterator<Item = T::Node> + Clone + '_ {
        self.0
             .0
            .iter()
            .filter_map(|(node, selected)| selected.then_some(node))
            .cloned()
    }
}

impl<V, E> SelectionMap<(V, E)> {
    /// Extend the selection using reachability.
    /// If `direction` is `None`, extend the selection in both directions.
    /// Otherwise extend the selection in the given direction up to the given depth.
    pub fn extend_selection(&mut self, direction: Option<(Direction, usize)>) {
        macro_rules! extend {
            ($reachable:expr) => {
                for node in $reachable {
                    self[&node] = true;
                }
            };
        }
        match direction {
            None => {
                extend!(NReachable::bidirectional_from(self.iter()))
            }
            Some((Direction::Forward, depth)) => {
                extend!(NReachable::forward_from_n(self.iter(), depth))
            }
            Some((Direction::Backward, depth)) => {
                extend!(NReachable::backward_from_n(self.iter(), depth))
            }
        };
    }
}
