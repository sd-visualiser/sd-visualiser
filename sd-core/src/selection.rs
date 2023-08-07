use std::ops::{Index, IndexMut};

use derivative::Derivative;
use indexmap::{IndexMap, IndexSet};

use crate::{
    common::{Addr, Direction},
    hypergraph::{reachability::NReachable, traits::NodeLike},
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
    #[must_use]
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = T::Node> + Clone + '_ {
        self.0
             .0
            .iter()
            .filter_map(|(node, selected)| selected.then_some(node))
            .cloned()
    }

    /// Number of selected nodes.
    #[must_use]
    pub fn len(&self) -> usize {
        self.iter().count()
    }

    /// Whether any nodes are selected.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Normalise the selection.
    pub fn normalize(&mut self)
    where
        T::Node: NodeLike<T = T>,
        T::Thunk: NodeLike<T = T>,
    {
        let root_selection = normalise_selection(self);
        for (k, v) in self.0 .0.iter_mut() {
            *v = contains_transitively::<T>(&root_selection, k);
        }
    }

    /// Iterator of selected nodes that are not contained in other selected nodes.
    ///
    /// This should only be used for normalised selections.
    #[must_use]
    pub fn roots(&self) -> impl DoubleEndedIterator<Item = T::Node> + '_
    where
        T::Node: NodeLike<T = T>,
    {
        self.iter().filter(|node| {
            node.backlink()
                .map_or(true, |backlink| !self[&T::Node::from(backlink)])
        })
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

/// Finds the ancestor of given node which is contained in containing, returning none if no such ancestor exists
fn find_ancestor<T: Addr>(containing: &Option<T::Thunk>, mut node: T::Node) -> Option<T::Node>
where
    T::Node: NodeLike<T = T>,
{
    while &node.backlink() != containing {
        node = node.backlink()?.into();
    }
    Some(node)
}

#[must_use]
fn normalise_selection<T: Addr>(selection: &SelectionMap<T>) -> IndexSet<T::Node>
where
    T::Node: NodeLike<T = T>,
    T::Thunk: NodeLike<T = T>,
{
    let selected: Vec<_> = selection.iter().collect();
    if let Some(op) = selected.first() {
        let mut containing = op.backlink();
        for node in &selected[1..] {
            while find_ancestor::<T>(&containing, node.clone()).is_none() {
                containing = containing.unwrap().backlink();
            }
        }

        selected
            .into_iter()
            .map(|node| find_ancestor::<T>(&containing, node).unwrap())
            .collect()
    } else {
        IndexSet::new()
    }
}

fn contains_transitively<T: Addr>(selection: &IndexSet<T::Node>, node: &T::Node) -> bool
where
    T::Node: NodeLike<T = T>,
{
    selection.contains(node)
        || node.backlink().map_or(false, |thunk| {
            contains_transitively::<T>(selection, &thunk.into())
        })
}
