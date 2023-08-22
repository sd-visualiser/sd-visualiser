use std::ops::{Index, IndexMut};

use derivative::Derivative;
use indexmap::{IndexMap, IndexSet};

use crate::{
    common::Direction,
    hypergraph::{
        generic::{Ctx, Node},
        reachability::NReachable,
        traits::NodeLike,
        utils::find_ancestor,
    },
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
pub struct SelectionMap<T: Ctx>(WeakMap<Node<T>, bool>);

impl<T> From<IndexMap<Node<T>, bool>> for SelectionMap<T>
where
    T: Ctx,
{
    fn from(map: IndexMap<Node<T>, bool>) -> Self {
        Self(WeakMap(map))
    }
}

impl<T> Index<&Node<T>> for SelectionMap<T>
where
    T: Ctx,
{
    type Output = bool;

    fn index(&self, index: &Node<T>) -> &Self::Output {
        &self.0[index]
    }
}

impl<T> IndexMut<&Node<T>> for SelectionMap<T>
where
    T: Ctx,
{
    fn index_mut(&mut self, index: &Node<T>) -> &mut Self::Output {
        &mut self.0[index]
    }
}

impl<T> SelectionMap<T>
where
    T: Ctx,
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
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = Node<T>> + Clone + '_ {
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
    pub fn normalize(&mut self) {
        let root_selection = normalise_selection(self);
        for (k, v) in self.0 .0.iter_mut() {
            *v = contains_transitively::<T>(&root_selection, k);
        }
    }

    /// Iterator of selected nodes that are not contained in other selected nodes.
    ///
    /// This should only be used for normalised selections.
    #[must_use]
    pub fn roots(&self) -> impl DoubleEndedIterator<Item = Node<T>> + '_ {
        self.iter().filter(|node| {
            node.backlink()
                .map_or(true, |backlink| !self[&Node::Thunk(backlink)])
        })
    }

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

#[must_use]
fn normalise_selection<T: Ctx>(selection: &SelectionMap<T>) -> IndexSet<Node<T>> {
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

fn contains_transitively<T: Ctx>(selection: &IndexSet<Node<T>>, node: &Node<T>) -> bool {
    selection.contains(node)
        || node.backlink().map_or(false, |thunk| {
            contains_transitively::<T>(selection, &Node::Thunk(thunk))
        })
}
