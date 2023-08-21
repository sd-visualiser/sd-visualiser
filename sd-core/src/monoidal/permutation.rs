use std::collections::{HashMap, VecDeque};

use crate::{
    common::{Direction, Link},
    hypergraph::generic::Ctx,
};

/// Consume the first `n` items of an iterator `iter`
pub(crate) fn advance_by<I: Iterator>(iter: &mut I, n: usize) {
    if n != 0 {
        iter.nth(n - 1);
    }
}

/// Possible destinations of an edge in a "permutation layer"
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PermutationOutput {
    /// The edge passes through the layer to the edge indexed by the given number
    Output(usize),
    /// The edge is deleted in this layer
    Deleted,
    /// The edge is part of a cup or cap with the edge indexed by the given number
    Paired(usize),
}

impl From<PermutationOutput> for Option<usize> {
    fn from(value: PermutationOutput) -> Self {
        if let PermutationOutput::Output(a) = value {
            Some(a)
        } else {
            None
        }
    }
}

/// Calculates the permutation between two layers
///
/// # Inputs
/// `start`: An iterator identifying the items before permuting
/// `end`: An iterator identifying the items after permuting
///
/// # Returns
/// The items in `start` paired with a `PermutationOutput` specifying where they should be sent
pub(crate) fn generate_permutation<'a, T: Ctx>(
    start: impl Iterator<Item = Link<T>> + 'a,
    end: impl Iterator<Item = Link<T>> + 'a,
) -> Vec<(Link<T>, PermutationOutput)> {
    // Create a mapping of edges to indices they appear in the output
    let mut end_map: HashMap<Link<T>, VecDeque<usize>> = HashMap::default();
    for (idx, x) in end.enumerate() {
        end_map.entry(x).or_default().push_back(idx);
    }

    // Initialise the output by assigning each edge to be deleted
    let mut out: Vec<_> = start.map(|x| (x, PermutationOutput::Deleted)).collect();

    // Pair up edges that need pairing
    for i in 0..out.len() {
        let k @ (x, dir) = &out[i].0;
        if *dir == Direction::Backward && !end_map.contains_key(k) {
            if let Some(j) = out
                .iter()
                .enumerate()
                .filter(|(_, ((y, dir), _))| y == x && *dir == Direction::Forward)
                .map(|(a, _)| a)
                .min_by_key(|a| a.abs_diff(i))
            {
                out[j].1 = PermutationOutput::Paired(i);
                out[i].1 = PermutationOutput::Paired(j);
            }
        }
    }

    // Don't delete edges that appear in the outputs
    for (k, output) in &mut out {
        if *output == PermutationOutput::Deleted {
            if let Some(u) = end_map.get_mut(k).and_then(VecDeque::pop_front) {
                *output = PermutationOutput::Output(u);
            }
        }
    }

    out
}
