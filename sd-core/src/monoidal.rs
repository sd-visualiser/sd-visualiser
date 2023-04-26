use std::{collections::HashSet, fmt::Debug};

use crate::{
    common::{InOut, Slice},
    hypergraph::NodeIndex,
    monoidal_wired::{MonoidalWiredGraph, MonoidalWiredOp, Wiring},
};
use itertools::Itertools;

impl Wiring {
    pub fn to_slices<O>(&self) -> Vec<Slice<MonoidalOp<O>>> {
        let mut slices = Slice::permutation_to_swaps(self.backward.clone());
        let mut copy_slice = Vec::new();
        let mut is_empty = true;
        for x in &self.forward {
            let copies = x.len();
            if copies != 1 {
                is_empty = false;
            }
            copy_slice.push(MonoidalOp::Copy { copies })
        }
        if !is_empty {
            slices.push(Slice { ops: copy_slice });
        }
        slices.reverse();
        slices
    }
}

impl<O> Slice<MonoidalOp<O>> {
    pub fn permutation_to_swaps(mut permutation: Vec<usize>) -> Vec<Self> {
        let mut slices = Vec::new();

        let mut finished = false;

        while !finished {
            let mut slice_ops = Vec::new();
            finished = true; // We set finished back to false if we make a swap
            let mut i = 0; // Iterate through windows
            while i + 1 < permutation.len() {
                if permutation[i] <= permutation[i + 1] {
                    i += 1;
                    slice_ops.push(MonoidalOp::ID);
                } else {
                    finished = false;
                    slice_ops.push(MonoidalOp::Swap);
                    permutation.swap(i, i + 1);
                    i += 2;
                }
            }
            if i + 1 == permutation.len() {
                slice_ops.push(MonoidalOp::ID);
            }
            if !finished {
                // Slice is non trivial
                slices.push(Slice { ops: slice_ops });
            }
        }

        slices
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct MonoidalGraph<O> {
    pub inputs: usize,
    pub slices: Vec<Slice<MonoidalOp<O>>>,
}

impl<O> MonoidalGraph<O> {
    pub fn selected(&self) -> Option<(Vec<NodeIndex>, HashSet<NodeIndex>)> {
        // Addresses of selected nodes.
        let mut selections = HashSet::default();

        // Internal selection state (prefix and addresses) of the first selected thunk.
        let mut thunk = None;

        for slice in &self.slices {
            for op in &slice.ops {
                match op {
                    MonoidalOp::Operation { addr, selected, .. } => {
                        if *selected {
                            selections.insert(*addr);
                        }
                    }
                    MonoidalOp::Thunk { addr, body, .. } => {
                        if let Some((mut prefix, subselections)) = body.selected() {
                            selections.insert(*addr);

                            if thunk.is_none() {
                                prefix.insert(0, *addr);
                                thunk = Some((prefix, subselections));
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        match (selections.len(), thunk) {
            (0, _) => None,
            (1, Some((prefix, subselections))) => Some((prefix, subselections)),
            _ => Some((vec![], selections)),
        }
    }
}

impl<O> Default for MonoidalGraph<O> {
    fn default() -> Self {
        MonoidalGraph {
            inputs: 0,
            slices: vec![],
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum MonoidalOp<O> {
    Copy {
        copies: usize,
    },
    Operation {
        addr: NodeIndex,
        inputs: usize,
        op_name: O,
        selected: bool,
    },
    Thunk {
        addr: NodeIndex,
        args: usize,
        body: MonoidalGraph<O>,
        expanded: bool,
    },
    Swap,
}

impl<O> InOut for MonoidalOp<O> {
    fn number_of_inputs(&self) -> usize {
        match self {
            Self::Copy { .. } => 1,
            Self::Operation { inputs, .. } => *inputs,
            Self::Thunk { args, body, .. } => body.inputs - args,
            Self::Swap => 2,
        }
    }

    fn number_of_outputs(&self) -> usize {
        match self {
            Self::Copy { copies } => *copies,
            Self::Operation { .. } => 1,
            Self::Thunk { .. } => 1,
            Self::Swap => 2,
        }
    }
}

impl<O> MonoidalOp<O> {
    pub const ID: Self = MonoidalOp::Copy { copies: 1 };
    pub const DELETE: Self = MonoidalOp::Copy { copies: 0 };
}

impl<O: Copy> From<&MonoidalWiredOp<O>> for MonoidalOp<O> {
    fn from(op: &MonoidalWiredOp<O>) -> Self {
        match op {
            MonoidalWiredOp::Id { .. } => Self::ID,
            MonoidalWiredOp::Operation {
                addr,
                inputs,
                op_name,
            } => Self::Operation {
                addr: *addr,
                inputs: inputs.len(),
                op_name: *op_name,
                selected: false,
            },
            MonoidalWiredOp::Thunk {
                addr, args, body, ..
            } => Self::Thunk {
                addr: *addr,
                args: *args,
                body: body.into(),
                expanded: true,
            },
        }
    }
}

impl<'a, A: InOut, B: InOut + From<&'a A>> From<&'a Slice<A>> for Slice<B> {
    fn from(value: &'a Slice<A>) -> Self {
        Self {
            ops: value.ops.iter().map(B::from).collect(),
        }
    }
}

impl<O: Copy> From<&MonoidalWiredGraph<O>> for MonoidalGraph<O> {
    fn from(graph: &MonoidalWiredGraph<O>) -> Self {
        let wiring_slices = graph.wirings.iter().map(|w| w.to_slices());
        let slices: Vec<Slice<MonoidalOp<O>>> = wiring_slices
            .into_iter()
            .interleave(graph.slices.iter().map(|x| vec![x.into()]))
            .concat();
        Self {
            inputs: graph.inputs,
            slices,
        }
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Result;
    use rstest::rstest;

    use crate::graph::Op;

    use super::*;

    #[rstest]
    #[case(vec![0,1], vec![])]
    #[case(vec![1,0], vec![Slice { ops: vec![MonoidalOp::Swap]}])]
    #[case(vec![1,2,0], vec![Slice { ops: vec![MonoidalOp::ID, MonoidalOp::Swap]}, Slice { ops: vec![MonoidalOp::Swap, MonoidalOp::ID]}])]
    fn test_permutation(
        #[case] permutation: Vec<usize>,
        #[case] result: Vec<Slice<MonoidalOp<Op>>>,
    ) -> Result<()> {
        assert_eq!(Slice::permutation_to_swaps(permutation), result);
        Ok(())
    }
}
