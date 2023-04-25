use std::fmt::Debug;

use good_lp::{variable, Expression, ResolutionError, Solution, Variable};
use itertools::Itertools;
use sd_core::{
    common::InOut,
    monoidal::{FromHyperError, MonoidalGraph, MonoidalOp},
};
use thiserror::Error;

use crate::lp::LpProblem;

#[cfg(test)]
use serde::Serialize;

#[derive(Clone, Debug, Error)]
pub enum LayoutError {
    #[error("An error occurred when solving the problem: {0}")]
    ResolutionError(#[from] ResolutionError),
    #[error("An error occurred when converting from hypergraph: {0}")]
    FromHyperError(#[from] FromHyperError),
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub enum Node<T> {
    Atom(T),
    Thunk(LayoutInternal<T>),
}

impl<T> Node<T> {
    pub fn min(&self) -> &T {
        match self {
            Self::Atom(x) => x,
            Self::Thunk(layout) => &layout.min,
        }
    }

    pub fn max(&self) -> &T {
        match self {
            Self::Atom(x) => x,
            Self::Thunk(layout) => &layout.max,
        }
    }

    pub fn unwrap_atom(&self) -> &T {
        match self {
            Self::Atom(x) => x,
            Self::Thunk(_layout) => panic!(),
        }
    }

    pub fn unwrap_thunk(&self) -> &LayoutInternal<T> {
        match self {
            Self::Atom(_x) => panic!(),
            Self::Thunk(layout) => layout,
        }
    }
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub struct LayoutInternal<T> {
    pub min: T,
    pub max: T,
    pub nodes: Vec<Vec<Node<T>>>,
    pub wires: Vec<Vec<T>>,
}

impl<T> LayoutInternal<T> {
    pub fn inputs(&self) -> &[T] {
        self.wires.first().unwrap()
    }

    pub fn outputs(&self) -> &[T] {
        self.wires.last().unwrap()
    }
}

pub type Layout = LayoutInternal<f32>;

impl Layout {
    pub fn width(&self) -> f32 {
        self.max - self.min
    }

    pub fn height(&self) -> f32 {
        (0..self.nodes.len())
            .map(|j| self.slice_height(j))
            .sum::<f32>()
            + 1.0
    }

    pub fn slice_height(&self, j: usize) -> f32 {
        self.nodes[j]
            .iter()
            .map(|n| match n {
                Node::Atom(_) => 0.0,
                Node::Thunk(body) => body.height(),
            })
            .max_by(|x, y| x.partial_cmp(y).unwrap())
            .unwrap()
            + 1.0
    }

    fn from_solution(layout: LayoutInternal<Variable>, solution: &impl Solution) -> Self {
        Layout {
            min: solution.value(layout.min) as f32,
            max: solution.value(layout.max) as f32,
            nodes: layout
                .nodes
                .into_iter()
                .map(|ns| {
                    ns.into_iter()
                        .map(|n| match n {
                            Node::Atom(x) => Node::Atom(solution.value(x) as f32),
                            Node::Thunk(layout) => {
                                Node::Thunk(Self::from_solution(layout, solution))
                            }
                        })
                        .collect()
                })
                .collect(),
            wires: layout
                .wires
                .into_iter()
                .map(|vs| vs.into_iter().map(|v| solution.value(v) as f32).collect())
                .collect(),
        }
    }
}

fn layout_internal<O>(
    graph: &MonoidalGraph<O>,
    problem: &mut LpProblem,
) -> LayoutInternal<Variable> {
    // STEP 1. Generate variables for each layer.
    let min = problem.add_variable(variable().min(0.0));
    let max = problem.add_variable(variable().min(0.0));

    let mut nodes = Vec::default();
    let mut wires = Vec::default();

    macro_rules! add_constraints_wires {
        ($vs:expr) => {
            if let Some(x) = $vs.first().copied() {
                problem.add_constraint((x - min).geq(0.5));
            }
            if let Some(x) = $vs.last().copied() {
                problem.add_constraint((max - x).geq(0.5));
            }
            for (x, y) in $vs.iter().copied().tuple_windows() {
                problem.add_constraint((y - x).geq(1.0));
            }
        };
    }
    macro_rules! add_constraints_nodes {
        ($ns:expr) => {
            if let Some(x) = $ns.first() {
                problem.add_constraint((*x.min() - min).geq(0.5));
            }
            if let Some(x) = $ns.last() {
                problem.add_constraint((max - *x.max()).geq(0.5));
            }
            for (x, y) in $ns.iter().tuple_windows() {
                problem.add_constraint((*y.min() - *x.max()).geq(1.0));
            }
        };
    }

    let inputs = problem.add_variables(variable().min(0.0), graph.inputs);
    add_constraints_wires!(&inputs);
    wires.push(inputs);
    for slice in &graph.slices {
        let outputs = problem.add_variables(variable().min(0.0), slice.number_of_outputs());
        add_constraints_wires!(&outputs);
        wires.push(outputs);

        let ns = slice
            .ops
            .iter()
            .map(|op| match op {
                MonoidalOp::Thunk { body, expanded, .. } if *expanded => {
                    Node::Thunk(layout_internal(body, problem))
                }
                _ => Node::Atom(problem.add_variable(variable().min(0.0))),
            })
            .collect_vec();
        add_constraints_nodes!(&ns);
        nodes.push(ns);
    }

    // STEP 2. Add constraints between layers.
    for (j, slice) in graph.slices.iter().enumerate() {
        let mut prev_op = None;
        let mut prev_in = None;
        let mut prev_out = None;
        let mut offset_i = 0;
        let mut offset_o = 0;
        for (i, op) in slice.ops.iter().enumerate() {
            let ni = op.number_of_inputs();
            let no = op.number_of_outputs();

            assert_ne!(ni + no, 0, "Scalars are not allowed!");

            let op = &nodes[j][i];
            let ins = &wires[j][offset_i..offset_i + ni];
            let outs = &wires[j + 1][offset_o..offset_o + no];

            // Distance constraints
            let constraints = [
                (prev_in, Some(*op.min())),
                (prev_out, Some(*op.min())),
                (prev_op, ins.first().copied()),
                (prev_op, outs.first().copied()),
                (prev_in, outs.first().copied()),
                (prev_out, ins.first().copied()),
            ];
            for (x, y) in constraints.into_iter().filter_map(|(x, y)| x.zip(y)) {
                problem.add_constraint((y - x).geq(1.0));
            }

            match op {
                Node::Atom(op) => {
                    // Fair averaging constraints
                    if ni > 0 {
                        let sum_ins: Expression = ins.iter().sum();
                        problem.add_constraint((*op * ni as f64).eq(sum_ins));
                    }
                    if no > 0 {
                        let sum_outs: Expression = outs.iter().sum();
                        problem.add_constraint((*op * no as f64).eq(sum_outs));
                    }
                }
                Node::Thunk(layout) => {
                    // Align internal wires with the external ones.
                    for (&x, &y) in ins.iter().zip(layout.inputs()) {
                        problem.add_constraint((x - y).eq(0.0));
                    }
                    for (&x, &y) in outs.iter().zip(layout.outputs()) {
                        problem.add_constraint((x - y).eq(0.0));
                    }
                }
            }

            prev_op = Some(*op.max());
            prev_in = ins.last().copied();
            prev_out = outs.last().copied();

            offset_i += ni;
            offset_o += no;
        }
    }

    LayoutInternal {
        min,
        max,
        nodes,
        wires,
    }
}

pub fn layout<O>(graph: &MonoidalGraph<O>) -> Result<Layout, LayoutError> {
    let mut problem = LpProblem::default();

    let layout = layout_internal(graph, &mut problem);
    let solution = problem.minimise(layout.max, good_lp::default_solver)?;

    Ok(Layout::from_solution(layout, &solution))
}

#[cfg(test)]
mod tests {
    use sd_core::examples;

    use super::layout;

    #[test]
    fn int() {
        insta::with_settings!({sort_maps => true}, {
            insta::assert_ron_snapshot!(layout(&examples::int()).expect("Layout failed"));
        });
    }

    #[test]
    fn copy() {
        insta::with_settings!({sort_maps => true}, {
                insta::assert_ron_snapshot!(layout(&examples::copy()).expect("Layout failed"));
        });
    }
}
