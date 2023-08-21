use std::fmt::{Debug, Display};

use egui::Vec2;
use good_lp::{variable, Expression, ResolutionError, Solution, Variable};
use itertools::Itertools;
use sd_core::{
    common::{Addr, InOut},
    hypergraph::traits::WithWeight,
    monoidal::graph::{MonoidalGraph, MonoidalOp},
    weak_map::WeakMap,
};
#[cfg(test)]
use serde::Serialize;
use thiserror::Error;

use crate::{common::RADIUS_OPERATION, lp::LpProblem};

#[derive(Clone, Debug, Error)]
pub enum LayoutError {
    #[error("An error occurred when solving the problem: {0}")]
    ResolutionError(#[from] ResolutionError),
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub struct NodeOffset<T> {
    pub(crate) node: Node<T>,
    input_offset: usize,
    inputs: usize,
    output_offset: usize,
    outputs: usize,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub enum AtomType {
    Cup,
    Cap,
    Other,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub enum Node<T> {
    Atom {
        pos: T,
        extra_size: f32,
        atype: AtomType,
    },
    Swap {
        pos: T,
        out_to_in: Vec<usize>,
    },
    Thunk {
        layout: LayoutInternal<T>,
    },
}

impl Node<Variable> {
    #[must_use]
    pub fn min(&self) -> Expression {
        match self {
            Self::Atom {
                pos, extra_size, ..
            } => *pos - *extra_size,
            Self::Swap { pos, .. } => (*pos).into(),
            Self::Thunk { layout, .. } => layout.min.into(),
        }
    }

    #[must_use]
    pub fn max(&self) -> Expression {
        match self {
            Self::Atom {
                pos, extra_size, ..
            } => *pos + *extra_size,
            Self::Swap { pos, .. } => (*pos).into(),
            Self::Thunk { layout, .. } => layout.max.into(),
        }
    }
}

impl<T> Node<T> {
    pub fn unwrap_atom(&self) -> &T {
        match self {
            Self::Atom { pos, .. } | Self::Swap { pos, .. } => pos,
            Self::Thunk { .. } => panic!(),
        }
    }

    pub fn unwrap_thunk(&self) -> &LayoutInternal<T> {
        match self {
            Self::Atom { .. } | Self::Swap { .. } => panic!(),
            Self::Thunk { layout, .. } => layout,
        }
    }
}

impl<T> InOut for NodeOffset<T> {
    fn number_of_inputs(&self) -> usize {
        self.inputs
    }

    fn number_of_outputs(&self) -> usize {
        self.outputs
    }
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub struct LayoutInternal<T> {
    pub min: T,
    pub max: T,
    pub nodes: Vec<Vec<NodeOffset<T>>>,
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
    #[must_use]
    pub fn width(&self) -> f32 {
        self.max - self.min
    }

    #[must_use]
    pub fn height(&self) -> f32 {
        (0..self.nodes.len())
            .map(|j| self.slice_height(j))
            .sum::<f32>()
            + 1.0
    }

    #[must_use]
    pub fn size(&self) -> Vec2 {
        Vec2::new(self.width(), self.height())
    }

    #[must_use]
    pub fn slice_height(&self, j: usize) -> f32 {
        self.nodes[j]
            .iter()
            .map(|n| match &n.node {
                Node::Atom { .. } => {
                    let in_width = if n.inputs < 2 {
                        1.0
                    } else {
                        self.wires[j][n.input_offset + n.inputs - 1] - self.wires[j][n.input_offset]
                    };

                    let out_width = if n.outputs < 2 {
                        1.0
                    } else {
                        self.wires[j + 1][n.output_offset + n.outputs - 1]
                            - self.wires[j + 1][n.output_offset]
                    };

                    f32::sqrt((in_width + out_width) / 2.0) - 1.0
                }
                Node::Swap { out_to_in, .. } => out_to_in
                    .iter()
                    .enumerate()
                    .map(|(i, x)| {
                        (f32::sqrt((self.wires[j + 1][i] - self.wires[j][*x]).abs()) - 1.0)
                            .clamp(0.0, f32::INFINITY)
                    })
                    .max_by(|x, y| x.partial_cmp(y).unwrap())
                    .unwrap_or_default(),
                Node::Thunk { layout } => {
                    let height_above = self.wires[j][n.input_offset..]
                        .iter()
                        .zip(layout.inputs().iter())
                        .map(|(x, y)| (f32::sqrt((x - y).abs()) - 1.0).clamp(0.0, f32::INFINITY))
                        .max_by(|x, y| x.partial_cmp(y).unwrap())
                        .unwrap_or_default();

                    let height_below = self.wires[j + 1][n.output_offset..]
                        .iter()
                        .zip(layout.outputs().iter())
                        .map(|(x, y)| (f32::sqrt((x - y).abs()) - 1.0).clamp(0.0, f32::INFINITY))
                        .max_by(|x, y| x.partial_cmp(y).unwrap())
                        .unwrap_or_default();

                    layout.height() + height_above + height_below
                }
            })
            .max_by(|x, y| x.partial_cmp(y).unwrap())
            .unwrap_or_default()
            + 1.0
    }

    #[allow(clippy::cast_possible_truncation)]
    fn from_solution(layout: LayoutInternal<Variable>, solution: &impl Solution) -> Self {
        Layout {
            min: solution.value(layout.min) as f32,
            max: solution.value(layout.max) as f32,
            nodes: layout
                .nodes
                .into_iter()
                .map(|ns| {
                    ns.into_iter()
                        .map(|n| NodeOffset {
                            node: match n.node {
                                Node::Atom {
                                    pos,
                                    extra_size,
                                    atype,
                                } => Node::Atom {
                                    pos: (solution.value(pos) as f32),
                                    extra_size,
                                    atype,
                                },
                                Node::Swap { pos, out_to_in } => Node::Swap {
                                    pos: solution.value(pos) as f32,
                                    out_to_in,
                                },
                                Node::Thunk { layout } => Node::Thunk {
                                    layout: Self::from_solution(layout, solution),
                                },
                            },
                            input_offset: n.input_offset,
                            inputs: n.inputs,
                            output_offset: n.output_offset,
                            outputs: n.outputs,
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

#[allow(clippy::too_many_lines)]
fn layout_internal<T: Addr>(
    graph: &MonoidalGraph<T>,
    expanded: &WeakMap<T::Thunk, bool>,
    problem: &mut LpProblem,
) -> LayoutInternal<Variable>
where
    T::Operation: WithWeight,
    <T::Operation as WithWeight>::Weight: Display,
{
    // STEP 1. Generate variables for each layer.
    let min = problem.add_variable(variable().min(0.0));
    let max = problem.add_variable(variable().min(0.0));

    let mut nodes = Vec::default();
    let mut wires = Vec::default();

    let add_constraints_wires = |problem: &mut LpProblem, vs: &Vec<Variable>| {
        if let Some(x) = vs.first().copied() {
            problem.add_constraint((x - min).geq(0.5));
        }
        if let Some(x) = vs.last().copied() {
            problem.add_constraint((max - x).geq(0.5));
        }
        for (x, y) in vs.iter().copied().tuple_windows() {
            problem.add_constraint((y - x).geq(1.0));
        }
    };

    let add_constraints_nodes = |problem: &mut LpProblem, ns: &Vec<NodeOffset<Variable>>| {
        if let Some(x) = ns.first() {
            problem.add_constraint((x.node.min() - min).geq(0.5));
        }
        if let Some(x) = ns.last() {
            problem.add_constraint((max - x.node.max()).geq(0.5));
        }
        for (x, y) in ns.iter().tuple_windows() {
            problem.add_constraint((y.node.min() - x.node.max()).geq(1.0));
        }
    };

    let inputs = problem.add_variables(
        variable().min(0.0),
        graph.free_inputs.len() + graph.bound_inputs.len(),
    );
    add_constraints_wires(problem, &inputs);
    wires.push(inputs);
    for slice in &graph.slices {
        let outputs = problem.add_variables(variable().min(0.0), slice.number_of_outputs());
        add_constraints_wires(problem, &outputs);
        wires.push(outputs);

        let mut input_offset = 0;
        let mut output_offset = 0;

        let ns = slice
            .ops
            .iter()
            .map(|op| {
                let node = match op {
                    MonoidalOp::Thunk { body, addr, .. } if expanded[addr] => Node::Thunk {
                        layout: layout_internal(body, expanded, problem),
                    },
                    MonoidalOp::Swap { out_to_in, .. } => Node::Swap {
                        pos: problem.add_variable(variable().min(0.0)),
                        out_to_in: out_to_in.clone(),
                    },
                    MonoidalOp::Cup { .. } => Node::Atom {
                        pos: problem.add_variable(variable().min(0.0)),
                        extra_size: 0.0,
                        atype: AtomType::Cup,
                    },
                    MonoidalOp::Cap { .. } => Node::Atom {
                        pos: problem.add_variable(variable().min(0.0)),
                        extra_size: 0.0,
                        atype: AtomType::Cap,
                    },
                    MonoidalOp::Operation { addr } => Node::Atom {
                        pos: problem.add_variable(variable().min(0.0)),
                        extra_size: (addr.weight().to_string().chars().count().saturating_sub(1)
                            as f32
                            / 2.0)
                            * RADIUS_OPERATION,
                        atype: AtomType::Other,
                    },
                    _ => Node::Atom {
                        pos: problem.add_variable(variable().min(0.0)),
                        extra_size: 0.0,
                        atype: AtomType::Other,
                    },
                };
                let node_offset = NodeOffset {
                    node,
                    input_offset,
                    output_offset,
                    inputs: op.number_of_inputs(),
                    outputs: op.number_of_outputs(),
                };
                input_offset += op.number_of_inputs();
                output_offset += op.number_of_outputs();
                node_offset
            })
            .collect_vec();
        add_constraints_nodes(problem, &ns);
        nodes.push(ns);
    }

    // STEP 2. Add constraints between layers.
    for (nodes, (wires_i, wires_o)) in nodes.iter().zip(wires.iter().tuple_windows()) {
        let mut prev_op = None;
        for node in nodes {
            let ni = node.number_of_inputs();
            let no = node.number_of_outputs();

            assert_ne!(ni + no, 0, "Scalars are not allowed!");

            let ins = &wires_i[node.input_offset..node.input_offset + ni];
            let outs = &wires_o[node.output_offset..node.output_offset + no];

            let prev_in: Option<Expression> = if node.input_offset == 0 {
                None
            } else {
                wires_i.get(node.input_offset - 1).copied().map(Into::into)
            };
            let prev_out: Option<Expression> = if node.output_offset == 0 {
                None
            } else {
                wires_o.get(node.output_offset - 1).copied().map(Into::into)
            };

            // Distance constraints
            let constraints = [
                (prev_in.clone(), Some(node.node.min())),
                (prev_out.clone(), Some(node.node.min())),
                (prev_op.clone(), ins.first().copied().map(Into::into)),
                (prev_op, outs.first().copied().map(Into::into)),
                (prev_in, outs.first().copied().map(Into::into)),
                (prev_out, ins.first().copied().map(Into::into)),
            ];
            for (x, y) in constraints.into_iter().filter_map(|(x, y)| x.zip(y)) {
                problem.add_constraint((y - x).geq(1.0));
            }

            match &node.node {
                Node::Atom { pos, atype, .. } => {
                    match atype {
                        AtomType::Cup => {
                            for (x, y) in ins[1..].iter().copied().zip(outs) {
                                problem.add_constraint(Expression::eq(x.into(), y));
                            }
                            problem.add_constraint((*pos * 2.0).eq(ins[ni - 1] + ins[0]));
                            problem.add_objective(ins[ni - 1] - ins[0]);
                        }
                        AtomType::Cap => {
                            for (x, y) in outs[1..].iter().copied().zip(ins) {
                                problem.add_constraint(Expression::eq(x.into(), y));
                            }
                            problem.add_constraint((*pos * 2.0).eq(outs[no - 1] + outs[0]));
                            problem.add_objective(outs[no - 1] - outs[0]);
                        }
                        AtomType::Other => {
                            // Try to "squish" inputs and outputs
                            if ni >= 2 {
                                problem.add_objective((ins[ni - 1] - ins[0]) * ni as f32);
                            }

                            if no >= 2 {
                                problem.add_objective((outs[no - 1] - outs[0]) * no as f32);
                            }

                            // Fair averaging constraints
                            if ni > 0 {
                                let sum_ins: Expression = ins.iter().sum();
                                problem.add_constraint((*pos * ni as f64).eq(sum_ins));
                            }
                            if no > 0 {
                                let sum_outs: Expression = outs.iter().sum();
                                problem.add_constraint((*pos * no as f64).eq(sum_outs));
                            }
                        }
                    }
                }
                Node::Swap { pos, out_to_in, .. } => {
                    let in_outs: Expression = ins.iter().chain(outs.iter()).sum();
                    problem.add_constraint((*pos * (ni + no) as f64).eq(in_outs));

                    for (i, j) in out_to_in.iter().copied().enumerate() {
                        let distance = problem.add_variable(variable().min(0.0));
                        problem.add_constraint((ins[j] - outs[i]).leq(distance));
                        problem.add_constraint((outs[i] - ins[j]).leq(distance));
                        problem.add_objective(distance);
                    }
                }
                Node::Thunk { layout, .. } => {
                    // Align internal wires with the external ones.
                    for (&x, &y) in ins.iter().zip(layout.inputs()) {
                        let distance = problem.add_variable(variable().min(0.0));
                        problem.add_constraint((x - y).leq(distance));
                        problem.add_constraint((y - x).leq(distance));
                        problem.add_objective(distance * 1.5);
                    }
                    for (&x, &y) in outs.iter().zip(layout.outputs()) {
                        let distance = problem.add_variable(variable().min(0.0));
                        problem.add_constraint((x - y).leq(distance));
                        problem.add_constraint((y - x).leq(distance));
                        problem.add_objective(distance * 1.5);
                    }

                    problem.add_objective(layout.max - layout.min);
                }
            }

            prev_op = Some(node.node.max());
        }
    }

    LayoutInternal {
        min,
        max,
        nodes,
        wires,
    }
}

pub fn layout<T: Addr>(
    graph: &MonoidalGraph<T>,
    expanded: &WeakMap<T::Thunk, bool>,
) -> Result<Layout, LayoutError>
where
    T::Operation: WithWeight,
    <T::Operation as WithWeight>::Weight: Display,
{
    let mut problem = LpProblem::default();

    let layout = layout_internal(graph, expanded, &mut problem);
    problem.add_objective(layout.max);
    let solution = problem.minimise(good_lp::default_solver)?;

    Ok(Layout::from_solution(layout, &solution))
}

#[cfg(test)]
mod tests {
    use sd_core::{examples, weak_map::WeakMap};

    use super::layout;

    #[test]
    fn int() {
        insta::with_settings!({sort_maps => true}, {
            insta::assert_ron_snapshot!(layout(&examples::int(), &WeakMap::default()).expect("Layout failed"));
        });
    }

    #[test]
    fn copy() {
        insta::with_settings!({sort_maps => true}, {
            insta::assert_ron_snapshot!(layout(&examples::copy(), &WeakMap::default()).expect("Layout failed"));
        });
    }
}
