use std::{collections::HashMap, fmt::Debug};

use good_lp::{variable, Expression, ResolutionError, Solution, Variable};
use itertools::{Either, Itertools};
use sd_core::monoidal::{MonoidalGraph, MonoidalOp};
use thiserror::Error;

use crate::lp::LpProblem;

#[derive(Clone, Debug, Error)]
pub enum LayoutError {
    #[error("An error occurred when solving the problem: {0}")]
    ResolutionError(#[from] ResolutionError),
}

pub struct Layout {
    internal: LayoutInternal,
    solution: HashMap<Variable, f64>,
}

#[derive(Clone, Debug)]
struct LayoutInternal {
    min: Variable,
    max: Variable,
    nodes: Vec<Vec<Node>>,
    wires: Vec<Vec<Variable>>,
}

impl Layout {
    pub fn min(&self) -> f64 {
        self.solution[&self.internal.min]
    }

    pub fn max(&self) -> f64 {
        self.solution[&self.internal.max]
    }

    pub fn wire(&self, j: usize, i: usize) -> f64 {
        self.solution[&self.internal.wires[j][i]]
    }

    pub fn wires(&self, j: usize) -> impl Iterator<Item = f64> + '_ {
        self.internal.wires[j].iter().map(|v| self.solution[v])
    }

    pub fn node(&self, j: usize, i: usize) -> Either<f64, Layout> {
        match &self.internal.nodes[j][i] {
            Node::Op(v) => Either::Left(self.solution[v]),
            Node::Thunk(layout) => Either::Right(Layout {
                internal: layout.clone(),
                solution: self.solution.clone(), // TODO(@calintat): THIS IS VERY BAD!
            }),
        }
    }
}

impl LayoutInternal {
    fn inputs(&self) -> &[Variable] {
        &self.wires[0]
    }

    fn outputs(&self) -> &[Variable] {
        self.wires.last().unwrap()
    }
}

impl Debug for Layout {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[allow(dead_code)]
        #[derive(Debug)]
        struct Layout {
            min: f64,
            max: f64,
            nodes: Vec<Vec<f64>>,
            wires: Vec<Vec<f64>>,
        }
        let layout = Layout {
            min: self.min(),
            max: self.max(),
            nodes: self
                .internal
                .nodes
                .iter()
                .map(|vs| {
                    vs.iter()
                        .map(|v| match v {
                            Node::Op(v) => self.solution[v],
                            Node::Thunk(_layout) => unimplemented!(),
                        })
                        .collect()
                })
                .collect(),
            wires: self
                .internal
                .wires
                .iter()
                .map(|vs| vs.iter().map(|v| self.solution[v]).collect())
                .collect(),
        };
        layout.fmt(f)
    }
}

/// Represents the layout information for a node in the graph.
#[derive(Clone, Debug)]
enum Node {
    Op(Variable),
    Thunk(LayoutInternal),
}

impl Node {
    pub fn min(&self) -> Variable {
        match self {
            Self::Op(v) => *v,
            Self::Thunk(layout) => layout.min,
        }
    }

    pub fn max(&self) -> Variable {
        match self {
            Self::Op(v) => *v,
            Self::Thunk(layout) => layout.max,
        }
    }
}

fn layout_internal(graph: &MonoidalGraph, problem: &mut LpProblem) -> LayoutInternal {
    // STEP 1. Generate variables for each layer.
    let min = problem.add_variable(variable().min(0.0));
    let max = problem.add_variable(variable().min(0.0));

    let mut nodes = Vec::default();
    let mut wires = Vec::default();

    macro_rules! add_constraints_wires {
        ($vs:expr) => {
            if let Some(x) = $vs.first().copied() {
                problem.add_constraint((x - min).geq(0.0));
            }
            if let Some(x) = $vs.last().copied() {
                problem.add_constraint((max - x).geq(0.0));
            }
            for (x, y) in $vs.iter().copied().tuple_windows() {
                problem.add_constraint((y - x).geq(1.0));
            }
        };
    }
    macro_rules! add_constraints_nodes {
        ($ns:expr) => {
            if let Some(x) = $ns.first() {
                problem.add_constraint((x.min() - min).geq(0.0));
            }
            if let Some(x) = $ns.last() {
                problem.add_constraint((max - x.max()).geq(0.0));
            }
            for (x, y) in $ns.iter().tuple_windows() {
                problem.add_constraint((y.min() - x.max()).geq(1.0));
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
            .map(|(op, _)| {
                if let MonoidalOp::Thunk { body, .. } = op {
                    Node::Thunk(layout_internal(body, problem))
                } else {
                    Node::Op(problem.add_variable(variable().min(0.0)))
                }
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
        for (i, (op, _)) in slice.ops.iter().enumerate() {
            let ni = op.number_of_inputs();
            let no = op.number_of_outputs();

            assert_ne!(ni + no, 0, "Scalars are not allowed!");

            let op = &nodes[j][i];
            let ins = &wires[j][offset_i..offset_i + ni];
            let outs = &wires[j + 1][offset_o..offset_o + no];

            // Distance constraints
            let constraints = [
                (prev_in, Some(op.min())),
                (prev_out, Some(op.min())),
                (prev_op, ins.first().copied()),
                (prev_op, outs.first().copied()),
                (prev_in, outs.first().copied()),
                (prev_out, ins.first().copied()),
            ];
            for (x, y) in constraints.into_iter().filter_map(|(x, y)| x.zip(y)) {
                problem.add_constraint((y - x).geq(1.0));
            }

            match op {
                Node::Op(op) => {
                    // Fair averaging constraints
                    let sum_ins: Expression = ins.iter().sum();
                    let sum_outs: Expression = outs.iter().sum();
                    problem.add_constraint((*op * ni as f64 - sum_ins).eq(0.0));
                    problem.add_constraint((*op * no as f64 - sum_outs).eq(0.0));
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

            prev_op = Some(op.max());
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

pub fn layout(graph: &MonoidalGraph) -> Result<Layout, LayoutError> {
    let mut problem = LpProblem::default();

    let layout = layout_internal(graph, &mut problem);
    let variables = problem.variables();
    let solution = problem.minimise(layout.max, good_lp::default_solver)?;
    let fake_solution = HashMap::from_iter(variables.into_iter().map(|v| (v, solution.value(v))));

    Ok(Layout {
        internal: layout,
        solution: fake_solution,
    })
}

#[cfg(test)]
mod tests {
    use insta::assert_debug_snapshot;
    use sd_core::examples;

    use super::layout;

    #[test]
    fn copy() {
        assert_debug_snapshot!(layout(&examples::copy()).expect("Layout failed"));
    }
}
