use std::{collections::HashMap, fmt::Debug};

use good_lp::{variable, Expression, ResolutionError, Solution, Variable};
use itertools::Itertools;
use sd_core::monoidal::MonoidalGraph;
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
    pub min: Variable,
    pub max: Variable,
    pub nodes: Vec<Vec<Variable>>,
    pub wires: Vec<Vec<Variable>>,
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

    pub fn node(&self, j: usize, i: usize) -> f64 {
        self.solution[&self.internal.nodes[j][i]]
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
                .map(|vs| vs.iter().map(|v| self.solution.value(*v)).collect())
                .collect(),
            wires: self
                .internal
                .wires
                .iter()
                .map(|vs| vs.iter().map(|v| self.solution.value(*v)).collect())
                .collect(),
        };
        layout.fmt(f)
    }
}

fn layout_internal(graph: &MonoidalGraph, problem: &mut LpProblem) -> LayoutInternal {
    // STEP 1. Generate variables for each layer.
    let min = problem.add_variable(variable().min(0.0));
    let max = problem.add_variable(variable().min(0.0));

    let mut nodes = Vec::default();
    let mut wires = Vec::default();

    let mut mk_variables = |len| {
        let variables = problem.add_variables(variable().min(0.0), len);
        for (x, y) in std::iter::once(min)
            .chain(variables.iter().copied())
            .chain(std::iter::once(max))
            .tuple_windows()
        {
            problem.add_constraint((y - x).geq(1.0));
        }
        variables
    };

    wires.push(mk_variables(graph.inputs));
    for slice in &graph.slices {
        nodes.push(mk_variables(slice.ops.len()));
        wires.push(mk_variables(slice.number_of_outputs()));
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

            let op = nodes[j][i];
            let ins = &wires[j][offset_i..offset_i + ni];
            let outs = &wires[j + 1][offset_o..offset_o + no];

            // Distance constraints
            let constraints = [
                (prev_in, Some(op)),
                (prev_out, Some(op)),
                (prev_op, ins.first().copied()),
                (prev_op, outs.first().copied()),
                (prev_in, outs.first().copied()),
                (prev_out, ins.first().copied()),
            ];
            for (x, y) in constraints.into_iter().filter_map(|(x, y)| x.zip(y)) {
                problem.add_constraint((y - x).geq(1.0));
            }

            // Fair averaging constraints
            let sum_ins: Expression = ins.iter().sum();
            let sum_outs: Expression = outs.iter().sum();
            problem.add_constraint((op * ni as f64 - sum_ins).eq(0.0));
            problem.add_constraint((op * no as f64 - sum_outs).eq(0.0));

            prev_op = Some(op);
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
