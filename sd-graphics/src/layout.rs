use good_lp::{variable, Expression, ResolutionError, Solution};
use itertools::Itertools;
use sd_core::monoidal::MonoidalGraph;
use thiserror::Error;

use crate::lp::LpProblem;

#[derive(Clone, Debug, Error)]
pub enum LayoutError {
    #[error("An error occurred when solving the problem: {0}")]
    ResolutionError(#[from] ResolutionError),
}

#[derive(Clone, Debug)]
pub struct Layout {
    pub min: f64,
    pub max: f64,
    pub nodes: Vec<Vec<f64>>,
    pub wires: Vec<Vec<f64>>,
}

pub fn layout(graph: &MonoidalGraph) -> Result<Layout, LayoutError> {
    let mut problem = LpProblem::default();

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
        let mut offset_i = 0;
        let mut offset_o = 0;
        for (i, (op, _)) in slice.ops.iter().enumerate() {
            let ni = op.number_of_inputs();
            let no = op.number_of_outputs();

            assert_ne!(ni + no, 0, "Scalars are not allowed!");

            let op = nodes[j][i];
            let ins = &wires[j][offset_i..offset_i + ni];
            let outs = &wires[j + 1][offset_o..offset_o + no];

            // Fair averaging constraints
            let sum_ins: Expression = ins.iter().sum();
            let sum_outs: Expression = outs.iter().sum();
            problem.add_constraint((op * ni as f64 - sum_ins).eq(0.0));
            problem.add_constraint((op * no as f64 - sum_outs).eq(0.0));

            offset_i += ni;
            offset_o += no;
        }
    }

    let solution = problem.minimise(max, good_lp::default_solver)?;

    Ok(Layout {
        min: solution.value(min),
        max: solution.value(max),
        nodes: nodes
            .into_iter()
            .map(|vs| vs.into_iter().map(|v| solution.value(v)).collect())
            .collect(),
        wires: wires
            .into_iter()
            .map(|vs| vs.into_iter().map(|v| solution.value(v)).collect())
            .collect(),
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
