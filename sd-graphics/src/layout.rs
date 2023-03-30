use good_lp::{
    variable, Constraint, Expression, ProblemVariables, ResolutionError, Solution, SolverModel,
};
use sd_core::monoidal::MonoidalGraph;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LayoutError {
    #[error("An error occurred when solving the problem: {0}")]
    ResolutionError(#[from] ResolutionError),
}

pub struct Layout {
    pub width: f64,
    pub nodes: Vec<Vec<f64>>,
}

pub fn layout(graph: MonoidalGraph) -> Result<Layout, LayoutError> {
    let mut problem = ProblemVariables::new();
    let mut constraints = Vec::<Constraint>::new();

    let width = problem.add(variable().min(0.0));
    let mut nodes = vec![problem.add_vector(variable().min(0.0), graph.inputs)];

    // Distance constraints
    for xs in nodes[0].windows(2) {
        constraints.push((xs[1] - xs[0]).geq(1.0));
    }

    // Width constraints
    if let Some(x) = nodes[0].last().copied() {
        constraints.push((width - x).geq(0.0));
    }

    for (i, slice) in graph.slices.iter().enumerate() {
        let mut prev_in = None;
        let mut prev_out = None;

        let mut offset = 0;
        let mut outputs = Vec::new();

        for op in &slice.ops {
            let ni = op.number_of_inputs();
            let no = op.number_of_outputs();

            assert_ne!(ni + no, 0, "Scalars are not allowed!");

            let local_inputs = &nodes[i][offset..offset + ni];
            let local_outputs = problem.add_vector(variable().min(0.0), no);

            // Distance constraints
            for xs in local_outputs.windows(2) {
                constraints.push((xs[1] - xs[0]).geq(1.0));
            }
            if let Some(y) = local_inputs.first().copied() {
                if let Some(x) = prev_out {
                    constraints.push((y - x).geq(1.0));
                }
            }
            if let Some(y) = local_outputs.first().copied() {
                if let Some(x) = prev_in {
                    constraints.push((y - x).geq(1.0));
                }
                if let Some(x) = prev_out {
                    constraints.push((y - x).geq(1.0));
                }
            }

            // Fair averaging constraints
            let sum_i: Expression = local_inputs.iter().sum();
            let sum_o: Expression = local_outputs.iter().sum();
            constraints.push((sum_i * no as f64 - sum_o * ni as f64).eq(0.0));

            prev_in = local_inputs.last().copied();
            prev_out = local_outputs.last().copied();

            offset += ni;
            outputs.push(local_outputs);
        }

        nodes.push(outputs.concat());

        // Width constraints
        if let Some(x) = nodes[i + 1].last().copied() {
            constraints.push((width - x).geq(0.0));
        }
    }

    let mut model = problem.minimise(width).using(good_lp::default_solver);
    for c in constraints {
        model.add_constraint(c);
    }

    let solution = model.solve()?;

    Ok(Layout {
        width: solution.value(width),
        nodes: nodes
            .into_iter()
            .map(|xs| xs.into_iter().map(|x| solution.value(x)).collect())
            .collect(),
    })
}

#[cfg(test)]
mod tests {
    use sd_core::monoidal::{MonoidalGraph, MonoidalOp, Slice};

    use super::layout;

    #[test]
    fn copy() {
        use MonoidalOp::*;

        let graph = MonoidalGraph {
            inputs: 2,
            slices: vec![
                Slice {
                    ops: vec![Copy { copies: 2 }, Id],
                },
                Slice {
                    ops: vec![Copy { copies: 2 }, Id, Id],
                },
            ],
        };
        let layout = layout(graph).expect("Layout failed");
        assert_eq!(layout.width, 3.0);
        assert_eq!(
            layout.nodes,
            vec![
                vec![1.25, 3.0],
                vec![0.5, 2.0, 3.0],
                vec![0.0, 1.0, 2.0, 3.0]
            ]
        );
    }
}
