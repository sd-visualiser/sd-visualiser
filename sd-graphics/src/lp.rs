use good_lp::{
    Constraint, IntoAffineExpression, ProblemVariables, Solver, SolverModel, Variable,
    VariableDefinition,
};
use itertools::Itertools;

#[derive(Default)]
pub(crate) struct LpProblem {
    problem: ProblemVariables,
    constraints: Vec<Constraint>,
}

impl LpProblem {
    pub(crate) fn add_variable(&mut self, var_def: VariableDefinition) -> Variable {
        self.problem.add(var_def)
    }

    pub(crate) fn add_variables(
        &mut self,
        var_def: VariableDefinition,
        len: usize,
    ) -> Vec<Variable> {
        self.problem.add_vector(var_def, len)
    }

    pub(crate) fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint)
    }

    pub(crate) fn minimise<S: Solver>(
        self,
        objective: impl IntoAffineExpression,
        solver: S,
    ) -> Result<
        <<S as Solver>::Model as SolverModel>::Solution,
        <<S as Solver>::Model as SolverModel>::Error,
    > {
        let mut model = self.problem.minimise(objective).using(solver);
        for c in self.constraints {
            model.add_constraint(c);
        }
        model.solve()
    }

    pub(crate) fn variables(&self) -> Vec<Variable> {
        self.problem
            .iter_variables_with_def()
            .map(|(var, _)| var)
            .collect_vec()
    }
}
