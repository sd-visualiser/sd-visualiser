use good_lp::{
    Constraint, Expression, IntoAffineExpression, ProblemVariables, Solver, SolverModel, Variable,
    VariableDefinition,
};

pub const LP_BACKEND: &str =
    // good_lp default solver hierarchy is cbc > minilp > highs
    if cfg!(feature = "cbc") {
        "cbc"
    } else if cfg!(feature = "minilp") {
        "minilp"
    } else if cfg!(feature = "highs") {
        "highs"
    } else {
        unreachable!()
    };

#[derive(Default)]
pub struct LpProblem {
    problem: ProblemVariables,
    constraints: Vec<Constraint>,
    objective: Expression,
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

    pub(crate) fn add_objective(&mut self, objective: impl IntoAffineExpression) {
        self.objective += objective;
    }

    pub(crate) fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }

    pub(crate) fn minimise<S: Solver>(
        self,
        solver: S,
    ) -> Result<
        <<S as Solver>::Model as SolverModel>::Solution,
        <<S as Solver>::Model as SolverModel>::Error,
    > {
        let mut model = self.problem.minimise(self.objective).using(solver);
        for c in self.constraints {
            model.add_constraint(c);
        }
        model.solve()
    }
}
