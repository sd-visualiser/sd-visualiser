use good_lp::{
    Constraint, Expression, IntoAffineExpression, ProblemVariables, ResolutionError, Solution,
    SolverModel, Variable, VariableDefinition,
};

pub const LP_BACKEND: &str =
    // good_lp default solver hierarchy is cbc > minilp > highs
    if cfg!(feature = "cbc") {
        "cbc"
    } else if cfg!(feature = "minilp") {
        "minilp"
    } else if cfg!(feature = "highs") {
        "highs"
    } else if cfg!(feature = "clarabel") {
        "clarabel"
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
    pub fn add_variable(&mut self, var_def: VariableDefinition) -> Variable {
        self.problem.add(var_def)
    }

    pub fn add_variables(&mut self, var_def: VariableDefinition, len: usize) -> Vec<Variable> {
        self.problem.add_vector(var_def, len)
    }

    pub fn add_objective(&mut self, objective: impl IntoAffineExpression) {
        self.objective += objective;
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }

    pub fn minimise(self) -> Result<impl Solution, ResolutionError> {
        let to_solve = self.problem.minimise(self.objective);

        #[cfg(feature = "clarabel")]
        let mut model = to_solve.using(|x| {
            let mut prob = good_lp::solvers::clarabel::clarabel(x);
            prob.set_parameter("log", "0");
            prob
        });

        #[cfg(feature = "cbc")]
        let mut model = to_solve.using(|x| {
            let mut prob = good_lp::solvers::coin_cbc::coin_cbc(x);
            prob.set_parameter("log", "0");
            prob
        });

        #[cfg(all(not(feature = "cbc"), not(feature = "clarabel")))]
        let mut model = to_solve.using(good_lp::default_solver);

        for c in self.constraints {
            model.add_constraint(c);
        }
        model.solve()
    }
}
