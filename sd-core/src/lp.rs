use clap_derive::ValueEnum;
#[cfg(feature = "gurobi")]
use good_lp::solvers::lp_solvers::LpSolver;
use good_lp::{
    Constraint, Expression, IntoAffineExpression, ProblemVariables, ResolutionError, Solution,
    SolverModel, Variable, VariableDefinition,
};

#[derive(Default)]
pub struct LpProblem {
    problem: ProblemVariables,
    constraints: Vec<Constraint>,
    objective: Expression,
}

#[derive(ValueEnum, Clone, Copy, Default, Debug)]
pub enum Solver {
    Clarabel,
    #[cfg(feature = "gurobi")]
    Gurobi,
    #[cfg(feature = "highs")]
    Highs,
    #[cfg(feature = "cbc")]
    Cbc,
    #[default]
    Minilp,
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

    pub fn minimise(self, s: Solver) -> Result<Box<dyn Solution>, ResolutionError> {
        fn run_model<S: SolverModel<Error = ResolutionError>>(
            mut model: S,
            constraints: Vec<Constraint>,
        ) -> Result<Box<dyn Solution>, ResolutionError>
        where
            S::Solution: 'static,
        {
            for c in constraints {
                model.add_constraint(c);
            }
            let sln = model.solve()?;
            Ok(Box::new(sln))
        }

        let to_solve = self.problem.minimise(self.objective);

        match s {
            Solver::Clarabel => run_model(
                to_solve.using(good_lp::solvers::clarabel::clarabel),
                self.constraints,
            ),
            #[cfg(feature = "gurobi")]
            Solver::Gurobi => {
                let solver = LpSolver(good_lp::solvers::lp_solvers::GurobiSolver::new());
                run_model(to_solve.using(solver), self.constraints)
            }

            #[cfg(feature = "highs")]
            Solver::Highs => run_model(
                to_solve.using(good_lp::solvers::highs::highs),
                self.constraints,
            ),
            #[cfg(feature = "cbc")]
            Solver::Cbc => run_model(
                to_solve.using(|x| {
                    let mut prob = good_lp::solvers::coin_cbc::coin_cbc(x);
                    prob.set_parameter("logLevel", "0");
                    prob.set_parameter("slogLevel", "0");
                    prob
                }),
                self.constraints,
            ),
            Solver::Minilp => run_model(
                to_solve.using(good_lp::solvers::minilp::minilp),
                self.constraints,
            ),
        }
    }
}
