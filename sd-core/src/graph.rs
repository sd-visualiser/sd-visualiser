use std::{collections::HashMap, fmt::Display};
use thiserror::Error;

use crate::hypergraph_good::{EdgeStrength, Fragment, Graph, HyperGraph, HyperGraphError, OutPort};
use crate::language::{ActiveOp, BindClause, Expr, PassiveOp, Term, Thunk, Value, Variable};

#[cfg(not(test))]
use std::collections::HashSet;

#[cfg(test)]
use serde::Serialize;
#[cfg(test)]
use std::collections::BTreeSet as HashSet;

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(Serialize))]
pub enum Op {
    Active(ActiveOp),
    Passive(PassiveOp),
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Active(op) => op.fmt(f),
            Self::Passive(op) => op.fmt(f),
        }
    }
}

impl From<ActiveOp> for Op {
    fn from(a: ActiveOp) -> Self {
        Op::Active(a)
    }
}

impl From<PassiveOp> for Op {
    fn from(p: PassiveOp) -> Self {
        Op::Passive(p)
    }
}

pub type SyntaxHyperGraphBuilder = HyperGraph<Op, ()>;
pub type SyntaxHyperGraph = HyperGraph<Op, ()>;

#[derive(Debug, Error)]
pub enum ConvertError {
    #[error("Error constructing hypergraph")]
    HyperGraphError(#[from] HyperGraphError<Op, ()>),
    #[error("Couldn't find location of variable `{0}`")]
    VariableError(Variable),
    #[error("Fragment did not have output")]
    NoOutputError,
}

pub(crate) trait Syntax {
    /// This piece of syntax generates both bound and free variables.
    fn free_variables(&self, bound: &mut HashSet<Variable>, vars: &mut HashSet<Variable>);

    type ProcessOutput;

    /// Insert this piece of syntax into a hypergraph and update the mapping of variables to ports.
    ///
    /// # Errors
    ///
    /// This function will return an error if variables are malformed.
    fn process<F>(
        &self,
        fragment: &mut F,
        mapping: &mut HashMap<Variable, (OutPort<Op, (), false>, EdgeStrength)>,
    ) -> Result<Self::ProcessOutput, ConvertError>
    where
        F: Fragment<Op, ()> + Graph<Op, (), false>;
}

impl Syntax for Expr {
    fn free_variables(&self, bound: &mut HashSet<Variable>, vars: &mut HashSet<Variable>) {
        for bc in &self.binds {
            bc.free_variables(bound, vars);
        }
        self.value.free_variables(bound, vars);
    }

    type ProcessOutput = ();

    fn process<F>(
        &self,
        fragment: &mut F,
        mapping: &mut HashMap<Variable, (OutPort<Op, (), false>, EdgeStrength)>,
    ) -> Result<Self::ProcessOutput, ConvertError>
    where
        F: Fragment<Op, ()> + Graph<Op, (), false>,
    {
        for bc in &self.binds {
            bc.process(fragment, mapping)?;
        }
        let (port, _) = self.value.process(fragment, mapping)?;
        let output_port = fragment
            .graph_outputs()
            .next()
            .ok_or(ConvertError::NoOutputError)?;
        fragment.link(port, output_port, EdgeStrength::Strong)?;
        Ok(())
    }
}

impl Syntax for BindClause {
    fn free_variables(&self, bound: &mut HashSet<Variable>, vars: &mut HashSet<Variable>) {
        self.term.free_variables(bound, vars);
        bound.insert(self.var.clone());
    }

    type ProcessOutput = ();

    fn process<F>(
        &self,
        fragment: &mut F,
        mapping: &mut HashMap<Variable, (OutPort<Op, (), false>, EdgeStrength)>,
    ) -> Result<Self::ProcessOutput, ConvertError>
    where
        F: Fragment<Op, ()> + Graph<Op, (), false>,
    {
        let port = self.term.process(fragment, mapping)?;
        mapping.insert(self.var.clone(), port);
        Ok(())
    }
}

impl Syntax for Term {
    fn free_variables(&self, bound: &mut HashSet<Variable>, vars: &mut HashSet<Variable>) {
        match self {
            Term::Value(v) => v.free_variables(bound, vars),
            Term::ActiveOp(_, vs) => {
                for v in vs {
                    v.free_variables(bound, vars);
                }
            }
            Term::Thunk(thunk) => thunk.free_variables(bound, vars),
        }
    }

    type ProcessOutput = (OutPort<Op, (), false>, EdgeStrength);

    fn process<F>(
        &self,
        fragment: &mut F,
        mapping: &mut HashMap<Variable, (OutPort<Op, (), false>, EdgeStrength)>,
    ) -> Result<Self::ProcessOutput, ConvertError>
    where
        F: Fragment<Op, ()> + Graph<Op, (), false>,
    {
        match self {
            Term::Value(v) => v.process(fragment, mapping),
            Term::ActiveOp(op, vals) => {
                let operation = fragment.add_operation(vals.len(), vec![()], (*op).into());
                for (v, in_port) in vals.iter().zip(operation.inputs()) {
                    let (out_port, strength) = v.process(fragment, mapping)?;
                    fragment.link(out_port, in_port, strength)?;
                }

                let output = operation
                    .outputs()
                    .next()
                    .ok_or(ConvertError::NoOutputError)?;

                Ok((output, EdgeStrength::Strong))
            }
            Term::Thunk(thunk) => Ok((thunk.process(fragment, mapping)?, EdgeStrength::Strong)),
        }
    }
}

impl Syntax for Value {
    fn free_variables(&self, bound: &mut HashSet<Variable>, vars: &mut HashSet<Variable>) {
        match self {
            Value::Var(v) => {
                if !bound.contains(v) {
                    vars.insert(v.clone());
                }
            }
            Value::PassiveOp(_, vs) => {
                for v in vs {
                    v.free_variables(bound, vars)
                }
            }
        }
    }

    type ProcessOutput = (OutPort<Op, (), false>, EdgeStrength);

    fn process<F>(
        &self,
        fragment: &mut F,
        mapping: &mut HashMap<Variable, (OutPort<Op, (), false>, EdgeStrength)>,
    ) -> Result<Self::ProcessOutput, ConvertError>
    where
        F: Fragment<Op, ()> + Graph<Op, (), false>,
    {
        match self {
            Value::Var(v) => mapping
                .get(v)
                .cloned()
                .ok_or(ConvertError::VariableError(v.clone())),
            Value::PassiveOp(op, vals) => {
                let operation = fragment.add_operation(vals.len(), vec![()], (*op).into());

                for (v, in_port) in vals.iter().zip(operation.inputs()) {
                    let (out_port, strength) = v.process(fragment, mapping)?;
                    fragment.link(out_port, in_port, strength)?;
                }

                let output = operation
                    .outputs()
                    .next()
                    .ok_or(ConvertError::NoOutputError)?;

                Ok((output, EdgeStrength::Strong))
            }
        }
    }
}

impl Syntax for Thunk {
    fn free_variables(&self, bound: &mut HashSet<Variable>, vars: &mut HashSet<Variable>) {
        let mut bound = bound.clone(); // create new scope for bound variables in thunk

        for arg in &self.args {
            bound.insert(arg.clone());
        }

        self.body.free_variables(&mut bound, vars);
    }

    type ProcessOutput = OutPort<Op, (), false>;

    fn process<F>(
        &self,
        fragment: &mut F,
        mapping: &mut HashMap<Variable, (OutPort<Op, (), false>, EdgeStrength)>,
    ) -> Result<Self::ProcessOutput, ConvertError>
    where
        F: Fragment<Op, ()> + Graph<Op, (), false>,
    {
        let mut vars = HashSet::new();

        self.free_variables(&mut HashSet::new(), &mut vars);

        let thunk = fragment.add_thunk(vec![(); vars.len()], vec![(); self.args.len()], vec![()]);

        for (v, in_port) in vars.iter().zip(thunk.inputs()) {
            let (out_port, strength) = mapping
                .get(v)
                .cloned()
                .ok_or(ConvertError::VariableError(v.clone()))?;
            fragment.link(out_port, in_port, strength)?;
        }

        fragment.in_thunk(thunk.clone(), |mut inner_fragment| {
            let mut inner_mapping: HashMap<_, _> = vars
                .iter()
                .chain(self.args.iter())
                .cloned()
                .zip(
                    inner_fragment
                        .graph_inputs()
                        .map(|x| (x, EdgeStrength::Strong)),
                )
                .collect();
            self.body.process(&mut inner_fragment, &mut inner_mapping)
        })?;

        let out_port = thunk.outputs().next().ok_or(ConvertError::NoOutputError)?;

        Ok(out_port)
    }
}

impl TryFrom<&Expr> for SyntaxHyperGraph {
    type Error = ConvertError;

    fn try_from(expr: &Expr) -> Result<Self, Self::Error> {
        let mut vars = HashSet::new();

        expr.free_variables(&mut HashSet::new(), &mut vars);

        let mut graph = HyperGraph::new(vec![(); vars.len()], 1);

        let mut mapping: HashMap<_, _> = vars
            .into_iter()
            .zip(
                graph
                    .graph_inputs()
                    .map(|out_port| (out_port, EdgeStrength::Strong)),
            )
            .collect();

        expr.process(&mut graph, &mut mapping)?;

        Ok(graph.build()?)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet as HashSet;

    use anyhow::Result;
    use rstest::rstest;

    use super::Syntax;
    use crate::{
        graph::SyntaxHyperGraph,
        language::{tests::*, Expr, Variable},
    };

    #[rstest]
    #[case(basic_program(), vec![])]
    #[case(free_vars(), vec!["y".into(), "z".into()])]
    #[case(thunks(), vec!["x".into()])]
    #[case(fact(), vec![])]
    fn free_var_test(#[case] expr: Result<Expr>, #[case] vars: Vec<Variable>) -> Result<()> {
        let expr = expr?;
        let mut free_vars = HashSet::new();
        expr.free_variables(&mut HashSet::new(), &mut free_vars);

        assert_eq!(free_vars, vars.into_iter().collect());

        Ok(())
    }

    #[rstest]
    #[case("basic_program", basic_program())]
    #[case("free_vars", free_vars())]
    #[case("thunks", thunks())]
    #[case("fact", fact())]
    fn hypergraph_snapshots(#[case] name: &str, #[case] expr: Result<Expr>) -> Result<()> {
        let graph: SyntaxHyperGraph = (&expr?).try_into()?;

        // insta::with_settings!({sort_maps => true}, {
        //     insta::assert_ron_snapshot!(name, graph);
        // });

        Ok(())
    }
}
