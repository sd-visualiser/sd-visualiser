use std::{collections::HashMap, fmt::Display};
use thiserror::Error;

use crate::hypergraph::{EdgeStrength, Fragment, Graph, HyperGraph, HyperGraphError, OutPort};
use crate::language::visitor::Visitable;
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

struct Variables {
    free: HashSet<Variable>,
    bound: Vec<HashSet<Variable>>,
}

impl Default for Variables {
    fn default() -> Self {
        Self {
            free: Default::default(),
            bound: vec![Default::default()],
        }
    }
}

impl crate::language::visitor::Visitor for Variables {
    fn after_bind_clause(&mut self, bind_clause: &BindClause) {
        let scope = self.bound.last_mut().unwrap();
        scope.insert(bind_clause.var.clone());
    }

    fn visit_value(&mut self, value: &Value) {
        if let Value::Var(v) = value {
            if !self.bound.iter().any(|scope| scope.contains(v)) {
                self.free.insert(v.clone());
            }
        }
    }

    fn visit_thunk(&mut self, thunk: &Thunk) {
        // make new scope for thunk
        self.bound.push(thunk.args.iter().cloned().collect());
    }

    fn after_thunk(&mut self, _thunk: &Thunk) {
        // discard scope
        assert!(self.bound.pop().is_some());
    }
}

pub(crate) trait Syntax {
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
    type ProcessOutput = OutPort<Op, (), false>;

    fn process<F>(
        &self,
        fragment: &mut F,
        mapping: &mut HashMap<Variable, (OutPort<Op, (), false>, EdgeStrength)>,
    ) -> Result<Self::ProcessOutput, ConvertError>
    where
        F: Fragment<Op, ()> + Graph<Op, (), false>,
    {
        let mut vars: Variables = Default::default();

        self.walk(&mut vars);

        let thunk = fragment.add_thunk(
            vec![(); vars.free.len()],
            vec![(); self.args.len()],
            vec![()],
        );

        for (v, in_port) in vars.free.iter().zip(thunk.inputs()) {
            let (out_port, strength) = mapping
                .get(v)
                .cloned()
                .ok_or(ConvertError::VariableError(v.clone()))?;
            fragment.link(out_port, in_port, strength)?;
        }

        fragment.in_thunk(thunk.clone(), |mut inner_fragment| {
            let mut inner_mapping: HashMap<_, _> = vars
                .free
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
        let mut vars: Variables = Default::default();

        expr.walk(&mut vars);

        let mut graph = HyperGraph::new(vec![(); vars.free.len()], 1);

        let mut mapping: HashMap<_, _> = vars
            .free
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
    use anyhow::Result;
    use rstest::rstest;

    use crate::{
        graph::{SyntaxHyperGraph, Variables},
        language::{tests::*, visitor::Visitable, Expr, Variable},
    };

    #[rstest]
    #[case(basic_program(), vec![])]
    #[case(free_vars(), vec!["y".into(), "z".into()])]
    #[case(thunks(), vec!["x".into()])]
    #[case(fact(), vec![])]
    fn free_var_test(#[case] expr: Result<Expr>, #[case] free_vars: Vec<Variable>) -> Result<()> {
        let expr = expr?;
        let mut vars: Variables = Default::default();
        expr.walk(&mut vars);

        assert_eq!(vars.free, free_vars.into_iter().collect());

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
