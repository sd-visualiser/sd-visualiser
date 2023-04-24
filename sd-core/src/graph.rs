use std::{
    collections::{BTreeSet, HashMap},
    fmt::Display,
};
use thiserror::Error;

use crate::hypergraph::{HyperGraph, HyperGraphError, Node, Port, PortIndex};
use crate::language::{ActiveOp, BindClause, Expr, PassiveOp, Term, Thunk, Value, Variable};

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
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

pub type SyntaxHyperGraph = HyperGraph<Op>;

#[derive(Debug, Error)]
pub enum ConvertError {
    #[error("Error constructing hypergraph")]
    HyperGraphError(#[from] HyperGraphError),
    #[error("Couldn't find location of variable `{0}`")]
    VariableError(Variable),
}

pub(crate) trait Syntax {
    /// This piece of syntax generates both bound and free variables.
    fn free_variables(&self, bound: &mut BTreeSet<Variable>, vars: &mut BTreeSet<Variable>);

    type ProcessOutput;

    /// Insert this piece of syntax into a hypergraph and update the mapping of variables to ports.
    ///
    /// # Errors
    ///
    /// This function will return an error if variables are malformed.
    fn process(
        &self,
        graph: &mut SyntaxHyperGraph,
        mapping: &mut HashMap<Variable, Port>,
    ) -> Result<Self::ProcessOutput, ConvertError>;
}

impl Syntax for Expr {
    fn free_variables(&self, bound: &mut BTreeSet<Variable>, vars: &mut BTreeSet<Variable>) {
        for bc in &self.binds {
            bc.free_variables(bound, vars);
        }
        self.value.free_variables(bound, vars);
    }

    type ProcessOutput = ();

    fn process(
        &self,
        graph: &mut SyntaxHyperGraph,
        mapping: &mut HashMap<Variable, Port>,
    ) -> Result<Self::ProcessOutput, ConvertError> {
        for bc in &self.binds {
            bc.process(graph, mapping)?;
        }
        let port = self.value.process(graph, mapping)?;
        graph.add_node(Node::Output, vec![port], 0)?;
        Ok(())
    }
}

impl Syntax for BindClause {
    fn free_variables(&self, bound: &mut BTreeSet<Variable>, vars: &mut BTreeSet<Variable>) {
        self.term.free_variables(bound, vars);
        bound.insert(self.var.clone());
    }

    type ProcessOutput = ();

    fn process(
        &self,
        graph: &mut SyntaxHyperGraph,
        mapping: &mut HashMap<Variable, Port>,
    ) -> Result<Self::ProcessOutput, ConvertError> {
        let port = self.term.process(graph, mapping)?;
        mapping.insert(self.var.clone(), port);
        Ok(())
    }
}

impl Syntax for Term {
    fn free_variables(&self, bound: &mut BTreeSet<Variable>, vars: &mut BTreeSet<Variable>) {
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

    type ProcessOutput = Port;

    fn process(
        &self,
        graph: &mut SyntaxHyperGraph,
        mapping: &mut HashMap<Variable, Port>,
    ) -> Result<Self::ProcessOutput, ConvertError> {
        match self {
            Term::Value(v) => v.process(graph, mapping),
            Term::ActiveOp(op, vals) => {
                let mut inputs = vec![];
                for v in vals {
                    inputs.push(v.process(graph, mapping)?);
                }
                let node = graph.add_node(Node::w(*op), inputs, 1)?;
                Ok(Port {
                    node,
                    index: PortIndex(0),
                })
            }
            Term::Thunk(thunk) => thunk.process(graph, mapping),
        }
    }
}

impl Syntax for Value {
    fn free_variables(&self, bound: &mut BTreeSet<Variable>, vars: &mut BTreeSet<Variable>) {
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

    type ProcessOutput = Port;

    fn process(
        &self,
        graph: &mut SyntaxHyperGraph,
        mapping: &mut HashMap<Variable, Port>,
    ) -> Result<Self::ProcessOutput, ConvertError> {
        match self {
            Value::Var(v) => mapping
                .get(v)
                .copied()
                .ok_or(ConvertError::VariableError(v.clone())),
            Value::PassiveOp(op, vals) => {
                let mut inputs = vec![];
                for v in vals {
                    inputs.push(v.process(graph, mapping)?);
                }
                let node = graph.add_node(Node::w(*op), inputs, 1)?;
                Ok(Port {
                    node,
                    index: PortIndex(0),
                })
            }
        }
    }
}

impl Syntax for Thunk {
    fn free_variables(&self, bound: &mut BTreeSet<Variable>, vars: &mut BTreeSet<Variable>) {
        let mut bound = bound.clone(); // create new scope for bound variables in thunk

        for arg in &self.args {
            bound.insert(arg.clone());
        }

        self.body.free_variables(&mut bound, vars);
    }

    type ProcessOutput = Port;

    fn process(
        &self,
        graph: &mut SyntaxHyperGraph,
        mapping: &mut HashMap<Variable, Port>,
    ) -> Result<Self::ProcessOutput, ConvertError> {
        let mut vars = BTreeSet::new();

        self.free_variables(&mut BTreeSet::new(), &mut vars);

        let inputs = vars
            .iter()
            .map(|v| {
                mapping
                    .get(v)
                    .cloned()
                    .ok_or(ConvertError::VariableError(v.clone()))
            })
            .collect::<Result<Vec<_>, _>>()?;

        let mut vars_vec: Vec<_> = vars.into_iter().collect();

        vars_vec.extend(self.args.clone());

        let graph_inner = self.body.to_hypergraph_from_inputs(vars_vec)?;

        let node = graph.add_node(
            Node::Thunk {
                args: self.args.len(),
                body: graph_inner,
            },
            inputs,
            1,
        )?;

        Ok(Port {
            node,
            index: PortIndex(0),
        })
    }
}

impl TryFrom<&Expr> for SyntaxHyperGraph {
    type Error = ConvertError;

    fn try_from(expr: &Expr) -> Result<Self, Self::Error> {
        let mut vars = BTreeSet::new();

        expr.free_variables(&mut BTreeSet::new(), &mut vars);

        expr.to_hypergraph_from_inputs(vars.into_iter().collect())
    }
}

impl Expr {
    pub(crate) fn to_hypergraph_from_inputs(
        &self,
        inputs: Vec<Variable>,
    ) -> Result<SyntaxHyperGraph, ConvertError> {
        let mut mapping: HashMap<Variable, Port> = HashMap::new();

        let mut graph = SyntaxHyperGraph::new();

        let input_node = graph.add_node(Node::Input, vec![], inputs.len())?;

        for (i, var) in inputs.into_iter().enumerate() {
            mapping.insert(
                var,
                Port {
                    node: input_node,
                    index: PortIndex(i),
                },
            );
        }

        self.process(&mut graph, &mut mapping)?;

        Ok(graph)
    }
}

impl From<&str> for Variable {
    fn from(value: &str) -> Self {
        Variable(value.to_string())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use anyhow::Result;
    use insta::assert_debug_snapshot;
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
        let mut free_vars = BTreeSet::new();
        expr.free_variables(&mut BTreeSet::new(), &mut free_vars);

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

        assert_debug_snapshot!(name, graph);

        Ok(())
    }
}
