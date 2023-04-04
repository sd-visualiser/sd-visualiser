use std::collections::{BTreeSet, HashMap};
use thiserror::Error;

use crate::language::grammar::{ActiveOp, Expr, PassiveOp, Term, Thunk, Value, Variable};
use sd_hyper::graph::{Graph, HyperGraphError, Port, PortIndex};

#[derive(Clone, Debug)]
pub enum Op {
    Passive(PassiveOp),
    Active(ActiveOp),
    Input,
    Output,
    Thunk { args: usize, body: Box<HyperGraph> },
}

impl Op {
    pub fn is_input(&self) -> bool {
        matches!(self, Op::Input)
    }

    pub fn is_output(&self) -> bool {
        matches!(self, Op::Output)
    }
}

pub type HyperGraph = Graph<Op>;

#[derive(Debug, Error)]
pub enum ConvertError {
    #[error("Error constructing hypergraph")]
    HyperGraphError(#[from] HyperGraphError),
    #[error("Couldn't find location of variable `{}`", .0.var)]
    VariableError(Variable),
}

impl Expr {
    pub(crate) fn free_variables(
        &self,
        bound: &mut BTreeSet<Variable>,
        vars: &mut BTreeSet<Variable>,
    ) {
        match self {
            Expr::Val(v) => v.free_variables(bound, vars),
            Expr::Bind(_, var, _, term, _, expr) => {
                term.free_variables(bound, vars);
                bound.insert(var.clone());
                expr.free_variables(bound, vars);
            }
        }
    }

    pub fn to_hypergraph(&self) -> Result<HyperGraph, ConvertError> {
        let mut vars = BTreeSet::new();

        self.free_variables(&mut BTreeSet::new(), &mut vars);

        self.to_hypergraph_from_inputs(vars.into_iter().collect())
    }

    pub(crate) fn to_hypergraph_from_inputs(
        &self,
        inputs: Vec<Variable>,
    ) -> Result<HyperGraph, ConvertError> {
        let mut mapping: HashMap<Variable, Port> = HashMap::new();

        let mut graph = HyperGraph::new();

        let input_node = graph.add_node(Op::Input, vec![], inputs.len())?;

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

    pub(crate) fn process(
        &self,
        graph: &mut HyperGraph,
        mapping: &mut HashMap<Variable, Port>,
    ) -> Result<(), ConvertError> {
        match self {
            Expr::Val(v) => {
                let port = v.process(graph, mapping)?;
                graph.add_node(Op::Output, vec![port], 0)?;
            }
            Expr::Bind(_, var, _, term, _, expr) => {
                let port = term.process(graph, mapping)?;
                mapping.insert(var.clone(), port);
                expr.process(graph, mapping)?;
            }
        }
        Ok(())
    }
}

impl Term {
    pub(crate) fn free_variables(&self, bound: &BTreeSet<Variable>, vars: &mut BTreeSet<Variable>) {
        match self {
            Term::Val(v) => v.free_variables(bound, vars),
            Term::ActiveOp(_, _, vs, _) => {
                for v in vs {
                    v.free_variables(bound, vars);
                }
            }
            Term::Thunk(thunk) => thunk.free_variables(bound, vars),
        }
    }

    pub(crate) fn process(
        &self,
        graph: &mut HyperGraph,
        mapping: &mut HashMap<Variable, Port>,
    ) -> Result<Port, ConvertError> {
        match self {
            Term::Val(v) => v.process(graph, mapping),
            Term::ActiveOp(op, _, vals, _) => {
                let mut inputs = vec![];
                for v in vals {
                    inputs.push(v.process(graph, mapping)?);
                }
                let node = graph.add_node(Op::Active(*op), inputs, 1)?;
                Ok(Port {
                    node,
                    index: PortIndex(0),
                })
            }
            Term::Thunk(thunk) => thunk.process(graph, mapping),
        }
    }
}

impl Value {
    pub(crate) fn free_variables(&self, bound: &BTreeSet<Variable>, vars: &mut BTreeSet<Variable>) {
        match self {
            Value::Var(v) => {
                if !bound.contains(v) {
                    vars.insert(v.clone());
                }
            }
            Value::PassiveOp(_, _, vs, _) => {
                for v in vs {
                    v.free_variables(bound, vars)
                }
            }
        }
    }

    pub(crate) fn process(
        &self,
        graph: &mut HyperGraph,
        mapping: &mut HashMap<Variable, Port>,
    ) -> Result<Port, ConvertError> {
        match self {
            Value::Var(v) => mapping
                .get(v)
                .copied()
                .ok_or(ConvertError::VariableError(v.clone())),
            Value::PassiveOp(op, _, vals, _) => {
                let mut inputs = vec![];
                for v in vals {
                    inputs.push(v.process(graph, mapping)?);
                }
                let node = graph.add_node(Op::Passive(*op), inputs, 1)?;
                Ok(Port {
                    node,
                    index: PortIndex(0),
                })
            }
        }
    }
}

impl Thunk {
    pub(crate) fn free_variables(&self, bound: &BTreeSet<Variable>, vars: &mut BTreeSet<Variable>) {
        let mut bound = bound.clone();

        for arg in &self.args {
            bound.insert(arg.clone());
        }

        self.body.free_variables(&mut bound, vars);
    }

    pub(crate) fn process(
        &self,
        graph: &mut HyperGraph,
        mapping: &mut HashMap<Variable, Port>,
    ) -> Result<Port, ConvertError> {
        let mut vars = BTreeSet::new();

        self.free_variables(&BTreeSet::new(), &mut vars);

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
            Op::Thunk {
                args: self.args.len(),
                body: Box::new(graph_inner),
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

impl From<&str> for Variable {
    fn from(value: &str) -> Self {
        Variable {
            var: value.to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use anyhow::{anyhow, Context, Result};
    use insta::assert_debug_snapshot;
    use rstest::{fixture, rstest};

    use crate::language::grammar::{parse, Expr, Variable};

    #[fixture]
    fn basic_program() -> Result<Expr> {
        parse("bind x = 1() in x")
            .map_err(|x| anyhow!("{:?}", x))
            .context("Could not parse basic program")
    }

    #[fixture]
    fn free_vars() -> Result<Expr> {
        parse("bind x = y in z")
            .map_err(|x| anyhow!("{:?}", x))
            .context("Could not parse free variable program")
    }

    // Make this something meaningful
    #[fixture]
    fn thunks() -> Result<Expr> {
        parse("bind a = x0.1() in bind b = x0.bind z = +(x0,y) in z in bind x = +(a,b) in x")
            .map_err(|x| anyhow!("{:?}", x))
            .context("Could not parse thunk program")
    }

    #[rstest]
    fn check_parse(
        basic_program: Result<Expr>,
        free_vars: Result<Expr>,
        thunks: Result<Expr>,
    ) -> Result<()> {
        basic_program?;
        free_vars?;
        thunks?;
        Ok(())
    }

    #[rstest]
    #[case(basic_program(), vec![])]
    #[case(free_vars(), vec!["y".into(), "z".into()])]
    #[case(thunks(), vec!["y".into()])]
    fn free_var_test(#[case] expr: Result<Expr>, #[case] vars: Vec<Variable>) -> Result<()> {
        let expr = expr?;
        let mut free_vars = BTreeSet::new();
        expr.free_variables(&mut BTreeSet::new(), &mut free_vars);

        assert_eq!(free_vars, vars.into_iter().collect());

        Ok(())
    }

    #[rstest]
    fn hypergraph_test_basic(basic_program: Result<Expr>) -> Result<()> {
        let graph = basic_program?.to_hypergraph()?;

        assert_debug_snapshot!(graph);

        Ok(())
    }

    #[rstest]
    fn hypergraph_test_free_var(free_vars: Result<Expr>) -> Result<()> {
        let graph = free_vars?.to_hypergraph()?;

        assert_debug_snapshot!(graph);

        Ok(())
    }

    #[rstest]
    fn hypergraph_test_thunk(thunks: Result<Expr>) -> Result<()> {
        let graph = thunks?.to_hypergraph()?;

        assert_debug_snapshot!(graph);

        Ok(())
    }
}
