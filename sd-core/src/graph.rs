use std::collections::{HashMap, HashSet};

use thiserror::Error;
use tracing::debug;

use crate::{
    free_vars::FreeVars,
    hypergraph::{Fragment, Graph, HyperGraph, HyperGraphError, InPort, OutPort},
    language::spartan::{Expr, Op, Thunk, Value, Variable},
};

pub type SyntaxHyperGraphBuilder = HyperGraph<Op, Name, false>;
pub type SyntaxHyperGraph = HyperGraph<Op, Name>;
pub type SyntaxInPort<const BUILT: bool = true> = InPort<Op, Name, BUILT>;
pub type SyntaxOutPort<const BUILT: bool = true> = OutPort<Op, Name, BUILT>;

#[derive(Debug, Error)]
pub enum ConvertError {
    #[error("Error constructing hypergraph")]
    HyperGraphError(#[from] HyperGraphError<Op, Name>),
    #[error("Couldn't find location of variable `{0}`")]
    VariableError(Variable),
    #[error("Variable aliased `{0}`")]
    Aliased(Variable),
    #[error("Fragment did not have output")]
    NoOutputError,
}

pub type Name = Option<Variable>;

#[derive(Debug)]
struct Environment<'env, F> {
    free_vars: &'env FreeVars,
    fragment: F,
    inputs: HashMap<SyntaxInPort<false>, Variable>,
    outputs: HashMap<Variable, SyntaxOutPort<false>>,
}

impl<'env, F> Environment<'env, F>
where
    F: Fragment<NodeWeight = Op, EdgeWeight = Name>,
{
    fn new(free_vars: &'env FreeVars, fragment: F) -> Self {
        Self {
            free_vars,
            fragment,
            inputs: HashMap::default(),
            outputs: HashMap::default(),
        }
    }

    /// Insert value into a hypergraph and update the environment.
    ///
    /// # Returns
    ///
    /// This function returns an outport, which should be linked by the caller.
    ///
    /// # Errors
    ///
    /// This function will return an error if variables are malformed.
    fn process_value_out(&mut self, value: &Value) -> Result<SyntaxOutPort<false>, ConvertError> {
        match value {
            Value::Variable(var) => Ok(self.outputs[var].clone()),
            Value::Op { op, vs, ds } => {
                let operation_node =
                    self.fragment
                        .add_operation(vs.len() + ds.len(), [None], op.clone());
                for (value, inport) in vs.iter().zip(operation_node.inputs()) {
                    self.process_value(value, inport)?;
                }
                for (thunk, inport) in ds.iter().zip(operation_node.inputs().skip(vs.len())) {
                    self.process_thunk(thunk, inport)?;
                }

                let outport = operation_node
                    .outputs()
                    .next()
                    .ok_or(ConvertError::NoOutputError)?;
                Ok(outport)
            }
        }
    }

    /// Insert value into a hypergraph and update the environment.
    ///
    /// The caller expects the inport that is passed in to be linked.
    ///
    /// # Errors
    ///
    /// This function will return an error if variables are malformed.
    fn process_value(
        &mut self,
        value: &Value,
        inport: SyntaxInPort<false>,
    ) -> Result<(), ConvertError> {
        match value {
            Value::Variable(v) => self
                .inputs
                .insert(inport, v.clone())
                .is_none()
                .then_some(())
                .ok_or(ConvertError::Aliased(v.clone())),
            Value::Op { op, vs, ds } => {
                // make operation node
                let operation_node =
                    self.fragment
                        .add_operation(vs.len() + ds.len(), [None], op.clone());
                for (value, inport) in vs.iter().zip(operation_node.inputs()) {
                    self.process_value(value, inport)?;
                }
                for (thunk, inport) in ds.iter().zip(operation_node.inputs().skip(vs.len())) {
                    self.process_thunk(thunk, inport)?;
                }

                let outport = operation_node
                    .outputs()
                    .next()
                    .ok_or(ConvertError::NoOutputError)?;
                tracing::debug!("operation node made, inputs processed, about to link");
                self.fragment.link(outport, inport)?;

                Ok(())
            }
        }
    }

    /// Insert thunk into a hypergraph and update the environment.
    ///
    /// The caller expects the inport that is passed in to be linked.
    ///
    /// # Errors
    ///
    /// This function will return an error if variables are malformed.
    fn process_thunk(
        &mut self,
        thunk: &Thunk,
        inport: SyntaxInPort<false>,
    ) -> Result<(), ConvertError> {
        let mut free: HashSet<_> = self.free_vars[&thunk.body].iter().cloned().collect();

        for var in &thunk.args {
            free.remove(var);
        }
        let thunk_node = self.fragment.add_thunk(
            free.iter().cloned().map(Some),
            thunk.args.iter().map(|name| Some(name.clone())),
            [None],
        );
        debug!("new thunk node {:?}", thunk_node);

        for (var, inport) in free.iter().cloned().zip(thunk_node.inputs()) {
            self.inputs
                .insert(inport, var.clone())
                .is_none()
                .then_some(())
                .ok_or(ConvertError::Aliased(var))?;
        }

        self.fragment
            .in_thunk(thunk_node.clone(), |inner_fragment| {
                let mut thunk_env = Environment::new(self.free_vars, inner_fragment);

                for (var, outport) in free.iter().cloned().zip(thunk_node.free_inputs()) {
                    thunk_env
                        .outputs
                        .insert(var.clone(), outport)
                        .is_none()
                        .then_some(())
                        .ok_or(ConvertError::Aliased(var))?;
                }
                for (var, outport) in thunk.args.iter().zip(thunk_node.bound_inputs()) {
                    thunk_env
                        .outputs
                        .insert(var.clone(), outport)
                        .is_none()
                        .then_some(())
                        .ok_or(ConvertError::Aliased(var.clone()))?;
                }
                thunk_env.process_expr(&thunk.body)
            })?;

        let outport = thunk_node
            .outputs()
            .next()
            .ok_or(ConvertError::NoOutputError)?;
        self.fragment.link(outport, inport)?;

        Ok(())
    }

    /// Insert expression into a hypergraph, consuming the environment.
    ///
    /// # Returns
    ///
    /// This function returns the completed fragment of the hypergraph.
    ///
    /// # Errors
    ///
    /// This function will return an error if variables are malformed.
    fn process_expr(mut self, expr: &Expr) -> Result<F, ConvertError> {
        for bind in &expr.binds {
            let outport = self.process_value_out(&bind.value)?;
            self.outputs
                .insert(bind.var.clone(), outport)
                .is_none()
                .then_some(())
                .ok_or(ConvertError::Aliased(bind.var.clone()))?;
        }
        debug!("processed binds: {:?}", self.outputs);

        let graph_output = self
            .fragment
            .graph_outputs()
            .next()
            .ok_or(ConvertError::NoOutputError)?;
        self.process_value(&expr.value, graph_output)?;

        // link up loops
        for (inport, var) in self.inputs {
            let outport = self.outputs[&var].clone();
            self.fragment.link(outport, inport).unwrap();
        }

        Ok(self.fragment)
    }
}

impl TryFrom<&Expr> for SyntaxHyperGraph {
    type Error = ConvertError;

    #[tracing::instrument(ret, err)]
    fn try_from(expr: &Expr) -> Result<Self, Self::Error> {
        let mut free_vars = FreeVars::default();

        free_vars.expr(expr);

        let free: Vec<Variable> = free_vars[expr].iter().cloned().collect();
        debug!("free variables: {:?}", free);
        let graph = HyperGraph::new(free.iter().cloned().map(Some).collect(), 1);
        debug!("made initial hypergraph: {:?}", graph);

        let mut env = Environment::new(&free_vars, graph);
        debug!("determined environment: {:?}", env);

        for (var, outport) in free.iter().zip(env.fragment.graph_inputs()) {
            env.outputs
                .insert(var.clone(), outport)
                .is_none()
                .then_some(())
                .ok_or(ConvertError::Aliased(var.clone()))?;
        }
        debug!("processed free variables: {:?}", env.outputs);

        let graph = env.process_expr(expr)?;

        Ok(graph.build()?)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use anyhow::Result;
    use rstest::rstest;

    use crate::{
        free_vars::FreeVars,
        graph::SyntaxHyperGraph,
        language::spartan::{
            tests::{
                aliasing, bad, basic_program, buggy, fact, free_vars, nest, recursive, thunks,
            },
            Expr, Variable,
        },
    };

    #[rstest]
    #[case(basic_program(), vec![])]
    #[case(free_vars(), vec!["y".into(), "z".into()])]
    #[case(thunks(), vec!["x".into()])]
    #[case(fact(), vec![])]
    #[case(nest(), vec![])]
    fn free_var_test(#[case] expr: Result<Expr>, #[case] free_vars: Vec<Variable>) -> Result<()> {
        let expr = expr?;

        let mut fv = FreeVars::default();
        fv.expr(&expr);

        assert_eq!(
            fv[&expr].clone(),
            free_vars.into_iter().collect::<HashSet<_>>()
        );

        Ok(())
    }

    #[rstest]
    #[case("bad", bad())]
    #[case("basic_program", basic_program())]
    #[case("buggy", buggy())]
    #[case("fact", fact())]
    #[case("free_vars", free_vars())]
    #[case("recursive", recursive())]
    #[case("thunks", thunks())]
    #[case("aliasing", aliasing())]
    fn hypergraph_snapshots(#[case] name: &str, #[case] expr: Result<Expr>) -> Result<()> {
        let graph: SyntaxHyperGraph = (&expr?).try_into()?;

        // insta::with_settings!({sort_maps => true}, {
        //     insta::assert_ron_snapshot!(name, graph);
        // });

        Ok(())
    }
}
