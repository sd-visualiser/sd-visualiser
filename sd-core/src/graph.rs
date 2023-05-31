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
struct Environment<'env> {
    free_vars: &'env FreeVars,
    inputs: HashMap<SyntaxInPort<false>, Variable>,
    outputs: HashMap<Variable, SyntaxOutPort<false>>,
}

impl<'env> Environment<'env> {
    fn new(free_vars: &'env FreeVars) -> Self {
        Self {
            free_vars,
            inputs: HashMap::default(),
            outputs: HashMap::default(),
        }
    }
}

trait ProcessOut {
    /// Insert this piece of syntax into a hypergraph and update the mapping of variables to outports.
    ///
    /// # Returns
    ///
    /// This function returns an outport, which should be linked by the caller.
    ///
    /// # Errors
    ///
    /// This function will return an error if variables are malformed.
    fn process_out<F>(
        &self,
        fragment: &mut F,
        environment: &mut Environment,
    ) -> Result<SyntaxOutPort<false>, ConvertError>
    where
        F: Fragment<NodeWeight = Op, EdgeWeight = Name>;
}

trait ProcessIn {
    /// Insert this piece of syntax into a hypergraph and update the mapping of inports to variables.
    ///
    /// The caller expects the inport that is passed in to be linked.
    ///
    /// # Errors
    ///
    /// This function will return an error if variables are malformed.
    fn process_in<F>(
        &self,
        fragment: &mut F,
        environment: &mut Environment,
        inport: SyntaxInPort<false>,
    ) -> Result<(), ConvertError>
    where
        F: Fragment<NodeWeight = Op, EdgeWeight = Name>;
}

trait Process {
    /// Insert this piece of syntax into a hypergraph and update the mapping of inports to variables.
    ///
    /// # Errors
    ///
    /// This function will return an error if variables are malformed.
    fn process<F>(&self, fragment: &mut F, environment: Environment) -> Result<(), ConvertError>
    where
        F: Fragment<NodeWeight = Op, EdgeWeight = Name>;
}

impl ProcessIn for Value {
    fn process_in<F>(
        &self,
        fragment: &mut F,
        environment: &mut Environment,
        inport: SyntaxInPort<false>,
    ) -> Result<(), ConvertError>
    where
        F: Fragment<NodeWeight = Op, EdgeWeight = Name>,
    {
        match self {
            Value::Variable(v) => environment
                .inputs
                .insert(inport, v.clone())
                .is_none()
                .then_some(())
                .ok_or(ConvertError::Aliased(v.clone())),
            Value::Op { op, vs, ds } => {
                // make operation node
                let operation_node =
                    fragment.add_operation(vs.len() + ds.len(), [None], op.clone());
                for (value, inport) in vs.iter().zip(operation_node.inputs()) {
                    value.process_in(fragment, environment, inport)?;
                }
                for (thunk, inport) in ds.iter().zip(operation_node.inputs().skip(vs.len())) {
                    thunk.process_in(fragment, environment, inport)?;
                }

                let outport = operation_node
                    .outputs()
                    .next()
                    .ok_or(ConvertError::NoOutputError)?;
                tracing::debug!("operation node made, inputs processed, about to link");
                fragment.link(outport, inport)?;

                Ok(())
            }
        }
    }
}

impl Process for Expr {
    fn process<F>(&self, fragment: &mut F, mut environment: Environment) -> Result<(), ConvertError>
    where
        F: Fragment<NodeWeight = Op, EdgeWeight = Name>,
    {
        for bind in &self.binds {
            let outport = bind.value.process_out(fragment, &mut environment)?;
            environment
                .outputs
                .insert(bind.var.clone(), outport)
                .is_none()
                .then_some(())
                .ok_or(ConvertError::Aliased(bind.var.clone()))?;
        }
        debug!("processed binds: {:?}", environment.outputs);

        let graph_output = fragment
            .graph_outputs()
            .next()
            .ok_or(ConvertError::NoOutputError)?;
        self.value
            .process_in(fragment, &mut environment, graph_output)?;

        // link up loops
        for (inport, var) in environment.inputs {
            let outport = environment.outputs[&var].clone();
            fragment.link(outport, inport).unwrap();
        }

        Ok(())
    }
}

impl ProcessIn for Thunk {
    fn process_in<F>(
        &self,
        fragment: &mut F,
        environment: &mut Environment,
        inport: SyntaxInPort<false>,
    ) -> Result<(), ConvertError>
    where
        F: Fragment<NodeWeight = Op, EdgeWeight = Name>,
    {
        let mut free: HashSet<_> = environment.free_vars[&self.body].iter().cloned().collect();

        for var in &self.args {
            free.remove(var);
        }
        let thunk_node = fragment.add_thunk(
            free.iter().cloned().map(Some),
            self.args.iter().map(|name| Some(name.clone())),
            [None],
        );
        debug!("new thunk node {:?}", thunk_node);

        let mut thunk_env = Environment::new(environment.free_vars);

        for (var, outport) in free.into_iter().zip(thunk_node.free_inputs()) {
            environment
                .inputs
                .insert(
                    thunk_node
                        .externalise_input(&outport)
                        .expect("no corresponding inport for outport in thunk"),
                    var.clone(),
                )
                .is_none()
                .then_some(())
                .ok_or(ConvertError::Aliased(var.clone()))?;
            thunk_env
                .outputs
                .insert(var.clone(), outport)
                .is_none()
                .then_some(())
                .ok_or(ConvertError::Aliased(var.clone()))?;
        }
        for (var, outport) in self.args.iter().zip(thunk_node.bound_inputs()) {
            thunk_env
                .outputs
                .insert(var.clone(), outport)
                .is_none()
                .then_some(())
                .ok_or(ConvertError::Aliased(var.clone()))?;
        }
        fragment.in_thunk(thunk_node.clone(), |mut inner_fragment| {
            self.body.process(&mut inner_fragment, thunk_env)
        })?;

        let outport = thunk_node
            .outputs()
            .next()
            .ok_or(ConvertError::NoOutputError)?;
        fragment.link(outport, inport)?;

        Ok(())
    }
}

impl ProcessOut for Value {
    fn process_out<F>(
        &self,
        fragment: &mut F,
        environment: &mut Environment,
    ) -> Result<SyntaxOutPort<false>, ConvertError>
    where
        F: Fragment<NodeWeight = Op, EdgeWeight = Name>,
    {
        match self {
            Value::Variable(var) => Ok(environment.outputs[var].clone()),
            Value::Op { op, vs, ds } => {
                let operation_node =
                    fragment.add_operation(vs.len() + ds.len(), [None], op.clone());
                for (value, inport) in vs.iter().zip(operation_node.inputs()) {
                    value.process_in(fragment, environment, inport)?;
                }
                for (thunk, inport) in ds.iter().zip(operation_node.inputs().skip(vs.len())) {
                    thunk.process_in(fragment, environment, inport)?;
                }

                let outport = operation_node
                    .outputs()
                    .next()
                    .ok_or(ConvertError::NoOutputError)?;
                Ok(outport)
            }
        }
    }
}

impl TryFrom<&Expr> for SyntaxHyperGraph {
    type Error = ConvertError;

    #[tracing::instrument(ret, err)]
    fn try_from(expr: &Expr) -> Result<Self, Self::Error> {
        let mut free_vars = FreeVars::default();

        free_vars.expr(expr);

        let mut env: Environment = Environment::new(&free_vars);

        debug!("determined environment: {:?}", env);

        let free: Vec<Variable> = env.free_vars[expr].iter().cloned().collect();
        debug!("free variables: {:?}", free);
        let mut graph = HyperGraph::new(free.iter().cloned().map(Some).collect(), 1);
        debug!("made initial hypergraph: {:?}", graph);
        for (var, outport) in free.iter().zip(graph.graph_inputs()) {
            env.outputs
                .insert(var.clone(), outport)
                .is_none()
                .then_some(())
                .ok_or(ConvertError::Aliased(var.clone()))?;
        }
        debug!("processed free variables: {:?}", env.outputs);

        expr.process(&mut graph, env)?;

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
