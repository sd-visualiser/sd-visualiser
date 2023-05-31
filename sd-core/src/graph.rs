use std::collections::{HashMap, HashSet};

use thiserror::Error;
use tracing::debug;

use crate::{
    free_vars::FreeVars,
    hypergraph::{Fragment, Graph, HyperGraph, HyperGraphError, InPort, OutPort},
    language::{
        spartan::{Expr, Op, Thunk, Value, Variable},
        visitor::{Visitable, Visitor},
    },
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
type Scope = Option<*const Thunk>;

#[derive(Debug)]
struct ThunkContext {
    parent: Scope, // parent scope
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct ScopedVariable {
    var: Variable,
    scope: Scope,
}

#[derive(Debug)]
struct Environment {
    free_vars: FreeVars,
    map: HashMap<Scope, ThunkContext>,
    current_scope: Scope,
    inputs: HashMap<SyntaxInPort<false>, ScopedVariable>,
    outputs: HashMap<ScopedVariable, SyntaxOutPort<false>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            free_vars: FreeVars::default(),
            map: [(None, ThunkContext { parent: None })].into(),
            current_scope: Option::default(),
            inputs: HashMap::default(),
            outputs: HashMap::default(),
        }
    }
}

impl Environment {
    fn push(&mut self, scope: &Thunk) {
        self.map.insert(
            Some(scope),
            ThunkContext {
                parent: self.current_scope,
            },
        );
        self.current_scope = Some(scope);
    }

    fn pop(&mut self) -> Option<Scope> {
        self.map
            .get(&self.current_scope)
            .map(|ThunkContext { parent }| {
                let old = self.current_scope;
                self.current_scope = *parent;
                old
            })
    }

    fn set_scope(&mut self, scope: Scope) -> Scope {
        let old = self.current_scope;
        self.current_scope = scope;
        old
    }

    fn scoped<'a>(&'a self, var: &'a Variable) -> ScopedVariable {
        ScopedVariable {
            var: var.clone(),
            scope: self.current_scope,
        }
    }
}

impl<'ast> Visitor<'ast> for Environment {
    fn visit_thunk(&mut self, thunk: &'ast Thunk) {
        self.push(thunk);
    }

    fn after_thunk(&mut self, _thunk: &'ast Thunk) {
        assert!(self.pop().is_some());
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

trait ProcessIn<'env> {
    /// Insert this piece of syntax into a hypergraph and update the mapping of inports to variables.
    ///
    /// The caller expects the inport that is passed in to be linked.
    ///
    /// # Errors
    ///
    /// This function will return an error if variables are malformed.
    fn process_in<F>(
        &'env self,
        fragment: &mut F,
        environment: &mut Environment,
        inport: SyntaxInPort<false>,
    ) -> Result<(), ConvertError>
    where
        F: Fragment<NodeWeight = Op, EdgeWeight = Name>;
}

impl<'env> ProcessIn<'env> for Value {
    fn process_in<F>(
        &'env self,
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
                .insert(inport, environment.scoped(v))
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

impl<'env> ProcessIn<'env> for Thunk {
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

        for (var, outport) in free.into_iter().zip(thunk_node.free_inputs()) {
            environment
                .inputs
                .insert(
                    thunk_node
                        .externalise_input(&outport)
                        .expect("no corresponding inport for outport in thunk"),
                    ScopedVariable {
                        var: var.clone(),
                        scope: environment.current_scope,
                    },
                )
                .is_none()
                .then_some(())
                .ok_or(ConvertError::Aliased(var.clone()))?;
            environment
                .outputs
                .insert(
                    ScopedVariable {
                        var: var.clone(),
                        scope: Some(self),
                    },
                    outport,
                )
                .is_none()
                .then_some(())
                .ok_or(ConvertError::Aliased(var.clone()))?;
        }
        for (var, outport) in self.args.iter().zip(thunk_node.bound_inputs()) {
            environment
                .outputs
                .insert(
                    ScopedVariable {
                        var: var.clone(),
                        scope: Some(self),
                    },
                    outport,
                )
                .is_none()
                .then_some(())
                .ok_or(ConvertError::Aliased(var.clone()))?;
        }
        fragment.in_thunk(thunk_node.clone(), |mut inner_fragment| {
            environment.set_scope(Some(self));
            for bind in &self.body.binds {
                let outport = bind.value.process_out(&mut inner_fragment, environment)?;
                environment
                    .outputs
                    .insert(
                        ScopedVariable {
                            var: bind.var.clone(),
                            scope: Some(self),
                        },
                        outport,
                    )
                    .is_none()
                    .then_some(())
                    .ok_or(ConvertError::Aliased(bind.var.clone()))?;
            }
            let graph_output = inner_fragment.graph_outputs().next().unwrap();
            let res = self
                .body
                .value
                .process_in(&mut inner_fragment, environment, graph_output);
            environment.pop();
            res
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
            Value::Variable(var) => Ok(environment.outputs[&environment.scoped(var)].clone()),
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
        let mut env: Environment = Environment::default();

        expr.walk(&mut env);
        env.free_vars.expr(expr);

        debug!("determined environment: {:?}", env);

        let free: Vec<Variable> = env.free_vars[expr].iter().cloned().collect();
        debug!("free variables: {:?}", free);
        let mut graph = HyperGraph::new(free.iter().cloned().map(Some).collect(), 1);
        debug!("made initial hypergraph: {:?}", graph);
        for (var, outport) in free.iter().zip(graph.graph_inputs()) {
            env.outputs
                .insert(
                    ScopedVariable {
                        var: var.clone(),
                        scope: None,
                    },
                    outport,
                )
                .is_none()
                .then_some(())
                .ok_or(ConvertError::Aliased(var.clone()))?;
        }
        debug!("processed free variables: {:?}", env.outputs);

        for bind in &expr.binds {
            let outport = bind.value.process_out(&mut graph, &mut env)?;
            env.outputs
                .insert(
                    ScopedVariable {
                        var: bind.var.clone(),
                        scope: None,
                    },
                    outport,
                )
                .is_none()
                .then_some(())
                .ok_or(ConvertError::Aliased(bind.var.clone()))?;
        }
        debug!("processed binds: {:?}", env.outputs);

        let graph_output = graph.graph_outputs().next().unwrap();
        expr.value.process_in(&mut graph, &mut env, graph_output)?;

        debug!(env = ?env);
        // link up loops
        for (inport, var) in env.inputs {
            let outport = env.outputs[&var].clone();
            graph.link(outport, inport).unwrap();
        }

        Ok(graph.build()?)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use anyhow::Result;
    use rstest::rstest;

    use crate::{
        graph::{Environment, SyntaxHyperGraph},
        language::{
            spartan::{
                tests::{
                    aliasing, bad, basic_program, buggy, fact, free_vars, nest, recursive, thunks,
                },
                Expr, Variable,
            },
            visitor::Visitable,
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
        let mut env: Environment = Environment::default();
        env.free_vars.expr(&expr);

        assert_eq!(
            env.free_vars[&expr].clone(),
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
