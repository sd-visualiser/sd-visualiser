use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use thiserror::Error;
use tracing::debug;

use crate::{
    hypergraph::{Fragment, Graph, HyperGraph, HyperGraphError, InPort, OutPort},
    language::{
        spartan::{BindClause, Expr, Op, Thunk, Value, Variable},
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum BoundVariable<'a> {
    BindClause(&'a Variable),
    Thunk(&'a Variable),
}

#[derive(Debug)]
struct ThunkContext<'a> {
    parent: Scope,                     // parent scope
    bound: HashSet<BoundVariable<'a>>, // new variables
    free: HashSet<&'a Variable>,       // free variables
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct ScopedVariable {
    var: Variable,
    scope: Scope,
}

#[derive(Debug)]
pub(crate) struct Environment<'a> {
    map: HashMap<Scope, ThunkContext<'a>>,
    current_scope: Scope,
    inputs: HashMap<SyntaxInPort<false>, ScopedVariable>,
    outputs: HashMap<ScopedVariable, SyntaxOutPort<false>>,
}

impl<'a> Default for Environment<'a> {
    fn default() -> Self {
        Self {
            map: [(
                None,
                ThunkContext {
                    parent: None,
                    bound: HashSet::default(),
                    free: HashSet::default(),
                },
            )]
            .into(),
            current_scope: Option::default(),
            inputs: HashMap::default(),
            outputs: HashMap::default(),
        }
    }
}

impl<'env> Environment<'env> {
    fn push(&mut self, scope: &'env Thunk, bound: HashSet<&'env Variable>) {
        self.map.insert(
            Some(scope),
            ThunkContext {
                parent: self.current_scope,
                bound: bound.into_iter().map(BoundVariable::Thunk).collect(),
                free: HashSet::default(),
            },
        );
        self.current_scope = Some(scope);
    }

    fn pop(&mut self) -> Option<Scope> {
        self.map
            .get(&self.current_scope)
            .map(|ThunkContext { parent, .. }| {
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

    fn bind(&mut self, var: &'env Variable) -> bool {
        let ThunkContext { bound, .. } = self.map.get_mut(&self.current_scope).unwrap();
        bound.insert(BoundVariable::BindClause(var))
    }

    fn add_free(&mut self, var: &'env Variable) {
        let scopestack = std::iter::successors(Some(self.current_scope), |scope| {
            self.map[scope].parent.map(Some)
        })
        .chain(std::iter::once(None))
        .take_while(|&scope| {
            !(self.bound(scope).contains(&BoundVariable::Thunk(var))
                || self.bound(scope).contains(&BoundVariable::BindClause(var)))
        })
        .collect::<Vec<_>>();
        for scope in scopestack {
            let ThunkContext { free, .. } = self.map.get_mut(&scope).unwrap();
            free.insert(var);
        }
    }

    fn bound(&self, scope: Scope) -> impl Iterator<Item = BoundVariable> {
        self.map[&scope].bound.iter().copied()
    }

    fn scoped<'a>(&'a self, var: &'a Variable) -> ScopedVariable {
        ScopedVariable {
            var: var.clone(),
            scope: self.current_scope,
        }
    }
}

impl<'ast> Visitor<'ast> for Environment<'ast> {
    fn visit_bind_clause(&mut self, bind_clause: &'ast BindClause) {
        debug!("visit bind");
        assert!(
            self.bind(&bind_clause.var),
            "tried to rebind variable in same scope"
        );
    }

    fn visit_value(&mut self, value: &'ast Value) {
        if let Value::Variable(v) = value {
            self.add_free(v);
        }
    }

    fn visit_thunk(&mut self, thunk: &'ast Thunk) {
        // make new scope for thunk
        let args: HashSet<_> = thunk.args.iter().collect();
        self.push(thunk, args);
    }

    fn after_thunk(&mut self, _thunk: &'ast Thunk) {
        assert!(self.pop().is_some());
    }
}

pub(crate) trait ProcessOut<'env> {
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
        &'env self,
        fragment: &mut F,
        environment: &mut Environment<'env>,
    ) -> Result<SyntaxOutPort<false>, ConvertError>
    where
        F: Fragment<NodeWeight = Op, EdgeWeight = Name> + Graph<false>;
}

pub(crate) trait ProcessIn<'env> {
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
        environment: &mut Environment<'env>,
        inport: SyntaxInPort<false>,
    ) -> Result<(), ConvertError>
    where
        F: Fragment<NodeWeight = Op, EdgeWeight = Name> + Graph<false>;
}

impl<'env> ProcessIn<'env> for Value {
    fn process_in<F>(
        &'env self,
        fragment: &mut F,
        environment: &mut Environment<'env>,
        inport: SyntaxInPort<false>,
    ) -> Result<(), ConvertError>
    where
        F: Fragment<NodeWeight = Op, EdgeWeight = Name> + Graph<false>,
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
        &'env self,
        fragment: &mut F,
        environment: &mut Environment<'env>,
        inport: SyntaxInPort<false>,
    ) -> Result<(), ConvertError>
    where
        F: Fragment<NodeWeight = Op, EdgeWeight = Name> + Graph<false>,
    {
        let free: Vec<_> = environment.map[&Some(self as *const Thunk)]
            .free
            .iter()
            .copied()
            .cloned()
            .collect();
        let bound: Vec<_> = environment
            .bound(Some(self))
            .filter_map(|bv| match bv {
                BoundVariable::BindClause(_) => None,
                BoundVariable::Thunk(var) => Some(var),
            })
            .collect();
        let thunk_node = fragment.add_thunk(
            free.iter().cloned().map(Some),
            bound.iter().copied().cloned().map(Some),
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

impl<'env> ProcessOut<'env> for Value {
    fn process_out<F>(
        &'env self,
        fragment: &mut F,
        environment: &mut Environment<'env>,
    ) -> Result<SyntaxOutPort<false>, ConvertError>
    where
        F: Fragment<NodeWeight = Op, EdgeWeight = Name> + Graph<false>,
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
        debug!("determined environment: {:?}", env);

        let free: Vec<_> = env.map[&None].free.iter().copied().cloned().collect();
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
                tests::{bad, basic_program, buggy, fact, free_vars, nest, recursive, thunks},
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
        expr.walk(&mut env);

        assert_eq!(
            env.map[&None]
                .free
                .iter()
                .copied()
                .cloned()
                .collect::<HashSet<_>>(),
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
    fn hypergraph_snapshots(#[case] name: &str, #[case] expr: Result<Expr>) -> Result<()> {
        let graph: SyntaxHyperGraph = (&expr?).try_into()?;

        // insta::with_settings!({sort_maps => true}, {
        //     insta::assert_ron_snapshot!(name, graph);
        // });

        Ok(())
    }
}
