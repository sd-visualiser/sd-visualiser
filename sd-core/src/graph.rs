#![allow(clippy::type_complexity)]

use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
};

use derivative::Derivative;
use thiserror::Error;
use tracing::{debug, Level};

use crate::{
    free_vars::FreeVars,
    hypergraph::{fragment::Fragment, Graph, HyperGraph, HyperGraphError, InPort, OutPort},
    language::{spartan::Spartan, Expr, Language, Thunk, Value},
};

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub struct Op<T: Language = Spartan>(pub T::Op);

impl<T: Language> Display for Op<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub enum Name<T: Language = Spartan> {
    Null,
    Thunk(T::Addr),
    Variable(T::Var),
}

impl<T: Language> Display for Name<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Name::Null => write!(f, ""),
            Name::Thunk(_addr) => write!(f, ""),
            Name::Variable(var) => write!(f, "{var}"),
        }
    }
}

pub type SyntaxHyperGraph<T = Spartan> = HyperGraph<Op<T>, Name<T>>;

#[derive(Derivative, Error)]
#[derivative(Debug(bound = ""))]
pub enum ConvertError<T: Language> {
    #[error("Error constructing hypergraph")]
    HyperGraphError(#[from] HyperGraphError<Op<T>, Name<T>>),
    #[error("Couldn't find location of variable `{0}`")]
    VariableError(T::Var),
    #[error("Attempted to alias `{0}` to `{1}`")]
    Aliased(T::Var, T::Var),
    #[error("Attempted to shadow `{0}`")]
    Shadowed(T::Var),
    #[error("Fragment did not have output")]
    NoOutputError,
}

#[derive(Derivative)]
#[derivative(Debug(bound = "F: Debug"))]
struct Environment<'env, F, T: Language> {
    free_vars: &'env FreeVars<T>,
    fragment: F,
    inputs: Vec<(InPort<Op<T>, Name<T>, false>, T::Var)>,
    outputs: HashMap<T::Var, OutPort<Op<T>, Name<T>, false>>,
}

enum ProcessInput<T: Language> {
    Variable(T::Var),
    InPort(InPort<Op<T>, Name<T>, false>),
}

impl<'env, T, F> Environment<'env, F, T>
where
    T: Language + 'static,
    F: Fragment<NodeWeight = Op<T>, EdgeWeight = Name<T>>,
{
    fn new(free_vars: &'env FreeVars<T>, fragment: F) -> Self {
        Self {
            free_vars,
            fragment,
            inputs: Vec::default(),
            outputs: HashMap::default(),
        }
    }

    /// Insert value into a hypergraph and update the environment.
    ///
    /// If an `InPort` is passed in it should be linked.
    /// If a `Variable` is passed in it should be assigned to the outport the value generates.
    ///
    /// # Returns
    ///
    /// This function returns an outport, which should be linked by the caller.
    ///
    /// # Errors
    ///
    /// This function will return an error if variables are malformed.
    fn process_value(
        &mut self,
        value: &Value<T>,
        input: ProcessInput<T>,
    ) -> Result<(), ConvertError<T>> {
        match (value, input) {
            (Value::Variable(var), ProcessInput::Variable(var2)) => {
                Err(ConvertError::Aliased(var2, var.clone()))
            }
            (Value::Variable(var), ProcessInput::InPort(in_port)) => {
                self.inputs.push((in_port, var.clone()));
                Ok(())
            }
            (Value::Op { op, vs, ds }, input) => {
                let output_weight = if let ProcessInput::Variable(v) = &input {
                    Name::Variable(v.clone())
                } else {
                    Name::Null
                };

                let operation_node = self.fragment.add_operation(
                    vs.len() + ds.len(),
                    [output_weight],
                    Op(op.clone()),
                );
                for (value, inport) in vs
                    .iter()
                    .rev()
                    .zip(operation_node.inputs().rev().skip(ds.len()))
                {
                    self.process_value(value, ProcessInput::InPort(inport))?;
                }
                for (thunk, inport) in ds.iter().rev().zip(operation_node.inputs().rev()) {
                    self.process_thunk(thunk, inport)?;
                }

                let out_port = operation_node
                    .outputs()
                    .next()
                    .ok_or(ConvertError::NoOutputError)?;

                match input {
                    ProcessInput::Variable(v) => {
                        self.outputs
                            .insert(v.clone(), out_port)
                            .is_none()
                            .then_some(())
                            .ok_or(ConvertError::Shadowed(v))?;
                    }
                    ProcessInput::InPort(in_port) => self.fragment.link(out_port, in_port)?,
                }

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
        thunk: &Thunk<T>,
        inport: InPort<Op<T>, Name<T>, false>,
    ) -> Result<(), ConvertError<T>> {
        let mut free: HashSet<_> = self.free_vars[&thunk.body].iter().cloned().collect();

        for arg in &thunk.args {
            free.remove(&arg.var);
        }
        let thunk_node = self.fragment.add_thunk(
            free.iter().cloned().map(Name::Variable),
            thunk
                .args
                .iter()
                .cloned()
                .map(|def| Name::Variable(def.var)),
            [Name::Thunk(thunk.addr.clone())],
        );
        debug!("new thunk node {:?}", thunk_node);

        for (var, inport) in free.iter().cloned().zip(thunk_node.inputs()) {
            self.inputs.push((inport, var.clone()));
        }

        self.fragment
            .in_thunk(thunk_node.clone(), |inner_fragment| {
                let mut thunk_env = Environment::new(self.free_vars, inner_fragment);

                for (var, outport) in free.iter().cloned().zip(thunk_node.unbound_graph_inputs()) {
                    thunk_env
                        .outputs
                        .insert(var.clone(), outport)
                        .is_none()
                        .then_some(())
                        .ok_or(ConvertError::Shadowed(var))?;
                }
                for (def, outport) in thunk.args.iter().zip(thunk_node.bound_graph_inputs()) {
                    thunk_env
                        .outputs
                        .insert(def.var.clone(), outport)
                        .is_none()
                        .then_some(())
                        .ok_or(ConvertError::Shadowed(def.var.clone()))?;
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
    fn process_expr(mut self, expr: &Expr<T>) -> Result<F, ConvertError<T>> {
        let graph_output = self
            .fragment
            .graph_outputs()
            .next()
            .ok_or(ConvertError::NoOutputError)?;
        self.process_value(&expr.value, ProcessInput::InPort(graph_output))?;

        for bind in expr.binds.iter().rev() {
            self.process_value(&bind.value, ProcessInput::Variable(bind.def.var.clone()))?;
        }
        debug!("processed binds: {:?}", self.outputs);

        // link up loops
        for (inport, var) in self.inputs {
            let outport = self.outputs[&var].clone();
            self.fragment.link(outport, inport).unwrap();
        }

        Ok(self.fragment)
    }
}

impl<T: Language + 'static> TryFrom<&Expr<T>> for SyntaxHyperGraph<T> {
    type Error = ConvertError<T>;

    #[tracing::instrument(level=Level::DEBUG, ret, err)]
    fn try_from(expr: &Expr<T>) -> Result<Self, Self::Error> {
        let mut free_vars = FreeVars::default();

        free_vars.expr(expr);

        let free: Vec<T::Var> = free_vars[expr].iter().cloned().collect();
        debug!("free variables: {:?}", free);
        let graph = HyperGraph::new(free.iter().cloned().map(Name::Variable).collect(), 1);
        debug!("made initial hypergraph: {:?}", graph);

        let mut env = Environment::new(&free_vars, graph);
        debug!("determined environment: {:?}", env);

        for (var, outport) in free.iter().zip(env.fragment.graph_inputs()) {
            env.outputs
                .insert(var.clone(), outport)
                .is_none()
                .then_some(())
                .ok_or(ConvertError::Shadowed(var.clone()))?;
        }
        debug!("processed free variables: {:?}", env.outputs);

        let graph = env.process_expr(expr)?;

        Ok(graph.build()?)
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Result;
    use dir_test::{dir_test, Fixture};

    use crate::{free_vars::FreeVars, graph::SyntaxHyperGraph, language::spartan::Expr};

    #[allow(clippy::needless_pass_by_value)]
    #[dir_test(dir: "$CARGO_MANIFEST_DIR/../examples", glob: "**/*", loader: crate::language::tests::parse, postfix: "free_vars")]
    fn free_vars(fixture: Fixture<(&str, &str, Expr)>) {
        let (lang, name, expr) = fixture.content();

        let mut fv = FreeVars::default();
        fv.expr(expr);
        let mut variables = fv[expr].iter().cloned().collect::<Vec<_>>();
        variables.sort();

        insta::assert_debug_snapshot!(format!("free_vars_{name}.{lang}"), variables);
    }

    #[allow(clippy::needless_pass_by_value)]
    #[dir_test(dir: "$CARGO_MANIFEST_DIR/../examples", glob: "**/*", loader: crate::language::tests::parse, postfix: "hypergraph_snapshot")]
    #[dir_test_attr(
        #[allow(unused_must_use)]
    )]
    fn hypergraph_snapshots(fixture: Fixture<(&str, &str, Expr)>) -> Result<()> {
        let (lang, name, expr) = fixture.content();
        let graph: SyntaxHyperGraph = expr.try_into()?;

        // insta::with_settings!({sort_maps => true}, {
        //     insta::assert_ron_snapshot!(name, graph);
        // });

        Ok(())
    }
}
