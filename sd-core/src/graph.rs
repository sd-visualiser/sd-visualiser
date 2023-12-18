use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use derivative::Derivative;
use either::Either::{self, Left, Right};
#[cfg(test)]
use serde::Serialize;
use thiserror::Error;
use tracing::{debug, Level};

use crate::{
    common::Matchable,
    hypergraph::{
        builder::{
            fragment::{Fragment, ThunkCursor},
            HypergraphBuilder, HypergraphError, InPort, OutPort, ThunkBuilder,
        },
        Hypergraph, Weight,
    },
    language::{ControlFlow, Expr, GetVar, Language, Value, CF},
    prettyprinter::PrettyPrint,
};

pub struct Syntax<T: Language> {
    _phantom: std::marker::PhantomData<T>,
}

impl<T: Language> Weight for Syntax<T> {
    type EdgeWeight = Name<T>;
    type OperationWeight = T::Op;
    type ThunkWeight = Either<T::Addr, T::BlockAddr>;
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
#[cfg_attr(test, derive(Serialize))]
pub enum Name<T: Language> {
    Nil,
    FreeVar(T::Var),
    BoundVar(T::VarDef),
}

impl<T: Language> Display for Name<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Name::Nil => write!(f, ""),
            Name::FreeVar(var) => write!(f, "{var}"),
            Name::BoundVar(def) => write!(f, "{def}"),
        }
    }
}

impl<T: Language> Matchable for Name<T> {
    fn is_match(&self, query: &str) -> bool {
        match self {
            Name::Nil => false,
            Name::FreeVar(var) => var.is_match(query),
            Name::BoundVar(def) => def.is_match(query),
        }
    }
}

impl<T: Language> Name<T> {
    pub fn into_var(self) -> Option<T::Var> {
        match self {
            Name::Nil => None,
            Name::FreeVar(var) => Some(var),
            Name::BoundVar(def) => Some(def.into_var()),
        }
    }
}

pub type SyntaxHypergraph<T> = Hypergraph<Syntax<T>>;

#[derive(Derivative, Error)]
#[derivative(Debug(bound = ""))]
pub enum ConvertError<T: Language> {
    #[error("Error constructing hypergraph")]
    HypergraphError(#[from] HypergraphError<Syntax<T>>),
    #[error("Couldn't find location of variable `{}`", .0.to_pretty())]
    VariableError(T::Var),
    #[error("Attempted to alias `{}` to `{}`", .0.to_pretty(), .1.to_pretty())]
    Aliased(Vec<T::Var>, T::Var),
    #[error("Attempted to shadow `{}`", .0.to_pretty())]
    Shadowed(T::Var),
    #[error("Fragment did not have output")]
    NoOutputError,
}

/// Environments capture the local information needed to build a hypergraph from an AST
#[derive(Derivative)]
#[derivative(Debug(bound = "F: Debug"))]
#[allow(clippy::type_complexity)]
struct Environment<F, T: Language> {
    /// The fragment of the hypergraph we are building in
    fragment: F,
    /// Hanging input ports of nodes, with the variable they should be connected to
    inputs: Vec<(InPort<Syntax<T>>, T::Var)>,
    /// Mapping from variables to the output port that corresponds to them
    outputs: HashMap<T::Var, OutPort<Syntax<T>>>,
    /// Control flow wires to be connected
    cf_outputs: Vec<(Option<T::BlockAddr>, OutPort<Syntax<T>>)>,
}

enum ProcessInput<T: Language> {
    Variables(Vec<T::VarDef>),
    InPort(InPort<Syntax<T>>),
}

impl<T, F> Environment<F, T>
where
    T: Language + 'static,
    F: Fragment<Weight = Syntax<T>>,
{
    /// Create a new empty environment from a given `fragment`
    fn new(fragment: F) -> Self {
        Self {
            fragment,
            inputs: Vec::default(),
            outputs: HashMap::default(),
            cf_outputs: Vec::default(),
        }
    }

    fn in_thunk<S>(
        &mut self,
        thunk: ThunkBuilder<Syntax<T>>,
        f: impl FnOnce(&mut Environment<ThunkCursor<Syntax<T>>, T>) -> S,
    ) -> S {
        self.fragment.in_thunk(thunk, |inner_fragment| {
            let mut new_env = Environment {
                fragment: inner_fragment,
                inputs: std::mem::take(&mut self.inputs),
                outputs: std::mem::take(&mut self.outputs),
                cf_outputs: std::mem::take(&mut self.cf_outputs),
            };
            let ret = f(&mut new_env);
            self.inputs = std::mem::take(&mut new_env.inputs);
            self.outputs = std::mem::take(&mut new_env.outputs);
            self.cf_outputs = std::mem::take(&mut new_env.cf_outputs);
            ret
        })
    }

    /// Insert `value` into a hypergraph and update the environment.
    ///
    /// If an `InPort` is passed in to `input` it should be linked or assigned a variable in the environment.
    /// If a `Variable` is passed in to `input` it should be assigned to the `out_port` the value generates.
    /// If `CF` is passed to `input` then the operation is assumed to be a control flow operation and will only
    /// generate control flow outputs
    ///
    /// # Errors
    ///
    /// This function will return an error if variables are malformed.
    #[allow(clippy::too_many_lines)]
    fn process_value(
        &mut self,
        value: &Value<T>,
        input: ProcessInput<T>,
    ) -> Result<(), ConvertError<T>> {
        match value {
            Value::Variable(var) => {
                match input {
                    ProcessInput::Variables(inputs) => {
                        // We have tried to assign a variable to another variable
                        Err(ConvertError::Aliased(
                            inputs.into_iter().map(GetVar::into_var).collect(),
                            var.clone(),
                        ))
                    }
                    ProcessInput::InPort(in_port) => {
                        self.inputs.push((in_port, var.clone()));
                        Ok(())
                    }
                }
            }
            Value::Thunk(thunk) => {
                let output_weights = if let ProcessInput::Variables(inputs) = &input {
                    inputs.iter().map(|x| Name::BoundVar(x.clone())).collect()
                } else {
                    vec![Name::Nil]
                };

                let mut cf_free_vars = HashMap::new();
                thunk.body.cf_free_vars(&mut cf_free_vars);
                for b in &thunk.blocks {
                    b.expr.cf_free_vars(&mut cf_free_vars);
                }

                let thunk_node = self.fragment.add_thunk(
                    0,
                    thunk.args.iter().cloned().map(Name::BoundVar),
                    thunk.body.values.len() + cf_free_vars.get(&None).copied().unwrap_or_default(),
                    output_weights,
                    Left(thunk.addr.clone()),
                );

                self.fragment
                    .in_thunk(thunk_node.clone(), |inner_fragment| {
                        let mut thunk_env = Environment::new(inner_fragment);

                        // Add bound inputs of the thunk to the environment
                        for (def, out_port) in thunk.args.iter().zip(thunk_node.bound_inputs()) {
                            let var = def.var();
                            thunk_env
                                .outputs
                                .insert(var.clone(), out_port)
                                .is_none()
                                .then_some(())
                                .ok_or(ConvertError::Shadowed(var.clone()))?;
                        }

                        let mut blocks: HashMap<T::BlockAddr, Vec<_>> = HashMap::new();

                        println!("{cf_free_vars:?}");

                        for b in &thunk.blocks {
                            let block = thunk_env.fragment.add_thunk(
                                cf_free_vars
                                    .get(&Some(b.addr.clone()))
                                    .copied()
                                    .unwrap_or_default(),
                                b.args.iter().map(|x| Name::BoundVar(x.clone())),
                                0,
                                vec![],
                                Right(b.addr.clone()),
                            );

                            blocks.insert(b.addr.clone(), block.inputs().collect());

                            for (def, out_port) in b.args.iter().zip(block.bound_inputs()) {
                                let var = def.var();
                                thunk_env
                                    .outputs
                                    .insert(var.clone(), out_port)
                                    .is_none()
                                    .then_some(())
                                    .ok_or(ConvertError::Shadowed(var.clone()))?;
                            }

                            thunk_env.in_thunk(block, |block_env| {
                                block_env.process_expr(&b.expr)?;
                                Ok::<_, ConvertError<T>>(())
                            })?;
                        }

                        thunk_env.process_expr(&thunk.body)?;

                        let mut return_in_ports: Vec<_> = thunk_env
                            .fragment
                            .graph_outputs()
                            .skip(thunk.body.values.len())
                            .collect();

                        println!("Blocks {blocks:?}");
                        println!("Outputs {:?}", thunk_env.cf_outputs);

                        for (addr, port) in thunk_env.cf_outputs {
                            let in_port = match addr {
                                Some(a) => blocks.get_mut(&a).unwrap().pop().unwrap(),
                                None => return_in_ports.pop().unwrap(),
                            };
                            thunk_env.fragment.link(port, in_port)?;
                        }

                        // Add any free inputs of the thunk to the outer environment
                        self.inputs.extend(thunk_env.inputs);
                        Ok::<_, ConvertError<T>>(())
                    })?;

                let mut out_ports = thunk_node.outputs();

                match input {
                    ProcessInput::Variables(inputs) => {
                        for (input, out_port) in inputs.into_iter().zip(out_ports) {
                            let var = input.var();
                            self.outputs
                                .insert(var.clone(), out_port)
                                .is_none()
                                .then_some(())
                                .ok_or(ConvertError::Shadowed(var.clone()))?;
                        }
                    }
                    ProcessInput::InPort(in_port) => {
                        self.fragment.link(out_ports.next().unwrap(), in_port)?;
                    }
                }

                Ok(())
            }
            Value::Op { op, args } => {
                let mut output_weights = match &input {
                    ProcessInput::Variables(inputs) => {
                        inputs.iter().map(|x| Name::BoundVar(x.clone())).collect()
                    }
                    ProcessInput::InPort(_) => vec![Name::Nil],
                };

                let cf = op.get_cf();

                match &cf {
                    Some(CF::Return) => {
                        output_weights.push(Name::Nil);
                    }
                    Some(CF::Brs(bs)) => {
                        output_weights.extend(bs.iter().map(|_| Name::Nil));
                    }
                    None => {}
                }

                let operation_node =
                    self.fragment
                        .add_operation(args.len(), output_weights, op.clone());
                for (arg, in_port) in args.iter().rev().zip(operation_node.inputs().rev()) {
                    self.process_value(arg, ProcessInput::InPort(in_port))?;
                }

                let mut out_ports = operation_node.outputs();

                match cf {
                    None => {}
                    Some(CF::Return) => self.cf_outputs.push((None, out_ports.next().unwrap())),
                    Some(CF::Brs(bs)) => {
                        for b in bs {
                            self.cf_outputs.push((Some(b), out_ports.next().unwrap()));
                        }
                    }
                }

                match input {
                    ProcessInput::Variables(inputs) => {
                        for (input, out_port) in inputs.into_iter().zip(out_ports) {
                            let var = input.var();
                            self.outputs
                                .insert(var.clone(), out_port)
                                .is_none()
                                .then_some(())
                                .ok_or(ConvertError::Shadowed(var.clone()))?;
                        }
                    }
                    ProcessInput::InPort(in_port) => {
                        self.fragment.link(out_ports.next().unwrap(), in_port)?;
                    }
                }

                Ok(())
            }
        }
    }

    /// Insert `expr` into a hypergraph, updating the environment.
    ///
    /// # Returns
    ///
    /// This function returns the completed fragment of the hypergraph.
    ///
    /// # Errors
    ///
    /// This function will return an error if variables are malformed.
    fn process_expr(&mut self, expr: &Expr<T>) -> Result<(), ConvertError<T>> {
        // Add all nodes in reverse order
        let mut graph_outputs = self
            .fragment
            .graph_outputs()
            .collect::<Vec<_>>()
            .into_iter();
        for value in &expr.values {
            let process_input = ProcessInput::InPort(graph_outputs.next().unwrap());
            self.process_value(value, process_input)?;
        }

        for bind in expr.binds.iter().rev() {
            self.process_value(&bind.value, ProcessInput::Variables(bind.defs.clone()))?;
        }
        debug!("processed binds: {:?}", self.outputs);

        // link up loops
        self.inputs
            .retain(|(in_port, var)| match self.outputs.get(var) {
                Some(out_port) => {
                    self.fragment
                        .link(out_port.clone(), in_port.clone())
                        .unwrap();
                    false
                }
                None => true,
            });

        Ok(())
    }
}

impl<T> TryFrom<&Expr<T>> for SyntaxHypergraph<T>
where
    T: Language + 'static,
{
    type Error = ConvertError<T>;

    #[tracing::instrument(level=Level::TRACE, ret, err)]
    fn try_from(expr: &Expr<T>) -> Result<Self, Self::Error> {
        let free = expr.free_vars();
        debug!("free variables: {:?}", free);

        let graph = HypergraphBuilder::new(
            free.iter().cloned().map(Name::FreeVar).collect(),
            expr.values.len(),
        );
        debug!("made initial hypergraph: {:?}", graph);

        let mut env = Environment::new(graph);
        debug!("determined environment: {:?}", env);

        for (var, out_port) in free.iter().zip(env.fragment.graph_inputs()) {
            env.outputs
                .insert(var.clone(), out_port)
                .is_none()
                .then_some(())
                .ok_or(ConvertError::Shadowed(var.clone()))?;
        }
        debug!("processed free variables: {:?}", env.outputs);

        env.process_expr(expr)?;

        debug!("Expression processed");

        Ok(env.fragment.build()?)
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Result;
    use dir_test::{dir_test, Fixture};

    use crate::{
        graph::SyntaxHypergraph,
        hypergraph::petgraph::to_pet,
        language::spartan::{Expr, Spartan},
    };

    #[allow(clippy::needless_pass_by_value)]
    #[dir_test(dir: "$CARGO_MANIFEST_DIR/../examples", glob: "**/*", loader: crate::language::tests::parse, postfix: "free_vars")]
    fn free_vars(fixture: Fixture<(&str, &str, Expr)>) {
        let (lang, name, expr) = fixture.content();

        insta::assert_debug_snapshot!(format!("free_vars_{name}.{lang}"), expr.free_vars());
    }

    #[allow(clippy::needless_pass_by_value)]
    #[dir_test(dir: "$CARGO_MANIFEST_DIR/../examples", glob: "**/*", loader: crate::language::tests::parse, postfix: "hypergraph_snapshot")]
    #[dir_test_attr(
        #[allow(unused_must_use)]
    )]
    fn hypergraph_snapshots(fixture: Fixture<(&str, &str, Expr)>) -> Result<()> {
        let (lang, name, expr) = fixture.content();
        let graph: SyntaxHypergraph<Spartan> = expr.try_into()?;

        insta::assert_ron_snapshot!(format!("hypergraph_{name}.{lang}"), to_pet(&graph));

        Ok(())
    }
}
