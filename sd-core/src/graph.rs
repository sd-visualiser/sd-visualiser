#![allow(clippy::type_complexity)]

use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use derivative::Derivative;
use thiserror::Error;
use tracing::{debug, Level};

use crate::{
    hypergraph::{
        builder::{fragment::Fragment, HyperGraphBuilder, HyperGraphError, InPort, OutPort},
        HyperGraph,
    },
    language::{Arg, AsVar, Expr, Language, Thunk, Value},
    prettyprinter::PrettyPrint,
};

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub struct Op<T: Language>(pub T::Op);

impl<T: Language> Display for Op<T>
where
    T::Op: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T: Language> PrettyPrint for Op<T>
where
    T::Op: PrettyPrint,
{
    fn to_doc(&self) -> pretty::RcDoc<'_, ()> {
        self.0.to_doc()
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
pub enum Name<T: Language> {
    Op,
    Thunk(T::Addr),
    FreeVar(T::Var),
    BoundVar(T::VarDef),
}

impl<T: Language> Name<T> {
    pub fn to_var(&self) -> Option<&T::Var> {
        match self {
            Name::Op | Name::Thunk(_) => None,
            Name::FreeVar(var) => Some(var),
            Name::BoundVar(def) => Some(def.as_var()),
        }
    }
}

pub type SyntaxHyperGraph<T> = HyperGraph<Op<T>, Name<T>>;

#[derive(Derivative, Error)]
#[derivative(Debug(bound = ""))]
pub enum ConvertError<T: Language>
where
    T::Var: Display,
{
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
    #[error("Thunks must have exactly one output")]
    ThunkOutputError,
}

#[derive(Derivative)]
#[derivative(Debug(bound = "F: Debug"))]
struct Environment<F, T: Language> {
    fragment: F,
    inputs: Vec<(InPort<Op<T>, Name<T>>, T::Var)>,
    outputs: HashMap<T::Var, OutPort<Op<T>, Name<T>>>,
}

enum ProcessInput<T: Language> {
    Variable(T::VarDef),
    InPort(InPort<Op<T>, Name<T>>),
}

impl<T, F> Environment<F, T>
where
    T: Language + 'static,
    T::Var: Display,
    F: Fragment<NodeWeight = Op<T>, EdgeWeight = Name<T>>,
{
    fn new(fragment: F) -> Self {
        Self {
            fragment,
            inputs: Vec::default(),
            outputs: HashMap::default(),
        }
    }

    /// Insert value into a hypergraph and update the environment.
    ///
    /// If an `InPort` is passed in it should be linked.
    /// If a `Variable` is passed in it should be assigned to the `out_port` the value generates.
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
            (Value::Variable(var), ProcessInput::Variable(input)) => {
                Err(ConvertError::Aliased(input.as_var().clone(), var.clone()))
            }
            (Value::Variable(var), ProcessInput::InPort(in_port)) => {
                self.inputs.push((in_port, var.clone()));
                Ok(())
            }
            (Value::Op { op, args }, input) => {
                let output_weight = if let ProcessInput::Variable(input) = &input {
                    Name::BoundVar(input.clone())
                } else {
                    Name::Op
                };

                let operation_node =
                    self.fragment
                        .add_operation(args.len(), [output_weight], Op(op.clone()));
                for (arg, in_port) in args.iter().rev().zip(operation_node.inputs().rev()) {
                    match arg {
                        Arg::Value(value) => {
                            self.process_value(value, ProcessInput::InPort(in_port))?;
                        }
                        Arg::Thunk(thunk) => {
                            self.process_thunk(thunk, in_port)?;
                        }
                    }
                }

                let out_port = operation_node
                    .outputs()
                    .next()
                    .ok_or(ConvertError::NoOutputError)?;

                match input {
                    ProcessInput::Variable(input) => {
                        let var = input.as_var();
                        self.outputs
                            .insert(var.clone(), out_port)
                            .is_none()
                            .then_some(())
                            .ok_or(ConvertError::Shadowed(var.clone()))?;
                    }
                    ProcessInput::InPort(in_port) => self.fragment.link(out_port, in_port)?,
                }

                Ok(())
            }
        }
    }

    /// Insert thunk into a hypergraph and update the environment.
    ///
    /// The caller expects the `in_port` that is passed in to be linked.
    ///
    /// # Errors
    ///
    /// This function will return an error if variables are malformed.
    fn process_thunk(
        &mut self,
        thunk: &Thunk<T>,
        in_port: InPort<Op<T>, Name<T>>,
    ) -> Result<(), ConvertError<T>> {
        if thunk.body.values.len() != 1 {
            return Err(ConvertError::ThunkOutputError);
        }
        let thunk_node = self.fragment.add_thunk(
            thunk.args.iter().cloned().map(Name::BoundVar),
            [Name::Thunk(thunk.addr.clone())],
        );

        self.fragment
            .in_thunk(thunk_node.clone(), |inner_fragment| {
                let mut thunk_env = Environment::new(inner_fragment);

                for (def, out_port) in thunk.args.iter().zip(thunk_node.bound_inputs()) {
                    let var = def.as_var();
                    thunk_env
                        .outputs
                        .insert(var.clone(), out_port)
                        .is_none()
                        .then_some(())
                        .ok_or(ConvertError::Shadowed(var.clone()))?;
                }
                thunk_env.process_expr(&thunk.body)?;
                self.inputs.extend(thunk_env.inputs);
                Ok::<_, ConvertError<T>>(thunk_env.fragment)
            })?;

        let out_port = thunk_node
            .outputs()
            .next()
            .ok_or(ConvertError::NoOutputError)?;
        self.fragment.link(out_port, in_port)?;

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
    fn process_expr(&mut self, expr: &Expr<T>) -> Result<(), ConvertError<T>> {
        let graph_outputs = self.fragment.graph_outputs().collect::<Vec<_>>();
        for (value, port) in expr.values.iter().zip(graph_outputs) {
            self.process_value(value, ProcessInput::InPort(port))?;
        }

        for bind in expr.binds.iter().rev() {
            self.process_value(&bind.value, ProcessInput::Variable(bind.def.clone()))?;
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

impl<T> TryFrom<&Expr<T>> for SyntaxHyperGraph<T>
where
    T: Language + 'static,
    T::Var: Display,
{
    type Error = ConvertError<T>;

    #[tracing::instrument(level=Level::TRACE, ret, err)]
    fn try_from(expr: &Expr<T>) -> Result<Self, Self::Error> {
        let free: Vec<T::Var> = expr.free_vars().iter().cloned().collect();
        debug!("free variables: {:?}", free);

        let graph = HyperGraphBuilder::new(
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
        graph::SyntaxHyperGraph,
        language::spartan::{Expr, Spartan},
    };

    #[allow(clippy::needless_pass_by_value)]
    #[dir_test(dir: "$CARGO_MANIFEST_DIR/../examples", glob: "**/*", loader: crate::language::tests::parse, postfix: "free_vars")]
    fn free_vars(fixture: Fixture<(&str, &str, Expr)>) {
        let (lang, name, expr) = fixture.content();

        let mut variables = expr.free_vars().iter().cloned().collect::<Vec<_>>();
        variables.sort();

        insta::assert_debug_snapshot!(format!("free_vars_{name}.{lang}"), variables);
    }

    #[allow(clippy::needless_pass_by_value)]
    #[dir_test(dir: "$CARGO_MANIFEST_DIR/../examples", glob: "**/*", loader: crate::language::tests::parse, postfix: "hypergraph_snapshot")]
    #[dir_test_attr(
        #[allow(unused_must_use)]
    )]
    fn hypergraph_snapshots(fixture: Fixture<(&str, &str, Expr)>) -> Result<()> {
        let (_lang, _name, expr) = fixture.content();
        let _graph: SyntaxHyperGraph<Spartan> = expr.try_into()?;

        // insta::with_settings!({sort_maps => true}, {
        //     insta::assert_ron_snapshot!(name, graph);
        // });

        Ok(())
    }
}
