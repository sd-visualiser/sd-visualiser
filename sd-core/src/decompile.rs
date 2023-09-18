use std::collections::HashMap;

use itertools::Itertools;
use thiserror::Error;

use crate::{
    graph::Name,
    hypergraph::{
        generic::{Edge, Node, Operation, Thunk},
        traits::{EdgeLike, Graph, NodeLike, WithWeight},
    },
    language::{Arg, Bind, Expr, Fresh, Language, Thunk as SThunk, Value},
};

#[derive(Clone, Debug, Error)]
pub enum DecompileError {
    #[error("corrupt hypergraph")]
    Corrupt,

    #[error("thunks cannot have multiple outputs")]
    MultipleOutputs,
}

impl<T: Language> Expr<T> {
    pub fn decompile<G>(graph: &G) -> Result<Self, DecompileError>
    where
        G: Graph,
        Edge<G::Ctx>: WithWeight<Weight = Name<T>>,
        Operation<G::Ctx>: WithWeight<Weight = T::Op>,
        Thunk<G::Ctx>: WithWeight<Weight = T::Addr>,
    {
        let mut binds = Vec::default();

        // Maps hypergraph nodes to corresponding thunks (for thunk nodes) or values (for operation nodes).
        let mut node_to_syntax = HashMap::<Node<G::Ctx>, Arg<T>>::default();

        let mut fresh_vars = 0;
        for node in graph.nodes().rev() {
            match &node {
                Node::Operation(op) => {
                    let mut args = Vec::default();
                    for edge in op.inputs() {
                        match edge.weight().into_var() {
                            Some(var) => {
                                args.push(Arg::Value(Value::Variable(var)));
                            }
                            None => match edge.source() {
                                None => {
                                    args.push(Arg::Value(Value::Variable(T::Var::fresh(
                                        fresh_vars,
                                    ))));
                                    fresh_vars += 1;
                                }
                                Some(other_node) => {
                                    args.push(
                                        node_to_syntax
                                            .get(&other_node)
                                            .ok_or(DecompileError::Corrupt)?
                                            .clone(),
                                    );
                                }
                            },
                        }
                    }

                    let value = Value::Op {
                        op: op.weight(),
                        args,
                    };

                    match op
                        .outputs()
                        .map(|edge| match edge.weight() {
                            Name::Nil => Ok(None),
                            Name::FreeVar(_) => Err(DecompileError::Corrupt),
                            Name::BoundVar(def) => Ok(Some(def)),
                        })
                        .collect::<Result<Option<Vec<_>>, _>>()?
                    {
                        None => {
                            if op.number_of_outputs() != 1 {
                                return Err(DecompileError::Corrupt);
                            }
                            // The node has a unique output which does not have a name.
                            node_to_syntax.insert(node, Arg::Value(value));
                        }
                        Some(defs) => {
                            // The node has any number of outputs which are all bound variables.
                            binds.push(Bind { defs, value });
                        }
                    }
                }
                Node::Thunk(thunk) => {
                    let thunk = SThunk::decompile(thunk)?;

                    // Check the node has a unique output.
                    let output = node
                        .outputs()
                        .exactly_one()
                        .map_err(|_err| DecompileError::MultipleOutputs)?;

                    // Check the output does not have a name.
                    match output.weight() {
                        Name::Nil => {
                            node_to_syntax.insert(node, Arg::Thunk(thunk));
                        }
                        Name::FreeVar(_) | Name::BoundVar(_) => {
                            return Err(DecompileError::Corrupt)
                        }
                    }
                }
            }
        }

        let values = graph
            .graph_outputs()
            .map(|edge| match edge.weight().into_var() {
                Some(var) => Ok(Value::Variable(var)),
                None => match edge.source() {
                    None => Err(DecompileError::Corrupt),
                    Some(node) => node_to_syntax
                        .get(&node)
                        .and_then(|arg| arg.value().cloned())
                        .ok_or(DecompileError::Corrupt),
                },
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Expr { binds, values })
    }
}

impl<T: Language> SThunk<T> {
    pub fn decompile<G>(thunk: &G) -> Result<Self, DecompileError>
    where
        G: Graph + WithWeight<Weight = T::Addr>,
        Edge<G::Ctx>: WithWeight<Weight = Name<T>>,
        Operation<G::Ctx>: WithWeight<Weight = T::Op>,
        Thunk<G::Ctx>: WithWeight<Weight = T::Addr>,
    {
        Ok(SThunk {
            addr: thunk.weight(),
            args: thunk
                .bound_graph_inputs()
                .map(|edge| match edge.weight() {
                    Name::BoundVar(arg) => Ok(arg),
                    _ => Err(DecompileError::Corrupt),
                })
                .collect::<Result<Vec<_>, _>>()?,
            body: Expr::decompile(thunk)?,
        })
    }
}
