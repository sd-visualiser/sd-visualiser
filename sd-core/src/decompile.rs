use std::collections::HashMap;

use itertools::Itertools;
use thiserror::Error;

use crate::{
    graph::Name,
    hypergraph::{
        generic::{Edge, Node, Operation as GraphOperation, Thunk as GraphThunk},
        traits::{EdgeLike, Graph, NodeLike, WithWeight},
    },
    language::{chil, spartan, Arg, Bind, Expr, Language, Thunk, Value},
};

pub trait Fresh {
    fn fresh(number: usize) -> Self;
}

impl Fresh for chil::Variable {
    fn fresh(number: usize) -> Self {
        Self {
            name: None,
            addr: chil::Addr('?', number),
        }
    }
}

impl Fresh for spartan::Variable {
    fn fresh(number: usize) -> Self {
        Self(format!("?{number}"))
    }
}

#[derive(Clone, Debug, Error)]
pub enum DecompilationError {
    #[error("corrupt hypergraph")]
    Corrupt,

    #[error("thunks cannot have multiple outputs")]
    MultipleOutputs,
}

pub fn decompile<G, T: Language>(graph: &G) -> Result<Expr<T>, DecompilationError>
where
    G: Graph,
    Edge<G::Ctx>: WithWeight<Weight = Name<T>>,
    GraphOperation<G::Ctx>: WithWeight<Weight = T::Op>,
    GraphThunk<G::Ctx>: WithWeight<Weight = T::Addr>,
    T::Var: Fresh,
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
                                args.push(Arg::Value(Value::Variable(T::Var::fresh(fresh_vars))));
                                fresh_vars += 1;
                            }
                            Some(other_node) => {
                                args.push(
                                    node_to_syntax
                                        .get(&other_node)
                                        .ok_or(DecompilationError::Corrupt)?
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
                        Name::FreeVar(_) => Err(DecompilationError::Corrupt),
                        Name::BoundVar(def) => Ok(Some(def)),
                    })
                    .collect::<Result<Option<Vec<_>>, _>>()?
                {
                    None => {
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
                let thunk = Thunk {
                    addr: thunk.weight(),
                    args: thunk
                        .bound_graph_inputs()
                        .map(|edge| match edge.weight() {
                            Name::BoundVar(arg) => Ok(arg),
                            _ => Err(DecompilationError::Corrupt),
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                    body: decompile(thunk)?,
                };

                // Check the node has a unique output.
                let output = node
                    .outputs()
                    .exactly_one()
                    .map_err(|_err| DecompilationError::MultipleOutputs)?;

                // Check the output does not have a name.
                match output.weight() {
                    Name::Nil => {
                        node_to_syntax.insert(node, Arg::Thunk(thunk));
                    }
                    Name::FreeVar(_) | Name::BoundVar(_) => {
                        return Err(DecompilationError::Corrupt)
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
                None => Err(DecompilationError::Corrupt),
                Some(node) => node_to_syntax
                    .get(&node)
                    .and_then(|arg| arg.value().cloned())
                    .ok_or(DecompilationError::Corrupt),
            },
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Expr { binds, values })
}
