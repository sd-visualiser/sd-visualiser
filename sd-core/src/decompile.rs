use std::collections::HashMap;

use itertools::Itertools;
use thiserror::Error;

use crate::{
    graph::{Name, Op},
    hypergraph::{Graph, Node},
    language::{Arg, Bind, Expr, Language, Thunk, Value},
};

#[derive(Clone, Debug, Error)]
pub enum DecompilationError {
    #[error("corrupt hypergraph")]
    Corrupt,

    #[error("multiple outputs not allowed")]
    MultipleOutputs,
}

pub fn decompile<T: Language>(
    graph: &impl Graph<NodeWeight = Op<T>, EdgeWeight = Name<T>>,
) -> Result<Expr<T>, DecompilationError> {
    let mut binds = Vec::default();

    // Maps hypergraph nodes to corresponding thunks (for thunk nodes) or values (for operation nodes).
    let mut node_to_syntax = HashMap::<Node<_, _>, Arg<T>>::default();

    for node in graph.nodes().rev() {
        // Check the node has a unique output.
        let output = node
            .outputs()
            .exactly_one()
            .map_err(|_err| DecompilationError::MultipleOutputs)?;

        match &node {
            Node::Operation(op) => {
                let mut args = Vec::default();
                for port in op.inputs() {
                    match port.weight().to_var() {
                        Some(var) => {
                            args.push(Arg::Value(Value::Variable(var.clone())));
                        }
                        None => match port.node() {
                            None => return Err(DecompilationError::Corrupt),
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
                    op: op.weight().0.clone(),
                    args,
                };

                match output.weight() {
                    Name::Op => {
                        node_to_syntax.insert(node, Arg::Value(value));
                    }
                    Name::BoundVar(def) => {
                        binds.push(Bind {
                            def: def.clone(),
                            value,
                        });
                    }
                    Name::Thunk(_) | Name::FreeVar(_) => return Err(DecompilationError::Corrupt),
                }
            }
            Node::Thunk(thunk) => {
                let addr = match output.weight() {
                    Name::Thunk(addr) => addr.clone(),
                    _ => return Err(DecompilationError::Corrupt),
                };
                let thunk = Thunk {
                    addr,
                    args: thunk
                        .bound_graph_inputs()
                        .map(|port| match port.weight() {
                            Name::BoundVar(arg) => Ok(arg.clone()),
                            _ => Err(DecompilationError::Corrupt),
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                    body: decompile(thunk)?,
                };
                node_to_syntax.insert(node, Arg::Thunk(thunk));
            }
        }
    }

    let values = graph
        .graph_outputs()
        .map(|port| {
            let port = port.link();
            match port.weight().to_var() {
                Some(var) => Ok(Value::Variable(var.clone())),
                None => match port.node() {
                    None => Err(DecompilationError::Corrupt),
                    Some(node) => node_to_syntax
                        .get(&node)
                        .and_then(|arg| arg.value().cloned())
                        .ok_or(DecompilationError::Corrupt),
                },
            }
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Expr { binds, values })
}
