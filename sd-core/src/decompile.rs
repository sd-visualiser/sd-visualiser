use std::collections::HashMap;

use itertools::{Either, Itertools};
use thiserror::Error;

use crate::{
    graph::{Name, Op},
    hypergraph::{Graph, Node},
    language::{Bind, Expr, Language, Thunk, Value},
};

#[derive(Clone, Debug, Error)]
pub enum DecompilationError {
    #[error("corrupt hypergraph")]
    Corrupt,

    #[error("multiple outputs not allowed")]
    MultipleOutputs,
}

pub fn decompile<T>(
    graph: &impl Graph<NodeWeight = Op<T>, EdgeWeight = Name<T>>,
) -> Result<Expr<T>, DecompilationError>
where
    T: Language,
    T::Ty: Default,
{
    let mut binds = Vec::default();

    // Maps hypergraph nodes to corresponding thunks (for thunk nodes) or values (for operation nodes).
    let mut node_to_syntax = HashMap::<Node<_, _>, Either<Value<T>, Thunk<T>>>::default();

    for node in graph.nodes().rev() {
        // Check the node has a unique output.
        let output = node
            .outputs()
            .exactly_one()
            .map_err(|_err| DecompilationError::MultipleOutputs)?;

        match &node {
            Node::Operation(op) => {
                let mut args = Vec::default();
                for port in op.inputs().map(|port| port.link()) {
                    match port.weight() {
                        Name::Variable(var) => {
                            args.push(Either::Left(Value::Variable(var.clone())));
                        }
                        _ => match port.node() {
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
                    vs: args
                        .iter()
                        .filter_map(|x| x.as_ref().left().cloned())
                        .collect(),
                    ds: args
                        .iter()
                        .filter_map(|x| x.as_ref().right().cloned())
                        .collect(),
                };

                if let Name::Variable(var) = output.weight().clone() {
                    binds.push(Bind {
                        var,
                        value,
                        ty: T::Ty::default(),
                    });
                } else {
                    node_to_syntax.insert(node, Either::Left(value));
                }
            }
            Node::Thunk(thunk) => {
                let addr = match output.weight() {
                    Name::Thunk(addr) => addr.clone(),
                    _ => return Err(DecompilationError::Corrupt),
                };
                let x = Either::Right(Thunk {
                    addr,
                    args: thunk
                        .bound_graph_inputs()
                        .map(|port| match port.weight() {
                            Name::Variable(var) => Ok((var.clone(), T::Ty::default())),
                            _ => Err(DecompilationError::Corrupt),
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                    body: decompile(thunk)?,
                });
                node_to_syntax.insert(node, x);
            }
        }
    }

    // Check the graph has a unique output.
    let port = graph
        .graph_outputs()
        .exactly_one()
        .map_err(|_err| DecompilationError::MultipleOutputs)?
        .link();

    let value = match port.weight() {
        Name::Variable(var) => Value::Variable(var.clone()),
        Name::Thunk(_) => return Err(DecompilationError::Corrupt),
        Name::Null => match port.node() {
            None => return Err(DecompilationError::Corrupt),
            Some(node) => node_to_syntax
                .get(&node)
                .and_then(|x| x.as_ref().left().cloned())
                .ok_or(DecompilationError::Corrupt)?,
        },
    };

    Ok(Expr { binds, value })
}
