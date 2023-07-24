use std::collections::HashMap;

use itertools::Itertools;
use thiserror::Error;

use crate::{
    common::Addr,
    graph::{Name, Op},
    hypergraph::traits::{EdgeLike, Graph, NodeLike, WithWeight},
    language::{Arg, Bind, Expr, Language, Thunk, Value},
};

#[derive(Clone, Debug, Error)]
pub enum DecompilationError {
    #[error("corrupt hypergraph")]
    Corrupt,

    #[error("multiple outputs not allowed")]
    MultipleOutputs,
}

pub fn decompile<G, T: Language>(graph: &G) -> Result<Expr<T>, DecompilationError>
where
    G: Graph,
    <G::T as Addr>::Node: NodeLike<T = G::T>,
    <G::T as Addr>::Edge: EdgeLike<T = G::T> + WithWeight<Weight = Name<T>>,
    <G::T as Addr>::Operation: NodeLike<T = G::T> + WithWeight<Weight = Op<T>>,
    <G::T as Addr>::Thunk: NodeLike<T = G::T> + Graph<T = G::T>,
{
    let mut binds = Vec::default();

    // Maps hypergraph nodes to corresponding thunks (for thunk nodes) or values (for operation nodes).
    let mut node_to_syntax = HashMap::<<G::T as Addr>::Node, Arg<T>>::default();

    for node in graph.nodes().rev() {
        // Check the node has a unique output.
        let output = node
            .outputs()
            .exactly_one()
            .map_err(|_err| DecompilationError::MultipleOutputs)?;

        if let Ok(op) = <G::T as Addr>::Operation::try_from(node.clone()) {
            let mut args = Vec::default();
            for edge in op.inputs() {
                match edge.weight().to_var() {
                    Some(var) => {
                        args.push(Arg::Value(Value::Variable(var.clone())));
                    }
                    None => match edge.source() {
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
        } else if let Ok(thunk) = <G::T as Addr>::Thunk::try_from(node.clone()) {
            let addr = match output.weight() {
                Name::Thunk(addr) => addr.clone(),
                _ => return Err(DecompilationError::Corrupt),
            };
            let thunk = Thunk {
                addr,
                args: thunk
                    .bound_graph_inputs()
                    .map(|edge| match edge.weight() {
                        Name::BoundVar(arg) => Ok(arg.clone()),
                        _ => Err(DecompilationError::Corrupt),
                    })
                    .collect::<Result<Vec<_>, _>>()?,
                body: decompile(&thunk)?,
            };
            node_to_syntax.insert(node, Arg::Thunk(thunk));
        } else {
            return Err(DecompilationError::Corrupt);
        }
    }

    let values = graph
        .graph_outputs()
        .map(|edge| match edge.weight().to_var() {
            Some(var) => Ok(Value::Variable(var.clone())),
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
