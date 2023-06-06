use std::collections::HashMap;

use itertools::{Either, Itertools};
use thiserror::Error;

use crate::{
    graph::Name,
    hypergraph::{Graph, GraphView, Node},
    language::spartan::{BindClause, Expr, Op, Thunk as SThunk, Value},
};

#[derive(Clone, Debug, Error)]
pub enum DecompilationError {
    #[error("corrupt hypergraph")]
    Corrupt,

    #[error("multiple outputs not allowed")]
    MultipleOutputs,
}

pub fn decompile<G>(graph: &G) -> Result<Expr, DecompilationError>
where
    G: GraphView<NodeWeight = Op, EdgeWeight = Name>,
{
    let mut binds = Vec::default();

    // Maps hypergraph nodes to corresponding thunks (for thunk nodes) or values (for operation nodes).
    let mut node_to_syntax = HashMap::<Node<_, _>, Either<Value, SThunk>>::default();

    for node in graph.nodes().rev() {
        match &node {
            Node::Operation(op) => {
                let mut args = Vec::default();
                for port in op.inputs().map(|port| port.link()) {
                    match port.weight() {
                        Some(var) => {
                            args.push(Either::Left(Value::Variable(var.clone())));
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
                    op: op.weight().clone(),
                    vs: args
                        .iter()
                        .filter_map(|x| x.as_ref().left().cloned())
                        .collect(),
                    ds: args
                        .iter()
                        .filter_map(|x| x.as_ref().right().cloned())
                        .collect(),
                };

                // Check the operation has a unique output.
                let port = op
                    .outputs()
                    .exactly_one()
                    .map_err(|_err| DecompilationError::MultipleOutputs)?;

                if let Some(var) = port.weight().clone() {
                    binds.push(BindClause { var, value });
                } else {
                    node_to_syntax.insert(node, Either::Left(value));
                }
            }
            Node::Thunk(thunk) => {
                let x = Either::Right(SThunk {
                    args: thunk
                        .bound_graph_inputs()
                        .map(|out_port| out_port.weight().clone())
                        .collect::<Option<Vec<_>>>()
                        .ok_or(DecompilationError::Corrupt)?,
                    body: decompile(thunk)?,
                });
                node_to_syntax.insert(node, x);
            }
        }
    }

    // Check the operation has a unique output.
    let port = graph
        .graph_outputs()
        .exactly_one()
        .map_err(|_err| DecompilationError::MultipleOutputs)?
        .link();

    let value = match port.weight() {
        Some(var) => Value::Variable(var.clone()),
        None => match port.node() {
            None => return Err(DecompilationError::Corrupt),
            Some(node) => node_to_syntax
                .get(&node)
                .and_then(|x| x.as_ref().left().cloned())
                .ok_or(DecompilationError::Corrupt)?,
        },
    };

    Ok(Expr { binds, value })
}
