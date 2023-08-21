use std::collections::HashMap;

use itertools::Itertools;
use thiserror::Error;

use crate::{
    common::Addr,
    graph::{Name, Op},
    hypergraph::traits::{EdgeLike, Graph, NodeLike, WithWeight},
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

    #[error("multiple outputs not allowed")]
    MultipleOutputs,
}

pub fn decompile<G, T: Language>(graph: &G) -> Result<Expr<T>, DecompilationError>
where
    G: Graph,
    <G::T as Addr>::Edge: WithWeight<Weight = Name<T>>,
    <G::T as Addr>::Operation: WithWeight<Weight = Op<T>>,
    T::Var: Fresh,
{
    let mut binds = Vec::default();

    // Maps hypergraph nodes to corresponding thunks (for thunk nodes) or values (for operation nodes).
    let mut node_to_syntax = HashMap::<<G::T as Addr>::Node, Arg<T>>::default();

    let mut fresh_vars = 0;
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
                op: op.weight().0.clone(),
                args,
            };

            match output.weight() {
                Name::Op => {
                    node_to_syntax.insert(node, Arg::Value(value));
                }
                Name::BoundVar(def) => {
                    binds.push(Bind {
                        def: vec![def.clone()],
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
