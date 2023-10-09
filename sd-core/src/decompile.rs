use std::collections::HashMap;

use derivative::Derivative;
use itertools::Itertools;
use pretty::RcDoc;
use thiserror::Error;

use crate::{
    graph::Name,
    hypergraph::{
        generic::{Edge, Node, Operation, Thunk},
        traits::{EdgeLike, Graph, NodeLike, WithWeight},
    },
    language::{Bind, Expr, Fresh, Language, Thunk as SThunk, Value},
    prettyprinter::{paran_list, PrettyPrint},
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

        // Maps hypergraph nodes to values.
        let mut node_to_value = HashMap::<Node<G::Ctx>, Value<T>>::default();

        let mut fresh_vars = 0;
        for node in graph.nodes().rev() {
            match &node {
                Node::Operation(op) => {
                    let mut args = Vec::default();
                    for edge in op.inputs() {
                        match edge.weight().into_var() {
                            Some(var) => {
                                args.push(Value::Variable(var));
                            }
                            None => match edge.source().into_node() {
                                None => {
                                    args.push(Value::Variable(T::Var::fresh(fresh_vars)));
                                    fresh_vars += 1;
                                }
                                Some(other_node) => {
                                    args.push(
                                        node_to_value
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
                            node_to_value.insert(node, value);
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
                            node_to_value.insert(node, Value::Thunk(thunk));
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
                None => match edge.source().into_node() {
                    None => Err(DecompileError::Corrupt),
                    Some(node) => node_to_value
                        .get(&node)
                        .cloned()
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

/// Similar to `Value` but contains fresh variables, thunk addresses, and variable definitions.
#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub enum FakeValue<T: Language> {
    Fresh,
    Thunk(T::Addr),
    FreeVar(T::Var),
    BoundVar(T::VarDef),
    Operation(T::Op, Vec<FakeValue<T>>),
}

impl<T: Language> FakeValue<T> {
    pub fn decompile<E>(edge: &E) -> Self
    where
        E: EdgeLike + WithWeight<Weight = Name<T>>,
        Edge<E::Ctx>: WithWeight<Weight = Name<T>>,
        Operation<E::Ctx>: WithWeight<Weight = T::Op>,
        Thunk<E::Ctx>: WithWeight<Weight = T::Addr>,
    {
        match edge.weight() {
            Name::Nil => match edge.source().into_node() {
                None => Self::Fresh,
                Some(Node::Operation(op)) => Self::Operation(
                    op.weight(),
                    op.inputs().map(|edge| Self::decompile(&edge)).collect(),
                ),
                Some(Node::Thunk(thunk)) => Self::Thunk(thunk.weight()),
            },
            Name::FreeVar(var) => Self::FreeVar(var),
            Name::BoundVar(def) => Self::BoundVar(def),
        }
    }
}

impl<T: Language> PrettyPrint for FakeValue<T> {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Self::Fresh => RcDoc::text("?"),
            Self::Thunk(addr) => {
                let addr = addr.to_pretty();
                if addr.is_empty() {
                    RcDoc::text("thunk")
                } else {
                    RcDoc::text("thunk")
                        .append(RcDoc::space())
                        .append(RcDoc::text(addr))
                }
            }
            Self::FreeVar(var) => var.to_doc(),
            Self::BoundVar(def) => def.to_doc(),
            Self::Operation(op, vs) => {
                if vs.is_empty() {
                    op.to_doc()
                } else {
                    op.to_doc().append(paran_list(vs))
                }
            }
        }
    }
}
