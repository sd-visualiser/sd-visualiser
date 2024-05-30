use std::{collections::HashMap, fmt::Display, mem};

use dot_structures::{Id, Vertex};
use pretty::RcDoc;
use thiserror::Error;

use crate::{
    codeable::Codeable,
    common::Matchable,
    hypergraph::{
        builder::{Fragment, HypergraphBuilder, HypergraphError, InPort, OutPort},
        traits::{WireType, WithType, WithWeight},
        Edge, Hypergraph, Operation, Thunk, Weight,
    },
    prettyprinter::PrettyPrint,
};

#[derive(Debug, Clone)]
pub struct Label(pub String);

impl WithType for Label {
    fn get_type(&self) -> WireType {
        WireType::Data
    }
}

impl Matchable for Label {
    fn is_match(&self, query: &str) -> bool {
        self.0 == query
    }
}

impl PrettyPrint for Label {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text(&self.0)
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Codeable for Operation<DotWeight> {
    type Code = Label;

    fn code(&self) -> Self::Code {
        self.weight()
    }
}

impl Codeable for Thunk<DotWeight> {
    type Code = Label;

    fn code(&self) -> Self::Code {
        self.weight()
    }
}

impl Codeable for Edge<DotWeight> {
    type Code = Label;

    fn code(&self) -> Self::Code {
        self.weight()
    }
}

pub struct DotWeight;

impl Weight for DotWeight {
    type EdgeWeight = Label;

    type OperationWeight = Label;

    type ThunkWeight = Label;
}

#[derive(Error, Debug)]
pub enum DotError {
    #[error("Unsupported graph")]
    Unsupported,
    #[error("Hypergraph error: {0:?}")]
    HypergraphError(#[from] HypergraphError<DotWeight>),
}

fn id_to_string(id: &Id) -> String {
    match id {
        Id::Html(s) | Id::Plain(s) | Id::Anonymous(s) => s.clone(),
        Id::Escaped(s) => s.trim_matches(|x| x == '\"').to_owned(),
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Default)]
pub struct DotSettings {
    pub invert: bool,
    pub collect: bool,
}

pub fn dot_to_graph(
    dot: &dot_structures::Graph,
    settings: DotSettings,
) -> Result<Hypergraph<DotWeight>, DotError> {
    match dot {
        dot_structures::Graph::Graph { .. } => Err(DotError::Unsupported),
        dot_structures::Graph::DiGraph { stmts, .. } => {
            let mut in_out: HashMap<Id, (usize, usize, String)> = HashMap::new();
            let mut edges: Vec<(Id, Id)> = vec![];

            for s in stmts {
                match s {
                    dot_structures::Stmt::Node(n) => {
                        let label = n
                            .attributes
                            .iter()
                            .find(|x| x.0 == Id::Plain("label".to_owned()))
                            .map_or(String::new(), |x| id_to_string(&x.1));
                        in_out.insert(n.id.0.clone(), (0, 0, label));
                    }
                    dot_structures::Stmt::Edge(e) => match &e.ty {
                        dot_structures::EdgeTy::Pair(Vertex::N(x), Vertex::N(y)) => {
                            let mut x = x;
                            let mut y = y;
                            if settings.invert {
                                mem::swap(&mut x, &mut y);
                            };
                            in_out.entry(x.0.clone()).and_modify(|(_, y, _)| {
                                *y += 1;
                            });
                            in_out.entry(y.0.clone()).and_modify(|(x, _, _)| {
                                *x += 1;
                            });
                            edges.push((x.0.clone(), y.0.clone()));
                        }
                        _ => {
                            return Err(DotError::Unsupported);
                        }
                    },
                    _ => {
                        return Err(DotError::Unsupported);
                    }
                }
            }

            let mut builder = HypergraphBuilder::<DotWeight>::new(vec![], 0);
            #[allow(clippy::type_complexity)]
            let mut in_out_port: HashMap<
                Id,
                (Vec<InPort<DotWeight>>, Vec<OutPort<DotWeight>>),
            > = HashMap::new();
            for (id, (inputs, outputs, label)) in in_out {
                let op = builder.add_operation(
                    inputs,
                    if settings.collect {
                        if outputs == 0 {
                            vec![]
                        } else {
                            vec![Label(label.clone())]
                        }
                    } else {
                        vec![Label(label.clone()); outputs]
                    },
                    Label(label),
                );
                in_out_port.insert(id, (op.inputs().collect(), op.outputs().collect()));
            }
            for (out_node, in_node) in edges {
                builder.link(
                    if settings.collect {
                        in_out_port[&out_node].1.first().unwrap().clone()
                    } else {
                        in_out_port.get_mut(&out_node).unwrap().1.pop().unwrap()
                    },
                    in_out_port.get_mut(&in_node).unwrap().0.pop().unwrap(),
                )?;
            }

            let graph = builder.build()?;
            Ok(graph)
        }
    }
}
