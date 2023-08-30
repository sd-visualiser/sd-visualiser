#![allow(clippy::type_repetition_in_bounds)]

use std::{fmt::Debug, sync::Arc};

use derivative::Derivative;
use indexmap::IndexSet;
use itertools::Either;

use crate::{
    hypergraph::{
        generic::{Ctx, Edge, EdgeWeight, Node, Operation, OperationWeight, Thunk, ThunkWeight},
        traits::{EdgeLike, Graph, NodeLike, WithWeight},
        utils::create_cut_edges,
    },
    weak_map::WeakMap,
};

////////////////////////////////////////////////////////////////

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "G: Debug, Edge<G::Ctx>: Debug")
)]
pub struct CutGraph<G: Graph> {
    graph: G,
    cut_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>,
}

impl<G: Graph> CutGraph<G> {
    pub fn new(graph: G) -> Self {
        let cut_edges = create_cut_edges(&graph);
        Self {
            graph,
            cut_edges: Arc::new(cut_edges),
        }
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "Edge<G::Ctx>: Debug, Operation<G::Ctx>: Debug, Thunk<G::Ctx>: Debug")
)]
pub enum CutEdge<G: Graph> {
    Internal {
        edge: Edge<G::Ctx>,
        #[derivative(PartialEq = "ignore", Hash = "ignore", Debug = "ignore")]
        cut_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>,
    },
    Reuse {
        edge: Edge<G::Ctx>,
        target: Option<Node<G::Ctx>>,
        #[derivative(PartialEq = "ignore", Hash = "ignore", Debug = "ignore")]
        cut_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>,
    },
}

impl<G: Graph> CutEdge<G> {
    pub fn inner(&self) -> &Edge<G::Ctx> {
        match self {
            Self::Internal { edge, .. } | Self::Reuse { edge, .. } => edge,
        }
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "Edge<G::Ctx>: Debug, Operation<G::Ctx>: Debug, Thunk<G::Ctx>: Debug")
)]
pub enum CutOperation<G: Graph> {
    Internal {
        op: Operation<G::Ctx>,
        #[derivative(PartialEq = "ignore", Hash = "ignore", Debug = "ignore")]
        cut_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>,
    },
    Store {
        edge: Edge<G::Ctx>,
        #[derivative(PartialEq = "ignore", Hash = "ignore", Debug = "ignore")]
        cut_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>,
    },
    Reuse {
        edge: Edge<G::Ctx>,
        target: Option<Node<G::Ctx>>,
        #[derivative(PartialEq = "ignore", Hash = "ignore", Debug = "ignore")]
        cut_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>,
    },
}

impl<G: Graph> CutOperation<G> {
    pub fn inner(&self) -> Either<&Operation<G::Ctx>, &Edge<G::Ctx>> {
        match self {
            Self::Internal { op, .. } => Either::Left(op),
            Self::Store { edge, .. } | Self::Reuse { edge, .. } => Either::Right(edge),
        }
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "Thunk<G::Ctx>: Debug")
)]
pub struct CutThunk<G: Graph> {
    thunk: Thunk<G::Ctx>,
    #[derivative(PartialEq = "ignore", Hash = "ignore", Debug = "ignore")]
    cut_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>,
}

impl<G: Graph> CutThunk<G> {
    pub fn inner(&self) -> &Thunk<G::Ctx> {
        &self.thunk
    }
}

////////////////////////////////////////////////////////////////

pub type CutNode<G> = Node<CutGraph<G>>;

impl<G: Graph> CutNode<G> {
    fn internal(node: Node<G::Ctx>, cut_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>) -> Self {
        match node {
            Node::Operation(op) => Node::Operation(CutOperation::Internal { op, cut_edges }),
            Node::Thunk(thunk) => Node::Thunk(CutThunk { thunk, cut_edges }),
        }
    }

    fn store(edge: Edge<G::Ctx>, cut_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>) -> Self {
        Node::Operation(CutOperation::Store { edge, cut_edges })
    }

    fn reuse(
        edge: Edge<G::Ctx>,
        target: Option<Node<G::Ctx>>,
        cut_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>,
    ) -> Self {
        Node::Operation(CutOperation::Reuse {
            edge,
            target,
            cut_edges,
        })
    }

    pub fn inner(&self) -> Either<Node<G::Ctx>, &Edge<G::Ctx>> {
        match self {
            Node::Operation(op) => op.inner().map_left(|op| Node::Operation(op.clone())),
            Node::Thunk(thunk) => Either::Left(Node::Thunk(thunk.inner().clone())),
        }
    }
}

////////////////////////////////////////////////////////////////

impl<G: Graph> Ctx for CutGraph<G> {
    type Edge = CutEdge<G>;
    type Operation = CutOperation<G>;
    type Thunk = CutThunk<G>;
}

impl<G: Graph> Graph for CutGraph<G> {
    type Ctx = CutGraph<G>;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = CutEdge<G>> + '_> {
        Box::new(
            self.graph
                .free_graph_inputs()
                .map(|edge| CutEdge::Internal {
                    edge,
                    cut_edges: self.cut_edges.clone(),
                }),
        )
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = CutEdge<G>> + '_> {
        Box::new(std::iter::empty())
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = CutEdge<G>> + '_> {
        Box::new(self.graph.graph_outputs().map(|edge| {
            if self.cut_edges[&edge] {
                CutEdge::Reuse {
                    edge,
                    target: None,
                    cut_edges: self.cut_edges.clone(),
                }
            } else {
                CutEdge::Internal {
                    edge,
                    cut_edges: self.cut_edges.clone(),
                }
            }
        }))
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = CutNode<G>> + '_> {
        let mut nodes: IndexSet<CutNode<G>> = IndexSet::default();
        for edge in self.graph.graph_outputs() {
            if self.cut_edges[&edge] {
                nodes.insert(Node::reuse(edge, None, self.cut_edges.clone()));
            }
        }
        for node in self.graph.nodes() {
            for edge in node.outputs().filter(|edge| self.cut_edges[edge]) {
                nodes.insert(Node::store(edge, self.cut_edges.clone()));
            }

            nodes.insert(Node::internal(node.clone(), self.cut_edges.clone()));

            if let Node::Operation(_) = &node {
                for edge in node.inputs().filter(|edge| self.cut_edges[edge]) {
                    nodes.insert(Node::reuse(
                        edge,
                        Some(node.clone()),
                        self.cut_edges.clone(),
                    ));
                }
            }
        }
        for edge in self.graph.graph_inputs() {
            if self.cut_edges[&edge] {
                nodes.insert(Node::store(edge, self.cut_edges.clone()));
            }
        }
        Box::new(nodes.into_iter())
    }

    fn graph_backlink(&self) -> Option<CutThunk<G>> {
        None
    }
}

impl<G: Graph> EdgeLike for CutEdge<G> {
    type Ctx = CutGraph<G>;

    fn source(&self) -> Option<Node<Self::Ctx>> {
        match &self {
            Self::Internal { edge, cut_edges } => edge
                .source()
                .map(|node| Node::internal(node, cut_edges.clone())),
            Self::Reuse {
                edge,
                target,
                cut_edges,
            } => Some(Node::reuse(edge.clone(), target.clone(), cut_edges.clone())),
        }
    }

    fn targets(&self) -> Box<dyn DoubleEndedIterator<Item = Option<Node<Self::Ctx>>> + '_> {
        match &self {
            Self::Internal { edge, cut_edges } => {
                if cut_edges[edge] {
                    Box::new(std::iter::once(Some(Node::store(
                        edge.clone(),
                        cut_edges.clone(),
                    ))))
                } else {
                    Box::new(
                        edge.targets().map(|target| {
                            target.map(|node| Node::internal(node, cut_edges.clone()))
                        }),
                    )
                }
            }
            Self::Reuse {
                edge,
                target,
                cut_edges,
            } => {
                // Calculate how many times `target` is copied in `edge.targets()`.
                let copies = edge.targets().filter(|t| t == target).count();
                Box::new((0..copies).map(|_| {
                    target
                        .as_ref()
                        .map(|node| Node::internal(node.clone(), cut_edges.clone()))
                }))
            }
        }
    }
}

impl<G: Graph> NodeLike for CutOperation<G> {
    type Ctx = CutGraph<G>;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = CutEdge<G>> + '_> {
        match &self {
            Self::Internal { op, cut_edges } => Box::new(op.inputs().map(|edge| {
                if cut_edges[&edge] {
                    CutEdge::Reuse {
                        edge,
                        target: Some(Node::Operation(op.clone())),
                        cut_edges: cut_edges.clone(),
                    }
                } else {
                    CutEdge::Internal {
                        edge,
                        cut_edges: cut_edges.clone(),
                    }
                }
            })),
            Self::Store { edge, cut_edges } => Box::new(std::iter::once(CutEdge::Internal {
                edge: edge.clone(),
                cut_edges: cut_edges.clone(),
            })),
            Self::Reuse { .. } => Box::new(std::iter::empty()),
        }
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = CutEdge<G>> + '_> {
        match &self {
            Self::Internal { op, cut_edges } => {
                Box::new(op.outputs().map(|edge| CutEdge::Internal {
                    edge,
                    cut_edges: cut_edges.clone(),
                }))
            }
            Self::Store { .. } => Box::new(std::iter::empty()),
            Self::Reuse {
                edge,
                target,
                cut_edges,
            } => Box::new(std::iter::once(CutEdge::Reuse {
                edge: edge.clone(),
                target: target.clone(),
                cut_edges: cut_edges.clone(),
            })),
        }
    }

    fn backlink(&self) -> Option<CutThunk<G>> {
        match &self {
            Self::Internal { op, cut_edges } => Some(CutThunk {
                thunk: op.backlink()?,
                cut_edges: cut_edges.clone(),
            }),
            Self::Store { edge, cut_edges } => edge.source().and_then(|node| {
                Some(CutThunk {
                    thunk: node.backlink()?,
                    cut_edges: cut_edges.clone(),
                })
            }),
            Self::Reuse {
                target, cut_edges, ..
            } => target.as_ref().and_then(|node| {
                Some(CutThunk {
                    thunk: node.backlink()?,
                    cut_edges: cut_edges.clone(),
                })
            }),
        }
    }
}

impl<G: Graph> Graph for CutThunk<G> {
    type Ctx = CutGraph<G>;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = CutEdge<G>> + '_> {
        Box::new(
            self.thunk
                .free_graph_inputs()
                .filter(|edge| !self.cut_edges[edge])
                .map(|edge| CutEdge::Internal {
                    edge,
                    cut_edges: self.cut_edges.clone(),
                }),
        )
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = CutEdge<G>> + '_> {
        Box::new(
            self.thunk
                .bound_graph_inputs()
                .map(|edge| CutEdge::Internal {
                    edge,
                    cut_edges: self.cut_edges.clone(),
                }),
        )
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = CutEdge<G>> + '_> {
        Box::new(self.thunk.graph_outputs().map(|edge| {
            if self.cut_edges[&edge] {
                CutEdge::Reuse {
                    edge,
                    target: Some(Node::Thunk(self.thunk.clone())),
                    cut_edges: self.cut_edges.clone(),
                }
            } else {
                CutEdge::Internal {
                    edge,
                    cut_edges: self.cut_edges.clone(),
                }
            }
        }))
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = CutNode<G>> + '_> {
        let mut nodes: IndexSet<CutNode<G>> = IndexSet::default();
        for edge in self.thunk.graph_outputs() {
            if self.cut_edges[&edge] {
                nodes.insert(Node::reuse(
                    edge,
                    Some(Node::Thunk(self.thunk.clone())),
                    self.cut_edges.clone(),
                ));
            }
        }
        for node in self.thunk.nodes() {
            for edge in node.outputs().filter(|edge| self.cut_edges[edge]) {
                nodes.insert(Node::store(edge, self.cut_edges.clone()));
            }

            nodes.insert(Node::internal(node.clone(), self.cut_edges.clone()));

            if let Node::Operation(_) = &node {
                for edge in node.inputs().filter(|edge| self.cut_edges[edge]) {
                    nodes.insert(Node::reuse(
                        edge,
                        Some(node.clone()),
                        self.cut_edges.clone(),
                    ));
                }
            }
        }
        for edge in self.thunk.bound_graph_inputs() {
            if self.cut_edges[&edge] {
                nodes.insert(Node::store(edge, self.cut_edges.clone()));
            }
        }
        Box::new(nodes.into_iter())
    }

    fn graph_backlink(&self) -> Option<CutThunk<G>> {
        Some(CutThunk {
            thunk: self.thunk.clone(),
            cut_edges: self.cut_edges.clone(),
        })
    }
}

impl<G: Graph> NodeLike for CutThunk<G> {
    type Ctx = CutGraph<G>;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(
            self.thunk
                .inputs()
                .filter(|edge| !self.cut_edges[edge])
                .map(|edge| CutEdge::Internal {
                    edge,
                    cut_edges: self.cut_edges.clone(),
                }),
        )
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.thunk.outputs().map(|edge| CutEdge::Internal {
            edge,
            cut_edges: self.cut_edges.clone(),
        }))
    }

    fn backlink(&self) -> Option<Thunk<Self::Ctx>> {
        Some(CutThunk {
            thunk: self.thunk.backlink()?,
            cut_edges: self.cut_edges.clone(),
        })
    }
}

impl<G: Graph> WithWeight for CutEdge<G> {
    type Weight = EdgeWeight<G::Ctx>;

    fn weight(&self) -> Self::Weight {
        self.inner().weight()
    }
}

impl<G: Graph> WithWeight for CutOperation<G> {
    type Weight = Either<OperationWeight<G::Ctx>, EdgeWeight<G::Ctx>>;

    fn weight(&self) -> Self::Weight {
        self.inner()
            .map_either(WithWeight::weight, WithWeight::weight)
    }
}

impl<G: Graph> WithWeight for CutThunk<G> {
    type Weight = ThunkWeight<G::Ctx>;

    fn weight(&self) -> Self::Weight {
        self.inner().weight()
    }
}