#![allow(clippy::type_repetition_in_bounds)]

use std::{fmt::Debug, sync::Arc};

use derivative::Derivative;
use indexmap::IndexSet;
use itertools::Either;

use crate::{
    hypergraph::{
        generic::{Ctx, Edge, EdgeWeight, Node, NodeWeight, Operation, Thunk},
        traits::{EdgeLike, Graph, NodeLike, WithWeight},
        utils::create_hidden_edges,
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
pub struct InteractiveGraph<G: Graph> {
    graph: G,
    hidden_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>,
}

impl<G: Graph> InteractiveGraph<G> {
    pub fn new(graph: G) -> Self {
        let hidden_edges = create_hidden_edges(&graph);
        Self {
            graph,
            hidden_edges: Arc::new(hidden_edges),
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
pub enum InteractiveEdge<G: Graph> {
    Internal {
        edge: Edge<G::Ctx>,
        #[derivative(PartialEq = "ignore", Hash = "ignore", Debug = "ignore")]
        hidden_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>,
    },
    Reuse {
        edge: Edge<G::Ctx>,
        target: Option<Node<G::Ctx>>,
        #[derivative(PartialEq = "ignore", Hash = "ignore", Debug = "ignore")]
        hidden_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>,
    },
}

impl<G: Graph> InteractiveEdge<G> {
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
pub enum InteractiveOperation<G: Graph> {
    Internal {
        op: Operation<G::Ctx>,
        #[derivative(PartialEq = "ignore", Hash = "ignore", Debug = "ignore")]
        hidden_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>,
    },
    Store {
        edge: Edge<G::Ctx>,
        #[derivative(PartialEq = "ignore", Hash = "ignore", Debug = "ignore")]
        hidden_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>,
    },
    Reuse {
        edge: Edge<G::Ctx>,
        target: Option<Node<G::Ctx>>,
        #[derivative(PartialEq = "ignore", Hash = "ignore", Debug = "ignore")]
        hidden_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>,
    },
}

impl<G: Graph> InteractiveOperation<G> {
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
pub struct InteractiveThunk<G: Graph> {
    thunk: Thunk<G::Ctx>,
    #[derivative(PartialEq = "ignore", Hash = "ignore", Debug = "ignore")]
    hidden_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>,
}

impl<G: Graph> InteractiveThunk<G> {
    pub fn inner(&self) -> &Thunk<G::Ctx> {
        &self.thunk
    }
}

////////////////////////////////////////////////////////////////

pub type InteractiveNode<G> = Node<InteractiveGraph<G>>;

impl<G: Graph> InteractiveNode<G> {
    fn internal(node: Node<G::Ctx>, hidden_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>) -> Self {
        match node {
            Node::Operation(op) => {
                Node::Operation(InteractiveOperation::Internal { op, hidden_edges })
            }
            Node::Thunk(thunk) => Node::Thunk(InteractiveThunk {
                thunk,
                hidden_edges,
            }),
        }
    }

    fn store(edge: Edge<G::Ctx>, hidden_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>) -> Self {
        Node::Operation(InteractiveOperation::Store { edge, hidden_edges })
    }

    fn reuse(
        edge: Edge<G::Ctx>,
        target: Option<Node<G::Ctx>>,
        hidden_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>,
    ) -> Self {
        Node::Operation(InteractiveOperation::Reuse {
            edge,
            target,
            hidden_edges,
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

impl<G: Graph> Ctx for InteractiveGraph<G> {
    type Edge = InteractiveEdge<G>;
    type Operation = InteractiveOperation<G>;
    type Thunk = InteractiveThunk<G>;
}

impl<G: Graph> Graph for InteractiveGraph<G> {
    type Ctx = InteractiveGraph<G>;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = InteractiveEdge<G>> + '_> {
        Box::new(
            self.graph
                .free_graph_inputs()
                .map(|edge| InteractiveEdge::Internal {
                    edge,
                    hidden_edges: self.hidden_edges.clone(),
                }),
        )
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = InteractiveEdge<G>> + '_> {
        Box::new(std::iter::empty())
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = InteractiveEdge<G>> + '_> {
        Box::new(self.graph.graph_outputs().map(|edge| {
            if self.hidden_edges[&edge] {
                InteractiveEdge::Reuse {
                    edge,
                    target: None,
                    hidden_edges: self.hidden_edges.clone(),
                }
            } else {
                InteractiveEdge::Internal {
                    edge,
                    hidden_edges: self.hidden_edges.clone(),
                }
            }
        }))
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = InteractiveNode<G>> + '_> {
        let mut nodes: IndexSet<InteractiveNode<G>> = IndexSet::default();
        for edge in self.graph.graph_outputs() {
            if self.hidden_edges[&edge] {
                nodes.insert(Node::reuse(edge, None, self.hidden_edges.clone()));
            }
        }
        for node in self.graph.nodes() {
            for edge in node.outputs().filter(|edge| self.hidden_edges[edge]) {
                nodes.insert(Node::store(edge, self.hidden_edges.clone()));
            }

            nodes.insert(Node::internal(node.clone(), self.hidden_edges.clone()));

            if let Node::Operation(_) = &node {
                for edge in node.inputs().filter(|edge| self.hidden_edges[edge]) {
                    nodes.insert(Node::reuse(
                        edge,
                        Some(node.clone()),
                        self.hidden_edges.clone(),
                    ));
                }
            }
        }
        for edge in self.graph.graph_inputs() {
            if self.hidden_edges[&edge] {
                nodes.insert(Node::store(edge, self.hidden_edges.clone()));
            }
        }
        Box::new(nodes.into_iter())
    }

    fn graph_backlink(&self) -> Option<InteractiveThunk<G>> {
        None
    }
}

impl<G: Graph> EdgeLike for InteractiveEdge<G> {
    type Ctx = InteractiveGraph<G>;

    fn source(&self) -> Option<Node<Self::Ctx>> {
        match &self {
            Self::Internal { edge, hidden_edges } => edge
                .source()
                .map(|node| Node::internal(node, hidden_edges.clone())),
            Self::Reuse {
                edge,
                target,
                hidden_edges,
            } => Some(Node::reuse(
                edge.clone(),
                target.clone(),
                hidden_edges.clone(),
            )),
        }
    }

    fn targets(&self) -> Box<dyn DoubleEndedIterator<Item = Option<Node<Self::Ctx>>> + '_> {
        match &self {
            Self::Internal { edge, hidden_edges } => {
                if hidden_edges[edge] {
                    Box::new(std::iter::once(Some(Node::store(
                        edge.clone(),
                        hidden_edges.clone(),
                    ))))
                } else {
                    Box::new(edge.targets().map(|target| {
                        target.map(|node| Node::internal(node, hidden_edges.clone()))
                    }))
                }
            }
            Self::Reuse {
                edge,
                target,
                hidden_edges,
            } => {
                // Calculate how many times `target` is copied in `edge.targets()`.
                let copies = edge.targets().filter(|t| t == target).count();
                Box::new((0..copies).map(|_| {
                    target
                        .as_ref()
                        .map(|node| Node::internal(node.clone(), hidden_edges.clone()))
                }))
            }
        }
    }
}

impl<G: Graph> NodeLike for InteractiveOperation<G> {
    type Ctx = InteractiveGraph<G>;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = InteractiveEdge<G>> + '_> {
        match &self {
            Self::Internal { op, hidden_edges } => Box::new(op.inputs().map(|edge| {
                if hidden_edges[&edge] {
                    InteractiveEdge::Reuse {
                        edge,
                        target: Some(Node::Operation(op.clone())),
                        hidden_edges: hidden_edges.clone(),
                    }
                } else {
                    InteractiveEdge::Internal {
                        edge,
                        hidden_edges: hidden_edges.clone(),
                    }
                }
            })),
            Self::Store { edge, hidden_edges } => {
                Box::new(std::iter::once(InteractiveEdge::Internal {
                    edge: edge.clone(),
                    hidden_edges: hidden_edges.clone(),
                }))
            }
            Self::Reuse { .. } => Box::new(std::iter::empty()),
        }
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = InteractiveEdge<G>> + '_> {
        match &self {
            Self::Internal { op, hidden_edges } => {
                Box::new(op.outputs().map(|edge| InteractiveEdge::Internal {
                    edge,
                    hidden_edges: hidden_edges.clone(),
                }))
            }
            Self::Store { .. } => Box::new(std::iter::empty()),
            Self::Reuse {
                edge,
                target,
                hidden_edges,
            } => Box::new(std::iter::once(InteractiveEdge::Reuse {
                edge: edge.clone(),
                target: target.clone(),
                hidden_edges: hidden_edges.clone(),
            })),
        }
    }

    fn backlink(&self) -> Option<InteractiveThunk<G>> {
        match &self {
            Self::Internal { op, hidden_edges } => Some(InteractiveThunk {
                thunk: op.backlink()?,
                hidden_edges: hidden_edges.clone(),
            }),
            Self::Store { edge, hidden_edges } => edge.source().and_then(|node| {
                Some(InteractiveThunk {
                    thunk: node.backlink()?,
                    hidden_edges: hidden_edges.clone(),
                })
            }),
            Self::Reuse {
                target,
                hidden_edges,
                ..
            } => target.as_ref().and_then(|node| {
                Some(InteractiveThunk {
                    thunk: node.backlink()?,
                    hidden_edges: hidden_edges.clone(),
                })
            }),
        }
    }
}

impl<G: Graph> Graph for InteractiveThunk<G> {
    type Ctx = InteractiveGraph<G>;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = InteractiveEdge<G>> + '_> {
        Box::new(
            self.thunk
                .free_graph_inputs()
                .filter(|edge| !self.hidden_edges[edge])
                .map(|edge| InteractiveEdge::Internal {
                    edge,
                    hidden_edges: self.hidden_edges.clone(),
                }),
        )
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = InteractiveEdge<G>> + '_> {
        Box::new(
            self.thunk
                .bound_graph_inputs()
                .map(|edge| InteractiveEdge::Internal {
                    edge,
                    hidden_edges: self.hidden_edges.clone(),
                }),
        )
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = InteractiveEdge<G>> + '_> {
        Box::new(self.thunk.graph_outputs().map(|edge| {
            if self.hidden_edges[&edge] {
                InteractiveEdge::Reuse {
                    edge,
                    target: Some(Node::Thunk(self.thunk.clone())),
                    hidden_edges: self.hidden_edges.clone(),
                }
            } else {
                InteractiveEdge::Internal {
                    edge,
                    hidden_edges: self.hidden_edges.clone(),
                }
            }
        }))
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = InteractiveNode<G>> + '_> {
        let mut nodes: IndexSet<InteractiveNode<G>> = IndexSet::default();
        for edge in self.thunk.graph_outputs() {
            if self.hidden_edges[&edge] {
                nodes.insert(Node::reuse(
                    edge,
                    Some(Node::Thunk(self.thunk.clone())),
                    self.hidden_edges.clone(),
                ));
            }
        }
        for node in self.thunk.nodes() {
            for edge in node.outputs().filter(|edge| self.hidden_edges[edge]) {
                nodes.insert(Node::store(edge, self.hidden_edges.clone()));
            }

            nodes.insert(Node::internal(node.clone(), self.hidden_edges.clone()));

            if let Node::Operation(_) = &node {
                for edge in node.inputs().filter(|edge| self.hidden_edges[edge]) {
                    nodes.insert(Node::reuse(
                        edge,
                        Some(node.clone()),
                        self.hidden_edges.clone(),
                    ));
                }
            }
        }
        for edge in self.thunk.bound_graph_inputs() {
            if self.hidden_edges[&edge] {
                nodes.insert(Node::store(edge, self.hidden_edges.clone()));
            }
        }
        Box::new(nodes.into_iter())
    }

    fn graph_backlink(&self) -> Option<InteractiveThunk<G>> {
        Some(InteractiveThunk {
            thunk: self.thunk.clone(),
            hidden_edges: self.hidden_edges.clone(),
        })
    }
}

impl<G: Graph> NodeLike for InteractiveThunk<G> {
    type Ctx = InteractiveGraph<G>;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(
            self.thunk
                .inputs()
                .filter(|edge| !self.hidden_edges[edge])
                .map(|edge| InteractiveEdge::Internal {
                    edge,
                    hidden_edges: self.hidden_edges.clone(),
                }),
        )
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.thunk.outputs().map(|edge| InteractiveEdge::Internal {
            edge,
            hidden_edges: self.hidden_edges.clone(),
        }))
    }

    fn backlink(&self) -> Option<Thunk<Self::Ctx>> {
        Some(InteractiveThunk {
            thunk: self.thunk.backlink()?,
            hidden_edges: self.hidden_edges.clone(),
        })
    }
}

impl<G: Graph> WithWeight for InteractiveEdge<G> {
    type Weight = EdgeWeight<G::Ctx>;

    fn weight(&self) -> Self::Weight {
        self.inner().weight()
    }
}

impl<G: Graph> WithWeight for InteractiveOperation<G> {
    type Weight = Either<NodeWeight<G::Ctx>, EdgeWeight<G::Ctx>>;

    fn weight(&self) -> Self::Weight {
        self.inner()
            .map_either(WithWeight::weight, WithWeight::weight)
    }
}
