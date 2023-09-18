use std::sync::Arc;

use derivative::Derivative;
use indexmap::IndexSet;
use itertools::Either;

use crate::{
    codeable::{Code, Codeable},
    common::Matchable,
    hypergraph::{
        generic::{Ctx, Edge, EdgeWeight, Node, Operation, OperationWeight, Thunk, ThunkWeight},
        subgraph::ExtensibleEdge,
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
    Debug(bound = "")
)]
pub struct CutGraph<G: Graph> {
    graph: G,
    cut_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>,
}

impl<G: Graph> CutGraph<G> {
    pub fn new(graph: G) -> Self {
        Self {
            cut_edges: Arc::new(create_cut_edges(&graph)),
            graph,
        }
    }

    pub fn toggle(&mut self, edge: &Edge<G::Ctx>) {
        let mut cut_edges = (*self.cut_edges).clone();
        cut_edges[edge] ^= true;
        self.cut_edges = Arc::new(cut_edges);
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub enum CutEdge<G: Graph> {
    Inner {
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
            Self::Inner { edge, .. } | Self::Reuse { edge, .. } => edge,
        }
    }

    pub fn into_inner(self) -> Edge<G::Ctx> {
        match self {
            Self::Inner { edge, .. } | Self::Reuse { edge, .. } => edge,
        }
    }
}

#[derive(Derivative)]
#[derivative(
    Clone(bound = ""),
    Eq(bound = ""),
    PartialEq(bound = ""),
    Hash(bound = ""),
    Debug(bound = "")
)]
pub enum CutOperation<G: Graph> {
    Inner {
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
            Self::Inner { op, .. } => Either::Left(op),
            Self::Store { edge, .. } | Self::Reuse { edge, .. } => Either::Right(edge),
        }
    }

    pub fn into_inner(self) -> Either<Operation<G::Ctx>, Edge<G::Ctx>> {
        match self {
            Self::Inner { op, .. } => Either::Left(op),
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
    Debug(bound = "")
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

    pub fn into_inner(self) -> Thunk<G::Ctx> {
        self.thunk
    }
}

////////////////////////////////////////////////////////////////

pub type CutNode<G> = Node<CutGraph<G>>;

impl<G: Graph> CutNode<G> {
    fn new(node: Node<G::Ctx>, cut_edges: Arc<WeakMap<Edge<G::Ctx>, bool>>) -> Self {
        match node {
            Node::Operation(op) => Node::Operation(CutOperation::Inner { op, cut_edges }),
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

    pub fn into_inner(self) -> Either<Node<G::Ctx>, Edge<G::Ctx>> {
        match self {
            Node::Operation(op) => op.into_inner().map_left(Node::Operation),
            Node::Thunk(thunk) => Either::Left(Node::Thunk(thunk.into_inner())),
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

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.graph.free_graph_inputs().map(|edge| CutEdge::Inner {
            edge,
            cut_edges: self.cut_edges.clone(),
        }))
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(std::iter::empty())
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.graph.graph_outputs().map(|edge| {
            if self.cut_edges[&edge] {
                CutEdge::Reuse {
                    edge,
                    target: None,
                    cut_edges: self.cut_edges.clone(),
                }
            } else {
                CutEdge::Inner {
                    edge,
                    cut_edges: self.cut_edges.clone(),
                }
            }
        }))
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<Self::Ctx>> + '_> {
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

            nodes.insert(Node::new(node.clone(), self.cut_edges.clone()));

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

    fn graph_backlink(&self) -> Option<Thunk<Self::Ctx>> {
        None
    }
}

impl<G: Graph> EdgeLike for CutEdge<G> {
    type Ctx = CutGraph<G>;

    fn source(&self) -> Option<Node<Self::Ctx>> {
        match self {
            Self::Inner { edge, cut_edges } => {
                edge.source().map(|node| Node::new(node, cut_edges.clone()))
            }
            Self::Reuse {
                edge,
                target,
                cut_edges,
            } => Some(Node::reuse(edge.clone(), target.clone(), cut_edges.clone())),
        }
    }

    fn targets(&self) -> Box<dyn DoubleEndedIterator<Item = Option<Node<Self::Ctx>>> + '_> {
        match self {
            Self::Inner { edge, cut_edges } => {
                if cut_edges[edge] {
                    Box::new(std::iter::once(Some(Node::store(
                        edge.clone(),
                        cut_edges.clone(),
                    ))))
                } else {
                    Box::new(
                        edge.targets()
                            .map(|target| target.map(|node| Node::new(node, cut_edges.clone()))),
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
                        .map(|node| Node::new(node.clone(), cut_edges.clone()))
                }))
            }
        }
    }
}

impl<G: Graph> NodeLike for CutOperation<G> {
    type Ctx = CutGraph<G>;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        match self {
            Self::Inner { op, cut_edges } => Box::new(op.inputs().map(|edge| {
                if cut_edges[&edge] {
                    CutEdge::Reuse {
                        edge,
                        target: Some(Node::Operation(op.clone())),
                        cut_edges: cut_edges.clone(),
                    }
                } else {
                    CutEdge::Inner {
                        edge,
                        cut_edges: cut_edges.clone(),
                    }
                }
            })),
            Self::Store { edge, cut_edges } => Box::new(std::iter::once(CutEdge::Inner {
                edge: edge.clone(),
                cut_edges: cut_edges.clone(),
            })),
            Self::Reuse { .. } => Box::new(std::iter::empty()),
        }
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        match self {
            Self::Inner { op, cut_edges } => Box::new(op.outputs().map(|edge| CutEdge::Inner {
                edge,
                cut_edges: cut_edges.clone(),
            })),
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

    fn backlink(&self) -> Option<Thunk<Self::Ctx>> {
        match self {
            Self::Inner { op, cut_edges } => Some(CutThunk {
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

    fn number_of_inputs(&self) -> usize {
        match self {
            Self::Inner { op, .. } => op.number_of_inputs(),
            Self::Store { .. } => 1,
            Self::Reuse { .. } => 0,
        }
    }

    fn number_of_outputs(&self) -> usize {
        match self {
            Self::Inner { op, .. } => op.number_of_outputs(),
            Self::Store { .. } => 0,
            Self::Reuse { .. } => 1,
        }
    }
}

impl<G: Graph> Graph for CutThunk<G> {
    type Ctx = CutGraph<G>;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(
            self.thunk
                .free_graph_inputs()
                .filter(|edge| !self.cut_edges[edge])
                .map(|edge| CutEdge::Inner {
                    edge,
                    cut_edges: self.cut_edges.clone(),
                }),
        )
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.thunk.bound_graph_inputs().map(|edge| CutEdge::Inner {
            edge,
            cut_edges: self.cut_edges.clone(),
        }))
    }

    fn graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.thunk.graph_outputs().map(|edge| {
            if self.cut_edges[&edge] {
                CutEdge::Reuse {
                    edge,
                    target: Some(Node::Thunk(self.thunk.clone())),
                    cut_edges: self.cut_edges.clone(),
                }
            } else {
                CutEdge::Inner {
                    edge,
                    cut_edges: self.cut_edges.clone(),
                }
            }
        }))
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<Self::Ctx>> + '_> {
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

            nodes.insert(Node::new(node.clone(), self.cut_edges.clone()));

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

    fn graph_backlink(&self) -> Option<Thunk<Self::Ctx>> {
        Some(self.clone())
    }
}

impl<G: Graph> NodeLike for CutThunk<G> {
    type Ctx = CutGraph<G>;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(
            self.thunk
                .inputs()
                .filter(|edge| !self.cut_edges[edge])
                .map(|edge| CutEdge::Inner {
                    edge,
                    cut_edges: self.cut_edges.clone(),
                }),
        )
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.thunk.outputs().map(|edge| CutEdge::Inner {
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

    fn number_of_outputs(&self) -> usize {
        self.thunk.number_of_outputs()
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

impl<G: Graph> Codeable for CutEdge<G>
where
    Edge<G::Ctx>: Codeable,
{
    type Code = Code<Edge<G::Ctx>>;

    fn code(&self) -> Self::Code {
        self.inner().code()
    }
}

impl<G: Graph> Codeable for CutOperation<G>
where
    Edge<G::Ctx>: Codeable,
    Operation<G::Ctx>: Codeable,
{
    type Code = Either<Code<Operation<G::Ctx>>, Code<Edge<G::Ctx>>>;

    fn code(&self) -> Self::Code {
        self.inner().map_either(Codeable::code, Codeable::code)
    }
}

impl<G: Graph> Codeable for CutThunk<G>
where
    Thunk<G::Ctx>: Codeable,
{
    type Code = Code<Thunk<G::Ctx>>;

    fn code(&self) -> Self::Code {
        self.inner().code()
    }
}

impl<G: Graph> Matchable for CutEdge<G>
where
    Edge<G::Ctx>: Matchable,
{
    fn is_match(&self, query: &str) -> bool {
        self.inner().is_match(query)
    }
}

impl<G: Graph> Matchable for CutOperation<G>
where
    Edge<G::Ctx>: Matchable,
    Operation<G::Ctx>: Matchable,
{
    fn is_match(&self, query: &str) -> bool {
        self.inner()
            .either(|op| op.is_match(query), |edge| edge.is_match(query))
    }
}

impl<G: Graph> Matchable for CutThunk<G>
where
    Thunk<G::Ctx>: Matchable,
{
    fn is_match(&self, query: &str) -> bool {
        self.inner().is_match(query)
    }
}

impl<G: Graph> ExtensibleEdge for CutEdge<G> {}
