use std::sync::Arc;

use by_address::ByThinAddress;
use derivative::Derivative;
use indexmap::IndexSet;
use itertools::Either;

use crate::{
    codeable::{Code, Codeable},
    common::Matchable,
    hypergraph::{
        generic::{Ctx, Edge, Endpoint, Key, Node, Operation, Thunk, Weight},
        mapping::EdgeMap,
        subgraph::ExtensibleEdge,
        traits::{EdgeLike, Graph, Keyable, NodeLike, WithWeight},
    },
};

////////////////////////////////////////////////////////////////

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub struct CutGraph<G: Graph> {
    graph: G,
    cut_edges: ByThinAddress<Arc<EdgeMap<G::Ctx, bool>>>,
}

impl<G: Graph> CutGraph<G> {
    pub fn new(graph: G, cut_edges: EdgeMap<G::Ctx, bool>) -> Self {
        Self {
            graph,
            cut_edges: ByThinAddress(Arc::new(cut_edges)),
        }
    }

    pub fn inner(&self) -> &G {
        &self.graph
    }

    pub fn inner_mut(&mut self) -> &mut G {
        &mut self.graph
    }

    pub fn cut_edges(&self) -> &EdgeMap<G::Ctx, bool> {
        &self.cut_edges
    }

    pub fn toggle(&mut self, edge: &Edge<G::Ctx>) {
        let mut cut_edges = self.cut_edges().clone();
        cut_edges[&edge.key()] ^= true;
        self.cut_edges = ByThinAddress(Arc::new(cut_edges));
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
    #[derivative(Debug = "transparent")]
    Inner {
        edge: Edge<G::Ctx>,
        #[derivative(Debug = "ignore")]
        cut_edges: ByThinAddress<Arc<EdgeMap<G::Ctx, bool>>>,
    },
    Reuse {
        edge: Edge<G::Ctx>,
        target: Endpoint<G::Ctx>,
        #[derivative(Debug = "ignore")]
        cut_edges: ByThinAddress<Arc<EdgeMap<G::Ctx, bool>>>,
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
    #[derivative(Debug = "transparent")]
    Inner {
        op: Operation<G::Ctx>,
        #[derivative(Debug = "ignore")]
        cut_edges: ByThinAddress<Arc<EdgeMap<G::Ctx, bool>>>,
    },
    Store {
        edge: Edge<G::Ctx>,
        #[derivative(Debug = "ignore")]
        cut_edges: ByThinAddress<Arc<EdgeMap<G::Ctx, bool>>>,
    },
    Reuse {
        edge: Edge<G::Ctx>,
        target: Endpoint<G::Ctx>,
        #[derivative(Debug = "ignore")]
        cut_edges: ByThinAddress<Arc<EdgeMap<G::Ctx, bool>>>,
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
    Debug = "transparent"
)]
pub struct CutThunk<G: Graph> {
    thunk: Thunk<G::Ctx>,
    #[derivative(Debug = "ignore")]
    cut_edges: ByThinAddress<Arc<EdgeMap<G::Ctx, bool>>>,
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
    fn new(node: Node<G::Ctx>, cut_edges: ByThinAddress<Arc<EdgeMap<G::Ctx, bool>>>) -> Self {
        match node {
            Node::Operation(op) => Node::Operation(CutOperation::Inner { op, cut_edges }),
            Node::Thunk(thunk) => Node::Thunk(CutThunk { thunk, cut_edges }),
        }
    }

    fn store(edge: Edge<G::Ctx>, cut_edges: ByThinAddress<Arc<EdgeMap<G::Ctx, bool>>>) -> Self {
        Node::Operation(CutOperation::Store { edge, cut_edges })
    }

    fn reuse(
        edge: Edge<G::Ctx>,
        target: Endpoint<G::Ctx>,
        cut_edges: ByThinAddress<Arc<EdgeMap<G::Ctx, bool>>>,
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

pub type CutEndpoint<G> = Endpoint<CutGraph<G>>;

impl<G: Graph> CutEndpoint<G> {
    pub fn new(
        endpoint: Endpoint<G::Ctx>,
        cut_edges: ByThinAddress<Arc<EdgeMap<G::Ctx, bool>>>,
    ) -> Self {
        match endpoint {
            Endpoint::Node(node) => Endpoint::Node(Node::new(node, cut_edges)),
            Endpoint::Boundary(graph) => {
                Endpoint::Boundary(graph.map(|thunk| CutThunk { thunk, cut_edges }))
            }
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

    fn free_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(std::iter::empty())
    }

    fn bound_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.graph.bound_graph_outputs().map(|edge| {
            if self.cut_edges[&edge.key()] {
                CutEdge::Reuse {
                    edge,
                    target: Endpoint::Boundary(None),
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
            if self.cut_edges[&edge.key()] {
                nodes.insert(Node::reuse(
                    edge,
                    Endpoint::Boundary(None),
                    self.cut_edges.clone(),
                ));
            }
        }
        for node in self.graph.nodes() {
            for edge in node.outputs().filter(|edge| self.cut_edges[&edge.key()]) {
                nodes.insert(Node::store(edge, self.cut_edges.clone()));
            }

            nodes.insert(Node::new(node.clone(), self.cut_edges.clone()));

            if let Node::Operation(_) = &node {
                for edge in node.inputs().filter(|edge| self.cut_edges[&edge.key()]) {
                    nodes.insert(Node::reuse(
                        edge,
                        Endpoint::Node(node.clone()),
                        self.cut_edges.clone(),
                    ));
                }
            }
        }
        for edge in self.graph.graph_inputs() {
            if self.cut_edges[&edge.key()] {
                nodes.insert(Node::store(edge, self.cut_edges.clone()));
            }
        }
        Box::new(nodes.into_iter())
    }

    fn graph_backlink(&self) -> Option<Thunk<Self::Ctx>> {
        None
    }

    fn number_of_free_graph_inputs(&self) -> usize {
        self.graph.number_of_free_graph_inputs()
    }

    fn number_of_bound_graph_inputs(&self) -> usize {
        0
    }

    fn number_of_free_graph_outputs(&self) -> usize {
        0
    }

    fn number_of_bound_graph_outputs(&self) -> usize {
        self.graph.number_of_bound_graph_outputs()
    }
}

impl<G: Graph> EdgeLike for CutEdge<G> {
    type Ctx = CutGraph<G>;

    fn source(&self) -> Endpoint<Self::Ctx> {
        match self {
            Self::Inner { edge, cut_edges } => CutEndpoint::new(edge.source(), cut_edges.clone()),
            Self::Reuse {
                edge,
                target,
                cut_edges,
            } => CutEndpoint::Node(Node::reuse(edge.clone(), target.clone(), cut_edges.clone())),
        }
    }

    fn targets(&self) -> Box<dyn DoubleEndedIterator<Item = Endpoint<Self::Ctx>> + '_> {
        match self {
            Self::Inner { edge, cut_edges } => {
                if cut_edges[&edge.key()] {
                    Box::new(std::iter::once(Endpoint::Node(Node::store(
                        edge.clone(),
                        cut_edges.clone(),
                    ))))
                } else {
                    Box::new(
                        edge.targets()
                            .map(|target| CutEndpoint::new(target, cut_edges.clone())),
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
                Box::new((0..copies).map(|_| CutEndpoint::new(target.clone(), cut_edges.clone())))
            }
        }
    }
}

impl<G: Graph> NodeLike for CutOperation<G> {
    type Ctx = CutGraph<G>;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        match self {
            Self::Inner { op, cut_edges } => Box::new(op.inputs().map(|edge| {
                if cut_edges[&edge.key()] {
                    CutEdge::Reuse {
                        edge,
                        target: Endpoint::Node(Node::Operation(op.clone())),
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
            Self::Store { edge, cut_edges } => match edge.source() {
                Endpoint::Node(node) => node.backlink().map(|thunk| CutThunk {
                    thunk,
                    cut_edges: cut_edges.clone(),
                }),
                Endpoint::Boundary(graph) => graph.map(|thunk| CutThunk {
                    thunk,
                    cut_edges: cut_edges.clone(),
                }),
            },
            Self::Reuse {
                target, cut_edges, ..
            } => match target {
                Endpoint::Node(node) => node.backlink().map(|thunk| CutThunk {
                    thunk,
                    cut_edges: cut_edges.clone(),
                }),
                Endpoint::Boundary(graph) => graph.as_ref().map(|thunk| CutThunk {
                    thunk: thunk.clone(),
                    cut_edges: cut_edges.clone(),
                }),
            },
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
                .filter(|edge| !self.cut_edges[&edge.key()])
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

    fn free_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(
            self.thunk
                .free_graph_outputs()
                .filter(|edge| !self.cut_edges[&edge.key()])
                .map(|edge| CutEdge::Inner {
                    edge,
                    cut_edges: self.cut_edges.clone(),
                }),
        )
    }

    fn bound_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.thunk.bound_graph_outputs().map(|edge| {
            if self.cut_edges[&edge.key()] {
                CutEdge::Reuse {
                    edge,
                    target: Endpoint::Boundary(Some(self.thunk.clone())),
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
            if self.cut_edges[&edge.key()] {
                nodes.insert(Node::reuse(
                    edge,
                    Endpoint::Boundary(Some(self.thunk.clone())),
                    self.cut_edges.clone(),
                ));
            }
        }
        for node in self.thunk.nodes() {
            for edge in node.outputs().filter(|edge| self.cut_edges[&edge.key()]) {
                nodes.insert(Node::store(edge, self.cut_edges.clone()));
            }

            nodes.insert(Node::new(node.clone(), self.cut_edges.clone()));

            if let Node::Operation(_) = &node {
                for edge in node.inputs().filter(|edge| self.cut_edges[&edge.key()]) {
                    nodes.insert(Node::reuse(
                        edge,
                        Endpoint::Node(node.clone()),
                        self.cut_edges.clone(),
                    ));
                }
            }
        }
        for edge in self.thunk.bound_graph_inputs() {
            if self.cut_edges[&edge.key()] {
                nodes.insert(Node::store(edge, self.cut_edges.clone()));
            }
        }
        Box::new(nodes.into_iter())
    }

    fn graph_backlink(&self) -> Option<Thunk<Self::Ctx>> {
        Some(self.clone())
    }

    fn number_of_free_graph_inputs(&self) -> usize {
        self.free_graph_inputs().count() // can't do any better
    }

    fn number_of_bound_graph_inputs(&self) -> usize {
        self.thunk.number_of_bound_graph_inputs()
    }

    fn number_of_free_graph_outputs(&self) -> usize {
        self.free_graph_outputs().count() // can't do any better
    }

    fn number_of_bound_graph_outputs(&self) -> usize {
        self.thunk.number_of_bound_graph_outputs()
    }
}

impl<G: Graph> NodeLike for CutThunk<G> {
    type Ctx = CutGraph<G>;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        let free_inputs: IndexSet<_> = self.thunk.free_graph_inputs().collect();
        Box::new(
            self.thunk
                .inputs()
                .filter(move |edge| !free_inputs.contains(edge) || !self.cut_edges[&edge.key()])
                .map(|edge| CutEdge::Inner {
                    edge,
                    cut_edges: self.cut_edges.clone(),
                }),
        )
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        let free_outputs: IndexSet<_> = self.thunk.free_graph_outputs().collect();
        Box::new(
            self.thunk
                .outputs()
                .filter(move |edge| !free_outputs.contains(edge) || !self.cut_edges[&edge.key()])
                .map(|edge| CutEdge::Inner {
                    edge,
                    cut_edges: self.cut_edges.clone(),
                }),
        )
    }

    fn backlink(&self) -> Option<Thunk<Self::Ctx>> {
        Some(CutThunk {
            thunk: self.thunk.backlink()?,
            cut_edges: self.cut_edges.clone(),
        })
    }

    fn number_of_inputs(&self) -> usize {
        self.inputs().count() // can't do any better
    }

    fn number_of_outputs(&self) -> usize {
        self.outputs().count() // can't do any better
    }
}

impl<G: Graph> Keyable for CutGraph<G> {
    type Key = (Key<G>, ByThinAddress<Arc<EdgeMap<G::Ctx, bool>>>);

    fn key(&self) -> Self::Key {
        (self.graph.key(), self.cut_edges.clone())
    }
}

impl<G: Graph> Keyable for CutEdge<G> {
    type Key = Key<Edge<G::Ctx>>;

    fn key(&self) -> Self::Key {
        self.inner().key()
    }
}

impl<G: Graph> Keyable for CutOperation<G> {
    type Key = Either<Key<Operation<G::Ctx>>, Key<Edge<G::Ctx>>>;

    fn key(&self) -> Self::Key {
        self.inner().map_either(Keyable::key, Keyable::key)
    }
}

impl<G: Graph> Keyable for CutThunk<G> {
    type Key = Key<Thunk<G::Ctx>>;

    fn key(&self) -> Self::Key {
        self.inner().key()
    }
}

impl<G: Graph> WithWeight for CutEdge<G> {
    type Weight = Weight<Edge<G::Ctx>>;

    fn weight(&self) -> Self::Weight {
        self.inner().weight()
    }
}

impl<G: Graph> WithWeight for CutOperation<G> {
    type Weight = Either<Weight<Operation<G::Ctx>>, Weight<Edge<G::Ctx>>>;

    fn weight(&self) -> Self::Weight {
        self.inner()
            .map_either(WithWeight::weight, WithWeight::weight)
    }
}

impl<G: Graph> WithWeight for CutThunk<G> {
    type Weight = Weight<Thunk<G::Ctx>>;

    fn weight(&self) -> Self::Weight {
        self.inner().weight()
    }
}

impl<G: Graph + Codeable> Codeable for CutGraph<G> {
    type Code = Code<G>;

    fn code(&self) -> Self::Code {
        self.inner().code()
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
