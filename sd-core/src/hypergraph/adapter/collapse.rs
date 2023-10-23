use std::sync::Arc;

use by_address::ByThinAddress;
use derivative::Derivative;
use itertools::Either;

use crate::{
    codeable::{Code, Codeable},
    common::Matchable,
    hypergraph::{
        generic::{Ctx, Edge, Endpoint, Key, Node, Operation, Thunk, Weight},
        mapping::ThunkMap,
        subgraph::ExtensibleEdge,
        traits::{EdgeLike, Graph, Keyable, NodeLike, WithWeight},
    },
};

////////////////////////////////////////////////////////////////

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub struct CollapseGraph<G: Graph> {
    graph: G,
    expanded: ByThinAddress<Arc<ThunkMap<G::Ctx, bool>>>,
}

impl<G: Graph> CollapseGraph<G> {
    pub fn new(graph: G, expanded: ThunkMap<G::Ctx, bool>) -> Self {
        Self {
            graph,
            expanded: ByThinAddress(Arc::new(expanded)),
        }
    }

    pub fn inner(&self) -> &G {
        &self.graph
    }

    pub fn inner_mut(&mut self) -> &mut G {
        &mut self.graph
    }

    pub fn expanded(&self) -> &ThunkMap<G::Ctx, bool> {
        &self.expanded
    }

    pub fn toggle(&mut self, thunk: &Thunk<G::Ctx>) {
        let mut expanded = self.expanded().clone();
        expanded[&thunk.key()] ^= true;
        self.expanded = ByThinAddress(Arc::new(expanded));
    }

    pub fn set_all(&mut self, value: bool) {
        let mut expanded = self.expanded().clone();
        expanded.values_mut().for_each(|x| *x = value);
        self.expanded = ByThinAddress(Arc::new(expanded));
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
pub struct CollapseEdge<G: Graph> {
    edge: Edge<G::Ctx>,
    #[derivative(Debug = "ignore")]
    expanded: ByThinAddress<Arc<ThunkMap<G::Ctx, bool>>>,
}

impl<G: Graph> CollapseEdge<G> {
    pub fn inner(&self) -> &Edge<G::Ctx> {
        &self.edge
    }

    pub fn into_inner(self) -> Edge<G::Ctx> {
        self.edge
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
pub struct CollapseOperation<G: Graph> {
    node: Node<G::Ctx>,
    #[derivative(Debug = "ignore")]
    expanded: ByThinAddress<Arc<ThunkMap<G::Ctx, bool>>>,
}

impl<G: Graph> CollapseOperation<G> {
    pub fn inner(&self) -> &Node<G::Ctx> {
        &self.node
    }

    pub fn into_inner(self) -> Node<G::Ctx> {
        self.node
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
pub struct CollapseThunk<G: Graph> {
    thunk: Thunk<G::Ctx>,
    #[derivative(Debug = "ignore")]
    expanded: ByThinAddress<Arc<ThunkMap<G::Ctx, bool>>>,
}

impl<G: Graph> CollapseThunk<G> {
    pub fn inner(&self) -> &Thunk<G::Ctx> {
        &self.thunk
    }

    pub fn into_inner(self) -> Thunk<G::Ctx> {
        self.thunk
    }
}

////////////////////////////////////////////////////////////////

/// Finds the topmost ancestor of given thunk that is not expanded.
fn find_ancestor<T: Ctx>(thunk: T::Thunk, expanded: &ThunkMap<T, bool>) -> Option<T::Thunk> {
    let x = thunk
        .backlink()
        .and_then(|b| find_ancestor::<T>(b, expanded));
    x.or_else(|| (!expanded[&thunk.key()]).then_some(thunk))
}

pub type CollapseNode<G> = Node<CollapseGraph<G>>;

impl<G: Graph> CollapseNode<G> {
    fn new(node: Node<G::Ctx>, expanded: ByThinAddress<Arc<ThunkMap<G::Ctx, bool>>>) -> Self {
        match &node {
            Node::Operation(op) => Node::Operation(CollapseOperation {
                node: op
                    .backlink()
                    .and_then(|b| find_ancestor::<G::Ctx>(b, &expanded))
                    .map_or(node, Node::Thunk),
                expanded,
            }),
            Node::Thunk(thunk) => find_ancestor::<G::Ctx>(thunk.clone(), &expanded).map_or_else(
                || {
                    Node::Thunk(CollapseThunk {
                        thunk: thunk.clone(),
                        expanded: expanded.clone(),
                    })
                },
                |thunk| {
                    Node::Operation(CollapseOperation {
                        node: Node::Thunk(thunk),
                        expanded: expanded.clone(),
                    })
                },
            ),
        }
    }

    pub fn into_inner(self) -> Node<G::Ctx> {
        match self {
            Node::Operation(op) => op.into_inner(),
            Node::Thunk(thunk) => Node::Thunk(thunk.into_inner()),
        }
    }
}

////////////////////////////////////////////////////////////////

pub type CollapseEndpoint<G> = Endpoint<CollapseGraph<G>>;

impl<G: Graph> CollapseEndpoint<G> {
    pub fn new(
        endpoint: Endpoint<G::Ctx>,
        expanded: ByThinAddress<Arc<ThunkMap<G::Ctx, bool>>>,
    ) -> Self {
        match endpoint {
            Endpoint::Node(node) => Endpoint::Node(Node::new(node, expanded)),
            Endpoint::Boundary(graph) => match graph {
                Some(thunk) => find_ancestor::<G::Ctx>(thunk.clone(), &expanded).map_or_else(
                    || {
                        Endpoint::Boundary(Some(CollapseThunk {
                            thunk,
                            expanded: expanded.clone(),
                        }))
                    },
                    |thunk| {
                        Endpoint::Node(Node::Operation(CollapseOperation {
                            node: Node::Thunk(thunk),
                            expanded: expanded.clone(),
                        }))
                    },
                ),
                None => Endpoint::Boundary(None),
            },
        }
    }
}

////////////////////////////////////////////////////////////////

impl<G: Graph> Ctx for CollapseGraph<G> {
    type Edge = CollapseEdge<G>;
    type Operation = CollapseOperation<G>;
    type Thunk = CollapseThunk<G>;
}

impl<G: Graph> Graph for CollapseGraph<G> {
    type Ctx = CollapseGraph<G>;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.graph.free_graph_inputs().map(|edge| CollapseEdge {
            edge,
            expanded: self.expanded.clone(),
        }))
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(std::iter::empty())
    }

    fn free_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(std::iter::empty())
    }

    fn bound_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.graph.bound_graph_outputs().map(|edge| CollapseEdge {
            edge,
            expanded: self.expanded.clone(),
        }))
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<Self::Ctx>> + '_> {
        Box::new(
            self.graph
                .nodes()
                .map(|node| Node::new(node, self.expanded.clone())),
        )
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

impl<G: Graph> EdgeLike for CollapseEdge<G> {
    type Ctx = CollapseGraph<G>;

    fn source(&self) -> Endpoint<Self::Ctx> {
        CollapseEndpoint::new(self.edge.source(), self.expanded.clone())
    }

    fn targets(&self) -> Box<dyn DoubleEndedIterator<Item = Endpoint<Self::Ctx>> + '_> {
        Box::new(
            self.edge
                .targets()
                .map(|endpoint| CollapseEndpoint::new(endpoint, self.expanded.clone())),
        )
    }
}

impl<G: Graph> NodeLike for CollapseOperation<G> {
    type Ctx = CollapseGraph<G>;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.node.inputs().map(|edge| CollapseEdge {
            edge,
            expanded: self.expanded.clone(),
        }))
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.node.outputs().map(|edge| CollapseEdge {
            edge,
            expanded: self.expanded.clone(),
        }))
    }

    fn backlink(&self) -> Option<Thunk<Self::Ctx>> {
        self.node.backlink().map(|thunk| CollapseThunk {
            thunk,
            expanded: self.expanded.clone(),
        })
    }

    fn number_of_inputs(&self) -> usize {
        self.node.number_of_inputs()
    }

    fn number_of_outputs(&self) -> usize {
        self.node.number_of_outputs()
    }
}

impl<G: Graph> Graph for CollapseThunk<G> {
    type Ctx = CollapseGraph<G>;

    fn free_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.thunk.free_graph_inputs().map(|edge| CollapseEdge {
            edge,
            expanded: self.expanded.clone(),
        }))
    }

    fn bound_graph_inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.thunk.bound_graph_inputs().map(|edge| CollapseEdge {
            edge,
            expanded: self.expanded.clone(),
        }))
    }

    fn free_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.thunk.free_graph_outputs().map(|edge| CollapseEdge {
            edge,
            expanded: self.expanded.clone(),
        }))
    }

    fn bound_graph_outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.thunk.bound_graph_outputs().map(|edge| CollapseEdge {
            edge,
            expanded: self.expanded.clone(),
        }))
    }

    fn nodes(&self) -> Box<dyn DoubleEndedIterator<Item = Node<Self::Ctx>> + '_> {
        Box::new(
            self.thunk
                .nodes()
                .map(|node| Node::new(node, self.expanded.clone())),
        )
    }

    fn graph_backlink(&self) -> Option<Thunk<Self::Ctx>> {
        Some(self.clone())
    }

    fn number_of_free_graph_inputs(&self) -> usize {
        self.thunk.number_of_free_graph_inputs()
    }

    fn number_of_bound_graph_inputs(&self) -> usize {
        self.thunk.number_of_bound_graph_inputs()
    }

    fn number_of_free_graph_outputs(&self) -> usize {
        self.thunk.number_of_free_graph_outputs()
    }

    fn number_of_bound_graph_outputs(&self) -> usize {
        self.thunk.number_of_bound_graph_outputs()
    }
}

impl<G: Graph> NodeLike for CollapseThunk<G> {
    type Ctx = CollapseGraph<G>;

    fn inputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.thunk.inputs().map(|edge| CollapseEdge {
            edge,
            expanded: self.expanded.clone(),
        }))
    }

    fn outputs(&self) -> Box<dyn DoubleEndedIterator<Item = Edge<Self::Ctx>> + '_> {
        Box::new(self.thunk.outputs().map(|edge| CollapseEdge {
            edge,
            expanded: self.expanded.clone(),
        }))
    }

    fn backlink(&self) -> Option<Thunk<Self::Ctx>> {
        self.thunk.backlink().map(|thunk| CollapseThunk {
            thunk,
            expanded: self.expanded.clone(),
        })
    }

    fn number_of_inputs(&self) -> usize {
        self.thunk.number_of_inputs()
    }

    fn number_of_outputs(&self) -> usize {
        self.thunk.number_of_outputs()
    }
}

impl<G: Graph + Codeable> Codeable for CollapseGraph<G> {
    type Code = Code<G>;

    fn code(&self) -> Self::Code {
        self.graph.code()
    }
}

impl<G: Graph> Codeable for CollapseEdge<G>
where
    Edge<G::Ctx>: Codeable,
{
    type Code = Code<Edge<G::Ctx>>;

    fn code(&self) -> Self::Code {
        self.edge.code()
    }
}

impl<G: Graph> Codeable for CollapseOperation<G>
where
    Operation<G::Ctx>: Codeable,
    Thunk<G::Ctx>: Codeable,
{
    type Code = Either<Code<Operation<G::Ctx>>, Code<Thunk<G::Ctx>>>;

    fn code(&self) -> Self::Code {
        match &self.node {
            Node::Operation(op) => Either::Left(op.code()),
            Node::Thunk(thunk) => Either::Right(thunk.code()),
        }
    }
}

impl<G: Graph> Codeable for CollapseThunk<G>
where
    Thunk<G::Ctx>: Codeable,
{
    type Code = Code<Thunk<G::Ctx>>;

    fn code(&self) -> Self::Code {
        self.thunk.code()
    }
}

impl<G: Graph> Matchable for CollapseEdge<G>
where
    Edge<G::Ctx>: Matchable,
{
    fn is_match(&self, query: &str) -> bool {
        self.edge.is_match(query)
    }
}

impl<G: Graph> Matchable for CollapseOperation<G>
where
    Operation<G::Ctx>: Matchable,
    Thunk<G::Ctx>: Matchable,
{
    fn is_match(&self, query: &str) -> bool {
        match &self.node {
            Node::Operation(op) => op.is_match(query),
            Node::Thunk(thunk) => thunk.is_match(query),
        }
    }
}

impl<G: Graph> Matchable for CollapseThunk<G>
where
    Thunk<G::Ctx>: Matchable,
{
    fn is_match(&self, query: &str) -> bool {
        self.thunk.is_match(query)
    }
}

impl<G: Graph> Keyable for CollapseGraph<G> {
    type Key = (Key<G>, ByThinAddress<Arc<ThunkMap<G::Ctx, bool>>>);

    fn key(&self) -> Self::Key {
        (self.graph.key(), self.expanded.clone())
    }
}

impl<G: Graph> Keyable for CollapseEdge<G> {
    type Key = Key<Edge<G::Ctx>>;

    fn key(&self) -> Self::Key {
        self.edge.key()
    }
}

impl<G: Graph> Keyable for CollapseOperation<G> {
    type Key = Either<Key<Operation<G::Ctx>>, Key<Thunk<G::Ctx>>>;

    fn key(&self) -> Self::Key {
        match &self.node {
            Node::Operation(op) => Either::Left(op.key()),
            Node::Thunk(thunk) => Either::Right(thunk.key()),
        }
    }
}

impl<G: Graph> Keyable for CollapseThunk<G> {
    type Key = Key<Thunk<G::Ctx>>;

    fn key(&self) -> Self::Key {
        self.thunk.key()
    }
}

impl<G: Graph> WithWeight for CollapseEdge<G> {
    type Weight = Weight<Edge<G::Ctx>>;

    fn weight(&self) -> Self::Weight {
        self.edge.weight()
    }
}

impl<G: Graph> WithWeight for CollapseOperation<G> {
    type Weight = Either<Weight<Operation<G::Ctx>>, Weight<Thunk<G::Ctx>>>;

    fn weight(&self) -> Self::Weight {
        match &self.node {
            Node::Operation(op) => Either::Left(op.weight()),
            Node::Thunk(thunk) => Either::Right(thunk.weight()),
        }
    }
}

impl<G: Graph> WithWeight for CollapseThunk<G> {
    type Weight = Weight<Thunk<G::Ctx>>;

    fn weight(&self) -> Self::Weight {
        self.thunk.weight()
    }
}

impl<G: Graph> ExtensibleEdge for CollapseEdge<G>
where
    Edge<G::Ctx>: ExtensibleEdge,
{
    fn extend_source(&self) -> Option<Node<Self::Ctx>> {
        self.inner()
            .extend_source()
            .map(|node| Node::new(node, self.expanded.clone()))
    }

    fn extend_targets(&self) -> Box<dyn DoubleEndedIterator<Item = Node<Self::Ctx>> + '_> {
        Box::new(
            self.inner()
                .extend_targets()
                .map(|node| Node::new(node, self.expanded.clone())),
        )
    }
}
