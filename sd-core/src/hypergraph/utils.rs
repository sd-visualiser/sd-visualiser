use std::collections::HashSet;

use super::{
    generic::{Ctx, Endpoint, Node},
    traits::{EdgeLike, Graph, NodeLike},
};

pub enum Ancestor<T: Ctx> {
    OriginalNode,
    Contained(T::Thunk),
    NoAncestor,
}

impl<T: Ctx> Ancestor<T> {
    pub fn no_ancestor(&self) -> bool {
        matches!(self, Ancestor::NoAncestor)
    }

    pub fn to_node(self, original: Node<T>) -> Option<Node<T>> {
        match self {
            Ancestor::OriginalNode => Some(original),
            Ancestor::Contained(graph) => Some(Node::Thunk(graph)),
            Ancestor::NoAncestor => None,
        }
    }
}

/// Finds the ancestor of given node which is contained in containing, returning none if no such ancestor exists
pub fn find_ancestor<T: Ctx>(containing: Option<&T::Thunk>, node: &Node<T>) -> Ancestor<T> {
    let mut backlink = node.backlink();
    if backlink.as_ref() == containing {
        Ancestor::OriginalNode
    } else {
        let mut thunk;
        loop {
            if backlink.is_none() {
                return Ancestor::NoAncestor;
            }
            thunk = backlink.unwrap();
            backlink = thunk.backlink();
            if backlink.as_ref() == containing {
                break;
            }
        }
        Ancestor::Contained(thunk)
    }
}

pub fn normalised_targets<T: Ctx>(
    edge: &T::Edge,
    containing: Option<&T::Thunk>,
) -> Vec<Endpoint<T>> {
    let mut non_dupe_outputs = HashSet::new();
    let mut outputs = Vec::new();

    for x in edge.targets() {
        match x {
            Endpoint::Node(node) => match find_ancestor(containing, &node) {
                Ancestor::OriginalNode => {
                    outputs.push(Endpoint::Node(node));
                }
                Ancestor::Contained(graph) => {
                    non_dupe_outputs.insert(Endpoint::Node(Node::Thunk(graph)));
                }
                Ancestor::NoAncestor => {
                    non_dupe_outputs.insert(Endpoint::Boundary(containing.cloned()));
                }
            },
            Endpoint::Boundary(graph) if graph.as_ref() == containing => {
                outputs.push(Endpoint::Boundary(graph));
            }
            Endpoint::Boundary(Some(thunk)) => {
                let x = Node::Thunk(thunk.clone());
                match find_ancestor::<T>(containing, &x) {
                    Ancestor::OriginalNode => {
                        if thunk.free_graph_inputs().any(|e| &e == edge) {
                            outputs.push(Endpoint::Node(x));
                        }
                    }
                    Ancestor::Contained(graph) => {
                        if graph.free_graph_inputs().any(|e| &e == edge) {
                            non_dupe_outputs.insert(Endpoint::Node(Node::Thunk(graph)));
                        }
                    }
                    Ancestor::NoAncestor => {
                        non_dupe_outputs.insert(Endpoint::Boundary(containing.cloned()));
                    }
                }
            }
            Endpoint::Boundary(_) => {
                non_dupe_outputs.insert(Endpoint::Boundary(containing.cloned()));
            }
        }
    }

    outputs.extend(non_dupe_outputs);
    outputs
}
