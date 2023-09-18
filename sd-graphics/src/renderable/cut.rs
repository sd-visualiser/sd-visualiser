use itertools::Either;
use sd_core::hypergraph::adapter::cut::{CutEdge, CutGraph, CutNode, CutOperation, CutThunk};

use super::RenderableGraph;

impl<G: RenderableGraph> RenderableGraph for CutGraph<G> {
    // Inner nodes: delegate to inner graph.
    // Store and reuse nodes: always return false.
    fn selected(&self, node: CutNode<G>) -> bool {
        node.into_inner()
            .either(|node| self.inner().selected(node), |_edge| false)
    }

    // Toggle the edge.
    fn clicked_edge(&mut self, edge: CutEdge<G>) {
        self.toggle(edge.inner());
    }

    // Inner operations: delegate to inner graph.
    // Store and reuse operations: toggle the edge.
    fn clicked_operation(&mut self, op: CutOperation<G>, primary: bool) {
        match op.into_inner() {
            Either::Left(op) => {
                self.inner_mut().clicked_operation(op, primary);
            }
            Either::Right(edge) => {
                self.toggle(&edge);
            }
        }
    }

    // Delegate to inner graph.
    fn clicked_thunk(&mut self, thunk: CutThunk<G>, primary: bool) {
        self.inner_mut().clicked_thunk(thunk.into_inner(), primary);
    }

    // Filter out store and reuse edges, then delegate to inner graph.
    fn extend(&mut self, nodes: impl Iterator<Item = CutNode<G>>) {
        self.inner_mut()
            .extend(nodes.filter_map(|node| node.into_inner().left()));
    }
}
