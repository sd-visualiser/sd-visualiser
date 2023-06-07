use std::collections::HashSet;

use eframe::egui;
use sd_core::{
    graph::{Name, SyntaxHyperGraph},
    hypergraph::Operation,
    language::spartan::Op,
};

use crate::graph_ui::GraphUi;

pub(crate) struct Selection {
    pub(crate) name: String,
    pub(crate) displayed: bool,
    graph_ui: GraphUi,
}

impl Selection {
    pub(crate) fn new(
        selected_nodes: &HashSet<Operation<Op, Name>>,
        name: String,
        containing_graph: &SyntaxHyperGraph,
    ) -> Self {
        let mut graph_ui = GraphUi::default();

        let normalised = containing_graph.normalise_selection(selected_nodes);

        let hypergraph = containing_graph.generate_subgraph(&normalised);

        graph_ui.compile(hypergraph);

        Selection {
            name,
            displayed: true,
            graph_ui,
        }
    }

    pub(crate) fn ui(&mut self, ctx: &egui::Context) {
        egui::Window::new(self.name.clone())
            .open(&mut self.displayed)
            .show(ctx, |ui| {
                self.graph_ui.ui(ui);
            });
    }
}
