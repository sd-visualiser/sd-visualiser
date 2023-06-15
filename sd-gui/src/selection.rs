use std::collections::HashSet;

use eframe::egui;
use sd_core::{
    decompile::decompile,
    graph::{Name, Op, SyntaxHyperGraph},
    hypergraph::Operation,
    language::spartan::Spartan,
    prettyprinter::PrettyPrint,
};

use crate::{code_ui::code_ui, graph_ui::GraphUi, parser::UiLanguage};

pub(crate) struct Selection {
    pub(crate) name: String,
    pub(crate) displayed: bool,
    code: String,
    graph_ui: GraphUi,
}

impl Selection {
    pub(crate) fn new(
        selected_nodes: &HashSet<Operation<Op, Name>>,
        name: String,
        containing_graph: &SyntaxHyperGraph,
        ctx: &egui::Context,
    ) -> Self {
        let mut graph_ui = GraphUi::default();

        let normalised = containing_graph.normalise_selection(selected_nodes);
        let hypergraph = containing_graph.generate_subgraph(&normalised);

        let code = decompile::<Spartan>(&hypergraph)
            .map_or_else(|err| format!("Error: {err:?}"), |expr| expr.to_pretty());

        graph_ui.compile(hypergraph, ctx);

        Selection {
            code,
            name,
            displayed: true,
            graph_ui,
        }
    }

    pub(crate) fn ui(&mut self, ctx: &egui::Context) {
        egui::Window::new(self.name.clone())
            .open(&mut self.displayed)
            .show(ctx, |ui| {
                ui.columns(2, |columns| {
                    code_ui(
                        &mut columns[0],
                        &mut self.code.as_str(),
                        UiLanguage::Spartan,
                    );
                    self.graph_ui.ui(&mut columns[1]);
                });
            });
    }
}
