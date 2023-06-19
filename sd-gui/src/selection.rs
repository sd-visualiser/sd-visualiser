#![allow(clippy::inline_always)]

use std::{collections::HashSet, fmt::Display};

use delegate::delegate;
use eframe::egui;
use sd_core::{
    decompile::decompile,
    graph::{Name, Op, SyntaxHyperGraph},
    hypergraph::{subgraph::Free, Operation},
    language::{chil::Chil, spartan::Spartan, Expr, Language},
    prettyprinter::PrettyPrint,
};

use crate::{
    code_ui::code_ui,
    graph_ui::{GraphUi, GraphUiInternal},
    parser::UiLanguage,
};

pub(crate) enum Selection {
    Chil(SelectionInternal<Chil>),
    Spartan(SelectionInternal<Spartan>),
}

impl Selection {
    delegate! {
        to match self {
            Self::Chil(selection) => selection,
            Self::Spartan(selection) => selection,
        } {
            pub(crate) fn ui(&mut self, ctx: &egui::Context);
            pub(crate) fn name(&self) -> &str;
            pub(crate) fn displayed(&mut self) -> &mut bool;
        }
    }

    pub fn from_graph(graph_ui: &GraphUi, name: String, ctx: &egui::Context) -> Self {
        match graph_ui {
            GraphUi::Chil(graph_ui, selection) => Self::Chil(SelectionInternal::new(
                selection,
                name,
                &graph_ui.hypergraph,
                ctx,
            )),
            GraphUi::Spartan(graph_ui, selection) => Self::Spartan(SelectionInternal::new(
                selection,
                name,
                &graph_ui.hypergraph,
                ctx,
            )),
        }
    }
}

pub(crate) struct SelectionInternal<T: Language> {
    name: String,
    displayed: bool,
    code: String,
    graph_ui: GraphUiInternal<T>,
}

impl<T: 'static + Language> SelectionInternal<T> {
    pub(crate) fn new(
        selected_nodes: &HashSet<Operation<Op<T>, Name<T>>>,
        name: String,
        containing_graph: &SyntaxHyperGraph<T>,
        ctx: &egui::Context,
    ) -> Self
    where
        T::Op: Display,
        T::Var: Free,
        Expr<T>: PrettyPrint,
    {
        let normalised = containing_graph.normalise_selection(selected_nodes);
        let hypergraph = containing_graph.generate_subgraph(&normalised);

        let code = decompile(&hypergraph)
            .map_or_else(|err| format!("Error: {err:?}"), |expr| expr.to_pretty());

        let graph_ui = GraphUiInternal::from_graph(hypergraph, ctx);

        Self {
            code,
            name,
            displayed: true,
            graph_ui,
        }
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn displayed(&mut self) -> &mut bool {
        &mut self.displayed
    }

    pub(crate) fn ui(&mut self, ctx: &egui::Context)
    where
        T::Op: Display,
        T::Op: PrettyPrint,
        T::Var: PrettyPrint,
        T::Addr: Display,
        T::VarDef: PrettyPrint,
        Expr<T>: PrettyPrint,
    {
        egui::Window::new(self.name.clone())
            .open(&mut self.displayed)
            .show(ctx, |ui| {
                ui.columns(2, |columns| {
                    code_ui(
                        &mut columns[0],
                        &mut self.code.as_str(),
                        UiLanguage::Spartan,
                    );
                    self.graph_ui.ui(&mut columns[1], None);
                });
            });
    }
}