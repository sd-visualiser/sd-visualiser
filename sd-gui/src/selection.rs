#![allow(clippy::inline_always)]

use delegate::delegate;
use eframe::egui;
#[cfg(not(target_arch = "wasm32"))]
use sd_core::language::llvm_ir::LlvmIr;
use sd_core::{
    graph::SyntaxHypergraph,
    interactive::InteractiveSubgraph,
    language::{Expr, Language, Thunk, chil::Chil, mlir::Mlir, sd_lang::SdLang},
    lp::Solver,
    prettyprinter::PrettyPrint,
};

use crate::{
    code_generator::generate_code,
    code_ui::code_ui,
    graph_ui::{GraphUi, GraphUiInternal},
    parser::UiLanguage,
};

pub enum Selection {
    Chil(SelectionInternal<Chil>),
    #[cfg(not(target_arch = "wasm32"))]
    LlvmIr(SelectionInternal<LlvmIr>),
    Mlir(SelectionInternal<Mlir>),
    SdLang(SelectionInternal<SdLang>),
}

impl Selection {
    delegate! {
        to match self {
            Self::Chil(selection) => selection,
            #[cfg(not(target_arch = "wasm32"))]
            Self::LlvmIr(selection) => selection,
            Self::Mlir(selection) => selection,
            Self::SdLang(selection) => selection,
        } {
            pub(crate) fn ui(&mut self, ctx: &egui::Context);
            pub(crate) fn name(&self) -> &str;
            pub(crate) fn displayed(&mut self) -> &mut bool;
        }
    }

    pub fn from_graph(graph_ui: &GraphUi, name: String, solver: Solver) -> Option<Self> {
        match graph_ui {
            GraphUi::Chil(graph_ui) => Some(Self::Chil(SelectionInternal::new(
                graph_ui.graph.to_subgraph(),
                name,
                solver,
            ))),
            #[cfg(not(target_arch = "wasm32"))]
            GraphUi::LlvmIr(graph_ui) => Some(Self::LlvmIr(SelectionInternal::new(
                graph_ui.graph.to_subgraph(),
                name,
                solver,
            ))),
            GraphUi::Mlir(graph_ui) => Some(Self::Mlir(SelectionInternal::new(
                graph_ui.graph.to_subgraph(),
                name,
                solver,
            ))),
            GraphUi::SdLang(graph_ui) => Some(Self::SdLang(SelectionInternal::new(
                graph_ui.graph.to_subgraph(),
                name,
                solver,
            ))),
            GraphUi::Dot(_) => None,
        }
    }
}

pub struct SelectionInternal<T: Language> {
    name: String,
    displayed: bool,
    graph_ui: GraphUiInternal<InteractiveSubgraph<SyntaxHypergraph<T>>>,
}

impl<T: 'static + Language> SelectionInternal<T> {
    pub(crate) fn new(
        subgraph: InteractiveSubgraph<SyntaxHypergraph<T>>,
        name: String,
        solver: Solver,
    ) -> Self {
        let graph_ui = GraphUiInternal::new(subgraph, solver);

        Self {
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
        Expr<T>: PrettyPrint,
        Thunk<T>: PrettyPrint,
    {
        egui::Window::new(self.name.clone())
            .open(&mut self.displayed)
            .show(ctx, |ui| {
                ui.columns(2, |columns| {
                    let code = generate_code(&self.graph_ui.graph);
                    let guard = code.lock().unwrap();
                    if let Some(code) = guard.ready() {
                        code_ui(&mut columns[0], &mut code.as_str(), UiLanguage::SdLang);
                    }
                    self.graph_ui.ui(&mut columns[1], None);
                });
            });
    }
}
