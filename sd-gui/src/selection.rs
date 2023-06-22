#![allow(clippy::inline_always)]

use std::{fmt::Display, sync::Arc};

use delegate::delegate;
use eframe::{egui, epaint::mutex::Mutex};
use sd_core::{
    decompile::decompile,
    graph::{Name, Op},
    hypergraph::{subgraph::Free, Thunk},
    language::{chil::Chil, spartan::Spartan, Expr, Language},
    prettyprinter::PrettyPrint,
    selection::SelectionMap,
    weak_map::WeakMap,
};

use crate::{
    code_ui::code_ui,
    graph_ui::{GraphUi, GraphUiInternal},
    parser::UiLanguage,
    subgraph_generator::generate_subgraph,
};

pub enum Selection {
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

    pub fn from_graph(graph_ui: &GraphUi, name: String) -> Self {
        match graph_ui {
            GraphUi::Chil(graph_ui, selection) => Self::Chil(SelectionInternal::new(
                selection.clone(),
                graph_ui.get_expanded().clone(),
                name,
            )),
            GraphUi::Spartan(graph_ui, selection) => Self::Spartan(SelectionInternal::new(
                selection.clone(),
                graph_ui.get_expanded().clone(),
                name,
            )),
        }
    }
}

pub struct SelectionInternal<T: Language> {
    name: String,
    displayed: bool,
    code: String,
    selection: SelectionMap<(Op<T>, Name<T>)>,
    graph_ui: Arc<Mutex<GraphUiInternal<T>>>,
}

impl<T: 'static + Language> SelectionInternal<T> {
    pub(crate) fn new(
        selection: SelectionMap<(Op<T>, Name<T>)>,
        expanded: WeakMap<Thunk<Op<T>, Name<T>>, bool>,
        name: String,
    ) -> Self
    where
        T::Op: Display,
        T::Var: Free,
        Expr<T>: PrettyPrint,
    {
        let graph_ui = generate_subgraph(&selection, expanded);

        let code = decompile(graph_ui.lock().get_hypergraph())
            .map_or_else(|err| format!("Error: {err:?}"), |expr| expr.to_pretty());

        Self {
            code,
            name,
            selection,
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
        T::Op: Display + PrettyPrint,
        T::Var: PrettyPrint + Free,
        T::Addr: Display,
        T::VarDef: PrettyPrint,
        Expr<T>: PrettyPrint,
    {
        let new_graph_ui =
            generate_subgraph(&self.selection, self.graph_ui.lock().get_expanded().clone());

        if Arc::as_ptr(&self.graph_ui) != Arc::as_ptr(&new_graph_ui) {
            {
                let mut new = new_graph_ui.lock();
                let old = self.graph_ui.lock();

                new.update_from_other_ui(&old);
            }
            self.graph_ui = new_graph_ui;
        }

        egui::Window::new(self.name.clone())
            .open(&mut self.displayed)
            .show(ctx, |ui| {
                ui.columns(2, |columns| {
                    code_ui(
                        &mut columns[0],
                        &mut self.code.as_str(),
                        UiLanguage::Spartan,
                    );
                    self.graph_ui
                        .lock()
                        .ui(&mut columns[1], None, Some(&mut self.selection));
                });
            });
    }
}
