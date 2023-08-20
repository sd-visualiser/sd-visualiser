#![allow(clippy::inline_always)]

use std::fmt::Display;

use delegate::delegate;
use eframe::egui;
use indexmap::IndexMap;
use sd_core::{
    decompile::{decompile, Fresh},
    graph::{Name, Op, SyntaxHypergraph},
    hypergraph::{
        subgraph::{SubThunk, Subgraph},
        Thunk,
    },
    language::{chil::Chil, spartan::Spartan, Expr, Language},
    prettyprinter::PrettyPrint,
    selection::SelectionMap,
    weak_map::WeakMap,
};

use crate::{
    code_ui::code_ui,
    graph_ui::{GraphUi, GraphUiInternal},
    parser::UiLanguage,
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
            GraphUi::Chil(graph_ui, selection) => {
                Self::Chil(SelectionInternal::new(selection, &graph_ui.expanded, name))
            }
            GraphUi::Spartan(graph_ui, selection) => {
                Self::Spartan(SelectionInternal::new(selection, &graph_ui.expanded, name))
            }
        }
    }
}

pub struct SelectionInternal<T: Language> {
    name: String,
    displayed: bool,
    code: String,
    graph_ui: GraphUiInternal<Subgraph<SyntaxHypergraph<T>>>,
}

impl<T: 'static + Language> SelectionInternal<T> {
    pub(crate) fn new(
        selection: &SelectionMap<SyntaxHypergraph<T>>,
        expanded: &WeakMap<Thunk<Op<T>, Name<T>>, bool>,
        name: String,
    ) -> Self
    where
        T::Op: Display,
        T::Var: Fresh,
        Expr<T>: PrettyPrint,
    {
        let subgraph = Subgraph::new(selection.clone());
        let expanded = WeakMap::from(
            expanded
                .iter()
                .map(|(thunk, &expanded)| {
                    (
                        SubThunk {
                            inner: thunk.clone(),
                            selection: subgraph.selection.clone(),
                        },
                        expanded,
                    )
                })
                .collect::<IndexMap<_, _>>(),
        );

        let graph_ui = GraphUiInternal::new(subgraph, expanded);

        let code = decompile(&graph_ui.graph)
            .map_or_else(|err| format!("Error: {err:?}"), |expr| expr.to_pretty());

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
        T::Op: Display + PrettyPrint,
        T::Var: PrettyPrint + Fresh,
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
