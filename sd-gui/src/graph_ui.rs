#![allow(clippy::inline_always)]

use std::{fmt::Display, sync::Arc};

use delegate::delegate;
use eframe::{
    egui,
    epaint::{Rounding, Shape},
};
use sd_core::{
    common::Direction,
    graph::{Name, Op, SyntaxHyperGraph, SyntaxSubgraph},
    hypergraph::{subgraph::Subgraph, Thunk},
    language::{chil::Chil, spartan::Spartan, Expr, Language},
    monoidal::MonoidalGraph,
    monoidal_wired::MonoidalWiredGraph,
    prettyprinter::PrettyPrint,
    selection::SelectionMap,
    weak_map::WeakMap,
};
use sd_graphics::common::GraphMetadata;
use tracing::debug;

use crate::{panzoom::Panzoom, shape_generator::generate_shapes};

pub enum GraphUi {
    Chil(GraphUiInternal<Chil>, SelectionMap<(Op<Chil>, Name<Chil>)>),
    Spartan(
        GraphUiInternal<Spartan>,
        SelectionMap<(Op<Spartan>, Name<Spartan>)>,
    ),
}

impl GraphUi {
    pub(crate) fn new_chil(hypergraph: SyntaxHyperGraph<Chil>) -> Self {
        let selected = hypergraph.create_selected();
        Self::Chil(GraphUiInternal::from_graph(hypergraph), selected)
    }

    pub(crate) fn new_spartan(hypergraph: SyntaxHyperGraph<Spartan>) -> Self {
        let selected = hypergraph.create_selected();
        Self::Spartan(GraphUiInternal::from_graph(hypergraph), selected)
    }

    delegate! {
        to match self {
            GraphUi::Chil(graph_ui, _) => graph_ui,
            GraphUi::Spartan(graph_ui, _) => graph_ui,
        } {
            pub(crate) const fn ready(&self) -> bool;
            pub(crate) fn reset(&mut self);
            pub(crate) fn zoom_in(&mut self);
            pub(crate) fn zoom_out(&mut self);
            pub(crate) fn export_svg(&self) -> String;
        }
    }

    pub(crate) fn ui(&mut self, ui: &mut egui::Ui) {
        match self {
            GraphUi::Chil(graph_ui, selection) => graph_ui.ui(ui, Some(selection)),
            GraphUi::Spartan(graph_ui, selection) => graph_ui.ui(ui, Some(selection)),
        }
    }

    delegate! {
        to match self {
            GraphUi::Chil(_, selection) => selection,
            GraphUi::Spartan(_, selection) => selection,
        } {
            pub(crate) fn clear_selection(&mut self);
            pub(crate) fn extend_selection(&mut self, direction: Option<(Direction, usize)>);
        }
    }
}

pub struct GraphUiInternal<T: Language> {
    #[allow(dead_code)] // Dropping this breaks the app
    hypergraph: SyntaxHyperGraph<T>,
    monoidal_graph: Arc<MonoidalGraph<(Op<T>, Name<T>)>>,
    metadata: GraphMetadata<(Op<T>, Name<T>)>,
    panzoom: Panzoom,
    ready: bool,
    reset_requested: bool,
}

impl<T: 'static + Language> GraphUiInternal<T> {
    pub(crate) fn ui(
        &mut self,
        ui: &mut egui::Ui,
        current_selection: Option<&mut SelectionMap<(Op<T>, Name<T>)>>,
    ) where
        T::Op: Display + PrettyPrint,
        T::Var: PrettyPrint,
        T::Addr: Display,
        T::VarDef: PrettyPrint,
        Expr<T>: PrettyPrint,
    {
        let shapes = generate_shapes(&self.monoidal_graph, &self.metadata);
        let guard = shapes.lock();
        if let Some(shapes) = guard.ready() {
            let (response, painter) =
                ui.allocate_painter(ui.available_size_before_wrap(), egui::Sense::hover());
            let to_screen = self.panzoom.transform(response.rect);
            if let Some(hover_pos) = response.hover_pos() {
                let anchor = to_screen.inverse().transform_pos(hover_pos);
                ui.input(|i| {
                    self.panzoom.zoom(i.zoom_delta(), anchor);
                    self.panzoom.pan(i.scroll_delta);
                });
                ui.input_mut(|i| {
                    let mut pan_by_key = |key, pan: fn(&mut Panzoom) -> ()| {
                        if i.consume_shortcut(&egui::KeyboardShortcut::new(
                            egui::Modifiers::NONE,
                            key,
                        )) {
                            pan(&mut self.panzoom);
                        }
                    };
                    pan_by_key(egui::Key::ArrowLeft, Panzoom::pan_left);
                    pan_by_key(egui::Key::H, Panzoom::pan_left);
                    pan_by_key(egui::Key::ArrowRight, Panzoom::pan_right);
                    pan_by_key(egui::Key::L, Panzoom::pan_right);
                    pan_by_key(egui::Key::ArrowUp, Panzoom::pan_up);
                    pan_by_key(egui::Key::K, Panzoom::pan_up);
                    pan_by_key(egui::Key::ArrowDown, Panzoom::pan_down);
                    pan_by_key(egui::Key::J, Panzoom::pan_down);
                });
            }

            if self.reset_requested {
                self.panzoom.reset(shapes.size);
                self.reset_requested = false;
            }
            // Background
            painter.add(Shape::rect_filled(
                response.rect,
                Rounding::none(),
                ui.visuals().faint_bg_color,
            ));

            painter.extend(sd_graphics::render::render(
                ui,
                &shapes.shapes,
                &response,
                &mut self.metadata,
                current_selection,
                to_screen,
            ));
            self.ready = true;
        } else {
            ui.centered_and_justified(eframe::egui::Ui::spinner);
            self.ready = false;
        }
    }

    pub(crate) fn from_graph(hypergraph: SyntaxHyperGraph<T>) -> Self
    where
        T::Op: Display,
    {
        let expanded = hypergraph.create_expanded();
        Self::from_subgraph(
            Subgraph {
                graph: hypergraph,
                mapping: None,
            },
            expanded,
        )
    }

    pub(crate) fn from_subgraph(
        subgraph: SyntaxSubgraph<T>,
        expanded: WeakMap<Thunk<Op<T>, Name<T>>, bool>,
    ) -> Self
    where
        T::Op: Display,
    {
        let hypergraph = subgraph.graph;

        debug!("Converting to monoidal term");
        let monoidal_term = MonoidalWiredGraph::from(&hypergraph);
        debug!("Got term {:#?}", monoidal_term);

        debug!("Inserting swaps and copies");
        let monoidal_graph = Arc::new(MonoidalGraph::from(&monoidal_term));
        debug!("Got graph {:#?}", monoidal_graph);

        let metadata = GraphMetadata {
            expanded,
            mapping: subgraph.mapping,
        };

        let mut this = Self {
            hypergraph,
            monoidal_graph,
            metadata,
            panzoom: Panzoom::default(),
            ready: bool::default(),
            reset_requested: bool::default(),
        };
        this.reset();
        this
    }

    pub(crate) const fn ready(&self) -> bool {
        self.ready
    }

    pub(crate) fn reset(&mut self)
    where
        T::Op: Display,
    {
        self.reset_requested = true;
    }

    delegate! {
        to self.panzoom {
            pub(crate) fn zoom_in(&mut self);
            pub(crate) fn zoom_out(&mut self);
        }
    }

    pub(crate) fn export_svg(&self) -> String
    where
        T::Op: Display,
    {
        let shapes = generate_shapes(&self.monoidal_graph, &self.metadata);
        let guard = shapes.lock(); // this would lock the UI, but by the time we get here
                                   // the shapes have already been computed
        guard.block_until_ready().to_svg().to_string()
    }

    pub(crate) const fn get_expanded(&self) -> &WeakMap<Thunk<Op<T>, Name<T>>, bool> {
        &self.metadata.expanded
    }
}
