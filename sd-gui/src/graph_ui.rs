#![allow(clippy::inline_always)]

use std::{fmt::Display, sync::Arc};

use delegate::delegate;
use eframe::{
    egui, emath,
    epaint::{Rect, Rounding, Shape},
};
use indexmap::IndexSet;
use sd_core::{
    graph::{Name, Op, SyntaxHyperGraph},
    hypergraph::{Operation, Thunk},
    language::{chil::Chil, spartan::Spartan, Expr, Language},
    monoidal::MonoidalGraph,
    monoidal_wired::MonoidalWiredGraph,
    prettyprinter::PrettyPrint,
    weak_map::WeakMap,
};
use tracing::debug;

use crate::{panzoom::Panzoom, shape_generator::generate_shapes};

pub(crate) enum GraphUi {
    Chil(
        GraphUiInternal<Chil>,
        IndexSet<Operation<Op<Chil>, Name<Chil>>>,
    ),
    Spartan(
        GraphUiInternal<Spartan>,
        IndexSet<Operation<Op<Spartan>, Name<Spartan>>>,
    ),
}

impl GraphUi {
    pub(crate) fn new_chil(hypergraph: SyntaxHyperGraph<Chil>) -> Self {
        Self::Chil(GraphUiInternal::from_graph(hypergraph), IndexSet::new())
    }

    pub(crate) fn new_spartan(hypergraph: SyntaxHyperGraph<Spartan>) -> Self {
        Self::Spartan(GraphUiInternal::from_graph(hypergraph), IndexSet::new())
    }

    delegate! {
        to match self {
            GraphUi::Chil(graph_ui, _) => graph_ui,
            GraphUi::Spartan(graph_ui, _) => graph_ui,
        } {
            pub(crate) fn ready(&self) -> bool;
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

    pub(crate) fn clear_selection(&mut self) {
        match self {
            GraphUi::Chil(_, selection) => selection.clear(),
            GraphUi::Spartan(_, selection) => selection.clear(),
        }
    }
}

pub(crate) struct GraphUiInternal<T: Language> {
    #[allow(dead_code)] // Dropping this breaks the app
    hypergraph: SyntaxHyperGraph<T>,
    monoidal_graph: Arc<MonoidalGraph<(Op<T>, Name<T>)>>,
    expanded: WeakMap<Thunk<Op<T>, Name<T>>, bool>,
    panzoom: Panzoom,
    ready: bool,
    reset_requested: bool,
}

impl<T: 'static + Language> GraphUiInternal<T> {
    pub(crate) fn ui(
        &mut self,
        ui: &mut egui::Ui,
        current_selection: Option<&mut IndexSet<Operation<Op<T>, Name<T>>>>,
    ) where
        T::Op: Display,
        T::Op: PrettyPrint,
        T::Var: PrettyPrint,
        T::Addr: Display,
        T::VarDef: PrettyPrint,
        Expr<T>: PrettyPrint,
    {
        let shapes = generate_shapes(&self.monoidal_graph, &self.expanded);
        let guard = shapes.lock();
        if let Some(shapes) = guard.ready() {
            let (response, painter) =
                ui.allocate_painter(ui.available_size_before_wrap(), egui::Sense::hover());
            let to_screen = emath::RectTransform::from_to(
                Rect::from_center_size(
                    self.panzoom.translation,
                    response.rect.size() / self.panzoom.zoom,
                ),
                response.rect,
            );
            if let Some(hover_pos) = response.hover_pos() {
                let anchor = to_screen.inverse().transform_pos(hover_pos);
                ui.input(|i| {
                    self.panzoom.zoom(i.zoom_delta(), anchor);
                    self.panzoom.translation -= i.scroll_delta / self.panzoom.zoom;
                });
                ui.input_mut(|i| {
                    const PAN_FACTOR: f32 = 10.0;
                    const LEFT: egui::Vec2 = egui::vec2(-PAN_FACTOR, 0.0);
                    const RIGHT: egui::Vec2 = egui::vec2(PAN_FACTOR, 0.0);
                    const UP: egui::Vec2 = egui::vec2(0.0, -PAN_FACTOR);
                    const DOWN: egui::Vec2 = egui::vec2(0.0, PAN_FACTOR);
                    macro_rules! pan_by_key {
                        ($key:expr, $direction:ident) => {
                            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                                egui::Modifiers::NONE,
                                $key,
                            )) {
                                self.panzoom.translation += $direction / self.panzoom.zoom;
                            }
                        };
                    }

                    pan_by_key!(egui::Key::ArrowLeft, LEFT);
                    pan_by_key!(egui::Key::H, LEFT);
                    pan_by_key!(egui::Key::ArrowRight, RIGHT);
                    pan_by_key!(egui::Key::L, RIGHT);
                    pan_by_key!(egui::Key::ArrowUp, UP);
                    pan_by_key!(egui::Key::K, UP);
                    pan_by_key!(egui::Key::ArrowDown, DOWN);
                    pan_by_key!(egui::Key::J, DOWN);
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
                &mut self.expanded,
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

        debug!("Converting to monoidal term");
        let monoidal_term = MonoidalWiredGraph::from(&hypergraph);
        debug!("Got term {:#?}", monoidal_term);

        debug!("Inserting swaps and copies");
        let monoidal_graph = Arc::new(MonoidalGraph::from(&monoidal_term));
        debug!("Got graph {:#?}", monoidal_graph);

        let mut this = Self {
            hypergraph,
            monoidal_graph,
            expanded,
            panzoom: Panzoom::default(),
            ready: bool::default(),
            reset_requested: bool::default(),
        };
        this.reset();
        this
    }

    pub(crate) fn ready(&self) -> bool {
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
        let shapes = generate_shapes(&self.monoidal_graph, &self.expanded);
        let guard = shapes.lock(); // this would lock the UI, but by the time we get here
                                   // the shapes have already been computed
        guard.block_until_ready().to_svg().to_string()
    }
}
