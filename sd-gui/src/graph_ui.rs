#![allow(clippy::inline_always)]

use std::fmt::Display;

use delegate::delegate;
use eframe::{
    egui,
    epaint::{CornerRadius, Shape},
};
use sd_core::{
    codeable::Codeable,
    common::{Direction, Matchable},
    dot::DotWeight,
    graph::SyntaxHypergraph,
    hypergraph::{
        Hypergraph,
        generic::{Edge, Operation, Thunk, Weight},
        subgraph::ExtensibleEdge,
        traits::{Graph, WithType},
    },
    interactive::InteractiveGraph,
    language::{chil::Chil, mlir::Mlir, spartan::Spartan},
    lp::Solver,
};
use sd_graphics::{common::Shapeable, renderable::RenderableGraph};

use crate::{panzoom::Panzoom, shape_generator::generate_shapes};

pub enum GraphUi {
    Chil(GraphUiInternal<InteractiveGraph<SyntaxHypergraph<Chil>>>),
    Mlir(GraphUiInternal<InteractiveGraph<SyntaxHypergraph<Mlir>>>),
    Spartan(GraphUiInternal<InteractiveGraph<SyntaxHypergraph<Spartan>>>),
    Dot(GraphUiInternal<InteractiveGraph<Hypergraph<DotWeight>>>),
}

impl GraphUi {
    pub(crate) fn new_chil(graph: SyntaxHypergraph<Chil>, solver: Solver) -> Self {
        Self::Chil(GraphUiInternal::new(InteractiveGraph::new(graph), solver))
    }

    pub(crate) fn new_mlir(graph: SyntaxHypergraph<Mlir>, solver: Solver) -> Self {
        Self::Mlir(GraphUiInternal::new(InteractiveGraph::new(graph), solver))
    }

    pub(crate) fn new_spartan(graph: SyntaxHypergraph<Spartan>, solver: Solver) -> Self {
        Self::Spartan(GraphUiInternal::new(InteractiveGraph::new(graph), solver))
    }

    pub(crate) fn new_dot(graph: Hypergraph<DotWeight>, solver: Solver) -> Self {
        Self::Dot(GraphUiInternal::new(InteractiveGraph::new(graph), solver))
    }

    delegate! {
        to match self {
            GraphUi::Chil(graph_ui) => graph_ui,
            GraphUi::Mlir(graph_ui) => graph_ui,
            GraphUi::Spartan(graph_ui) => graph_ui,
            GraphUi::Dot(graph_ui) => graph_ui
        } {
            pub(crate) fn ui(&mut self, ui: &mut egui::Ui, search: Option<&str>);
            pub(crate) const fn ready(&self) -> bool;
            pub(crate) fn reset(&mut self);
            pub(crate) fn zoom_in(&mut self);
            pub(crate) fn zoom_out(&mut self);
            pub(crate) fn find(&mut self, query: &str, offset: usize);
            pub(crate) fn export_svg(&self) -> String;
        }
    }

    delegate! {
        to match self {
            GraphUi::Chil(graph_ui) => graph_ui.graph,
            GraphUi::Mlir(graph_ui) => graph_ui.graph,
            GraphUi::Spartan(graph_ui) => graph_ui.graph,
            GraphUi::Dot(graph_ui) => graph_ui.graph
        } {
            pub(crate) fn is_empty(&self) -> bool;
            pub(crate) fn clear_selection(&mut self);
            pub(crate) fn extend_selection(&mut self, direction: Option<(Direction, usize)>);
            pub(crate) fn set_expanded_all(&mut self, expanded: bool);
        }
    }
}

pub struct GraphUiInternal<G: Graph> {
    pub(crate) graph: G,
    panzoom: Panzoom,
    ready: bool,
    reset_requested: bool,
    solver: Solver,
}

impl<G> GraphUiInternal<G>
where
    G: Graph + 'static,
{
    pub(crate) fn new(graph: G, solver: Solver) -> Self {
        Self {
            graph,
            panzoom: Panzoom::default(),
            ready: false,
            reset_requested: true,
            solver,
        }
    }

    pub(crate) fn ui(&mut self, ui: &mut egui::Ui, search: Option<&str>)
    where
        // Needed for render
        G: RenderableGraph,
        Edge<G::Ctx>: Codeable,
        Operation<G::Ctx>: Codeable + Matchable,
        Thunk<G::Ctx>: Codeable + Matchable,
        // Needed for generate_shapes
        Edge<G::Ctx>: ExtensibleEdge,
        Operation<G::Ctx>: Shapeable,
        Weight<Operation<G::Ctx>>: Display,
        Weight<Edge<G::Ctx>>: WithType,
    {
        let shapes = generate_shapes(&self.graph, self.solver);
        let guard = shapes.lock().unwrap();
        if let Some(shapes) = guard.ready() {
            let (response, painter) =
                ui.allocate_painter(ui.available_size_before_wrap(), egui::Sense::drag());

            let to_screen = self.panzoom.transform(response.rect);

            if response.contains_pointer() {
                ui.input(|i| {
                    if let Some(hover_pos) = i.pointer.hover_pos() {
                        let anchor = to_screen.inverse().transform_pos(hover_pos);
                        self.panzoom.zoom(i.zoom_delta(), anchor);
                    }

                    self.panzoom.pan(i.smooth_scroll_delta);
                });
                self.panzoom.pan(response.drag_delta());

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
                self.panzoom
                    .reset(shapes.size, response.rect.max - response.rect.min);
                self.reset_requested = false;
            }
            // Background
            painter.add(Shape::rect_filled(
                response.rect,
                CornerRadius::ZERO,
                ui.visuals().faint_bg_color,
            ));

            painter.extend(sd_graphics::render::render(
                &mut self.graph,
                ui,
                &shapes.shapes,
                &response,
                to_screen,
                search,
            ));
            self.ready = true;
        } else {
            ui.centered_and_justified(eframe::egui::Ui::spinner);
            self.ready = false;
        }
    }

    pub(crate) const fn ready(&self) -> bool {
        self.ready
    }

    pub(crate) fn reset(&mut self) {
        self.reset_requested = true;
    }

    /// Searches through the shapes and pans to the one which matches the query
    pub(crate) fn find(&mut self, query: &str, offset: usize)
    where
        Edge<G::Ctx>: ExtensibleEdge,
        Operation<G::Ctx>: Matchable + Shapeable,
        Thunk<G::Ctx>: Matchable,
        Weight<Operation<G::Ctx>>: Display,
    {
        let shapes = generate_shapes(&self.graph, self.solver);
        let guard = shapes.lock().unwrap();

        if let Some(shapes) = guard.ready() {
            let matches = shapes
                .shapes
                .iter()
                .filter(|shape| shape.is_match(query))
                .collect::<Vec<_>>();
            if !matches.is_empty() {
                let shape = matches[offset % matches.len()];
                self.panzoom.set_pan(shape.center());
            }
        }
    }

    delegate! {
        to self.panzoom {
            pub(crate) fn zoom_in(&mut self);
            pub(crate) fn zoom_out(&mut self);
        }
    }

    pub(crate) fn export_svg(&self) -> String
    where
        Edge<G::Ctx>: ExtensibleEdge,
        Operation<G::Ctx>: Shapeable,
        Weight<Operation<G::Ctx>>: Display,
    {
        let shapes = generate_shapes(&self.graph, self.solver);
        let guard = shapes.lock().unwrap(); // this would lock the UI, but by the time we get here
        // the shapes have already been computed
        guard.block_until_ready().to_svg().to_string()
    }
}
