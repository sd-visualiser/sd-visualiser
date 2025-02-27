#![allow(clippy::inline_always)]

use std::fmt::Display;

use delegate::delegate;
use eframe::egui;
use egui::{CornerRadius, Pos2, Rect, Scene, Vec2};
#[cfg(not(target_arch = "wasm32"))]
use sd_core::language::llvm_ir::LlvmIr;
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
use sd_graphics::{
    common::{SCALE, Shapeable},
    renderable::RenderableGraph,
};

use crate::shape_generator::generate_shapes;

pub enum GraphUi {
    Chil(GraphUiInternal<InteractiveGraph<SyntaxHypergraph<Chil>>>),
    #[cfg(not(target_arch = "wasm32"))]
    LlvmIr(GraphUiInternal<InteractiveGraph<SyntaxHypergraph<LlvmIr>>>),
    Mlir(GraphUiInternal<InteractiveGraph<SyntaxHypergraph<Mlir>>>),
    Spartan(GraphUiInternal<InteractiveGraph<SyntaxHypergraph<Spartan>>>),
    Dot(GraphUiInternal<InteractiveGraph<Hypergraph<DotWeight>>>),
}

impl GraphUi {
    pub(crate) fn new_chil(graph: SyntaxHypergraph<Chil>, solver: Solver) -> Self {
        Self::Chil(GraphUiInternal::new(InteractiveGraph::new(graph), solver))
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn new_llvm_ir(graph: SyntaxHypergraph<LlvmIr>, solver: Solver) -> Self {
        Self::LlvmIr(GraphUiInternal::new(InteractiveGraph::new(graph), solver))
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
            #[cfg(not(target_arch = "wasm32"))]
            GraphUi::LlvmIr(graph_ui) => graph_ui,
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
            #[cfg(not(target_arch = "wasm32"))]
            GraphUi::LlvmIr(graph_ui) => graph_ui.graph,
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
    scene_rect: Rect,
    ready: bool,
    reset_requested: bool,
    solver: Solver,
}

enum PanDirection {
    Up,
    Down,
    Left,
    Right,
}

impl<G> GraphUiInternal<G>
where
    G: Graph + 'static,
{
    pub(crate) fn new(graph: G, solver: Solver) -> Self {
        Self {
            graph,
            scene_rect: Rect::ZERO,
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
            let response = Scene::new()
                .show(ui, &mut self.scene_rect, |ui| {
                    let painter = ui.painter();
                    let response = ui.response();

                    // Background
                    painter.add(egui::Shape::rect_filled(
                        ui.clip_rect(),
                        CornerRadius::ZERO,
                        ui.visuals().faint_bg_color,
                    ));

                    let shapes = sd_graphics::render::render(
                        &mut self.graph,
                        ui,
                        &shapes.shapes,
                        &response,
                        &ui.clip_rect(),
                        search,
                    );
                    painter.extend(shapes);
                })
                .response;

            if response.contains_pointer() {
                ui.input_mut(|i| {
                    let inv_zoom_factor = self.scene_rect.area() / ui.min_rect().area();
                    let mut pan_by_key = |key, direction| {
                        if i.consume_shortcut(&egui::KeyboardShortcut::new(
                            egui::Modifiers::NONE,
                            key,
                        )) {
                            self.pan(direction, inv_zoom_factor);
                        }
                    };
                    pan_by_key(egui::Key::ArrowLeft, PanDirection::Left);
                    pan_by_key(egui::Key::H, PanDirection::Left);
                    pan_by_key(egui::Key::ArrowRight, PanDirection::Right);
                    pan_by_key(egui::Key::L, PanDirection::Right);
                    pan_by_key(egui::Key::ArrowUp, PanDirection::Up);
                    pan_by_key(egui::Key::K, PanDirection::Up);
                    pan_by_key(egui::Key::ArrowDown, PanDirection::Down);
                    pan_by_key(egui::Key::J, PanDirection::Down);
                });
            }

            if self.reset_requested {
                self.scene_rect = Rect::from_min_max(Pos2::ZERO, Pos2::ZERO + SCALE * shapes.size);
                self.reset_requested = false;
            }
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

    const ZOOM_FACTOR: f32 = 1.25;

    pub(crate) fn zoom_in(&mut self) {
        self.scene_rect = self.scene_rect.scale_from_center(Self::ZOOM_FACTOR.recip());
    }

    pub(crate) fn zoom_out(&mut self) {
        self.scene_rect = self.scene_rect.scale_from_center(Self::ZOOM_FACTOR);
    }

    const PAN_FACTOR: f32 = 10.0;

    fn pan(&mut self, direction: PanDirection, scale: f32) {
        self.scene_rect = self.scene_rect.translate(
            scale
                * Self::PAN_FACTOR
                * match direction {
                    PanDirection::Up => Vec2::UP,
                    PanDirection::Down => Vec2::DOWN,
                    PanDirection::Left => Vec2::LEFT,
                    PanDirection::Right => Vec2::RIGHT,
                },
        );
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
                self.scene_rect = SCALE * shape.bounding_box();
            }
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
