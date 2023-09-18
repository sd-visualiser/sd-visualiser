#![allow(clippy::inline_always)]

use std::fmt::Display;

use delegate::delegate;
use eframe::{
    egui::{self, Id, Sense},
    epaint::{Rounding, Shape},
};
use sd_core::{
    codeable::Codeable,
    common::{Direction, Matchable},
    graph::SyntaxHypergraph,
    hypergraph::{
        generic::{Edge, Operation, OperationWeight, Thunk},
        subgraph::ExtensibleEdge,
        traits::Graph,
        utils::create_expanded,
    },
    interactive::InteractiveGraph,
    language::{chil::Chil, spartan::Spartan},
    weak_map::WeakMap,
};
use sd_graphics::renderable::RenderableGraph;

use crate::{panzoom::Panzoom, shape_generator::generate_shapes};

pub enum GraphUi {
    Chil(GraphUiInternal<InteractiveGraph<SyntaxHypergraph<Chil>>>),
    Spartan(GraphUiInternal<InteractiveGraph<SyntaxHypergraph<Spartan>>>),
}

impl GraphUi {
    pub(crate) fn new_chil(graph: SyntaxHypergraph<Chil>) -> Self {
        let graph = InteractiveGraph::new(graph);
        let expanded = create_expanded(&graph);
        Self::Chil(GraphUiInternal::new(graph, expanded))
    }

    pub(crate) fn new_spartan(graph: SyntaxHypergraph<Spartan>) -> Self {
        let graph = InteractiveGraph::new(graph);
        let expanded = create_expanded(&graph);
        Self::Spartan(GraphUiInternal::new(graph, expanded))
    }

    delegate! {
        to match self {
            GraphUi::Chil(graph_ui) => graph_ui,
            GraphUi::Spartan(graph_ui) => graph_ui,
        } {
            pub(crate) fn ui(&mut self, ui: &mut egui::Ui);
            pub(crate) const fn ready(&self) -> bool;
            pub(crate) fn reset(&mut self);
            pub(crate) fn zoom_in(&mut self);
            pub(crate) fn zoom_out(&mut self);
            pub(crate) fn find(&mut self, query: &str);
            pub(crate) fn export_svg(&self) -> String;
            pub(crate) fn set_expanded_all(&mut self, expanded: bool);
        }
    }

    delegate! {
        to match self {
            GraphUi::Chil(graph_ui) => graph_ui.graph.selection,
            GraphUi::Spartan(graph_ui) => graph_ui.graph.selection,
        } {
            pub(crate) fn clear_selection(&mut self);
            pub(crate) fn extend_selection(&mut self, direction: Option<(Direction, usize)>);
            pub(crate) fn is_empty(&self) -> bool;
        }
    }
}

pub struct GraphUiInternal<G: Graph> {
    pub(crate) graph: G,
    pub(crate) expanded: WeakMap<Thunk<G::Ctx>, bool>,
    panzoom: Panzoom,
    ready: bool,
    reset_requested: bool,
}

impl<G> GraphUiInternal<G>
where
    G: Graph + 'static,
{
    pub(crate) fn new(graph: G, expanded: WeakMap<Thunk<G::Ctx>, bool>) -> Self {
        Self {
            graph,
            expanded,
            panzoom: Panzoom::default(),
            ready: false,
            reset_requested: true,
        }
    }

    pub(crate) fn ui(&mut self, ui: &mut egui::Ui)
    where
        // Needed for render
        G: RenderableGraph,
        Edge<G::Ctx>: Codeable,
        Operation<G::Ctx>: Codeable,
        Thunk<G::Ctx>: Codeable,
        // Needed for generate_shapes
        Edge<G::Ctx>: ExtensibleEdge,
        OperationWeight<G::Ctx>: Display,
    {
        let shapes = generate_shapes(&self.graph, &self.expanded);
        let guard = shapes.lock().unwrap();
        if let Some(shapes) = guard.ready() {
            let (response, painter) =
                ui.allocate_painter(ui.available_size_before_wrap(), egui::Sense::hover());

            let to_screen = self.panzoom.transform(response.rect);
            if let Some(hover_pos) = response.hover_pos() {
                let drag_response = ui.interact_with_hovered(
                    response.rect,
                    true,
                    Id::new("drag_interact"),
                    Sense::drag(),
                );
                let anchor = to_screen.inverse().transform_pos(hover_pos);
                ui.input(|i| {
                    self.panzoom.zoom(i.zoom_delta(), anchor);
                    self.panzoom.pan(i.scroll_delta);
                    self.panzoom.pan(drag_response.drag_delta());
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
                self.panzoom
                    .reset(shapes.size, response.rect.max - response.rect.min);
                self.reset_requested = false;
            }
            // Background
            painter.add(Shape::rect_filled(
                response.rect,
                Rounding::none(),
                ui.visuals().faint_bg_color,
            ));

            painter.extend(sd_graphics::render::render(
                &mut self.graph,
                ui,
                &shapes.shapes,
                &response,
                &mut self.expanded,
                to_screen,
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
    pub(crate) fn find(&mut self, query: &str)
    where
        Edge<G::Ctx>: ExtensibleEdge,
        Operation<G::Ctx>: Matchable,
        Thunk<G::Ctx>: Matchable,
        OperationWeight<G::Ctx>: Display,
    {
        let shapes = generate_shapes(&self.graph, &self.expanded);
        let guard = shapes.lock().unwrap();

        if let Some(shapes) = guard.ready() {
            for shape in &shapes.shapes {
                if shape.is_match(query) {
                    self.panzoom.set_pan(shape.center());
                    break;
                }
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
        OperationWeight<G::Ctx>: Display,
    {
        let shapes = generate_shapes(&self.graph, &self.expanded);
        let guard = shapes.lock().unwrap(); // this would lock the UI, but by the time we get here
                                            // the shapes have already been computed
        guard.block_until_ready().to_svg().to_string()
    }

    pub(crate) fn set_expanded_all(&mut self, expanded: bool) {
        for x in self.expanded.values_mut() {
            *x = expanded;
        }
    }
}
