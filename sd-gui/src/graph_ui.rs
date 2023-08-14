#![allow(clippy::inline_always)]

use std::fmt::{Debug, Display};

use delegate::delegate;
use eframe::{
    egui::{self, Id, Sense},
    epaint::{Rounding, Shape},
};
use sd_core::{
    common::{Addr, Direction, Matchable},
    decompile::Fresh,
    graph::{Name, Op, SyntaxHypergraph},
    hypergraph::{
        create_expanded, create_selected,
        subgraph::{ExtensibleEdge, ModifiableGraph},
        traits::{Graph, NodeLike, WithWeight},
    },
    language::{chil::Chil, spartan::Spartan, Expr, Language},
    prettyprinter::PrettyPrint,
    selection::SelectionMap,
    weak_map::WeakMap,
};

use crate::{panzoom::Panzoom, shape_generator::generate_shapes};

pub enum GraphUi {
    Chil(
        GraphUiInternal<SyntaxHypergraph<Chil>>,
        SelectionMap<(Op<Chil>, Name<Chil>)>,
    ),
    Spartan(
        GraphUiInternal<SyntaxHypergraph<Spartan>>,
        SelectionMap<(Op<Spartan>, Name<Spartan>)>,
    ),
}

impl GraphUi {
    pub(crate) fn new_chil(graph: SyntaxHypergraph<Chil>) -> Self {
        let expanded = create_expanded(&graph);
        let selected = create_selected(&graph);
        Self::Chil(GraphUiInternal::new(graph, expanded), selected)
    }

    pub(crate) fn new_spartan(graph: SyntaxHypergraph<Spartan>) -> Self {
        let expanded = create_expanded(&graph);
        let selected = create_selected(&graph);
        Self::Spartan(GraphUiInternal::new(graph, expanded), selected)
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
            pub(crate) fn find_variable(&mut self, variable: &str);
            pub(crate) fn export_svg(&self) -> String;
            pub(crate) fn set_expanded_all(&mut self, expanded: bool);
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
            pub(crate) fn is_empty(&self) -> bool;
        }
    }
}

pub struct GraphUiInternal<G: Graph> {
    pub(crate) graph: G,
    pub(crate) expanded: WeakMap<<G::T as Addr>::Thunk, bool>,
    panzoom: Panzoom,
    ready: bool,
    reset_requested: bool,
}

impl<G> GraphUiInternal<G>
where
    G: ModifiableGraph + Send + Sync + 'static,
{
    pub(crate) fn new(graph: G, expanded: WeakMap<<G::T as Addr>::Thunk, bool>) -> Self {
        Self {
            graph,
            expanded,
            panzoom: Panzoom::default(),
            ready: false,
            reset_requested: true,
        }
    }

    pub(crate) fn ui<T>(&mut self, ui: &mut egui::Ui, selection: Option<&mut SelectionMap<G::T>>)
    where
        T: Language,
        T::Op: PrettyPrint,
        T::Var: PrettyPrint + Fresh,
        T::Addr: Display,
        T::VarDef: PrettyPrint,
        Expr<T>: PrettyPrint,
        <G::T as Addr>::Node: NodeLike<T = G::T> + Debug + Send + Sync,
        <G::T as Addr>::Edge:
            ExtensibleEdge<T = G::T> + WithWeight<Weight = Name<T>> + Debug + Send + Sync,
        <G::T as Addr>::Operation:
            NodeLike<T = G::T> + WithWeight<Weight = Op<T>> + Debug + Send + Sync,
        <G::T as Addr>::Thunk: NodeLike<T = G::T> + Graph<T = G::T> + Debug + Send + Sync,
        <<G::T as Addr>::Operation as WithWeight>::Weight: Display,
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
                selection,
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

    /// Searches through the shapes by variable name and pans to the operation which generates the variable
    pub(crate) fn find_variable(&mut self, variable: &str)
    where
        <G::T as Addr>::Node: NodeLike<T = G::T> + Debug + Send + Sync,
        <G::T as Addr>::Edge: ExtensibleEdge<T = G::T> + WithWeight + Debug + Send + Sync,
        <G::T as Addr>::Operation:
            NodeLike<T = G::T> + WithWeight + Matchable + Debug + Send + Sync,
        <G::T as Addr>::Thunk:
            NodeLike<T = G::T> + Graph<T = G::T> + Matchable + Debug + Send + Sync,
        <<G::T as Addr>::Operation as WithWeight>::Weight: Display,
    {
        let shapes = generate_shapes(&self.graph, &self.expanded);
        let guard = shapes.lock().unwrap();

        if let Some(shapes) = guard.ready() {
            for shape in &shapes.shapes {
                if let Some(center) = shape.find_variable(variable) {
                    self.panzoom.set_pan(center);
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
        <G::T as Addr>::Node: NodeLike<T = G::T> + Debug + Send + Sync,
        <G::T as Addr>::Edge: ExtensibleEdge<T = G::T> + WithWeight + Debug + Send + Sync,
        <G::T as Addr>::Operation: NodeLike<T = G::T> + WithWeight + Debug + Send + Sync,
        <G::T as Addr>::Thunk: NodeLike<T = G::T> + Graph<T = G::T> + Debug + Send + Sync,
        <<G::T as Addr>::Operation as WithWeight>::Weight: Display,
    {
        let shapes = generate_shapes(&self.graph, &self.expanded);
        let guard = shapes.lock().unwrap(); // this would lock the UI, but by the time we get here
                                            // the shapes have already been computed
        guard.block_until_ready().to_svg().to_string()
    }

    pub(crate) fn set_expanded_all(&mut self, expanded: bool) {
        self.expanded.set_all(expanded);
    }
}
