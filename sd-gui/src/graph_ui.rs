use std::collections::HashSet;

use eframe::{
    egui, emath,
    epaint::{Pos2, Rect, Rounding, Shape},
};
use sd_core::{
    graph::{Name, SyntaxHyperGraph},
    hypergraph::{Operation, Thunk},
    language::spartan::Op,
    monoidal::MonoidalGraph,
    monoidal_wired::MonoidalWiredGraph,
};
use sd_graphics::expanded::Expanded;
use tracing::debug;

use crate::layout::Layouter;

#[derive(Default)]
pub(crate) struct GraphUi {
    hypergraph: SyntaxHyperGraph,
    monoidal_term: MonoidalWiredGraph<Op, Name>,
    monoidal_graph: MonoidalGraph<(Op, Name)>,
    expanded: Expanded<Thunk<Op, Name>>,
    pub(crate) current_selection: HashSet<Operation<Op, Name>>,
    panzoom: Panzoom,
}

impl GraphUi {
    const ZOOM_FACTOR: f32 = 1.25;

    pub(crate) fn hypergraph(&self) -> &SyntaxHyperGraph {
        &self.hypergraph
    }

    pub(crate) fn ui(&mut self, ui: &mut egui::Ui) {
        let (response, painter) =
            ui.allocate_painter(ui.available_size_before_wrap(), egui::Sense::drag());
        let to_screen = emath::RectTransform::from_to(
            Rect::from_center_size(
                self.panzoom.translation,
                response.rect.size() / self.panzoom.zoom,
            ),
            response.rect,
        );
        self.panzoom.translation -= response.drag_delta() / self.panzoom.zoom;

        // Background
        painter.add(Shape::rect_filled(
            response.rect,
            Rounding::none(),
            ui.visuals().faint_bg_color,
        ));
        let layout = Layouter::layout(ui.ctx(), &self.monoidal_graph, &self.expanded).unwrap();
        painter.extend(sd_graphics::render::render(
            ui,
            &response,
            &layout,
            self.panzoom.zoom,
            &self.monoidal_graph,
            &mut self.expanded,
            &mut self.current_selection,
            response.rect,
            to_screen,
        ));
    }

    pub(crate) fn compile(&mut self, hypergraph: SyntaxHyperGraph, ctx: &egui::Context) {
        self.expanded = Expanded::from_graph(&hypergraph);

        self.hypergraph = hypergraph;

        debug!("Converting to monoidal term");
        self.monoidal_term = MonoidalWiredGraph::from(&self.hypergraph);
        debug!("Got term {:#?}", self.monoidal_term);

        debug!("Inserting swaps and copies");
        self.monoidal_graph = MonoidalGraph::from(&self.monoidal_term);
        debug!("Got graph {:#?}", self.monoidal_graph);

        self.reset(ctx);

        self.current_selection.clear();
    }

    pub(crate) fn reset(&mut self, ctx: &egui::Context) {
        let layout = Layouter::layout(ctx, &self.monoidal_graph, &self.expanded).unwrap();
        self.panzoom = Panzoom {
            translation: Pos2::new(layout.width() / 2.0, layout.height() / 2.0),
            ..Panzoom::default()
        }
    }

    pub(crate) fn zoom_in(&mut self) {
        self.panzoom.zoom *= Self::ZOOM_FACTOR;
    }

    pub(crate) fn zoom_out(&mut self) {
        self.panzoom.zoom /= Self::ZOOM_FACTOR;
    }
}

struct Panzoom {
    translation: Pos2,
    zoom: f32,
}

impl Default for Panzoom {
    fn default() -> Self {
        Self {
            translation: Pos2::default(),
            zoom: 50.0,
        }
    }
}
