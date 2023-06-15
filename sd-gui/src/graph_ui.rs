use std::{collections::HashSet, fmt::Display};

use eframe::{
    egui, emath,
    epaint::{Rect, Rounding, Shape},
};
use sd_core::{
    graph::{Name, Op, SyntaxHyperGraph},
    hypergraph::{Operation, Thunk},
    language::{chil::Chil, spartan::Spartan, Expr, Language},
    monoidal::MonoidalGraph,
    monoidal_wired::MonoidalWiredGraph,
    prettyprinter::PrettyPrint,
};
use sd_graphics::expanded::Expanded;
use tracing::debug;

use crate::{panzoom::Panzoom, shape_generator::ShapeGenerator};

#[derive(Default)]
pub(crate) enum GraphUi {
    #[default]
    Empty,
    Chil(GraphUiInternal<Chil>),
    Spartan(GraphUiInternal<Spartan>),
}

impl GraphUi {
    pub(crate) fn new_chil(ctx: &egui::Context, hypergraph: SyntaxHyperGraph<Chil>) -> Self {
        Self::Chil(GraphUiInternal::from_graph(hypergraph, ctx))
    }

    pub(crate) fn new_spartan(ctx: &egui::Context, hypergraph: SyntaxHyperGraph<Spartan>) -> Self {
        Self::Spartan(GraphUiInternal::from_graph(hypergraph, ctx))
    }

    pub(crate) fn ui(&mut self, ui: &mut egui::Ui) {
        match self {
            GraphUi::Empty => {}
            GraphUi::Chil(graph_ui) => graph_ui.ui(ui),
            GraphUi::Spartan(graph_ui) => graph_ui.ui(ui),
        }
    }

    pub(crate) fn reset(&mut self, ctx: &egui::Context) {
        match self {
            GraphUi::Empty => {}
            GraphUi::Chil(graph_ui) => graph_ui.reset(ctx),
            GraphUi::Spartan(graph_ui) => graph_ui.reset(ctx),
        }
    }

    pub(crate) fn zoom_in(&mut self) {
        match self {
            GraphUi::Empty => {}
            GraphUi::Chil(graph_ui) => graph_ui.panzoom.zoom_in(),
            GraphUi::Spartan(graph_ui) => graph_ui.panzoom.zoom_in(),
        }
    }

    pub(crate) fn zoom_out(&mut self) {
        match self {
            GraphUi::Empty => {}
            GraphUi::Chil(graph_ui) => graph_ui.panzoom.zoom_out(),
            GraphUi::Spartan(graph_ui) => graph_ui.panzoom.zoom_out(),
        }
    }

    #[allow(dead_code)]
    pub(crate) fn clear_selection(&mut self) {
        match self {
            GraphUi::Empty => {}
            GraphUi::Chil(graph_ui) => graph_ui.current_selection.clear(),
            GraphUi::Spartan(graph_ui) => graph_ui.current_selection.clear(),
        }
    }
}

pub(crate) struct GraphUiInternal<T: Language> {
    #[allow(dead_code)]
    hypergraph: SyntaxHyperGraph<T>,
    monoidal_graph: MonoidalGraph<(Op<T>, Name<T>)>,
    expanded: Expanded<Thunk<Op<T>, Name<T>>>,
    pub(crate) current_selection: HashSet<Operation<Op<T>, Name<T>>>,
    panzoom: Panzoom,
}

impl<T: 'static + Language> GraphUiInternal<T> {
    pub(crate) fn ui(&mut self, ui: &mut egui::Ui)
    where
        T::Op: Display,
        T::Op: PrettyPrint,
        T::Var: PrettyPrint,
        T::Addr: Display,
        T::VarDef: PrettyPrint,
        Expr<T>: PrettyPrint,
    {
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

        let shapes =
            ShapeGenerator::generate_shapes(ui.ctx(), &self.monoidal_graph, &self.expanded);
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
            &mut self.current_selection,
            to_screen,
        ));
    }

    pub(crate) fn from_graph(hypergraph: SyntaxHyperGraph<T>, ctx: &egui::Context) -> Self
    where
        T::Op: Display,
    {
        let expanded = Expanded::from_graph(&hypergraph);

        debug!("Converting to monoidal term");
        let monoidal_term = MonoidalWiredGraph::from(&hypergraph);
        debug!("Got term {:#?}", monoidal_term);

        debug!("Inserting swaps and copies");
        let monoidal_graph = MonoidalGraph::from(&monoidal_term);
        debug!("Got graph {:#?}", monoidal_graph);

        let mut this = Self {
            hypergraph,
            monoidal_graph,
            expanded,
            current_selection: HashSet::new(),
            panzoom: Panzoom::default(),
        };
        this.reset(ctx);
        this
    }

    pub(crate) fn reset(&mut self, ctx: &egui::Context)
    where
        T::Op: Display,
    {
        let shapes = ShapeGenerator::generate_shapes(ctx, &self.monoidal_graph, &self.expanded);
        self.panzoom.reset(shapes.size);
    }
}
