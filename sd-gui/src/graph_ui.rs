#![allow(clippy::inline_always)]

use std::fmt::Display;

use delegate::delegate;
use eframe::{
    egui, emath,
    epaint::{Rect, Rounding, Shape},
};
use indexmap::IndexSet;
use sd_core::{
    graph::{Name, Op, SyntaxHyperGraph},
    hypergraph::{HyperGraph, Operation, Thunk},
    language::{chil::Chil, spartan::Spartan, Expr, Language},
    monoidal::MonoidalGraph,
    monoidal_wired::MonoidalWiredGraph,
    prettyprinter::PrettyPrint,
    weak_map::WeakMap,
};
use tracing::debug;

use crate::{panzoom::Panzoom, shape_generator::ShapeGenerator};

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
    pub(crate) fn new_chil(ctx: &egui::Context, hypergraph: SyntaxHyperGraph<Chil>) -> Self {
        Self::Chil(
            GraphUiInternal::from_graph(hypergraph, ctx),
            IndexSet::new(),
        )
    }

    pub(crate) fn new_spartan(ctx: &egui::Context, hypergraph: SyntaxHyperGraph<Spartan>) -> Self {
        Self::Spartan(
            GraphUiInternal::from_graph(hypergraph, ctx),
            IndexSet::new(),
        )
    }

    delegate! {
        to match self {
            GraphUi::Chil(graph_ui, _) => graph_ui,
            GraphUi::Spartan(graph_ui, _) => graph_ui,
        } {
            pub(crate) fn reset(&mut self, ctx: &egui::Context);
            pub(crate) fn zoom_in(&mut self);
            pub(crate) fn zoom_out(&mut self);
            pub(crate) fn export_svg(&self, ctx: &egui::Context) -> String;
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
    hypergraph: HyperGraph<Op<T>, Name<T>>,

    monoidal_graph: MonoidalGraph<(Op<T>, Name<T>)>,
    expanded: WeakMap<Thunk<Op<T>, Name<T>>, bool>,
    panzoom: Panzoom,
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
        }

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
            current_selection,
            to_screen,
        ));
    }

    pub(crate) fn from_graph(hypergraph: SyntaxHyperGraph<T>, ctx: &egui::Context) -> Self
    where
        T::Op: Display,
    {
        let expanded = hypergraph.create_expanded();

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

    delegate! {
        to self.panzoom {
            pub(crate) fn zoom_in(&mut self);
            pub(crate) fn zoom_out(&mut self);
        }
    }

    pub(crate) fn export_svg(&self, ctx: &egui::Context) -> String
    where
        T::Op: Display,
    {
        let shapes = ShapeGenerator::generate_shapes(ctx, &self.monoidal_graph, &self.expanded);
        shapes.as_ref().to_svg().to_string()
    }
}
