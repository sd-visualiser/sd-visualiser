use eframe::{
    egui, emath,
    epaint::{Color32, Pos2, Rect, Rounding, Shape, Vec2},
};
use sd_core::{language, monoidal::MonoidalGraph};

use crate::highlighter;

#[derive(Default)]
pub struct App {
    code: String,
    // TODO: eventually want to store monoidal representation too
}

impl App {
    /// Called once before the first frame.
    pub fn new(_cc: &eframe::CreationContext<'_>) -> Self {
        // This is also where you can customize the look and feel of egui using
        // `cc.egui_ctx.set_visuals` and `cc.egui_ctx.set_fonts`.

        // Load previous app state (if any).
        // Note that you must enable the `persistence` feature for this to work.
        // if let Some(storage) = cc.storage {
        //     return eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default();
        // }

        Default::default()
    }

    fn code_ui(&mut self, ui: &mut egui::Ui) {
        let mut layouter = |ui: &egui::Ui, source: &str, wrap_width: f32| {
            let mut layout_job = highlighter::highlight(source);
            layout_job.wrap.max_width = wrap_width;
            ui.fonts(|f| f.layout_job(layout_job))
        };
        ui.add(
            egui::TextEdit::multiline(&mut self.code)
                .font(egui::TextStyle::Monospace)
                .layouter(&mut layouter),
        );
    }

    fn graph_ui(&mut self, ui: &mut egui::Ui) {
        let block = || {
            let expr = language::grammar::parse(&self.code).ok()?;
            let hypergraph = expr.to_hypergraph().ok()?;
            MonoidalGraph::from_hypergraph(&hypergraph).ok()
        };
        let Some(graph) = block() else { return };

        // let graph = graph.unfold();

        let (response, painter) = ui.allocate_painter(
            Vec2::new(ui.available_width(), ui.available_height()),
            egui::Sense::drag(),
        );
        let to_screen = emath::RectTransform::from_to(
            Rect::from_min_size(Pos2::ZERO, response.rect.size()),
            response.rect,
        );

        // Background
        painter.add(Shape::rect_filled(
            response.rect,
            Rounding::none(),
            Color32::WHITE,
        ));
        painter
            .extend(sd_graphics::render::render(graph, response.rect.size(), to_screen).unwrap());
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.columns(2, |columns| {
                egui::ScrollArea::vertical()
                    .id_source("code")
                    .show(&mut columns[0], |ui| self.code_ui(ui));
                egui::ScrollArea::both()
                    .id_source("graph")
                    .show(&mut columns[1], |ui| self.graph_ui(ui));
            })
        });
    }
}
