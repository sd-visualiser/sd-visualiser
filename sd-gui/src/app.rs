use eframe::egui;
use sd_core::language;

use crate::highlighter;

#[derive(Default)]
pub struct App {
    code: String,
    // TODO: eventually want to store monoidal representation too
}

impl App {
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
        ui.label(format!(
            "Parse result: {:?}",
            language::grammar::parse(&self.code)
        ));
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
