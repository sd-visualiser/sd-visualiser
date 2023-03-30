#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

use eframe::egui;
use sd_core::language;
use sd_gui::highlighter;

fn main() -> Result<(), eframe::Error> {
    // Log to stdout (if you run with `RUST_LOG=debug`).
    tracing_subscriber::fmt::init();

    let options = eframe::NativeOptions {
        initial_window_size: Some(egui::vec2(1280.0, 1024.0)),
        ..Default::default()
    };
    eframe::run_native(
        "My egui App",
        options,
        Box::new(|_cc| Box::<App>::default()),
    )
}

#[derive(Default)]
struct App {
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
            ui.heading("SD visualiser");
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
