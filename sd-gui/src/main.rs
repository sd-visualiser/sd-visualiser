#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

use eframe::egui;
use sd_gui::app::App;

fn main() -> Result<(), eframe::Error> {
    // Log to stdout (if you run with `RUST_LOG=debug`).
    tracing_subscriber::fmt::init();

    let options = eframe::NativeOptions {
        initial_window_size: Some(egui::vec2(1280.0, 1024.0)),
        ..Default::default()
    };
    eframe::run_native(
        "sd-visualiser",
        options,
        Box::new(|_cc| Box::<App>::default()),
    )
}
