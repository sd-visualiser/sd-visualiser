use eframe::{
    egui,
    egui::{TextBuffer, text_edit::TextEditOutput},
};

use crate::{
    highlighter::{CodeTheme, highlight},
    parser::UiLanguage,
};

pub fn code_ui(
    ui: &mut egui::Ui,
    code: &mut dyn TextBuffer,
    language: UiLanguage,
) -> TextEditOutput {
    let theme = CodeTheme::from_style(ui.style());

    let mut layouter = |ui: &egui::Ui, source: &str, _wrap_width: f32| {
        let layout_job = highlight(ui.ctx(), &theme, source, language.name());
        ui.fonts(|f| f.layout_job(layout_job))
    };

    let hint_text = format!("Type {} code here...", language.name());

    egui::TextEdit::multiline(code)
        .code_editor()
        .hint_text(hint_text)
        .desired_width(f32::INFINITY)
        .layouter(&mut layouter)
        .min_size(ui.available_size())
        .show(ui)
}
