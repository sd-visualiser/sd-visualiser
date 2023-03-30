use eframe::{
    egui::TextFormat,
    emath::Align,
    epaint::{text::LayoutJob, Color32, Stroke},
};
use sd_core::language;
use tree_sitter_highlight::{Highlight, HighlightEvent};

pub(crate) fn highlight(source: &str) -> LayoutJob {
    let mut job = LayoutJob::default();
    let mut highlight: Option<Highlight> = None;
    for event in language::highlight(source) {
        match event {
            HighlightEvent::HighlightStart(h) => highlight = Some(h),
            HighlightEvent::HighlightEnd => highlight = None,
            HighlightEvent::Source { start, end } => {
                job.append(
                    &source[start..end],
                    0.0,
                    highlight.map(format_from_highlight).unwrap_or_default(),
                );
            }
        }
    }
    job
}

fn format_from_highlight(highlight: Highlight) -> TextFormat {
    let color = if highlight.0 <= 1 {
        // keywords and operators
        Color32::YELLOW
    } else {
        Color32::GRAY
    };

    TextFormat {
        font_id: Default::default(),
        color,
        background: Color32::TRANSPARENT,
        italics: false,
        underline: Stroke::NONE,
        strikethrough: Stroke::NONE,
        valign: Align::BOTTOM,
    }
}
