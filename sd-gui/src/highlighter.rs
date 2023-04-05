use eframe::{
    egui::{
        util::cache::{ComputerMut, FrameCache},
        Context, TextFormat,
    },
    emath::Align,
    epaint::{text::LayoutJob, Color32, Stroke},
};
use sd_core::language::{self, highlighter_config};
use tree_sitter_highlight::{Highlight, HighlightConfiguration, HighlightEvent};

pub struct Highlighter {
    config: HighlightConfiguration,
}

impl ComputerMut<&str, LayoutJob> for Highlighter {
    fn compute(&mut self, source: &str) -> LayoutJob {
        highlight(&self.config, source)
    }
}

type HighlightCache<'a> = FrameCache<LayoutJob, Highlighter>;

impl Highlighter {
    pub fn new() -> Self {
        Highlighter {
            config: highlighter_config(),
        }
    }

    pub fn highlight(ctx: &Context, source: &str) -> LayoutJob {
        ctx.memory_mut(|mem| mem.caches.cache::<HighlightCache<'_>>().get(source))
    }
}

impl Default for Highlighter {
    fn default() -> Self {
        Self::new()
    }
}

pub(crate) fn highlight(config: &HighlightConfiguration, source: &str) -> LayoutJob {
    let mut job = LayoutJob::default();
    let mut highlight: Option<Highlight> = None;
    for event in language::highlight(config, source) {
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
