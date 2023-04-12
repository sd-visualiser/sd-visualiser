use eframe::{
    egui::{
        util::cache::{ComputerMut, FrameCache},
        Context, Style, TextFormat,
    },
    emath::Align,
    epaint::{text::LayoutJob, Color32, Stroke},
};
use sd_core::language::{self, highlighter_config};
use tracing::{event, Level};
use tree_sitter_highlight::{Highlight, HighlightConfiguration, HighlightEvent};

pub struct Highlighter {
    config: HighlightConfiguration,
}

impl ComputerMut<(CodeTheme, &str), LayoutJob> for Highlighter {
    fn compute(&mut self, (theme, source): (CodeTheme, &str)) -> LayoutJob {
        event!(Level::DEBUG, "Highlighting");
        highlight(&self.config, theme, source)
    }
}

type HighlightCache<'a> = FrameCache<LayoutJob, Highlighter>;

impl Highlighter {
    pub fn new() -> Self {
        Highlighter {
            config: highlighter_config(),
        }
    }

    pub fn highlight(ctx: &Context, theme: CodeTheme, source: &str) -> LayoutJob {
        ctx.memory_mut(|mem| {
            mem.caches
                .cache::<HighlightCache<'_>>()
                .get((theme, source))
        })
    }
}

impl Default for Highlighter {
    fn default() -> Self {
        Self::new()
    }
}

fn highlight(config: &HighlightConfiguration, theme: CodeTheme, source: &str) -> LayoutJob {
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
                    highlight
                        .map(|highlight| theme.format_from_highlight(highlight))
                        .unwrap_or_default(),
                );
            }
        }
    }
    job
}

#[derive(Copy, Clone, Hash, Debug)]
pub struct CodeTheme {
    keyword_color: Color32,
    operator_color: Color32,
    variable_color: Color32,
    punctuation_color: Color32,
}

impl CodeTheme {
    pub fn from_style(style: &Style) -> Self {
        if style.visuals.dark_mode {
            Self::dark()
        } else {
            Self::light()
        }
    }

    pub fn dark() -> Self {
        Self {
            keyword_color: Color32::from_rgb(255, 100, 100),
            operator_color: Color32::LIGHT_GRAY,
            variable_color: Color32::from_rgb(87, 165, 171),
            punctuation_color: Color32::LIGHT_GRAY,
        }
    }

    pub fn light() -> Self {
        Self {
            keyword_color: Color32::from_rgb(235, 0, 0),
            operator_color: Color32::DARK_GRAY,
            variable_color: Color32::from_rgb(153, 134, 255),
            punctuation_color: Color32::DARK_GRAY,
        }
    }

    fn format_from_highlight(&self, highlight: Highlight) -> TextFormat {
        let color = match highlight.0 {
            0 => self.keyword_color,
            1 => self.operator_color,
            2 => self.variable_color,
            3 | 4 => self.punctuation_color,
            _ => panic!("Unexpected highlight"),
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
}
