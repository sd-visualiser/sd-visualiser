use std::ops::Range;

use eframe::{
    egui,
    egui::TextFormat,
    epaint::{
        text::{LayoutJob, LayoutSection},
        FontId,
    },
};
use syntect::{
    easy::HighlightLines,
    highlighting::{FontStyle, ThemeSet},
    parsing::{SyntaxDefinition, SyntaxSet, SyntaxSetBuilder},
    util::LinesWithEndings,
};

/// Memoized syntax highlighting
pub fn highlight(ctx: &egui::Context, theme: &CodeTheme, code: &str, language: &str) -> LayoutJob {
    impl egui::util::cache::ComputerMut<(&CodeTheme, &str, &str), LayoutJob> for Highlighter {
        fn compute(&mut self, (theme, code, language): (&CodeTheme, &str, &str)) -> LayoutJob {
            self.highlight(theme, code, language)
        }
    }

    type HighlightCache = egui::util::cache::FrameCache<LayoutJob, Highlighter>;

    ctx.memory_mut(|mem| {
        mem.caches
            .cache::<HighlightCache>()
            .get((theme, code, language))
    })
}

// ----------------------------------------------------------------------------

#[derive(Clone, Hash, Debug)]
pub struct CodeTheme {
    dark_mode: bool,
    syntect_theme: String,
}

impl Default for CodeTheme {
    fn default() -> Self {
        Self::dark()
    }
}

impl CodeTheme {
    pub fn from_style(style: &egui::Style) -> Self {
        if style.visuals.dark_mode {
            Self::dark()
        } else {
            Self::light()
        }
    }

    pub fn dark() -> Self {
        Self {
            dark_mode: true,
            syntect_theme: "Solarized (dark)".to_string(),
        }
    }

    pub fn light() -> Self {
        Self {
            dark_mode: false,
            syntect_theme: "Solarized (light)".to_string(),
        }
    }
}

// ----------------------------------------------------------------------------

pub const CHIL_SYNTAX: &str = include_str!("highlighter/chil.sublime-syntax");
pub const SPARTAN_SYNTAX: &str = include_str!("highlighter/spartan.sublime-syntax");

struct Highlighter {
    themes: ThemeSet,
    syntaxes: SyntaxSet,
}

impl Default for Highlighter {
    fn default() -> Self {
        let syntaxes = {
            let mut builder = SyntaxSetBuilder::new();
            builder.add(SyntaxDefinition::load_from_str(CHIL_SYNTAX, true, None).unwrap());
            builder.add(SyntaxDefinition::load_from_str(SPARTAN_SYNTAX, true, None).unwrap());
            builder.build()
        };

        Self {
            syntaxes,
            themes: ThemeSet::load_defaults(),
        }
    }
}

impl Highlighter {
    fn highlight(&self, theme: &CodeTheme, code: &str, language: &str) -> LayoutJob {
        self.highlight_impl(theme, code, language)
            .unwrap_or_else(|| {
                // Fallback:
                LayoutJob::simple(
                    code.into(),
                    Default::default(),
                    if theme.dark_mode {
                        egui::Color32::LIGHT_GRAY
                    } else {
                        egui::Color32::DARK_GRAY
                    },
                    f32::INFINITY,
                )
            })
    }

    fn highlight_impl(&self, theme: &CodeTheme, code: &str, language: &str) -> Option<LayoutJob> {
        let syntax = self
            .syntaxes
            .find_syntax_by_name(language)
            .or_else(|| self.syntaxes.find_syntax_by_extension(language))?;

        let theme = &theme.syntect_theme;
        let mut h = HighlightLines::new(syntax, &self.themes.themes[theme]);

        let mut job = LayoutJob {
            text: code.into(),
            ..Default::default()
        };

        for line in LinesWithEndings::from(code) {
            for (style, range) in h.highlight_line(line, &self.syntaxes).ok()? {
                let fg = style.foreground;
                let text_color = egui::Color32::from_rgb(fg.r, fg.g, fg.b);
                let italics = style.font_style.contains(FontStyle::ITALIC);
                let underline = style.font_style.contains(FontStyle::ITALIC);
                let underline = if underline {
                    egui::Stroke::new(1.0, text_color)
                } else {
                    egui::Stroke::NONE
                };
                job.sections.push(LayoutSection {
                    leading_space: 0.0,
                    byte_range: as_byte_range(code, range),
                    format: TextFormat {
                        font_id: FontId::monospace(13.5),
                        color: text_color,
                        italics,
                        underline,
                        ..Default::default()
                    },
                });
            }
        }

        Some(job)
    }
}

fn as_byte_range(whole: &str, range: &str) -> Range<usize> {
    let whole_start = whole.as_ptr() as usize;
    let range_start = range.as_ptr() as usize;
    assert!(whole_start <= range_start);
    assert!(range_start + range.len() <= whole_start + whole.len());
    let offset = range_start - whole_start;
    offset..offset + range.len()
}
