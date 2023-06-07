use anyhow::anyhow;
use eframe::{
    egui::{
        self, show_tooltip_at_pointer, text_edit::TextEditOutput, FontDefinitions, Id, RichText,
    },
    epaint::{Color32, FontId, Pos2, QuadraticBezierShape, Stroke},
};
use egui_notify::Toasts;
use pest::error::LineColLocation;
use sd_core::{graph::SyntaxHyperGraph, prettyprinter::PrettyPrint};
use tracing::debug;

use crate::{
    code_ui::code_ui,
    graph_ui::GraphUi,
    parser::{Language, ParseError, ParseOutput, Parser},
    selection::Selection,
};

#[derive(Default)]
pub struct App {
    code: String,
    language: Language,
    graph_ui: GraphUi,
    selections: Vec<Selection>,
    toasts: Toasts,
}

fn is_in_line(cursor: usize, line_col: &LineColLocation) -> bool {
    // Pest lines are 1 indexed, egui are 0 ☹
    match line_col {
        LineColLocation::Pos((l, _)) => l - 1 == cursor,
        LineColLocation::Span((l1, _), (l2, _)) => l1 - 1 <= cursor && l2 - 1 <= cursor,
    }
}

fn lines_contained(line_col: &LineColLocation) -> Vec<usize> {
    match line_col {
        LineColLocation::Pos((l, _)) => vec![l - 1],
        LineColLocation::Span((l1, _), (l2, _)) => (*l1..=*l2).collect(),
    }
}

impl App {
    /// Called once before the first frame.
    #[must_use]
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        // This is also where you can customize the look and feel of egui using
        // `cc.egui_ctx.set_visuals` and `cc.egui_ctx.set_fonts`.

        // Load previous app state (if any).
        // Note that you must enable the `persistence` feature for this to work.
        // if let Some(storage) = cc.storage {
        //     return eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default();
        // }

        let font_name = "mono_font".to_owned();

        let mut font_definitions = FontDefinitions::default();

        font_definitions.font_data.insert(
            font_name.clone(),
            egui::FontData::from_static(include_bytes!("../assets/JetBrainsMonoNL-Regular.ttf")),
        );

        font_definitions
            .families
            .entry(egui::FontFamily::Monospace)
            .or_default()
            .insert(0, font_name);

        cc.egui_ctx.set_fonts(font_definitions);

        App::default()
    }

    fn code_edit_ui(&mut self, ui: &mut egui::Ui) {
        let text_edit_out = code_ui(ui, &mut self.code, self.language);

        let parse = Parser::parse(ui.ctx(), &self.code, self.language);
        if let Err(ParseError::Chil(err)) = parse.as_ref() {
            Self::code_edit_error_ui(ui, &text_edit_out, err.to_string(), &err.line_col);
        }
        if let Err(ParseError::Spartan(err)) = parse.as_ref() {
            Self::code_edit_error_ui(ui, &text_edit_out, err.to_string(), &err.line_col);
        }
    }

    fn code_edit_error_ui(
        ui: &mut egui::Ui,
        text_edit_out: &TextEditOutput,
        err: String,
        line_col: &LineColLocation,
    ) {
        let painter = ui.painter();
        for l in lines_contained(line_col) {
            if let Some(row) = text_edit_out.galley.rows.get(l) {
                // Draw squiggly line under error line
                const SQUIGGLE_HEIGHT: f32 = 5.0;
                const SQUIGGLES_PER_CHAR: usize = 3;
                let left = row.rect.min.x;
                let right = row.rect.max.x;
                let base = row.rect.max.y;
                let count = u16::try_from(row.glyphs.len() * SQUIGGLES_PER_CHAR).unwrap();
                // Takes weighted average of 'left' and 'right' where
                // 0 <= i <= count
                let w_avg = |i: f32| {
                    let count_f = f32::from(count);
                    (left * (count_f - i) + right * i) / count_f
                };
                for i in 0..count {
                    let start: Pos2 = Pos2::from((w_avg(f32::from(i)), base))
                        + text_edit_out.text_draw_pos.to_vec2();
                    let control: Pos2 = Pos2::from((
                        w_avg(f32::from(i) + 0.5),
                        base + SQUIGGLE_HEIGHT * (f32::from((i + 1) % 2) - 0.5),
                    )) + text_edit_out.text_draw_pos.to_vec2();
                    let end: Pos2 = Pos2::from((w_avg(f32::from(i + 1)), base))
                        + text_edit_out.text_draw_pos.to_vec2();
                    painter.add(QuadraticBezierShape {
                        points: [start, control, end],
                        closed: false,
                        fill: Color32::TRANSPARENT,
                        stroke: Stroke::new(1.0, ui.style().visuals.error_fg_color),
                    });
                }
            }
        }

        if let Some(x) = text_edit_out.response.hover_pos() {
            let pos = x - text_edit_out.text_draw_pos;

            if text_edit_out.galley.rect.contains((pos.x, pos.y).into()) {
                let cursor = text_edit_out.galley.cursor_from_pos(pos);
                if is_in_line(cursor.rcursor.row, line_col) {
                    show_tooltip_at_pointer(ui.ctx(), Id::new("hover_tooltip"), |ui| {
                        ui.label(RichText::new(err).font(FontId::monospace(13.5)))
                    });
                }
            }
        }
    }

    fn selection_ui(&mut self, ui: &mut egui::Ui) {
        ui.vertical(|ui| {
            for selection in &mut self.selections {
                ui.toggle_value(&mut selection.displayed, &selection.name);
            }
        });
    }

    fn compile(&mut self, ctx: &egui::Context) -> anyhow::Result<()> {
        let parse = Parser::parse(ctx, &self.code, self.language);
        let expr = match parse.as_ref().as_ref().map_err(|e| anyhow!("{:?}", e))? {
            ParseOutput::ChilExpr(expr) => {
                // Prettify the code.
                self.code = expr.to_pretty();
                expr.clone().into()
            }
            ParseOutput::SpartanExpr(expr) => {
                // Prettify the code.
                self.code = expr.to_pretty();
                expr.clone()
            }
        };

        debug!("Converting to hypergraph");
        let hypergraph = SyntaxHyperGraph::try_from(&expr)?;

        self.graph_ui.compile(hypergraph);

        self.selections.clear();

        Ok(())
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("menu").show(ctx, |ui| {
            egui::trace!(ui);
            ui.horizontal_wrapped(|ui| {
                ui.visuals_mut().button_frame = false;
                egui::widgets::global_dark_light_mode_buttons(ui);

                ui.separator();

                ui.menu_button("Language", |ui| {
                    ui.radio_value(&mut self.language, Language::Chil, "Chil");
                    ui.radio_value(&mut self.language, Language::Spartan, "Spartan");
                });

                ui.separator();

                if ui.button("Reset").clicked() {
                    self.graph_ui.reset();
                }
                if ui.button("Zoom In").clicked() {
                    self.graph_ui.zoom_in();
                }
                if ui.button("Zoom Out").clicked() {
                    self.graph_ui.zoom_out();
                }

                ui.separator();

                if ui.button("Compile").clicked() {
                    if let Err(err) = self.compile(ui.ctx()) {
                        self.toasts.error(err.to_string());
                        debug!("{:?}", err);
                    }
                }

                if ui.button("Save selection").clicked() {
                    self.selections.push(Selection::new(
                        &self.graph_ui.current_selection,
                        format!("Selection {}", self.selections.len()),
                        self.graph_ui.hypergraph(),
                    ));
                    self.graph_ui.current_selection.clear();
                }
            });
        });

        for selection in &mut self.selections {
            selection.ui(ctx);
        }

        egui::SidePanel::right("selection_panel").show(ctx, |ui| {
            egui::ScrollArea::vertical()
                .id_source("selections")
                .show(ui, |ui| self.selection_ui(ui));
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.columns(2, |columns| {
                egui::ScrollArea::both()
                    .id_source("code")
                    .show(&mut columns[0], |ui| self.code_edit_ui(ui));
                egui::ScrollArea::both()
                    .id_source("graph")
                    .show(&mut columns[1], |ui| self.graph_ui.ui(ui));
            });
        });

        self.toasts.show(ctx);
    }
}
