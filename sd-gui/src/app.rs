use anyhow::anyhow;
use eframe::{
    egui::{self, show_tooltip_at_pointer, FontDefinitions, Id, RichText},
    emath,
    epaint::{FontId, Pos2, Rect, Rounding, Shape, Vec2},
};
use pest::error::LineColLocation;
use sd_core::{
    graph::HyperGraph,
    monoidal::{MonoidalGraph, MonoidalWiredGraph},
};
use tracing::{debug, event, Level};

use crate::{
    highlighter::{highlight, CodeTheme},
    layout::Layouter,
    parser::{ParseError, Parser},
};

pub struct App {
    code: String,
    parsed: bool,
    hypergraph: HyperGraph,
    monoidal_term: MonoidalWiredGraph,
    monoidal_graph: MonoidalGraph,
    panzoom: Panzoom,
}

struct Panzoom {
    translation: Vec2,
    zoom: f32,
}

impl Default for Panzoom {
    fn default() -> Self {
        Self {
            translation: Default::default(),
            zoom: 50.0,
        }
    }
}

fn is_in_line(cursor: usize, line_col: &LineColLocation) -> bool {
    // Pest lines are 1 indexed, egui are 0 ☹
    match line_col {
        LineColLocation::Pos((l, _)) => l - 1 == cursor,
        LineColLocation::Span((l1, _), (l2, _)) => l1 - 1 <= cursor && l2 - 1 <= cursor,
    }
}

impl App {
    /// Called once before the first frame.
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

        App {
            code: Default::default(),
            parsed: Default::default(),
            hypergraph: Default::default(),
            monoidal_term: Default::default(),
            monoidal_graph: Default::default(),
            panzoom: Default::default(),
        }
    }

    fn code_ui(&mut self, ui: &mut egui::Ui) {
        let theme = CodeTheme::from_style(ui.style());

        let mut layouter = |ui: &egui::Ui, source: &str, wrap_width: f32| {
            let mut layout_job = highlight(ui.ctx(), &theme, source, "sd");
            layout_job.wrap.max_width = wrap_width;
            ui.fonts(|f| f.layout_job(layout_job))
        };

        let text_edit_out = egui::TextEdit::multiline(&mut self.code)
            .code_editor()
            .layouter(&mut layouter)
            .min_size(ui.available_size())
            .show(ui);

        let parse = Parser::parse(ui.ctx(), &self.code);

        if let Some(x) = text_edit_out.response.hover_pos() {
            let pos = x - text_edit_out.text_draw_pos;

            if text_edit_out.galley.rect.contains((pos.x, pos.y).into()) {
                let cursor = text_edit_out.galley.cursor_from_pos(pos);
                event!(Level::DEBUG, "{:?}", cursor);
                if let Err(ParseError::PError(err)) = parse.as_ref() {
                    event!(Level::DEBUG, "{:?}", err);
                    if is_in_line(cursor.rcursor.row, &err.line_col) {
                        show_tooltip_at_pointer(ui.ctx(), Id::new("hover_tooltip"), |ui| {
                            ui.label(
                                RichText::new(format!("{}", err)).font(FontId::monospace(12.0)),
                            )
                        });
                    }
                }
            }
        }

        if text_edit_out.response.changed() {
            event!(Level::DEBUG, "Reparsing");
            let block = |app: &mut App| -> anyhow::Result<()> {
                let parse = Parser::parse(ui.ctx(), &app.code);
                let expr = parse.as_ref().as_ref().map_err(|e| anyhow!("{:?}", e))?;
                app.parsed = true;
                event!(Level::DEBUG, "Converting to hypergraph");
                app.hypergraph = expr.to_hypergraph()?;
                event!(Level::DEBUG, "Converting to monoidal term");
                app.monoidal_term = MonoidalWiredGraph::from_hypergraph(&app.hypergraph, &[])?;
                event!(Level::DEBUG, "Got term {:?}", app.monoidal_term);
                event!(Level::DEBUG, "Inserting swaps and copies");
                app.monoidal_graph = app.monoidal_term.to_graph(&[])?;
                event!(Level::DEBUG, "Got graph {:?}", app.monoidal_graph);
                Ok(())
            };
            if let Err(e) = block(self) {
                debug!("{:?}", e);
                // Display error to user?
                self.parsed = false;
            }
        }
    }

    fn graph_ui(&mut self, ui: &mut egui::Ui) {
        let (response, painter) =
            ui.allocate_painter(ui.available_size_before_wrap(), egui::Sense::drag());
        let to_screen = emath::RectTransform::from_to(
            Rect::from_min_size(Pos2::ZERO, response.rect.size()),
            response.rect.translate(self.panzoom.translation),
        );
        self.panzoom.translation += response.drag_delta();

        // Background
        painter.add(Shape::rect_filled(
            response.rect,
            Rounding::none(),
            ui.visuals().faint_bg_color,
        ));
        let layout = Layouter::layout(ui.ctx(), &self.monoidal_graph).unwrap();
        painter.extend(sd_graphics::render::render(
            ui,
            &response,
            &layout,
            self.panzoom.zoom,
            &mut self.monoidal_graph,
            response.rect.size(),
            to_screen,
        ));
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

                if ui.button("Reset").clicked() {
                    self.panzoom = Default::default();
                }
                if ui.button("Zoom In").clicked() {
                    self.panzoom.zoom *= 1.25;
                }
                if ui.button("Zoom Out").clicked() {
                    self.panzoom.zoom /= 1.25;
                }
            });
        });

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
