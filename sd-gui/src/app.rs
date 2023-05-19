use anyhow::anyhow;
use eframe::{
    egui::{
        self, show_tooltip_at_pointer, text_edit::TextEditOutput, FontDefinitions, Id, RichText,
    },
    emath,
    epaint::{Color32, FontId, Pos2, QuadraticBezierShape, Rect, Rounding, Shape, Stroke, Vec2},
};
use egui_notify::Toasts;
use pest::error::LineColLocation;
use sd_core::{
    graph::{Op, SyntaxHyperGraph},
    monoidal::MonoidalGraph,
    monoidal_wired::MonoidalWiredGraph,
    prettyprinter::PrettyPrint,
};
use tracing::{debug, event, Level};

use crate::{
    highlighter::{highlight, CodeTheme},
    layout::Layouter,
    parser::{Language, ParseError, ParseOutput, Parser},
};

#[derive(Default)]
pub struct App {
    code: String,
    language: Language,
    hypergraph: SyntaxHyperGraph,
    monoidal_term: MonoidalWiredGraph<Op, ()>,
    monoidal_graph: MonoidalGraph<Op, ()>,
    panzoom: Panzoom,
    toasts: Toasts,
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

fn lines_contained(line_col: &LineColLocation) -> Vec<usize> {
    match line_col {
        LineColLocation::Pos((l, _)) => vec![l - 1],
        LineColLocation::Span((l1, _), (l2, _)) => (*l1..=*l2).collect(),
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

        App::default()
    }

    fn code_edit_ui(&mut self, ui: &mut egui::Ui) {
        let theme = CodeTheme::from_style(ui.style());

        let mut layouter = |ui: &egui::Ui, source: &str, wrap_width: f32| {
            let mut layout_job = highlight(ui.ctx(), &theme, source, self.language.name());
            layout_job.wrap.max_width = wrap_width;
            ui.fonts(|f| f.layout_job(layout_job))
        };

        let text_edit_out = egui::TextEdit::multiline(&mut self.code)
            .code_editor()
            .layouter(&mut layouter)
            .min_size(ui.available_size())
            .desired_width(f32::INFINITY)
            .show(ui);

        let parse = Parser::parse(ui.ctx(), &self.code, self.language);
        if let Err(ParseError::Chil(err)) = parse.as_ref() {
            self.code_edit_error_ui(ui, &text_edit_out, err.to_string(), &err.line_col)
        }
        if let Err(ParseError::Spartan(err)) = parse.as_ref() {
            self.code_edit_error_ui(ui, &text_edit_out, err.to_string(), &err.line_col)
        }
    }

    fn code_edit_error_ui(
        &self,
        ui: &mut egui::Ui,
        text_edit_out: &TextEditOutput,
        err: String,
        line_col: &LineColLocation,
    ) {
        let painter = ui.ctx().layer_painter(text_edit_out.response.layer_id);
        for l in lines_contained(line_col) {
            if let Some(row) = text_edit_out.galley.rows.get(l) {
                // Draw squiggly line under error line
                const SQUIGGLE_HEIGHT: f32 = 5.0;
                const SQUIGGLES_PER_CHAR: usize = 3;
                let left = row.rect.min.x;
                let right = row.rect.max.x;
                let base = row.rect.max.y;
                let count = row.glyphs.len() * SQUIGGLES_PER_CHAR;
                // Takes weighted average of 'left' and 'right' where
                // 0 <= i <= count
                let w_avg = |i: f32| {
                    let count_f = count as f32;
                    (left * (count_f - i) + right * i) / count_f
                };
                for i in 0..count {
                    let start: Pos2 =
                        Pos2::from((w_avg(i as f32), base)) + text_edit_out.text_draw_pos.to_vec2();
                    let control: Pos2 = Pos2::from((
                        w_avg(i as f32 + 0.5),
                        base + SQUIGGLE_HEIGHT * (((i + 1) % 2) as f32 - 0.5),
                    )) + text_edit_out.text_draw_pos.to_vec2();
                    let end: Pos2 = Pos2::from((w_avg((i + 1) as f32), base))
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

        event!(Level::DEBUG, "Converting to hypergraph");
        self.hypergraph = SyntaxHyperGraph::try_from(&expr)?;

        event!(Level::DEBUG, "Converting to monoidal term");
        self.monoidal_term = MonoidalWiredGraph::try_from(&self.hypergraph)?;
        event!(Level::DEBUG, "Got term {:#?}", self.monoidal_term);

        event!(Level::DEBUG, "Inserting swaps and copies");
        self.monoidal_graph = MonoidalGraph::from(&self.monoidal_term);
        event!(Level::DEBUG, "Got graph {:#?}", self.monoidal_graph);

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
                    self.panzoom = Default::default();
                }
                if ui.button("Zoom In").clicked() {
                    self.panzoom.zoom *= 1.25;
                }
                if ui.button("Zoom Out").clicked() {
                    self.panzoom.zoom /= 1.25;
                }

                ui.separator();

                if ui.button("Compile").clicked() {
                    if let Err(err) = self.compile(ui.ctx()) {
                        // Display error to user?
                        debug!("{:?}", err);
                    }
                }

                if ui.button("Gather").clicked() {
                    // if let Some((prefix, _selection)) = self.monoidal_graph.selected() {
                    //     let _graph = self.hypergraph.recurse(&prefix).unwrap();
                    //     // TODO(@calintat): Find the subgraph of graph containing the selected nodes.
                    // } else {
                    //     self.toasts.warning("No nodes selected");
                    // }
                }
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.columns(2, |columns| {
                egui::ScrollArea::vertical()
                    .id_source("code")
                    .show(&mut columns[0], |ui| self.code_edit_ui(ui));
                egui::ScrollArea::both()
                    .id_source("graph")
                    .show(&mut columns[1], |ui| self.graph_ui(ui));
            });
        });

        self.toasts.show(ctx);
    }
}
