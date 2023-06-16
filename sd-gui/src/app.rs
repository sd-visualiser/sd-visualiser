use anyhow::anyhow;
use eframe::egui::{self, FontDefinitions};
use egui_notify::Toasts;
use sd_core::{graph::SyntaxHyperGraph, prettyprinter::PrettyPrint};
use tracing::debug;

use crate::{
    code_ui::code_ui,
    graph_ui::GraphUi,
    parser::{ParseError, ParseOutput, Parser, UiLanguage},
    selection::Selection,
    squiggly_line::show_parse_error,
};

#[derive(Default)]
pub struct App {
    code: String,
    language: UiLanguage,
    graph_ui: Option<GraphUi>,
    selections: Vec<Selection>,
    toasts: Toasts,
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

    pub fn set_file(&mut self, code: String, language: UiLanguage) {
        self.code = code;
        self.language = language;
        // Could be worth triggering a compile here
    }

    fn code_edit_ui(&mut self, ui: &mut egui::Ui) {
        let text_edit_out = code_ui(ui, &mut self.code, self.language);

        match Parser::parse(ui.ctx(), &self.code, self.language).as_ref() {
            Err(ParseError::Chil(err)) => show_parse_error(ui, err, &text_edit_out),
            Err(ParseError::Spartan(err)) => show_parse_error(ui, err, &text_edit_out),
            _ => (),
        }
    }

    fn selection_ui(&mut self, ui: &mut egui::Ui) {
        ui.vertical(|ui| {
            for selection in &mut self.selections {
                let name = selection.name().to_owned();
                ui.toggle_value(selection.displayed(), name);
            }
        });
    }

    fn compile(&mut self, ctx: &egui::Context) -> anyhow::Result<()> {
        let parse = Parser::parse(ctx, &self.code, self.language);
        match parse.as_ref().as_ref().map_err(|e| anyhow!("{}", e))? {
            ParseOutput::ChilExpr(expr) => {
                // Prettify the code.
                self.code = expr.to_pretty();
                debug!("Converting to hypergraph...");
                self.graph_ui = Some(GraphUi::new_chil(ctx, SyntaxHyperGraph::try_from(expr)?));
            }
            ParseOutput::SpartanExpr(expr) => {
                // Prettify the code.
                self.code = expr.to_pretty();
                debug!("Converting to hypergraph...");
                self.graph_ui = Some(GraphUi::new_spartan(ctx, SyntaxHyperGraph::try_from(expr)?));
            }
        }

        self.selections.clear();

        Ok(())
    }
}

impl eframe::App for App {
    #[allow(clippy::too_many_lines)]
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("menu").show(ctx, |ui| {
            egui::trace!(ui);
            ui.horizontal_wrapped(|ui| {
                ui.visuals_mut().button_frame = false;
                egui::widgets::global_dark_light_mode_buttons(ui);

                ui.separator();

                ui.menu_button("Language", |ui| {
                    ui.radio_value(&mut self.language, UiLanguage::Chil, "Chil");
                    ui.radio_value(&mut self.language, UiLanguage::Spartan, "Spartan");
                });

                #[cfg(not(target_arch = "wasm32"))]
                if ui.button("Import file").clicked() {
                    if let Some(path) = rfd::FileDialog::new().pick_file() {
                        let language = match path.extension() {
                            Some(ext) if ext == "sd" => UiLanguage::Spartan,
                            Some(ext) if ext == "chil" => UiLanguage::Chil,
                            Some(_) | None => self.language,
                        };
                        self.set_file(
                            std::fs::read_to_string(path)
                                .expect("file picker returned invalid path"),
                            language,
                        );
                    }
                }

                ui.separator();

                if ui.button("Reset").clicked() {
                    if let Some(graph_ui) = &mut self.graph_ui {
                        graph_ui.reset(ui.ctx());
                    }
                }
                if ui.button("Zoom In").clicked() {
                    if let Some(graph_ui) = &mut self.graph_ui {
                        graph_ui.zoom_in();
                    }
                }
                if ui.button("Zoom Out").clicked() {
                    if let Some(graph_ui) = &mut self.graph_ui {
                        graph_ui.zoom_out();
                    }
                }

                ui.separator();

                if ui.button("Compile").clicked() {
                    if let Err(err) = self.compile(ui.ctx()) {
                        self.toasts.error(err.to_string());
                        debug!("{}", err);
                    }
                }

                if ui.button("Save selection").clicked() {
                    if let Some(graph_ui) = &mut self.graph_ui {
                        self.selections.push(Selection::from_graph(
                            graph_ui,
                            format!("Selection {}", self.selections.len()),
                            ui.ctx(),
                        ));
                        graph_ui.clear_selection();
                    }
                }

                ui.separator();

                if ui.button("Export SVG").clicked() {
                    if let Some(graph_ui) = &mut self.graph_ui {
                        let svg = graph_ui.export_svg(ui.ctx());
                        if let Some(path) = rfd::FileDialog::new().save_file() {
                            let _ = std::fs::write(path, svg);
                        }
                    }
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
                if let Some(graph_ui) = &mut self.graph_ui {
                    graph_ui.ui(&mut columns[1]);
                }
            });
        });

        self.toasts.show(ctx);
    }
}
