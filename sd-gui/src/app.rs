use std::sync::Arc;

use anyhow::anyhow;
use derivative::Derivative;
use eframe::{
    egui::{self, FontDefinitions},
    epaint::mutex::Mutex,
};
use egui_notify::Toasts;
use poll_promise::Promise;
use sd_core::{common::Direction, graph::SyntaxHyperGraph};

use crate::{
    code_ui::code_ui,
    graph_ui::GraphUi,
    parser::{parse, ParseError, ParseOutput, UiLanguage},
    selection::Selection,
    shape_generator::clear_shape_cache,
    squiggly_line::show_parse_error,
    subgraph_generator::clear_subgraph_cache,
};

#[derive(Derivative)]
#[derivative(Default)]
pub struct App {
    compile_requested: bool,
    #[derivative(Default(value = "true"))]
    editor: bool,
    code: String,
    #[allow(clippy::type_complexity)]
    last_parse: Option<Arc<Mutex<Promise<Result<ParseOutput, ParseError>>>>>,
    language: UiLanguage,
    graph_ui: Option<Promise<anyhow::Result<GraphUi>>>,
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
        self.compile_requested = true;
    }

    fn code_edit_ui(&mut self, ui: &mut egui::Ui) {
        let text_edit_out = code_ui(ui, &mut self.code, self.language);

        if text_edit_out.response.changed() {
            tracing::trace!("code changed changed");
            self.trigger_parse(ui.ctx());
        }
        if let Some(last_parse) = self
            .last_parse
            .as_ref()
            .and_then(|p| p.lock().ready()?.as_ref().err().cloned())
        {
            match last_parse {
                ParseError::Chil(err) => show_parse_error(ui, &err, &text_edit_out),
                ParseError::Spartan(err) => show_parse_error(ui, &err, &text_edit_out),
                ParseError::Conversion(_) => (),
            }
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

    fn trigger_parse(&mut self, ctx: &egui::Context) {
        let code = self.code.clone();
        let language = self.language;
        let ctx = ctx.clone();
        self.last_parse
            .replace(Arc::new(Mutex::new(Promise::spawn_thread(
                "parse",
                move || {
                    let parse = parse(&code, language);
                    ctx.request_repaint();
                    parse
                },
            ))));
    }

    fn trigger_compile(&mut self, ctx: &egui::Context) {
        clear_shape_cache();
        clear_subgraph_cache();
        self.trigger_parse(ctx);
        {
            let parse = self.last_parse.as_ref().unwrap().clone();
            let ctx = ctx.clone();
            self.graph_ui
                .replace(Promise::spawn_thread("compile", move || {
                    let promise = parse.lock();
                    let parse_output = promise
                        .block_until_ready()
                        .as_ref()
                        .map_err(|e| anyhow!("{}", e))?;
                    let compile = Ok(match parse_output {
                        ParseOutput::ChilExpr(expr) => {
                            tracing::debug!("Converting chil to hypergraph...");
                            GraphUi::new_chil(SyntaxHyperGraph::try_from(expr)?)
                        }
                        ParseOutput::SpartanExpr(expr) => {
                            tracing::debug!("Converting spartan to hypergraph...");
                            GraphUi::new_spartan(SyntaxHyperGraph::try_from(expr)?)
                        }
                    });
                    ctx.request_repaint();
                    compile
                }));
        }

        self.selections.clear();
    }
}

impl eframe::App for App {
    fn warm_up_enabled(&self) -> bool {
        self.compile_requested
    }

    #[allow(clippy::too_many_lines)]
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if self.compile_requested {
            self.compile_requested = false;
            self.trigger_compile(ctx);
        }

        egui::TopBottomPanel::top("menu").show(ctx, |ui| {
            egui::trace!(ui);
            ui.horizontal_wrapped(|ui| {
                macro_rules! button {
                    ($label:literal) => {
                        ui.button($label).clicked()
                    };
                    ($label:literal, $shortcut:expr) => {{
                        let shortcut =
                            egui::KeyboardShortcut::new(egui::Modifiers::NONE, $shortcut);
                        ui.add(
                            egui::Button::new($label).shortcut_text(ctx.format_shortcut(&shortcut)),
                        )
                        .clicked()
                            || ui.input_mut(|i| i.consume_shortcut(&shortcut))
                    }};
                    ($label:literal, $modifiers:expr, $shortcut:expr) => {{
                        let shortcut = egui::KeyboardShortcut::new($modifiers, $shortcut);
                        ui.add(
                            egui::Button::new($label).shortcut_text(ctx.format_shortcut(&shortcut)),
                        )
                        .clicked()
                            || ui.input_mut(|i| i.consume_shortcut(&shortcut))
                    }};
                }
                ui.visuals_mut().button_frame = false;

                if ui.selectable_label(self.editor, "Editor").clicked() {
                    self.editor = !self.editor;
                };

                egui::widgets::global_dark_light_mode_buttons(ui);

                ui.separator();

                ui.menu_button("Language", |ui| {
                    ui.radio_value(&mut self.language, UiLanguage::Chil, "Chil");
                    ui.radio_value(&mut self.language, UiLanguage::Spartan, "Spartan");
                });

                #[cfg(not(target_arch = "wasm32"))]
                if button!("Import file", egui::Modifiers::COMMAND, egui::Key::O) {
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

                if button!("Reset", egui::Key::Num0) {
                    if let Some(graph_ui) = finished_mut(&mut self.graph_ui) {
                        graph_ui.reset();
                    }
                }
                if button!("Zoom In", egui::Key::PlusEquals) {
                    if let Some(graph_ui) = finished_mut(&mut self.graph_ui) {
                        graph_ui.zoom_in();
                    }
                }
                if button!("Zoom Out", egui::Key::Minus) {
                    if let Some(graph_ui) = finished_mut(&mut self.graph_ui) {
                        graph_ui.zoom_out();
                    }
                }

                ui.separator();

                if button!("Compile", egui::Key::F5) {
                    self.trigger_compile(ui.ctx());
                }
                if let Some(err) = rejected(&self.graph_ui) {
                    self.toasts.error(err.to_string());
                    tracing::debug!("{}", err);
                }

                if button!("Save selection", egui::Modifiers::COMMAND, egui::Key::S) {
                    if let Some(graph_ui) = finished_mut(&mut self.graph_ui) {
                        self.selections.push(Selection::from_graph(
                            graph_ui,
                            format!("Selection {}", self.selections.len()),
                        ));
                        graph_ui.clear_selection();
                    }
                }
                if ui.button("Clear selection").clicked() {
                    if let Some(graph_ui) = finished_mut(&mut self.graph_ui) {
                        graph_ui.clear_selection();
                    }
                }
                ui.menu_button("Extend selection", |ui| {
                    for (label, direction) in [
                        ("Bidirectional", None),
                        ("Forward (1)", Some((Direction::Forward, 1))),
                        ("Forward", Some((Direction::Forward, usize::MAX))),
                        ("Backward (1)", Some((Direction::Backward, 1))),
                        ("Backward", Some((Direction::Backward, usize::MAX))),
                    ] {
                        if ui.button(label).clicked() {
                            if let Some(graph_ui) = finished_mut(&mut self.graph_ui) {
                                graph_ui.extend_selection(direction);
                            }
                        }
                    }
                });

                ui.separator();

                if let Some(graph_ui) = finished(&self.graph_ui)
                    .and_then(|graph_ui| graph_ui.ready().then_some(graph_ui))
                {
                    ui.add_enabled_ui(true, |ui| {
                        if ui.button("Export SVG").clicked() {
                            let svg = graph_ui.export_svg();
                            if let Some(path) = rfd::FileDialog::new().save_file() {
                                let _ = std::fs::write(path, svg);
                            }
                        }
                    });
                } else {
                    ui.add_enabled_ui(false, |ui| ui.button("Export SVG"));
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
            if self.editor {
                ui.columns(2, |columns| {
                    egui::ScrollArea::both()
                        .id_source("code")
                        .show(&mut columns[0], |ui| self.code_edit_ui(ui));
                    match self.graph_ui.as_mut().map(|p| p.ready_mut()?.as_mut().ok()) {
                        Some(Some(graph_ui)) => {
                            graph_ui.ui(&mut columns[1]);
                        }
                        Some(None) => {
                            // Compilation hasn't finished yet
                            columns[1].centered_and_justified(eframe::egui::Ui::spinner);
                        }
                        None => { /* No compilation triggered */ }
                    }
                });
            } else {
                match self.graph_ui.as_mut().map(|p| p.ready_mut()?.as_mut().ok()) {
                    Some(Some(graph_ui)) => {
                        graph_ui.ui(ui);
                    }
                    Some(None) => {
                        // Compilation hasn't finished yet
                        ui.centered_and_justified(eframe::egui::Ui::spinner);
                    }
                    None => { /* No compilation triggered */ }
                }
            }
        });

        self.toasts.show(ctx);
    }
}

#[allow(clippy::inline_always)]
#[inline(always)]
fn finished<T, E>(promise: &Option<Promise<Result<T, E>>>) -> Option<&T>
where
    T: Send,
    E: Send,
{
    promise.as_ref().and_then(|p| p.ready()?.as_ref().ok())
}

#[allow(clippy::inline_always)]
#[inline(always)]
fn finished_mut<T, E>(promise: &mut Option<Promise<Result<T, E>>>) -> Option<&mut T>
where
    T: Send,
    E: Send,
{
    promise.as_mut().and_then(|p| p.ready_mut()?.as_mut().ok())
}

#[allow(clippy::inline_always)]
#[inline(always)]
fn rejected<T, E>(promise: &Option<Promise<Result<T, E>>>) -> Option<&E>
where
    T: Send,
    E: Send,
{
    promise.as_ref().and_then(|p| p.ready()?.as_ref().err())
}
