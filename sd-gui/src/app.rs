use std::{
    sync::{
        mpsc::{channel, Receiver, Sender},
        Arc, Mutex,
    },
    task::Poll,
};

use anyhow::anyhow;
use eframe::{
    egui::{self, FontDefinitions, TextBuffer},
    emath::Align2,
    epaint::Vec2,
};
use egui_notify::Toasts;
use poll_promise::Promise;
use sd_core::{common::Direction, graph::SyntaxHypergraph};

use crate::{
    code_ui::code_ui,
    graph_ui::GraphUi,
    parser::{parse, ParseError, ParseOutput, UiLanguage},
    selection::Selection,
    shape_generator::clear_shape_cache,
    squiggly_line::show_parse_error,
    subgraph_generator::clear_subgraph_cache,
};

#[derive(Debug, Clone)]
enum Message {
    Compile,
    SetLanguage(UiLanguage),
    ParseError(ParseError),
}

pub struct App {
    // message queue
    tx: Sender<Message>,
    rx: Receiver<Message>,
    about: bool,
    editor: bool,
    code: Arc<Mutex<String>>,
    last_parse: Option<Arc<Mutex<Promise<Option<ParseOutput>>>>>,
    last_parse_error: Option<ParseError>,
    language: UiLanguage,
    graph_ui: Option<Promise<anyhow::Result<GraphUi>>>,
    selections: Vec<Selection>,
    find: Option<String>,
    toasts: Toasts,
}

impl Default for App {
    fn default() -> Self {
        let (tx, rx) = channel();
        Self {
            tx,
            rx,
            about: Default::default(),
            editor: Default::default(),
            code: Arc::default(),
            last_parse: Option::default(),
            last_parse_error: Option::default(),
            language: UiLanguage::default(),
            graph_ui: Option::default(),
            selections: Vec::default(),
            find: None,
            toasts: Toasts::default(),
        }
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

    pub fn set_file(&mut self, code: &str, language: Option<UiLanguage>) {
        self.code.lock().unwrap().replace(code);
        if let Some(language) = language {
            self.tx
                .send(Message::SetLanguage(language))
                .expect("failed to send message");
        }
        self.tx
            .send(Message::Compile)
            .expect("failed to send message");
    }

    fn code_edit_ui(&mut self, ui: &mut egui::Ui) {
        let text_edit_out = code_ui(ui, &mut *self.code.lock().unwrap(), self.language);

        if text_edit_out.response.changed() {
            tracing::trace!("code changed changed");
            self.trigger_parse(ui.ctx());
        }
        if let Some(error) = &self.last_parse_error {
            match error {
                ParseError::Chil(err) => show_parse_error(ui, err, &text_edit_out),
                ParseError::Spartan(err) => show_parse_error(ui, err, &text_edit_out),
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
        let tx = self.tx.clone();
        let code = self.code.clone();
        let language = self.language;
        let ctx = ctx.clone();
        self.last_parse_error.take();
        self.last_parse
            .replace(Arc::new(Mutex::new(crate::spawn!("parse", {
                let guard = code.lock().unwrap();
                let parsed = parse(&guard, language);
                match parsed {
                    Ok(parse) => {
                        ctx.request_repaint();
                        Some(parse)
                    }
                    Err(err) => {
                        tx.send(Message::ParseError(err))
                            .expect("failed to send message");
                        None
                    }
                }
            }))));
    }

    fn trigger_compile(&mut self, ctx: &egui::Context) {
        clear_shape_cache();
        clear_subgraph_cache();
        self.trigger_parse(ctx);
        {
            let parse = self.last_parse.as_ref().unwrap().clone();
            let ctx = ctx.clone();
            self.graph_ui.replace(crate::spawn!("compile", {
                let promise = parse.lock().unwrap();
                let parse_output = promise
                    .block_until_ready()
                    .as_ref()
                    .ok_or_else(|| anyhow!("no parse"))?;
                let compile = Ok(match parse_output {
                    ParseOutput::ChilExpr(expr) => {
                        tracing::debug!("Converting chil to hypergraph...");
                        GraphUi::new_chil(SyntaxHypergraph::try_from(expr)?)
                    }
                    ParseOutput::SpartanExpr(expr) => {
                        tracing::debug!("Converting spartan to hypergraph...");
                        GraphUi::new_spartan(SyntaxHypergraph::try_from(expr)?)
                    }
                });
                ctx.request_repaint();
                compile
            }));
        }

        self.selections.clear();
        self.find = None;
    }
}

impl eframe::App for App {
    #[allow(clippy::too_many_lines)]
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // process messages sent asynchronously
        while let Ok(message) = self.rx.try_recv() {
            tracing::debug!("Got asynchronous message {message:?}");
            match message {
                Message::Compile => self.trigger_compile(ctx),
                Message::SetLanguage(language) => {
                    self.language = language;
                }
                Message::ParseError(err) => {
                    self.toasts.error(err.to_string());
                    tracing::debug!("{}", err);
                    self.last_parse_error.replace(err);
                }
            }
        }

        let mut find_request_focus = false;

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

                if button!("Import file", egui::Modifiers::COMMAND, egui::Key::O) {
                    #[cfg(not(target_arch = "wasm32"))]
                    if let Some(path) = rfd::FileDialog::new().pick_file() {
                        let language = match path.extension() {
                            Some(ext) if ext == "sd" => Some(UiLanguage::Spartan),
                            Some(ext) if ext == "chil" => Some(UiLanguage::Chil),
                            Some(_) | None => None,
                        };
                        self.set_file(
                            &std::fs::read_to_string(path)
                                .expect("file picker returned invalid path"),
                            language,
                        );
                    }

                    #[cfg(target_arch = "wasm32")]
                    {
                        let task = rfd::AsyncFileDialog::new().pick_file();
                        let tx = self.tx.clone();
                        let code = self.code.clone();
                        wasm_bindgen_futures::spawn_local(async move {
                            let file = task.await.unwrap();
                            tracing::trace!("got file name {:?}", file.file_name());
                            let language = match file.file_name().split('.').last() {
                                Some("chil") => Some(UiLanguage::Chil),
                                Some("sd") => Some(UiLanguage::Spartan),
                                Some(_) | None => None,
                            };
                            let contents = file.read().await;
                            if let Ok(string) = String::from_utf8(contents) {
                                code.lock().unwrap().replace(&string);
                                if let Some(language) = language {
                                    tx.send(Message::SetLanguage(language))
                                        .expect("failed to send message");
                                }
                                tx.send(Message::Compile).expect("failed to send message");
                            }
                        });
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

                if button!("Find", egui::Modifiers::COMMAND, egui::Key::F) {
                    self.find = Some(String::new());
                    find_request_focus = true;
                }

                if button!("Expand all") {
                    if let Some(graph_ui) = finished_mut(&mut self.graph_ui) {
                        graph_ui.set_expanded_all(true);
                        graph_ui.reset();
                    }
                }

                if button!("Collapse all") {
                    if let Some(graph_ui) = finished_mut(&mut self.graph_ui) {
                        graph_ui.set_expanded_all(false);
                        graph_ui.reset();
                    }
                }

                ui.separator();

                if button!("Compile", egui::Key::F5) {
                    self.trigger_compile(ui.ctx());
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

                #[cfg(not(target_arch = "wasm32"))]
                {
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
                }

                ui.separator();
                if ui.selectable_label(self.about, "About").clicked() {
                    self.about = !self.about;
                };
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
            macro_rules! optional_editor {
                ($graph:expr) => {
                    if self.editor {
                        ui.columns(2, |columns| {
                            egui::ScrollArea::both()
                                .id_source("code")
                                .show(&mut columns[0], |ui| self.code_edit_ui(ui));
                            $graph(&mut columns[1]);
                        });
                    } else {
                        $graph(ui);
                    }
                };
            }
            optional_editor!(|ui| {
                match self
                    .graph_ui
                    .as_mut()
                    .map(|p| p.poll_mut().map(Result::as_mut))
                {
                    Some(Poll::Ready(Ok(graph_ui))) => {
                        graph_ui.ui(ui);
                    }
                    Some(Poll::Pending) => {
                        ui.centered_and_justified(eframe::egui::Ui::spinner);
                    }
                    Some(Poll::Ready(Err(_))) | None => { /* No pending successful compilation */ }
                }
            });
        });

        let mut clear_find = false;
        if let Some((find, graph_ui)) = self.find.as_mut().zip(finished_mut(&mut self.graph_ui)) {
            egui::Window::new("find_panel")
                .movable(false)
                .resizable(false)
                .anchor(Align2::RIGHT_TOP, Vec2::default())
                .title_bar(false)
                .show(ctx, |ui| {
                    let response = ui.text_edit_singleline(find);
                    if find_request_focus {
                        response.request_focus();
                    }
                    ui.horizontal(|ui| {
                        if ui.button("Find").clicked() {
                            graph_ui.find_variable(find);
                            clear_find = true;
                        }
                        if ui.button("Cancel").clicked() {
                            clear_find = true;
                        }
                    })
                });
        }
        if clear_find {
            self.find = None;
        }

        if self.about {
            egui::Window::new("about")
                .title_bar(false)
                .resizable(false)
                .anchor(Align2::CENTER_CENTER, Vec2::default())
                .show(ctx, |ui| {
                    ui.heading(format!("SD Visualiser ({})", env!("CARGO_PKG_VERSION")));
                    ui.label("A string diagram visualiser.");
                    ui.label(format!("LP backend: {}", sd_graphics::LP_BACKEND));
                    ui.horizontal(|ui| {
                        ui.label("Homepage:");
                        ui.hyperlink(env!("CARGO_PKG_HOMEPAGE"));
                    });
                    ui.horizontal(|ui| {
                        ui.label("Repository:");
                        ui.hyperlink(env!("CARGO_PKG_REPOSITORY"));
                    });
                    ui.collapsing(format!("License ({})", env!("CARGO_PKG_LICENSE")), |ui| {
                        ui.label(include_str!("../../LICENSE"));
                    });
                });
        }

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
