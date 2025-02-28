use std::{
    sync::{
        Arc, Mutex,
        mpsc::{Receiver, Sender, channel},
    },
    task::Poll,
};

use anyhow::anyhow;
use eframe::{
    egui::{self, FontDefinitions},
    emath::{Align, Align2},
    epaint::{Vec2, vec2},
};
use egui_notify::Toasts;
use poll_promise::Promise;
#[cfg(not(target_arch = "wasm32"))]
use sd_core::language::llvm_ir::LlvmIrSettings;
use sd_core::{
    common::Direction,
    dot::{DotSettings, dot_to_graph},
    language::mlir::MlirSettings,
    lp::Solver,
};

use crate::{
    code_generator::clear_code_cache,
    code_ui::code_ui,
    graph_ui::GraphUi,
    parser::{ParseError, ParseOutput, UiLanguage, parse},
    selection::Selection,
    shape_generator::clear_shape_cache,
    squiggly_line::show_parse_error,
};

#[derive(Debug, Clone)]
enum Message {
    Compile,
    SetLanguage(UiLanguage),
    ParseError(ParseError),
    #[cfg(target_arch = "wasm32")]
    Unsupported,
}

pub struct App {
    // message queue
    tx: Sender<Message>,
    rx: Receiver<Message>,
    about: bool,
    editor: bool,
    #[cfg(target_arch = "wasm32")]
    unsupported: bool,
    code: Arc<Mutex<String>>,
    last_parse: Option<Arc<Mutex<Promise<Option<ParseOutput>>>>>,
    last_parse_error: Option<ParseError>,
    language: UiLanguage,
    dot_settings: DotSettings,
    #[cfg(not(target_arch = "wasm32"))]
    llvm_ir_settings: LlvmIrSettings,
    mlir_settings: MlirSettings,
    graph_ui: Option<Promise<anyhow::Result<GraphUi>>>,
    selections: Vec<Selection>,
    find: Option<(String, usize)>,
    toasts: Toasts,
    solver: Solver,
}

impl App {
    /// Called once before the first frame.
    #[must_use]
    pub fn new(cc: &eframe::CreationContext<'_>, solver: Solver) -> Self {
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
            egui::FontData::from_static(include_bytes!("../assets/JetBrainsMonoNL-Regular.ttf"))
                .into(),
        );

        font_definitions
            .families
            .entry(egui::FontFamily::Monospace)
            .or_default()
            .insert(0, font_name);

        cc.egui_ctx.set_fonts(font_definitions);

        let (tx, rx) = channel();
        Self {
            tx,
            rx,
            about: Default::default(),
            editor: Default::default(),
            #[cfg(target_arch = "wasm32")]
            unsupported: Default::default(),
            code: Arc::default(),
            last_parse: Option::default(),
            last_parse_error: Option::default(),
            language: UiLanguage::default(),
            dot_settings: DotSettings::default(),
            #[cfg(not(target_arch = "wasm32"))]
            llvm_ir_settings: LlvmIrSettings::default(),
            mlir_settings: MlirSettings::default(),
            graph_ui: Option::default(),
            selections: Vec::default(),
            find: None,
            toasts: Toasts::default(),
            solver,
        }
    }

    pub fn set_file(&mut self, code: &str, language: Option<UiLanguage>) {
        *self.code.lock().unwrap() = code.to_string();
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
            self.trigger_parse(ui.ctx(), false);
        }
        if let Some(error) = &self.last_parse_error {
            match error {
                ParseError::Chil(err) => show_parse_error(ui, err, &text_edit_out),
                ParseError::Mlir(err) => show_parse_error(ui, err, &text_edit_out),
                ParseError::Spartan(err) => show_parse_error(ui, err, &text_edit_out),
                #[cfg(not(target_arch = "wasm32"))]
                ParseError::LlvmIr(_) => (),
                ParseError::Dot(_) | ParseError::Conversion(_) => (),
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

    fn trigger_parse(&mut self, ctx: &egui::Context, send_error: bool) {
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
                        if send_error {
                            tx.send(Message::ParseError(err))
                                .expect("failed to send message");
                        }
                        None
                    }
                }
            }))));
    }

    fn trigger_compile(&mut self, ctx: &egui::Context) {
        clear_shape_cache();
        self.trigger_parse(ctx, true);
        {
            let parse = self.last_parse.as_ref().unwrap().clone();
            let ctx = ctx.clone();
            let dot_settings = self.dot_settings;
            #[cfg(not(target_arch = "wasm32"))]
            let llvm_ir_settings = self.llvm_ir_settings;
            let mlir_settings = self.mlir_settings;
            let solver = self.solver;
            self.graph_ui.replace(crate::spawn!("compile", {
                let promise = parse.lock().unwrap();
                let parse_output = promise
                    .block_until_ready()
                    .as_ref()
                    .ok_or_else(|| anyhow!("no parse"))?;
                let compile = Ok(match parse_output {
                    ParseOutput::Chil(expr) => {
                        tracing::debug!("Converting chil to hypergraph...");
                        GraphUi::new_chil(expr.to_graph(false)?, solver)
                    }
                    #[cfg(not(target_arch = "wasm32"))]
                    ParseOutput::LlvmIr(expr) => {
                        tracing::debug!("Converting llvm ir to hypergraph...");
                        GraphUi::new_llvm_ir(
                            expr.to_graph(llvm_ir_settings.sym_name_linking)?,
                            solver,
                        )
                    }
                    ParseOutput::Mlir(expr) => {
                        tracing::debug!("Converting mlir to hypergraph...");
                        GraphUi::new_mlir(expr.to_graph(mlir_settings.sym_name_linking)?, solver)
                    }
                    ParseOutput::Spartan(expr) => {
                        tracing::debug!("Converting spartan to hypergraph...");
                        GraphUi::new_spartan(expr.to_graph(false)?, solver)
                    }
                    ParseOutput::Dot(graph) => {
                        tracing::debug!("Converting dot to hypergraph...");
                        GraphUi::new_dot(dot_to_graph(graph, dot_settings)?, solver)
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
                #[cfg(target_arch = "wasm32")]
                Message::Unsupported => {
                    self.unsupported = true;
                }
            }
        }

        let mut find_request_focus = false;

        egui::TopBottomPanel::top("menu").show(ctx, |ui| {
            // Code below is copied from ui.horizontal_wrapped,
            // except we add the with_main_align to get button shortcut text to work properly
            let initial_size = vec2(
                ui.available_size_before_wrap().x,
                ui.spacing().interact_size.y, // Assume there will be something interactive on the horizontal layout
            );

            let layout = if ui.layout().prefer_right_to_left() {
                egui::Layout::right_to_left(Align::Center)
            } else {
                egui::Layout::left_to_right(Align::Center)
            }
            .with_main_wrap(true)
            .with_main_align(Align::Min);

            #[allow(clippy::cognitive_complexity)]
            ui.allocate_ui_with_layout(initial_size, layout, |ui| {
                macro_rules! button {
                    ($label:literal) => {
                        button!($label, enabled = true)
                    };
                    ($label:literal, enabled = $enabled:expr) => {
                        ui.add_enabled($enabled, egui::Button::new($label))
                            .clicked()
                    };
                    ($label:literal, $shortcut:expr) => {{
                        button!($label, egui::Modifiers::NONE, $shortcut, enabled = true)
                    }};
                    ($label:literal, $shortcut:expr, enabled = $enabled:expr) => {{
                        button!($label, egui::Modifiers::NONE, $shortcut, enabled = $enabled)
                    }};
                    ($label:literal, $modifiers:expr, $shortcut:expr) => {{
                        button!($label, $modifiers, $shortcut, enabled = true)
                    }};
                    ($label:literal, $modifiers:expr, $shortcut:expr, enabled = $enabled:expr) => {{
                        let shortcut = egui::KeyboardShortcut::new($modifiers, $shortcut);
                        ui.add_enabled(
                            $enabled,
                            egui::Button::new($label).shortcut_text(ctx.format_shortcut(&shortcut)),
                        )
                        .clicked()
                            || ui.input_mut(|i| i.consume_shortcut(&shortcut))
                    }};
                }
                ui.visuals_mut().button_frame = false;
                ui.style_mut().wrap_mode = Some(egui::TextWrapMode::Truncate);

                if ui.selectable_label(self.editor, "Editor").clicked() {
                    self.editor = !self.editor;
                };

                egui::widgets::global_theme_preference_buttons(ui);

                ui.separator();

                ui.menu_button("Language", |ui| {
                    ui.radio_value(&mut self.language, UiLanguage::Chil, "Chil");
                    #[cfg(not(target_arch = "wasm32"))]
                    ui.radio_value(&mut self.language, UiLanguage::LlvmIr, "LlvmIr");
                    #[cfg(target_arch = "wasm32")]
                    ui.add_enabled_ui(false, |ui| {
                        ui.radio_value(&mut self.language, UiLanguage::LlvmIr, "LlvmIr (unsupported on web)");
                    });
                    ui.radio_value(&mut self.language, UiLanguage::Mlir, "Mlir");
                    ui.radio_value(&mut self.language, UiLanguage::Spartan, "Spartan");
                    ui.radio_value(&mut self.language, UiLanguage::Dot, "Dot");
                });

                match self.language {
                    UiLanguage::Dot => {
                        ui.menu_button("Settings", |ui| {
                            if ui
                                .selectable_label(self.dot_settings.invert, "Invert edges")
                                .clicked()
                            {
                                self.dot_settings.invert = !self.dot_settings.invert;
                                self.tx
                                    .send(Message::Compile)
                                    .expect("Failed to send message");
                            }
                            if ui
                                .selectable_label(self.dot_settings.collect, "Collect edges")
                                .clicked()
                            {
                                self.dot_settings.collect = !self.dot_settings.collect;
                                self.tx
                                    .send(Message::Compile)
                                    .expect("Failed to send message");
                            }
                        });
                    }
                    #[cfg(not(target_arch = "wasm32"))]
                    UiLanguage::LlvmIr => {
                        ui.menu_button("Settings", |ui| {
                            if ui
                                .selectable_label(
                                    self.llvm_ir_settings.sym_name_linking,
                                    "Link symbols",
                                )
                                .clicked()
                            {
                                self.llvm_ir_settings.sym_name_linking =
                                    !self.llvm_ir_settings.sym_name_linking;
                                self.tx
                                    .send(Message::Compile)
                                    .expect("Failed to send message");
                            }
                        });
                    }
                    UiLanguage::Mlir => {
                        ui.menu_button("Settings", |ui| {
                            if ui
                                .selectable_label(
                                    self.mlir_settings.sym_name_linking,
                                    "Link symbols",
                                )
                                .clicked()
                            {
                                self.mlir_settings.sym_name_linking =
                                    !self.mlir_settings.sym_name_linking;
                                self.tx
                                    .send(Message::Compile)
                                    .expect("Failed to send message");
                            }
                        });
                    }
                    _ => (),
                }

                if button!("Import file", egui::Modifiers::COMMAND, egui::Key::O) {
                    #[cfg(not(target_arch = "wasm32"))]
                    if let Some(path) = rfd::FileDialog::new().pick_file() {
                        let language = match path.extension() {
                            Some(ext) if ext == "chil" => Some(UiLanguage::Chil),
                            Some(ext) if ext == "ll" => Some(UiLanguage::LlvmIr),
                            Some(ext) if ext == "mlir" => Some(UiLanguage::Mlir),
                            Some(ext) if ext == "sd" => Some(UiLanguage::Spartan),
                            Some(ext) if ext == "dot" => Some(UiLanguage::Dot),
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
                            if let Some(file) = task.await {
                                tracing::trace!("got file name {:?}", file.file_name());
                                let name = file.file_name();
                                let ext = name.split('.').last();
                                if let Some("ll") = ext {
                                    tracing::error!("LLVM IR is not supported in the web version");
                                    tx.send(Message::Unsupported)
                                        .expect("failed to send message");
                                    return;
                                }
                                let language = match ext {
                                    Some("chil") => Some(UiLanguage::Chil),
                                    Some("ll") => Some(UiLanguage::LlvmIr),
                                    Some("mlir") => Some(UiLanguage::Mlir),
                                    Some("sd") => Some(UiLanguage::Spartan),
                                    Some("dot") => Some(UiLanguage::Dot),
                                    Some(_) | None => None,
                                };
                                let contents = file.read().await;
                                if let Ok(string) = String::from_utf8(contents) {
                                    *code.lock().unwrap() = string.to_owned();
                                    if let Some(language) = language {
                                        tx.send(Message::SetLanguage(language))
                                            .expect("failed to send message");
                                    }
                                    tx.send(Message::Compile).expect("failed to send message");
                                }
                            }
                        });
                    }
                }

                ui.separator();

                // will be true if any graph is currently being drawn
                let ready = finished(&self.graph_ui)
                    .map(GraphUi::ready)
                    .unwrap_or_default();
                let has_selections = finished(&self.graph_ui)
                    .map(|graph_ui| !graph_ui.is_empty())
                    .unwrap_or_default();
                if button!(
                    "Reset",
                    egui::Modifiers::COMMAND,
                    egui::Key::Num0,
                    enabled = ready
                ) {
                    if let Some(graph_ui) = finished_mut(&mut self.graph_ui) {
                        graph_ui.reset();
                    }
                }
                if button!("Zoom In", egui::Key::Plus, enabled = ready) {
                    if let Some(graph_ui) = finished_mut(&mut self.graph_ui) {
                        graph_ui.zoom_in();
                    }
                }
                if button!("Zoom Out", egui::Key::Minus, enabled = ready) {
                    if let Some(graph_ui) = finished_mut(&mut self.graph_ui) {
                        graph_ui.zoom_out();
                    }
                }

                if button!(
                    "Find",
                    egui::Modifiers::COMMAND,
                    egui::Key::F,
                    enabled = ready
                ) {
                    self.find = Some((String::new(), 0));
                    find_request_focus = true;
                }

                if button!("Expand all", enabled = ready) {
                    if let Some(graph_ui) = finished_mut(&mut self.graph_ui) {
                        graph_ui.set_expanded_all(true);
                        graph_ui.reset();
                    }
                }

                if button!("Collapse all", enabled = ready) {
                    if let Some(graph_ui) = finished_mut(&mut self.graph_ui) {
                        graph_ui.set_expanded_all(false);
                        graph_ui.reset();
                    }
                }

                ui.separator();

                if button!("Compile", egui::Key::F5) {
                    self.tx
                        .send(Message::Compile)
                        .expect("failed to send message");
                }

                if button!(
                    "Save selection",
                    egui::Modifiers::COMMAND,
                    egui::Key::S,
                    enabled = ready && has_selections
                ) {
                    if let Some(graph_ui) = finished_mut(&mut self.graph_ui) {
                        if let Some(sel) = Selection::from_graph(
                            graph_ui,
                            format!("Selection {}", self.selections.len()),
                            self.solver,
                        ) {
                            self.selections.push(sel);
                        }
                        graph_ui.clear_selection();
                    }
                }
                if button!("Clear selection", enabled = ready && has_selections) {
                    if let Some(graph_ui) = finished_mut(&mut self.graph_ui) {
                        clear_code_cache();
                        graph_ui.clear_selection();
                    }
                }
                ui.add_enabled_ui(ready && has_selections, |ui| {
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
                });

                ui.separator();
                if button!("Export SVG", enabled = ready) {
                    if let Some(graph_ui) = finished(&self.graph_ui) {
                        let svg = graph_ui.export_svg();
                        #[cfg(not(target_arch = "wasm32"))]
                        {
                            if let Some(path) = rfd::FileDialog::new()
                                .add_filter("svg", &["svg"])
                                .set_title("Export SVG")
                                .set_file_name("graph.svg")
                                .save_file()
                            {
                                let _ = std::fs::write(path, svg);
                            }
                        }
                        #[cfg(target_arch = "wasm32")]
                        {
                            let task = rfd::AsyncFileDialog::new()
                                .add_filter("svg", &["svg"])
                                .set_title("Export SVG")
                                .set_file_name("graph.svg")
                                .save_file();
                            wasm_bindgen_futures::spawn_local(async move {
                                if let Some(path) = task.await {
                                    let _ = path.write(svg.as_bytes()).await;
                                }
                            });
                        }
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
                .id_salt("selections")
                .show(ui, |ui| self.selection_ui(ui));
        });

        #[allow(clippy::redundant_closure_call)]
        egui::CentralPanel::default().show(ctx, |ui| {
            macro_rules! optional_editor {
                ($graph:expr) => {
                    if self.editor {
                        ui.columns(2, |columns| {
                            egui::ScrollArea::both()
                                .id_salt("code")
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
                        graph_ui.ui(ui, self.find.as_ref().map(|x| x.0.as_str()));
                    }
                    Some(Poll::Pending) => {
                        ui.centered_and_justified(eframe::egui::Ui::spinner);
                    }
                    Some(Poll::Ready(Err(_))) | None => { /* No pending successful compilation */ }
                }
            });
        });

        let mut clear_find = false;
        if let Some(((query, offset), graph_ui)) =
            self.find.as_mut().zip(finished_mut(&mut self.graph_ui))
        {
            egui::Window::new("find_panel")
                .movable(false)
                .resizable(false)
                .anchor(Align2::RIGHT_TOP, Vec2::default())
                .title_bar(false)
                .show(ctx, |ui| {
                    let response = ui.text_edit_singleline(query);
                    if find_request_focus {
                        response.request_focus();
                    }
                    if response.changed() {
                        *offset = 0;
                    }
                    ui.horizontal(|ui| {
                        if ui.button("Find").clicked() {
                            graph_ui.find(query, *offset);
                            *offset += 1;
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
                    ui.label(format!("LP backend: {:?}", self.solver));
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

        #[cfg(target_arch = "wasm32")]
        if self.unsupported {
            let modal = egui::Modal::new(egui::Id::new("unsupported")).show(ctx, |ui| {
                ui.heading("Unsupported");
                ui.label("LLVM IR is not supported in the web version.");
                ui.horizontal(|ui| {
                    ui.label("Please download the desktop version from:");
                    ui.hyperlink(env!("CARGO_PKG_REPOSITORY"));
                });
                ui.separator();
                egui::Sides::new().show(
                    ui,
                    |_ui| {},
                    |ui| {
                        if ui.button("OK").clicked() {
                            self.unsupported = false;
                        }
                    },
                );
            });
            if modal.should_close() {
                self.unsupported = false;
            }
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
