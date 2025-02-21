#![warn(clippy::all, rust_2018_idioms)]
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

use std::path::PathBuf;

use clap::Parser;
use sd_core::lp::Solver;

#[derive(Parser)]
#[command(
    help_template = "\
{before-help}{name} {version}
{author-with-newline}
{about-with-newline}
{usage-heading} {usage}

{all-args}{after-help}
",
    author,
    version,
    about
)]
/// String diagram visualiser
///
/// Homepage: <https://sd-visualiser.github.io/sd-visualiser>
///
/// Please report bugs at <https://github.com/sd-visualiser/sd-visualiser/issues>.
struct Args {
    /// Read in a chil file
    #[arg(long, value_name = "FILE")]
    chil: Option<PathBuf>,

    /// Read in a spartan file
    #[arg(long, value_name = "FILE")]
    spartan: Option<PathBuf>,

    /// Read in an llvm ir file
    #[arg(long, value_name = "FILE")]
    llvm_ir: Option<PathBuf>,

    /// Read in an mlir file
    #[arg(long, value_name = "FILE")]
    mlir: Option<PathBuf>,

    /// Read in an dot file
    #[arg(long, value_name = "FILE")]
    dot: Option<PathBuf>,

    /// Choose LP solver
    #[arg(long, value_enum, default_value_t)]
    solver: Solver,
}

// When compiling natively:
#[cfg(not(target_arch = "wasm32"))]
fn main() -> anyhow::Result<()> {
    // Log to stdout (if you run with `RUST_LOG=debug`).

    use anyhow::anyhow;
    use egui::ViewportBuilder;
    tracing_subscriber::fmt::fmt()
        .with_env_filter(tracing_subscriber::filter::EnvFilter::from_default_env())
        .with_thread_names(true)
        .init();

    let args = Args::parse();

    tracing::info!("lp solver: {:?}", args.solver);

    let native_options = eframe::NativeOptions {
        viewport: ViewportBuilder {
            maximized: Some(true),
            ..Default::default()
        },
        ..Default::default()
    };

    let file = if let Some(path) = args.chil {
        let code = std::fs::read_to_string(path)?;
        Some((code, sd_gui::UiLanguage::Chil))
    } else if let Some(path) = args.spartan {
        let code = std::fs::read_to_string(path)?;
        Some((code, sd_gui::UiLanguage::Spartan))
    } else if let Some(path) = args.llvm_ir {
        let code = std::fs::read_to_string(path)?;
        Some((code, sd_gui::UiLanguage::LlvmIr))
    } else if let Some(path) = args.mlir {
        let code = std::fs::read_to_string(path)?;
        Some((code, sd_gui::UiLanguage::Mlir))
    } else if let Some(path) = args.dot {
        let code = std::fs::read_to_string(path)?;
        Some((code, sd_gui::UiLanguage::Dot))
    } else {
        None
    };
    eframe::run_native(
        "SD Visualiser",
        native_options,
        Box::new(move |cc| {
            let mut app = sd_gui::App::new(cc, args.solver);

            if let Some((code, language)) = file {
                app.set_file(&code, Some(language));
            }

            Ok(Box::new(app))
        }),
    )
    .map_err(|err| anyhow!("{}", err))?;

    Ok(())
}

// When compiling to web using trunk:
#[cfg(target_arch = "wasm32")]
fn main() {
    use eframe::{wasm_bindgen::JsCast as _, web_sys};
    // Redirect `log` message to `console.log` and friends:
    eframe::WebLogger::init(log::LevelFilter::Debug).ok();
    tracing_wasm::set_as_global_default();
    tracing::info!("lp solver: Clarabel");

    let web_options = eframe::WebOptions::default();

    wasm_bindgen_futures::spawn_local(async {
        let document = web_sys::window()
            .expect("No window")
            .document()
            .expect("No document");

        let canvas = document
            .get_element_by_id("the_canvas_id")
            .expect("Failed to find the_canvas_id")
            .dyn_into::<web_sys::HtmlCanvasElement>()
            .expect("the_canvas_id was not a HtmlCanvasElement");

        let start_result = eframe::WebRunner::new()
            .start(
                canvas,
                web_options,
                Box::new(|cc| Ok(Box::new(sd_gui::App::new(cc, Solver::Clarabel)))),
            )
            .await;

        // Remove the loading text and spinner:
        if let Some(loading_text) = document.get_element_by_id("loading_text") {
            match start_result {
                Ok(_) => {
                    loading_text.remove();
                }
                Err(e) => {
                    loading_text.set_inner_html(
                        "<p> The app has crashed. See the developer console for details. </p>",
                    );
                    panic!("Failed to start eframe: {e:?}");
                }
            }
        }
    });
}
