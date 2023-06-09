#![warn(clippy::all, rust_2018_idioms)]
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
struct Args {
    /// Print version of the tool
    #[arg(short, long)]
    version: bool,

    /// Read in a chil file
    #[arg(long, value_name = "FILE")]
    chil: Option<PathBuf>,

    /// Read in a spartan file
    #[arg(long, value_name = "FILE")]
    spartan: Option<PathBuf>,
}

// When compiling natively:
#[cfg(not(target_arch = "wasm32"))]
fn main() -> anyhow::Result<()> {
    // Log to stdout (if you run with `RUST_LOG=debug`).

    use anyhow::anyhow;
    tracing_subscriber::fmt::init();

    let args = Args::parse();

    if args.version {
        println!("sd visualiser: 0.1.0");
        anyhow::Result::Ok(())
    } else {
        let native_options = eframe::NativeOptions::default();

        let file = if let Some(path) = args.chil {
            let code = std::fs::read_to_string(path)?;
            Some((code, sd_gui::Language::Chil))
        } else if let Some(path) = args.spartan {
            let code = std::fs::read_to_string(path)?;
            Some((code, sd_gui::Language::Spartan))
        } else {
            None
        };
        eframe::run_native(
            "SD Visualiser",
            native_options,
            Box::new(|cc| {
                let mut app = sd_gui::App::new(cc);

                if let Some((code, language)) = file {
                    app.set_file(code, language);
                }

                Box::new(app)
            }),
        )
        .map_err(|err| anyhow!("{}", err))?;

        Ok(())
    }
}

// when compiling to web using trunk.
#[cfg(target_arch = "wasm32")]
fn main() {
    // Make sure panics are logged using `console.error`.
    console_error_panic_hook::set_once();

    // Redirect tracing to console.log and friends:
    tracing_wasm::set_as_global_default();

    let web_options = eframe::WebOptions::default();

    wasm_bindgen_futures::spawn_local(async {
        eframe::start_web(
            "the_canvas_id", // hardcode it
            web_options,
            Box::new(|cc| Box::new(sd_gui::App::new(cc))),
        )
        .await
        .expect("failed to start eframe");
    });
}
