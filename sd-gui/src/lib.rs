#![warn(clippy::all, rust_2018_idioms)]
mod app;
pub(crate) mod code_ui;
pub(crate) mod graph_ui;
pub(crate) mod highlighter;
pub(crate) mod panzoom;
pub(crate) mod parser;
pub(crate) mod selection;
pub(crate) mod shape_generator;
pub(crate) mod squiggly_line;

pub use app::App;
pub use parser::UiLanguage;

#[cfg(not(target_arch = "wasm32"))]
macro_rules! spawn {
    ($name:expr, $body:block) => {
        poll_promise::Promise::spawn_thread($name, move || $body)
    };
}
#[cfg(target_arch = "wasm32")]
macro_rules! spawn {
    ($name:expr, $body:block) => {
        poll_promise::Promise::spawn_local(async move { $body })
    };
}
pub(crate) use spawn;
