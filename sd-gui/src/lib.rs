#![warn(clippy::all, rust_2018_idioms)]
mod app;
pub(crate) mod code;
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
