#![warn(clippy::all, rust_2018_idioms)]
mod app;
pub(crate) mod graph_ui;
pub(crate) mod highlighter;
pub(crate) mod layout;
pub(crate) mod parser;
pub(crate) mod selection;

pub use app::App;
