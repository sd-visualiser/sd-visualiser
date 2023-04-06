#![warn(clippy::all, rust_2018_idioms)]
mod app;
pub(crate) mod highlighter;
pub(crate) mod layout;
pub(crate) mod parser;

pub use app::App;
