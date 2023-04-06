use std::sync::Arc;

use eframe::egui::{
    util::cache::{ComputerMut, FrameCache},
    Context,
};
use rust_sitter::errors::ParseError;
use sd_core::language::{self, grammar::Expr};
use tracing::debug;

#[derive(Default)]
pub struct Parser {}

impl ComputerMut<&str, Arc<Result<Expr, Vec<ParseError>>>> for Parser {
    fn compute(&mut self, source: &str) -> Arc<Result<Expr, Vec<ParseError>>> {
        debug!("Parsing");
        Arc::new(language::grammar::parse(source))
    }
}

type ParserCache<'a> = FrameCache<Arc<Result<Expr, Vec<ParseError>>>, Parser>;

impl Parser {
    pub fn parse(ctx: &Context, source: &str) -> Arc<Result<Expr, Vec<ParseError>>> {
        ctx.memory_mut(|mem| mem.caches.cache::<ParserCache<'_>>().get(source))
    }
}
