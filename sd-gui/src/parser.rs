use std::sync::Arc;

use eframe::egui::{
    util::cache::{ComputerMut, FrameCache},
    Context,
};
use from_pest::{ConversionError, FromPest, Void};
use pest::error;
use pest::Parser as _;
use sd_core::language::spartan::{Expr, Rule, SpartanParser};
use thiserror::Error;
use tracing::debug;

#[derive(Error, Debug, Clone)]
pub enum ParseError {
    #[error("Parsing error:\n{0}")]
    PError(#[from] error::Error<Rule>),
    #[error("Convertion error:\n{0:?}")]
    CError(ConversionError<Void>),
}

impl From<ConversionError<Void>> for ParseError {
    fn from(e: ConversionError<Void>) -> Self {
        ParseError::CError(e)
    }
}

#[derive(Default)]
pub struct Parser {}

impl ComputerMut<&str, Arc<Result<Expr, ParseError>>> for Parser {
    fn compute(&mut self, source: &str) -> Arc<Result<Expr, ParseError>> {
        debug!("Parsing");
        Arc::new((|| {
            let mut pairs = SpartanParser::parse(Rule::program, source)?;
            let expr = Expr::from_pest(&mut pairs)?;
            Ok(expr)
        })())
    }
}

type ParserCache<'a> = FrameCache<Arc<Result<Expr, ParseError>>, Parser>;

impl Parser {
    pub fn parse(ctx: &Context, source: &str) -> Arc<Result<Expr, ParseError>> {
        ctx.memory_mut(|mem| mem.caches.cache::<ParserCache<'_>>().get(source))
    }
}
