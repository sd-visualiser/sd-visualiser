use std::sync::Arc;

use eframe::egui::{
    util::cache::{ComputerMut, FrameCache},
    Context,
};
use from_pest::{ConversionError, FromPest, Void};
use pest::{error, Parser as _};
use sd_core::language::{
    chil::{self, ChilParser},
    spartan::{self, SpartanParser},
};
use thiserror::Error;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Default)]
pub enum UiLanguage {
    Chil,
    #[default]
    Spartan,
}

impl UiLanguage {
    pub(crate) fn name(&self) -> &str {
        match self {
            Self::Chil => "chil",
            Self::Spartan => "spartan",
        }
    }
}

#[derive(Clone, Debug)]
pub enum ParseOutput {
    ChilExpr(chil::Expr),
    SpartanExpr(spartan::Expr),
}

#[derive(Clone, Debug, Error)]
pub enum ParseError {
    #[error("Chil parsing error:\n{0}")]
    Chil(#[from] error::Error<chil::Rule>),

    #[error("Spartan parsing error:\n{0}")]
    Spartan(#[from] error::Error<spartan::Rule>),

    #[error("Conversion error:\n{0}")]
    Conversion(#[from] ConversionError<Void>),
}

#[derive(Default)]
pub struct Parser;

impl ComputerMut<(&str, UiLanguage), Arc<Result<ParseOutput, ParseError>>> for Parser {
    fn compute(
        &mut self,
        (source, language): (&str, UiLanguage),
    ) -> Arc<Result<ParseOutput, ParseError>> {
        tracing::trace!("Parsing");
        Arc::new((|| match language {
            UiLanguage::Chil => {
                let mut pairs = ChilParser::parse(chil::Rule::program, source)?;
                let expr = chil::Expr::from_pest(&mut pairs)?;
                Ok(ParseOutput::ChilExpr(expr))
            }
            UiLanguage::Spartan => {
                let mut pairs = SpartanParser::parse(spartan::Rule::program, source)?;
                let expr = spartan::Expr::from_pest(&mut pairs)?;
                Ok(ParseOutput::SpartanExpr(expr))
            }
        })())
    }
}

type ParserCache<'a> = FrameCache<Arc<Result<ParseOutput, ParseError>>, Parser>;

impl Parser {
    pub fn parse(
        ctx: &Context,
        source: &str,
        language: UiLanguage,
    ) -> Arc<Result<ParseOutput, ParseError>> {
        ctx.memory_mut(|mem| {
            mem.caches
                .cache::<ParserCache<'_>>()
                .get((source, language))
        })
    }
}
