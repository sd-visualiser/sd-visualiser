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
    pub(crate) const fn name(&self) -> &str {
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
    Chil(#[from] Box<error::Error<chil::Rule>>),

    #[error("Spartan parsing error:\n{0}")]
    Spartan(#[from] Box<error::Error<spartan::Rule>>),

    #[error("Conversion error:\n{0}")]
    Conversion(#[from] ConversionError<Void>),
}

pub fn parse(source: &str, language: UiLanguage) -> Result<ParseOutput, ParseError> {
    match language {
        UiLanguage::Chil => {
            let mut pairs = ChilParser::parse(chil::Rule::program, source).map_err(Box::new)?;
            let expr = chil::Expr::from_pest(&mut pairs)?;
            Ok(ParseOutput::ChilExpr(expr))
        }
        UiLanguage::Spartan => {
            let mut pairs =
                SpartanParser::parse(spartan::Rule::program, source).map_err(Box::new)?;
            let expr = spartan::Expr::from_pest(&mut pairs)?;
            Ok(ParseOutput::SpartanExpr(expr))
        }
    }
}
