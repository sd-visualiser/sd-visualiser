use from_pest::{ConversionError, FromPest, Void};
use pest::{Parser as _, error};
#[cfg(not(target_arch = "wasm32"))]
use sd_core::language::llvm_ir;
use sd_core::language::{
    chil::{self, ChilParser},
    mlir::{
        self,
        internal::{MlirParser, TopLevelItem},
    },
    sd_lang::{self, SdLangParser},
};
use thiserror::Error;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Default)]
pub enum UiLanguage {
    Chil,
    #[default]
    SdLang,
    LlvmIr,
    Mlir,
    Dot,
}

impl UiLanguage {
    pub(crate) const fn name(&self) -> &str {
        match self {
            Self::Chil => "chil",
            Self::SdLang => "sd-lang",
            Self::LlvmIr => "llvm-ir",
            Self::Mlir => "mlir",
            Self::Dot => "dot",
        }
    }
}

#[derive(Clone, Debug)]
pub enum ParseOutput {
    Chil(chil::Expr),
    SdLang(sd_lang::Expr),
    #[cfg(not(target_arch = "wasm32"))]
    LlvmIr(llvm_ir::Expr),
    Mlir(mlir::Expr),
    Dot(dot_structures::Graph),
}

#[derive(Clone, Debug, Error)]
pub enum ParseError {
    #[error("Chil parsing error:\n{0}")]
    Chil(#[from] Box<error::Error<chil::Rule>>),

    #[error("Sd parsing error:\n{0}")]
    SdLang(#[from] Box<error::Error<sd_lang::Rule>>),

    #[cfg(not(target_arch = "wasm32"))]
    #[error("LlvmIr parsing error:\n{0}")]
    LlvmIr(String),

    #[error("Mlir parsing error:\n{0}")]
    Mlir(#[from] Box<error::Error<mlir::internal::Rule>>),

    #[error("Dot parsing error:\n{0}")]
    Dot(String),

    #[error("Conversion error:\n{0}")]
    Conversion(#[from] ConversionError<Void>),
}

pub fn parse(source: &str, language: UiLanguage) -> Result<ParseOutput, ParseError> {
    match language {
        UiLanguage::Chil => {
            let mut pairs = ChilParser::parse(chil::Rule::program, source).map_err(Box::new)?;
            let expr = chil::Expr::from_pest(&mut pairs)?;
            Ok(ParseOutput::Chil(expr))
        }
        UiLanguage::SdLang => {
            let mut pairs =
                SdLangParser::parse(sd_lang::Rule::program, source).map_err(Box::new)?;
            let expr = sd_lang::Expr::from_pest(&mut pairs)?;
            Ok(ParseOutput::SdLang(expr))
        }
        #[cfg(not(target_arch = "wasm32"))]
        UiLanguage::LlvmIr => Ok(ParseOutput::LlvmIr(
            llvm_ir::parse(source).map_err(ParseError::LlvmIr)?,
        )),
        #[cfg(target_arch = "wasm32")]
        UiLanguage::LlvmIr => unreachable!(),
        UiLanguage::Mlir => {
            let mut pairs =
                MlirParser::parse(mlir::internal::Rule::toplevel, source).map_err(Box::new)?;

            let items = Vec::<TopLevelItem>::from_pest(&mut pairs)?;
            let ops: Vec<mlir::internal::Operation> = items
                .into_iter()
                .filter_map(|x| match x {
                    TopLevelItem::Operation(y) => Some(y),
                    TopLevelItem::Other(_) => None,
                })
                .collect();
            let expr = mlir::Expr::from(ops);
            Ok(ParseOutput::Mlir(expr))
        }
        UiLanguage::Dot => {
            let graph = graphviz_rust::parse(source).map_err(ParseError::Dot)?;
            Ok(ParseOutput::Dot(graph))
        }
    }
}
