use pretty::RcDoc;

use super::PrettyPrint;
use crate::language::mlir::{BlockAddr, Expr, Op, Thunk, Var};

impl PrettyPrint for Op {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text(&self.name).append(&self.attributes)
    }
}

impl PrettyPrint for Var {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Var::Var { id } => RcDoc::text(id),
            Var::VarIdx { id, index } => {
                RcDoc::text(id).append("#").append(RcDoc::as_string(index))
            }
            Var::Symbol(s) => s.to_doc(),
        }
    }
}

impl PrettyPrint for BlockAddr {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text(&self.0)
    }
}

impl PrettyPrint for Expr {
    fn to_doc(&self) -> pretty::RcDoc<'_, ()> {
        RcDoc::nil() // TODO: Implement pretty printing for MLIR
    }
}

impl PrettyPrint for Thunk {
    fn to_doc(&self) -> pretty::RcDoc<'_, ()> {
        RcDoc::nil() // TODO: Implement pretty printing for MLIR
    }
}
