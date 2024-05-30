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
        let doc = RcDoc::text(&self.id);
        if let Some(idx) = self.index {
            doc.append("#").append(idx.to_string())
        } else {
            doc
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
