use pretty::RcDoc;

use super::PrettyPrint;
use crate::language::mlir::{BlockAddr, Op, Var};

impl PrettyPrint for Op {
    fn to_doc(&self) -> pretty::RcDoc<'_, ()> {
        RcDoc::text(&self.name)
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
