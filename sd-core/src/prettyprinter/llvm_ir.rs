use llvm_ir::Name;
use pretty::RcDoc;

use super::PrettyPrint;
use crate::language::llvm_ir::{Expr, Op, Thunk, Var};

impl PrettyPrint for Op {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text(self.to_string())
    }
}

impl PrettyPrint for Name {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text(self.to_string())
    }
}

impl PrettyPrint for Var {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text(self.to_string())
    }
}

impl PrettyPrint for Expr {
    fn to_doc(&self) -> pretty::RcDoc<'_, ()> {
        RcDoc::nil() // TODO: Implement pretty printing for LLVM IR
    }
}

impl PrettyPrint for Thunk {
    fn to_doc(&self) -> pretty::RcDoc<'_, ()> {
        RcDoc::nil() // TODO: Implement pretty printing for LLVM IR
    }
}
