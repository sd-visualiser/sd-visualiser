use super::PrettyPrint;
use crate::language::chil::{
    Arg, AtomicType, BindClause, Expr, FunctionType, Identifier, Name, Op, Thunk, Type, Value,
    Variable, VariableDef,
};
use pretty::RcDoc;

impl PrettyPrint for Expr {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::concat(self.binds.iter().map(PrettyPrint::to_doc))
            .append(RcDoc::text("output"))
            .append(RcDoc::space())
            .append(self.value.to_doc())
    }
}

impl PrettyPrint for BindClause {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text("def")
            .append(RcDoc::space())
            .append(self.var.to_doc())
            .append(RcDoc::space())
            .append(RcDoc::text("="))
            .append(RcDoc::space())
            .append(self.value.to_doc())
            .append(RcDoc::line())
    }
}

impl PrettyPrint for Variable {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Self::Id(id) => id.to_doc(),
            Self::Name(name, id) => name
                .to_doc()
                .append(RcDoc::text("("))
                .append(RcDoc::text("id"))
                .append(RcDoc::text(":"))
                .append(RcDoc::space())
                .append(id.to_doc())
                .append(RcDoc::text(")")),
        }
    }
}

impl PrettyPrint for VariableDef {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Self::Inferred(var) => var.to_doc(),
            Self::Manifest { var, ty } => var
                .to_doc()
                .append(RcDoc::space())
                .append(RcDoc::text(":"))
                .append(RcDoc::space())
                .append(ty.to_doc()),
        }
    }
}

impl PrettyPrint for Type {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Self::Atomic(aty) => aty.to_doc(),
            Self::Function(fty) => fty.to_doc(),
        }
    }
}

impl PrettyPrint for AtomicType {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text(&self.0)
    }
}

impl PrettyPrint for FunctionType {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text("(")
            .append(RcDoc::intersperse(
                self.ins.iter().map(PrettyPrint::to_doc),
                RcDoc::text(",").append(RcDoc::space()),
            ))
            .append(RcDoc::text(")"))
            .append(RcDoc::space())
            .append(RcDoc::text("->"))
            .append(RcDoc::space())
            .append(self.out.to_doc())
    }
}

impl PrettyPrint for Value {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Self::Var(x) => x.to_doc(),
            Self::Op(op, args) => {
                if args.is_empty() {
                    op.to_doc()
                } else {
                    op.to_doc()
                        .append(RcDoc::text("("))
                        .append(RcDoc::intersperse(
                            args.iter().map(PrettyPrint::to_doc),
                            RcDoc::text(",").append(RcDoc::space()),
                        ))
                        .append(RcDoc::text(")"))
                }
            }
        }
    }
}

impl PrettyPrint for Thunk {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text("thunk")
            .append(RcDoc::space())
            .append(self.id.to_doc())
            .append(RcDoc::space())
            .append(RcDoc::text("="))
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(RcDoc::space())
            .append(RcDoc::intersperse(
                self.args.iter().map(PrettyPrint::to_doc),
                RcDoc::text(",").append(RcDoc::space()),
            ))
            .append(RcDoc::space())
            .append(RcDoc::text("=>"))
            .append(RcDoc::line())
            .append(self.body.to_doc())
            .nest(4)
            .append(RcDoc::line())
            .append(RcDoc::text("}"))
    }
}

impl PrettyPrint for Arg {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Self::Value(v) => v.to_doc(),
            Self::Thunk(d) => d.to_doc(),
        }
    }
}

impl PrettyPrint for Op {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text(&self.0)
    }
}

impl PrettyPrint for Name {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text(&self.0)
    }
}

impl PrettyPrint for Identifier {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text(format!("%{}", self.0))
    }
}
