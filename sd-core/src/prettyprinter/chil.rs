use pretty::RcDoc;

use super::{list, paran_list, PrettyPrint};
use crate::language::{
    chil::{
        Addr, BaseType, Bind, Expr, FunctionType, GenericType, Identifier, Op, Thunk, TupleType,
        Type, Value, Variable, VariableDef,
    },
    Arg,
};

const INDENTATION: isize = 2;

impl PrettyPrint for Expr {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::concat(self.binds.iter().map(PrettyPrint::to_doc))
            .append(RcDoc::text("output"))
            .append(RcDoc::space())
            .append(list(&self.values))
    }
}

impl PrettyPrint for Bind {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text("def")
            .append(RcDoc::space())
            .append(self.def.to_doc())
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
            Self::Addr(addr) => addr.to_doc(),
            Self::Identifier(name, addr) => name
                .to_doc()
                .append(RcDoc::text("("))
                .append(RcDoc::text("id"))
                .append(RcDoc::text(":"))
                .append(RcDoc::space())
                .append(addr.to_doc())
                .append(RcDoc::text(")")),
        }
    }
}

impl PrettyPrint for VariableDef {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match &self.r#type {
            None => self.var.to_doc(),
            Some(ty) => self
                .var
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
            Self::Base(bty) => bty.to_doc(),
            Self::Generic(gty) => gty.to_doc(),
            Self::Tuple(tty) => tty.to_doc(),
            Self::Function(fty) => fty.to_doc(),
        }
    }
}

impl PrettyPrint for BaseType {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text(&self.0)
    }
}

impl PrettyPrint for GenericType {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        self.base
            .to_doc()
            .append(RcDoc::text("<"))
            .append(list(&self.params))
            .append(RcDoc::text(">"))
    }
}

impl PrettyPrint for TupleType {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        paran_list(&self.types)
    }
}

impl PrettyPrint for FunctionType {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        self.domain
            .to_doc()
            .append(RcDoc::space())
            .append(RcDoc::text("->"))
            .append(RcDoc::space())
            .append(self.codomain.to_doc())
    }
}

impl PrettyPrint for Value {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Self::Variable(var) => var.to_doc(),
            Self::Op { op, args } => {
                let mut doc = op.to_doc();
                if !args.is_empty() {
                    let vs = args.iter().filter_map(Arg::value).collect::<Vec<_>>();
                    let ds = args.iter().filter_map(Arg::thunk).collect::<Vec<_>>();
                    doc = doc.append(RcDoc::text("("));
                    if !vs.is_empty() {
                        doc = doc.append(list(vs.iter().copied()));
                    }
                    if !ds.is_empty() {
                        if !vs.is_empty() {
                            doc = doc.append(RcDoc::text(";"));
                        }
                        doc = doc
                            .append(RcDoc::line())
                            .append(RcDoc::concat(ds.into_iter().map(PrettyPrint::to_doc)))
                            .nest(INDENTATION);
                    }
                    doc = doc.append(RcDoc::text(")"));
                }
                doc
            }
        }
    }
}

impl PrettyPrint for Thunk {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text("thunk")
            .append(RcDoc::space())
            .append(self.addr.to_doc())
            .append(RcDoc::space())
            .append(RcDoc::text("="))
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(RcDoc::space())
            .append(list(&self.args))
            .append(RcDoc::space())
            .append(RcDoc::text("=>"))
            .append(RcDoc::line())
            .append(self.body.to_doc())
            .nest(INDENTATION)
            .append(RcDoc::line())
            .append(RcDoc::text("}"))
            .append(RcDoc::line())
    }
}

impl PrettyPrint for Op {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text(&self.0)
    }
}

impl PrettyPrint for Addr {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::as_string(self.0).append(RcDoc::as_string(self.1))
    }
}

impl PrettyPrint for Identifier {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text(&self.0)
    }
}

#[cfg(test)]
mod tests {
    use dir_test::{dir_test, Fixture};
    use insta::assert_snapshot;

    use crate::{language::chil::Expr, prettyprinter::PrettyPrint};

    #[allow(clippy::needless_pass_by_value)]
    #[dir_test(dir: "$CARGO_MANIFEST_DIR/../examples", glob: "**/*.chil", loader: crate::language::chil::tests::parse_chil, postfix: "pretty_print")]
    fn pretty_print(fixture: Fixture<(&str, Expr)>) {
        let (name, expr) = fixture.content();
        assert_snapshot!(format!("pretty_print_{name}"), expr.to_pretty());
    }
}
