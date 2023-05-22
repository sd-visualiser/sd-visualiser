use super::PrettyPrint;
use crate::language::spartan::{Arg, BindClause, Expr, Op, Thunk, Value, Variable};
use pretty::RcDoc;

impl PrettyPrint for Expr {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::concat(self.binds.iter().map(|bind| bind.to_doc())).append(self.value.to_doc())
    }
}

impl PrettyPrint for BindClause {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text("bind")
            .append(RcDoc::space())
            .append(self.var.to_doc())
            .append(RcDoc::space())
            .append(RcDoc::text("="))
            .append(RcDoc::space())
            .append(self.value.to_doc())
            .append(RcDoc::space())
            .append(RcDoc::text("in"))
            .append(RcDoc::line())
    }
}

impl PrettyPrint for Variable {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text(self.0.to_owned())
    }
}

impl PrettyPrint for Value {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Value::Var(v) => v.to_doc(),
            Value::Op(op, args) => {
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

impl PrettyPrint for Op {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match *self {
            Self::Plus => RcDoc::text("plus"),
            Self::Minus => RcDoc::text("minus"),
            Self::Times => RcDoc::text("times"),
            Self::Eq => RcDoc::text("eq"),
            Self::And => RcDoc::text("and"),
            Self::Or => RcDoc::text("or"),
            Self::Not => RcDoc::text("not"),
            Self::If => RcDoc::text("if"),
            Self::App => RcDoc::text("app"),
            Self::Rec => RcDoc::text("rec"),
            Self::Int(n) => RcDoc::as_string(n),
            Self::Bool(b) => RcDoc::as_string(b),
            Self::Lambda => RcDoc::text("lambda"),
        }
    }
}

impl PrettyPrint for Thunk {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::intersperse(self.args.iter().map(|arg| arg.to_doc()), RcDoc::space())
            .append(RcDoc::space())
            .append(RcDoc::text("."))
            .append(if self.body.binds.is_empty() {
                RcDoc::space().append(self.body.to_doc())
            } else {
                RcDoc::line()
                    .append(self.body.to_doc())
                    .nest(4)
                    .append(RcDoc::line())
            })
    }
}

impl PrettyPrint for Arg {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Arg::Value(v) => v.to_doc(),
            Arg::Thunk(t) => t.to_doc(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::language::spartan::tests::*;
    use anyhow::Result;
    use insta::assert_snapshot;
    use rstest::rstest;

    use crate::{language::spartan::Expr, prettyprinter::PrettyPrint};

    #[rstest]
    #[case("basic_program", basic_program())]
    #[case("free_vars", free_vars())]
    #[case("thunks", thunks())]
    #[case("fact", fact())]
    fn print_snapshots(#[case] name: &str, #[case] expr: Result<Expr>) -> Result<()> {
        let expr = expr?;
        // have to manually specify name due to https://github.com/la10736/rstest/issues/183
        assert_snapshot!(name, expr.to_pretty());
        Ok(())
    }
}
