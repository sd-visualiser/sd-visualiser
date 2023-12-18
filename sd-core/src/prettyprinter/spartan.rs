use pretty::RcDoc;

use super::{paran_list, PrettyPrint};
use crate::language::spartan::{Bind, Expr, Op, Thunk, Value, Variable};

impl PrettyPrint for Expr {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::concat(self.binds.iter().map(PrettyPrint::to_doc)).append({
            if self.values.is_empty() {
                RcDoc::text("()")
            } else if self.values.len() == 1 {
                self.values[0].to_doc()
            } else {
                paran_list(&self.values)
            }
        })
    }
}

impl PrettyPrint for Bind {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text("bind")
            .append(RcDoc::space())
            .append(if self.defs.len() == 1 {
                self.defs[0].to_doc()
            } else {
                paran_list(&self.defs)
            })
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
        RcDoc::text(&self.0)
    }
}

impl PrettyPrint for Value {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Self::Variable(var) => var.to_doc(),
            Self::Thunk(thunk) => thunk.to_doc(),
            Self::Op { op, args } => {
                if args.is_empty() {
                    op.to_doc()
                } else {
                    op.to_doc().append(paran_list(args))
                }
            }
        }
    }
}

impl PrettyPrint for Op {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Self::Plus => RcDoc::text("plus"),
            Self::Minus => RcDoc::text("minus"),
            Self::Times => RcDoc::text("times"),
            Self::Div => RcDoc::text("div"),
            Self::Rem => RcDoc::text("rem"),
            Self::And => RcDoc::text("and"),
            Self::Or => RcDoc::text("or"),
            Self::Not => RcDoc::text("not"),
            Self::If => RcDoc::text("if"),
            Self::Eq => RcDoc::text("eq"),
            Self::Neq => RcDoc::text("neq"),
            Self::Lt => RcDoc::text("lt"),
            Self::Leq => RcDoc::text("leq"),
            Self::Gt => RcDoc::text("gt"),
            Self::Geq => RcDoc::text("geq"),
            Self::App => RcDoc::text("app"),
            Self::Lambda => RcDoc::text("lambda"),
            Self::Atom => RcDoc::text("atom"),
            Self::Deref => RcDoc::text("deref"),
            Self::Assign => RcDoc::text("assign"),
            Self::Tuple => RcDoc::text("tuple"),
            Self::Detuple => RcDoc::text("detuple"),
            Self::Bool(b) => RcDoc::as_string(b),
            Self::Number(n) => RcDoc::as_string(n),
        }
    }
}

impl PrettyPrint for Thunk {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::intersperse(self.args.iter().map(PrettyPrint::to_doc), RcDoc::space())
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

#[cfg(test)]
mod tests {
    use dir_test::{dir_test, Fixture};
    use insta::assert_snapshot;

    use crate::{language::spartan::Expr, prettyprinter::PrettyPrint};

    #[allow(clippy::needless_pass_by_value)]
    #[dir_test(dir: "$CARGO_MANIFEST_DIR/../examples", glob: "**/*.sd", loader: crate::language::spartan::tests::parse_sd, postfix: "pretty_print")]
    fn pretty_print(fixture: Fixture<(&str, Expr)>) {
        let (name, expr) = fixture.content();
        assert_snapshot!(format!("pretty_print_{name}"), expr.to_pretty());
    }
}
