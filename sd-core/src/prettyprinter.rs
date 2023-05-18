use crate::language::{ActiveOp, Arg, BindClause, Expr, PassiveOp, Term, Thunk, Value, Variable};
use pretty::RcDoc;

pub trait PrettyPrint: std::fmt::Debug {
    fn to_doc(&self) -> RcDoc<'_, ()>;
    #[tracing::instrument]
    fn to_pretty(&self) -> String {
        tracing::debug!("self.to_doc(): {:#?}", self.to_doc());
        self.to_doc()
            .pretty(usize::MAX) // in principle, the pretty printer could do different things at different widths
            .to_string()
    }
}

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
            .append(self.term.to_doc())
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

impl PrettyPrint for Term {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Term::Value(v) => v.to_doc(),
            Term::ActiveOp(op, args) => op
                .to_doc()
                .append(RcDoc::text("("))
                .append(RcDoc::intersperse(
                    args.iter().map(PrettyPrint::to_doc),
                    RcDoc::text(",").append(RcDoc::space()),
                ))
                .append(RcDoc::text(")")),
        }
    }
}

impl PrettyPrint for ActiveOp {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text(match *self {
            Self::Plus => "plus",
            Self::Minus => "minus",
            Self::Times => "times",
            Self::Eq => "eq",
            Self::And => "and",
            Self::Or => "or",
            Self::Not => "not",
            Self::If => "if",
            Self::App => "app",
            Self::Rec => "rec",
        })
    }
}

impl PrettyPrint for PassiveOp {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text(match *self {
            Self::Int(n) => n.to_string(),
            Self::Bool(b) => b.to_string(),
            Self::Lambda => "lambda".to_string(),
        })
    }
}

impl PrettyPrint for Value {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Value::Var(v) => v.to_doc(),
            Value::PassiveOp(op, args) => {
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
    use crate::language::tests::*;
    use anyhow::Result;
    use insta::assert_snapshot;
    use rstest::rstest;

    use crate::{language::Expr, prettyprinter::PrettyPrint};

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
