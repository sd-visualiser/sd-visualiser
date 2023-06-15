use pretty::RcDoc;

pub mod chil;
pub mod spartan;

pub trait PrettyPrint {
    fn to_doc(&self) -> RcDoc<'_, ()>;

    fn to_pretty(&self) -> String {
        self.to_doc()
            .pretty(usize::MAX) // in principle, the pretty printer could do different things at different widths
            .to_string()
    }
}

/// Comma-separated list.
pub fn list<'a, T: 'a + PrettyPrint>(ts: impl IntoIterator<Item = &'a T>) -> RcDoc<'a, ()> {
    RcDoc::intersperse(
        ts.into_iter().map(PrettyPrint::to_doc),
        RcDoc::text(",").append(RcDoc::space()),
    )
}

/// Comma-separated list with parentheses around it.
pub fn paran_list<'a, T: 'a + PrettyPrint>(ts: impl IntoIterator<Item = &'a T>) -> RcDoc<'a, ()> {
    RcDoc::text("(").append(list(ts)).append(RcDoc::text(")"))
}
