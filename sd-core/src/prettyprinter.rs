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
