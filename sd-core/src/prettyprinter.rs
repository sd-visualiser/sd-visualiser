use pretty::RcDoc;

pub mod chil;
pub mod spartan;

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
