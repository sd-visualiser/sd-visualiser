pub mod chil;
pub mod spartan;

pub(super) mod visitor;

pub(crate) fn span_into_str(span: pest::Span) -> &str {
    span.as_str()
}
