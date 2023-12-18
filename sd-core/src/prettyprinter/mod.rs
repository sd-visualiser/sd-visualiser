use std::error::Error;

use itertools::Either;
use pretty::RcDoc;

pub mod chil;
pub mod mlir;
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

impl<T: PrettyPrint> PrettyPrint for Vec<T> {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        list(self)
    }
}

impl<T: PrettyPrint> PrettyPrint for Option<T> {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            None => RcDoc::nil(),
            Some(t) => t.to_doc(),
        }
    }
}

impl<T: PrettyPrint, E: Error> PrettyPrint for Result<T, E> {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Ok(t) => t.to_doc(),
            Err(e) => RcDoc::text(format!("Error: {e:?}")),
        }
    }
}

impl<L: PrettyPrint, R: PrettyPrint> PrettyPrint for Either<L, R> {
    fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Either::Left(l) => l.to_doc(),
            Either::Right(r) => r.to_doc(),
        }
    }
}
