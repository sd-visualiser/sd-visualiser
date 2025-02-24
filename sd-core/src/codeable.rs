use crate::{
    decompile::{DecompileError, FakeValue},
    graph::{Syntax, SyntaxHypergraph},
    hypergraph::{Edge, Operation, Thunk, subgraph::Subgraph, traits::WithWeight},
    language::{Expr, Language, Thunk as SThunk},
    prettyprinter::PrettyPrint,
};

/// Something which has an associated code that can be pretty printed.
pub trait Codeable {
    type Code: PrettyPrint;
    fn code(&self) -> Self::Code;
}

pub(crate) type Code<T> = <T as Codeable>::Code;

impl<T: Language> Codeable for Edge<Syntax<T>> {
    type Code = FakeValue<T>;

    fn code(&self) -> Self::Code {
        FakeValue::decompile(self)
    }
}

impl<T: Language> Codeable for Operation<Syntax<T>> {
    type Code = T::Op;

    fn code(&self) -> Self::Code {
        self.weight()
    }
}

impl<T: Language> Codeable for Thunk<Syntax<T>>
where
    SThunk<T>: PrettyPrint,
{
    type Code = Result<SThunk<T>, DecompileError>;

    fn code(&self) -> Self::Code {
        SThunk::decompile(self)
    }
}

impl<T: Language> Codeable for Subgraph<SyntaxHypergraph<T>>
where
    Expr<T>: PrettyPrint,
{
    type Code = Result<Expr<T>, DecompileError>;

    fn code(&self) -> Self::Code {
        Expr::decompile(self)
    }
}
