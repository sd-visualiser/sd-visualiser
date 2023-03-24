#[derive(Clone, Debug)]
pub struct Variable(String);

#[derive(Clone, Debug)]
pub enum Term {
    Var(Variable),
    Op(Vec<Term>, Vec<Thunk>),
}

#[derive(Clone, Debug)]
pub struct Thunk(Vec<Variable>, Expr);

#[derive(Clone, Debug)]
pub enum Expr {
    Term(Term),
    Bind(Variable, Term, Box<Expr>),
}
