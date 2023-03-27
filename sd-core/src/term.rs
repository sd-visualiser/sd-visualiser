#[derive(Clone, Debug)]
pub struct Variable(pub String);

#[derive(Clone, Debug)]
pub enum Term {
    Var(Variable),
    Op(Vec<Variable>, Vec<Thunk>),
}

#[derive(Clone, Debug)]
pub struct Thunk(pub Vec<Variable>, pub Expr);

#[derive(Clone, Debug)]
pub enum Expr {
    Term(Term),
    Bind(Variable, Term, Box<Expr>),
}
