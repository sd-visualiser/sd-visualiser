#[derive(Clone, Debug)]
pub enum Expr {
    Val(Value),
    Bind(Variable, Term, Box<Expr>),
}

#[derive(Clone, Debug)]
pub enum Term {
    Val(Value),
    ActiveOp(ActiveOp, Vec<Variable>, Vec<Thunk>),
}

#[derive(Clone, Debug)]
pub enum Value {
    Var(Variable),
    PassiveOp(PassiveOp, Vec<Variable>, Vec<Thunk>),
}

#[derive(Clone, Debug)]
pub struct Thunk(pub Vec<Variable>, pub Expr);

#[derive(Clone, Debug)]
pub enum ActiveOp {
    Plus,
    Times,
}

#[derive(Clone, Debug)]
pub enum PassiveOp {
    Int(usize),
}

#[derive(Clone, Debug)]
pub struct Variable(pub String);
