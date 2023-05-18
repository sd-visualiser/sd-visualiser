use super::{ActiveOp, Arg, BindClause, Expr, PassiveOp, Term, Thunk, Value, Variable};

#[allow(unused_variables)]
pub trait Visitor {
    fn visit_variable(&mut self, variable: &Variable) {}
    fn visit_bind_clause(&mut self, bind_clause: &BindClause) {}
    fn visit_expr(&mut self, expr: &Expr) {}
    fn visit_term(&mut self, term: &Term) {}
    fn visit_value(&mut self, value: &Value) {}
    fn visit_active_op(&mut self, active_op: &ActiveOp) {}
    fn visit_passive_op(&mut self, passive_op: &PassiveOp) {}
    fn visit_thunk(&mut self, thunk: &Thunk) {}
    fn after_variable(&mut self, variable: &Variable) {}
    fn after_bind_clause(&mut self, bind_clause: &BindClause) {}
    fn after_expr(&mut self, expr: &Expr) {}
    fn after_term(&mut self, term: &Term) {}
    fn after_value(&mut self, value: &Value) {}
    fn after_active_op(&mut self, active_op: &ActiveOp) {}
    fn after_passive_op(&mut self, passive_op: &PassiveOp) {}
    fn after_thunk(&mut self, thunk: &Thunk) {}
}

pub trait Visitable {
    fn walk(&self, visitor: &mut impl Visitor);
}

impl Visitable for Variable {
    fn walk(&self, visitor: &mut impl Visitor) {
        visitor.visit_variable(self);
        visitor.after_variable(self);
    }
}

impl Visitable for BindClause {
    fn walk(&self, visitor: &mut impl Visitor) {
        visitor.visit_bind_clause(self);
        self.var.walk(visitor);
        self.term.walk(visitor);
        visitor.after_bind_clause(self);
    }
}

impl Visitable for Expr {
    fn walk(&self, visitor: &mut impl Visitor) {
        visitor.visit_expr(self);
        self.binds.iter().for_each(|bind| bind.walk(visitor));
        self.value.walk(visitor);
        visitor.after_expr(self);
    }
}

impl Visitable for Term {
    fn walk(&self, visitor: &mut impl Visitor) {
        visitor.visit_term(self);
        match self {
            Term::Value(v) => {
                v.walk(visitor);
            }
            Term::ActiveOp(op, args) => {
                op.walk(visitor);
                args.iter().for_each(|arg| arg.walk(visitor));
            }
        }
        visitor.after_term(self);
    }
}

impl Visitable for Value {
    fn walk(&self, visitor: &mut impl Visitor) {
        visitor.visit_value(self);
        match self {
            Value::Var(var) => {
                var.walk(visitor);
            }
            Value::PassiveOp(op, args) => {
                op.walk(visitor);
                args.iter().for_each(|arg| arg.walk(visitor));
            }
        }
        visitor.after_value(self);
    }
}

impl Visitable for ActiveOp {
    fn walk(&self, visitor: &mut impl Visitor) {
        visitor.visit_active_op(self);
        visitor.after_active_op(self);
    }
}

impl Visitable for PassiveOp {
    fn walk(&self, visitor: &mut impl Visitor) {
        visitor.visit_passive_op(self);
        visitor.after_passive_op(self);
    }
}

impl Visitable for Thunk {
    fn walk(&self, visitor: &mut impl Visitor) {
        visitor.visit_thunk(self);
        self.args.iter().for_each(|var| var.walk(visitor));
        self.body.walk(visitor);
        visitor.after_thunk(self);
    }
}

impl Visitable for Arg {
    fn walk(&self, visitor: &mut impl Visitor) {
        match self {
            Arg::Value(v) => v.walk(visitor),
            Arg::Thunk(d) => d.walk(visitor),
        }
    }
}
