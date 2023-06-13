use std::{
    collections::{HashMap, HashSet},
    ops::Index,
};

use crate::language::spartan::{Expr, Thunk, Value, Variable};

#[derive(Debug, Default)]
pub(crate) struct FreeVars(HashMap<*const Expr, HashSet<Variable>>);

impl Index<&Expr> for FreeVars {
    type Output = HashSet<Variable>;

    fn index(&self, expr: &Expr) -> &Self::Output {
        let key: *const Expr = expr;
        &self.0[&key]
    }
}

impl FreeVars {
    pub(crate) fn expr(&mut self, expr: &Expr) {
        let mut vars: HashSet<Variable> = HashSet::new();

        for bc in &expr.binds {
            self.value(&mut vars, &bc.value);
        }

        self.value(&mut vars, &expr.value);

        for bc in &expr.binds {
            vars.remove(&bc.var);
        }

        self.0.insert(expr, vars);
    }

    pub(crate) fn value(&mut self, vars: &mut HashSet<Variable>, val: &Value) {
        match val {
            Value::Variable(v) => {
                vars.insert(v.clone());
            }
            Value::Op { vs, ds, .. } => {
                for v in vs {
                    self.value(vars, v);
                }

                for d in ds {
                    self.thunk(vars, d);
                }
            }
        }
    }

    pub(crate) fn thunk(&mut self, vars: &mut HashSet<Variable>, thunk: &Thunk) {
        self.expr(&thunk.body);
        let arg_set: HashSet<Variable> = thunk.args.iter().map(|(var, _)| var).cloned().collect();
        vars.extend(self[&thunk.body].difference(&arg_set).cloned());
    }
}
