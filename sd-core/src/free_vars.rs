use std::{
    collections::{HashMap, HashSet},
    ops::Index,
};

use derivative::Derivative;

use crate::language::{Expr, Language, Thunk, Value};

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Default(bound = ""))]
pub(crate) struct FreeVars<T: Language>(HashMap<*const Expr<T>, HashSet<T::Var>>);

impl<T: Language> Index<&Expr<T>> for FreeVars<T> {
    type Output = HashSet<T::Var>;

    fn index(&self, expr: &Expr<T>) -> &Self::Output {
        let key: *const Expr<T> = expr;
        &self.0[&key]
    }
}

impl<T: Language> FreeVars<T> {
    pub(crate) fn expr(&mut self, expr: &Expr<T>) {
        let mut vars: HashSet<T::Var> = HashSet::new();

        for bc in &expr.binds {
            self.value(&mut vars, &bc.value);
        }

        self.value(&mut vars, &expr.value);

        for bc in &expr.binds {
            vars.remove(&bc.var);
        }

        self.0.insert(expr, vars);
    }

    pub(crate) fn value(&mut self, vars: &mut HashSet<T::Var>, val: &Value<T>) {
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

    pub(crate) fn thunk(&mut self, vars: &mut HashSet<T::Var>, thunk: &Thunk<T>) {
        self.expr(&thunk.body);
        let arg_set: HashSet<T::Var> = thunk.args.iter().map(|(var, _)| var).cloned().collect();
        vars.extend(self[&thunk.body].difference(&arg_set).cloned());
    }
}
