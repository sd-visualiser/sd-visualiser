use std::collections::HashSet;

use crate::language::{Arg, AsVar, Expr, Language, Thunk, Value};

impl<T: Language> Expr<T> {
    pub(crate) fn free_vars(&self) -> HashSet<T::Var> {
        let mut vars: HashSet<T::Var> = HashSet::new();

        for bind in &self.binds {
            bind.value.free_vars(&mut vars);
        }

        for value in &self.values {
            value.free_vars(&mut vars);
        }

        for bind in &self.binds {
            vars.remove(bind.def.as_var());
        }

        vars
    }
}

impl<T: Language> Value<T> {
    pub(crate) fn free_vars(&self, vars: &mut HashSet<T::Var>) {
        match self {
            Value::Variable(v) => {
                vars.insert(v.clone());
            }
            Value::Op { args, .. } => {
                for arg in args {
                    match arg {
                        Arg::Value(v) => v.free_vars(vars),
                        Arg::Thunk(d) => d.free_vars(vars),
                    }
                }
            }
        }
    }
}

impl<T: Language> Thunk<T> {
    pub(crate) fn free_vars(&self, vars: &mut HashSet<T::Var>) {
        let body_vars = self.body.free_vars();
        let arg_set: HashSet<T::Var> = self.args.iter().map(AsVar::as_var).cloned().collect();
        vars.extend(body_vars.difference(&arg_set).cloned());
    }
}