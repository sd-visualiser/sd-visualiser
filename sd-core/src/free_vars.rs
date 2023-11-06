use std::collections::HashMap;

use indexmap::IndexSet;

use crate::language::{ControlFlow, Expr, GetVar, Language, Thunk, Value, CF};

impl<T: Language> Expr<T> {
    pub(crate) fn free_vars(&self) -> IndexSet<T::Var> {
        let mut vars: IndexSet<T::Var> = IndexSet::new();

        for bind in &self.binds {
            bind.value.free_vars(&mut vars);
        }

        for value in &self.values {
            value.free_vars(&mut vars);
        }

        for def in self.binds.iter().flat_map(|bind| &bind.defs) {
            vars.remove(def.var());
        }

        vars
    }

    pub(crate) fn cf_free_vars(&self, addrs: &mut HashMap<Option<T::BlockAddr>, usize>) {
        for v in &self.values {
            v.cf_free_vars(addrs);
        }
    }
}

impl<T: Language> Value<T> {
    pub(crate) fn free_vars(&self, vars: &mut IndexSet<T::Var>) {
        match self {
            Value::Variable(v) => {
                vars.insert(v.clone());
            }
            Value::Thunk(thunk) => {
                thunk.free_vars(vars);
            }
            Value::Op { args, .. } => {
                for arg in args {
                    arg.free_vars(vars);
                }
            }
        }
    }

    pub(crate) fn cf_free_vars(&self, addrs: &mut HashMap<Option<T::BlockAddr>, usize>) {
        match self.get_cf() {
            None => (),
            Some(CF::Return) => {
                *addrs.entry(None).or_insert(0) += 1;
            }
            Some(CF::Brs(bs)) => {
                for b in bs {
                    *addrs.entry(Some(b.block)).or_insert(0) += 1;
                }
            }
        }
    }
}

impl<T: Language> Thunk<T> {
    pub(crate) fn free_vars(&self, vars: &mut IndexSet<T::Var>) {
        let body_vars = self.body.free_vars();
        let arg_set: IndexSet<T::Var> = self.args.iter().map(GetVar::var).cloned().collect();
        vars.extend(body_vars.difference(&arg_set).cloned());
    }
}
