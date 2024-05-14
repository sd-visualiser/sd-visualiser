use std::collections::HashMap;

use indexmap::IndexSet;

use crate::language::{ControlFlow, Expr, GetVar, Language, Thunk, Value, CF};

impl<T: Language> Expr<T> {
    pub(crate) fn free_vars(&self) -> IndexSet<T::Var> {
        let mut vars: IndexSet<T::Var> = IndexSet::new();

        self.extend_free_vars(&mut vars);

        vars
    }

    pub(crate) fn extend_free_vars(&self, vars: &mut IndexSet<T::Var>) {
        for bind in &self.binds {
            bind.value.free_vars(vars);
        }

        for value in &self.values {
            value.free_vars(vars);
        }

        for def in self.binds.iter().flat_map(|bind| &bind.defs) {
            vars.swap_remove(def.var());
        }
    }

    pub(crate) fn cf_free_vars(&self, addrs: &mut HashMap<Option<T::BlockAddr>, usize>) {
        for b in &self.binds {
            b.value.cf_free_vars(addrs);
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
        match self {
            Value::Variable(_) | Value::Thunk(_) => {}
            Value::Op { op, .. } => match op.get_cf() {
                None => (),
                Some(CF::Return) => {
                    *addrs.entry(None).or_insert(0) += 1;
                }
                Some(CF::Brs(bs)) => {
                    for b in bs {
                        *addrs.entry(Some(b)).or_insert(0) += 1;
                    }
                }
            },
        }
    }
}

impl<T: Language> Thunk<T> {
    pub(crate) fn free_vars(&self, vars: &mut IndexSet<T::Var>) {
        let mut body_vars = self.body.free_vars();
        let mut arg_set: IndexSet<T::Var> = self.args.iter().map(GetVar::var).cloned().collect();
        for b in &self.blocks {
            b.expr.extend_free_vars(&mut body_vars);
            arg_set.extend(b.args.iter().map(GetVar::var).cloned());
        }
        vars.extend(body_vars.difference(&arg_set).cloned());
    }
}
