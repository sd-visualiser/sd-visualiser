use std::collections::HashMap;

use indexmap::IndexSet;

use crate::language::{Expr, GetVar, Language, OpInfo, Thunk, Value, CF};

impl<T: Language> Expr<T> {
    pub(crate) fn free_vars(&self, sym_name_link: bool) -> IndexSet<T::Var> {
        let mut vars: IndexSet<T::Var> = IndexSet::new();
        let mut to_remove: IndexSet<T::Var> = IndexSet::new();

        self.extend_free_vars(&mut vars, &mut to_remove, sym_name_link);

        vars.difference(&to_remove).cloned().collect()
    }

    pub(crate) fn extend_free_vars(
        &self,
        vars: &mut IndexSet<T::Var>,
        to_remove: &mut IndexSet<T::Var>,
        sym_name_link: bool,
    ) {
        for bind in &self.binds {
            bind.value.free_vars(vars, to_remove, sym_name_link);
        }

        for value in &self.values {
            value.free_vars(vars, to_remove, sym_name_link);
        }

        to_remove.extend(
            self.binds
                .iter()
                .flat_map(|bind| &bind.defs)
                .map(|def| def.var().clone()),
        );
    }

    pub(crate) fn cf_free_vars(&self, addrs: &mut HashMap<Option<T::BlockAddr>, usize>) {
        for b in &self.binds {
            b.value.cf_free_vars(addrs);
        }
    }
}

impl<T: Language> Value<T> {
    pub(crate) fn free_vars(
        &self,
        vars: &mut IndexSet<T::Var>,
        to_remove: &mut IndexSet<T::Var>,
        sym_name_link: bool,
    ) {
        match self {
            Value::Variable(v) => {
                vars.insert(v.clone());
            }
            Value::Thunk(thunk) => {
                thunk.free_vars(vars, to_remove, sym_name_link);
            }
            Value::Op { op, args } => {
                for arg in args {
                    arg.free_vars(vars, to_remove, sym_name_link);
                }
                if sym_name_link {
                    if let Some(s) = op.sym_name() {
                        to_remove.insert(s.into());
                    }
                    vars.extend(op.symbols_used().map(|s| s.into()))
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
    pub(crate) fn free_vars(
        &self,
        vars: &mut IndexSet<T::Var>,
        to_remove: &mut IndexSet<T::Var>,
        sym_name_link: bool,
    ) {
        let mut new_vars: IndexSet<T::Var> = IndexSet::new();
        self.body
            .extend_free_vars(&mut new_vars, to_remove, sym_name_link);
        to_remove.extend(self.args.iter().map(|def| def.var().clone()));
        for b in &self.blocks {
            b.expr
                .extend_free_vars(&mut new_vars, to_remove, sym_name_link);
            to_remove.extend(b.args.iter().map(|def| def.var().clone()));
        }

        vars.extend(new_vars.difference(to_remove).cloned());
    }
}
