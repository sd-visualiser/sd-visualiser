use std::hash::Hash;
use std::ops::{Deref, DerefMut};
use std::sync::Weak;

#[derive(Debug)]
pub(super) struct WeakByAddress<T>(pub(super) Weak<T>);

impl<T> PartialEq for WeakByAddress<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.ptr_eq(&other.0)
    }
}

impl<T> Eq for WeakByAddress<T> {}

impl<T> Hash for WeakByAddress<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.0.as_ptr(), state)
    }
}

impl<T> Deref for WeakByAddress<T> {
    type Target = Weak<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for WeakByAddress<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
