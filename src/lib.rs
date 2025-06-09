use std::ops::{Deref, DerefMut};

pub mod types;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T, S> {
    pub value: T,
    pub span: S,
}

impl<T, S> Spanned<T, S> {
    pub fn new(value: T, span: S) -> Self {
        Spanned { value, span }
    }

    pub fn into_inner(self) -> T {
        self.value
    }
}

impl<T, S> Deref for Spanned<T, S> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T, S> DerefMut for Spanned<T, S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}
