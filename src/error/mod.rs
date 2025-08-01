use crate::Span;
use ariadne::Label;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    Error(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Warning {}
