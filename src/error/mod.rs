use crate::{Result, Span, parser::lib::Parsable};
use ariadne::Label;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    ExpectedFound(Expected, Found, Span),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expected {
    EndOfInput,
    Keyword(String),
    Float,
    Number,
    Identifier,
    String,
    Operator,
    Thing(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Found {
    EndOfInput,
    Keyword(String),
    Float(f64),
    Number(i64),
    Identifier(String),
    String(String),
    Operator(String),
    Thing(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Warning {}

impl<'a> Parsable<'a> for Found {
    fn parse(
        input: &'a str,
        index: usize,
        location: &'static str,
    ) -> Result<(usize, Self), Warning, Error, Error> {
        todo!()
    }
}
