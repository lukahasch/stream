pub mod lib;
use lib::{identity, parser};

use crate::{
    Result, Span,
    error::{Error, Warning},
    parser::lib::{Parsable, Parser},
};

const KEYWORDS: &[&str] = &["def"];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier<'a> {
    identifier: &'a str,
    span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression<'a> {
    Define(Pattern<'a>, Box<Expression<'a>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern<'a> {
    Capture(Identifier<'a>),
}

impl<'a> Parsable<'a> for Identifier<'a> {
    fn parse(
        input: &'a str,
        index: usize,
        location: &'static str,
    ) -> Result<(usize, Self), Warning, Error, Error> {
        todo!()
    }
}
