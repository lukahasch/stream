pub mod lib;
use lib::{identity, parser};

use crate::{
    Result,
    error::{Error, Warning},
    parser::lib::{Parsable, Parser},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Word(String);

impl<'a> Parsable<'a, Warning, Error, Error> for Word {
    fn parse(
        input: &'a str,
        index: usize,
        _: &'static str,
    ) -> Result<(usize, Self), Warning, Error, Error> {
        let mut end = index;
        while end < input.len() && input[end..].chars().next().unwrap().is_alphabetic() {
            end += 1;
        }
        if end > index {
            Result::Ok((end, Word(input[index..end].to_string())))
        } else {
            Result::Error(Error::Error("Expected a word".to_string()))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Let {
    pub pattern: Word,
    pub expr: Word,
}

impl<'a> Parsable<'a, Warning, Error, Error> for Let {
    fn parse(
        input: &'a str,
        index: usize,
        location: &'static str,
    ) -> Result<(usize, Self), Warning, Error, Error> {
        parser!(let {:Word => pattern} = {:Word => expr})
            .parse(input, index, location)
            .map(|(index, output)| {
                (
                    index,
                    Let {
                        pattern: output.pattern,
                        expr: output.expr,
                    },
                )
            })
    }
}
