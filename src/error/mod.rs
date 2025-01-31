use nom::error::{FromExternalError, ParseError};

use crate::parser::spanned::Spanned;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    InternalParserError(String),
}

impl<'a> ParseError<Spanned<'a>> for Error {
    fn from_error_kind(input: Spanned<'a>, kind: nom::error::ErrorKind) -> Self {
        Self::InternalParserError(format!("{:?} {:?}", input, kind))
    }

    fn append(input: Spanned, kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }
}

impl<'a, E> FromExternalError<Spanned<'a>, E> for Error {
    fn from_external_error(input: Spanned<'a>, kind: nom::error::ErrorKind, _e: E) -> Self {
        Self::InternalParserError(format!("{:?} {:?}", input, kind))
    }
}
