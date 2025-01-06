use crate::{Span, parser::lexer::Token};

/// The error type used by the stream compiler
#[derive(Debug, Clone, PartialEq)]
#[repr(u32)]
pub enum Error {
    UnterminatedString(Span) = 0,
    ExpectedEOF {
        found: Option<String>,
        span: Span,
    } = 1,
    UnexpectedEOF {
        expected: Vec<String>,
        span: Span,
    } = 2,
    UnexpetedInput {
        found: String,
        expected: Vec<String>,
        span: Span,
    } = 3,
    UnclosedDelimiter {
        unclosed_span: Span,
        unclosed: String,
        span: Span,
        expected: String,
        found: Option<String>,
    } = 4,
    UnknownEscapeSequence {
        sequence: char,
        span: Span,
    } = 5,
}

pub fn display_char(c: char) -> String {
    format!("'{}'", c)
}

impl chumsky::Error<char> for Error {
    type Span = Span;
    type Label = &'static str;

    fn with_label(self, label: Self::Label) -> Self {
        self
    }

    fn merge(self, other: Self) -> Self {
        self
    }

    fn expected_input_found<Iter: IntoIterator<Item = Option<char>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<char>,
    ) -> Self {
        let expected: Vec<char> = expected.into_iter().filter_map(|c| c).collect();
        if expected.len() == 0 {
            Self::ExpectedEOF {
                found: found.map(|f| display_char(f)),
                span,
            }
        } else if found.is_none() {
            Self::UnexpectedEOF {
                expected: expected
                    .into_iter()
                    .map(|f| display_char(f))
                    .collect::<Vec<_>>(),
                span,
            }
        } else {
            Self::UnexpetedInput {
                found: format!("{:?}", found.unwrap()),
                expected: expected
                    .into_iter()
                    .map(|f| display_char(f))
                    .collect::<Vec<_>>(),
                span,
            }
        }
    }

    fn unclosed_delimiter(
        unclosed_span: Self::Span,
        unclosed: char,
        span: Self::Span,
        expected: char,
        found: Option<char>,
    ) -> Self {
        Self::UnclosedDelimiter {
            unclosed_span,
            unclosed: unclosed.into(),
            span,
            expected: expected.into(),
            found: found.map(|f| display_char(f)),
        }
    }
}

impl chumsky::Error<Token> for Error {
    type Span = Span;
    type Label = &'static str;

    fn with_label(self, label: Self::Label) -> Self {
        self
    }

    fn merge(self, other: Self) -> Self {
        self
    }

    fn expected_input_found<Iter: IntoIterator<Item = Option<Token>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<Token>,
    ) -> Self {
        let expected: Vec<Token> = expected.into_iter().filter_map(|c| c).collect();
        if expected.len() == 0 {
            Self::ExpectedEOF {
                found: found.map(|f| format!("{}", f)),
                span,
            }
        } else if found.is_none() {
            Self::UnexpectedEOF {
                expected: expected
                    .into_iter()
                    .map(|f| format!("{}", f))
                    .collect::<Vec<_>>(),
                span,
            }
        } else {
            Self::UnexpetedInput {
                found: format!("{}", found.unwrap()),
                expected: expected
                    .into_iter()
                    .map(|f| format!("{}", f))
                    .collect::<Vec<_>>(),
                span,
            }
        }
    }

    fn unclosed_delimiter(
        unclosed_span: Self::Span,
        unclosed: Token,
        span: Self::Span,
        expected: Token,
        found: Option<Token>,
    ) -> Self {
        Self::UnclosedDelimiter {
            unclosed_span,
            unclosed: format!("{}", unclosed),
            span,
            expected: format!("{}", expected),
            found: found.map(|f| format!("{}", f)),
        }
    }
}

impl Error {
    pub fn code(&self) -> u32 {
        // SAFETY: Because `Self` is marked `repr(u32)`, its layout is a `repr(C)` `union`
        //         between `repr(C)` structs, each of which has the `u32` discriminant as its first
        //         field, so we can read the discriminant without offsetting the pointer.
        unsafe { *<*const _>::from(self).cast::<u32>() }
    }
}
