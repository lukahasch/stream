use ariadne::{Color, Fmt, Label, Report, ReportKind};

use crate::{Span, beginning, parser::lexer::Token};

/// The error type used by the stream compiler
#[derive(Debug, Clone, PartialEq)]
#[repr(u32)]
pub enum Error {
    UnterminatedString(Span) = 0,
    UnexpetedInput {
        found: String,
        expected: Vec<String>,
        span: Span,
    } = 1,
    UnclosedDelimiter {
        unclosed_span: Span,
        unclosed: String,
        span: Span,
        expected: String,
        found: Option<String>,
    } = 2,
    UnknownEscapeSequence {
        sequence: char,
        span: Span,
    } = 3,
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
        match (self, other) {
            (
                Self::UnexpetedInput {
                    found,
                    expected,
                    span,
                },
                Self::UnexpetedInput { expected: e2, .. },
            ) => Self::UnexpetedInput {
                found,
                expected: expected.into_iter().chain(e2).collect(),
                span,
            },
            (s, _) => s,
        }
    }

    fn expected_input_found<Iter: IntoIterator<Item = Option<char>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<char>,
    ) -> Self {
        let e: Vec<char> = expected.into_iter().filter_map(|c| c).collect();
        let expected: Vec<String> = if e.len() == 0 {
            vec!["EOF".to_string()]
        } else {
            e.into_iter().map(|c| display_char(c)).collect()
        };
        let found = found.map(|f| display_char(f)).unwrap_or("EOF".to_string());
        Self::UnexpetedInput {
            found,
            expected,
            span,
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
        let e: Vec<Token> = expected.into_iter().filter_map(|c| c).collect();
        let expected: Vec<String> = if e.len() == 0 {
            vec!["EOF".to_string()]
        } else {
            e.into_iter().map(|c| format!("{}", c)).collect()
        };
        let found = found.map(|f| format!("{}", f)).unwrap_or("EOF".to_string());
        Self::UnexpetedInput {
            found,
            expected,
            span,
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
    pub fn report(&self) -> Report<Span> {
        match self {
            Self::UnterminatedString(span) => Report::build(ReportKind::Error, span.clone())
                .with_code(self.code())
                .with_message("Unterminated String")
                .with_label(
                    Label::new(beginning(span))
                        .with_message("This \" is unterminated")
                        .with_color(Color::Red),
                )
                .with_help("Terminate the string with a \"")
                .finish(),
            Self::UnexpetedInput {
                expected,
                found,
                span,
            } => Report::build(ReportKind::Error, span.clone())
                .with_code(self.code())
                .with_message(format!(
                    "Unexpected input: expected {}; found {}",
                    expected
                        .into_iter()
                        .map(|e| e.as_str().fg(Color::Green).to_string())
                        .intersperse(", ".fg(Color::White).to_string())
                        .collect::<String>(),
                    found.fg(Color::Red)
                ))
                .with_label(
                    Label::new(span.clone())
                        .with_message("Unexpected input")
                        .with_color(Color::Red),
                )
                .finish(),
            Self::UnclosedDelimiter {
                unclosed_span,
                unclosed,
                span,
                expected,
                found,
            } => Report::build(ReportKind::Error, span.clone())
                .with_code(self.code())
                .with_message(format!(
                    "Unclosed delimiter: expected {}; found {}",
                    expected.fg(Color::Green),
                    if let Some(found) = found {
                        found.fg(Color::Red).to_string()
                    } else {
                        "EOF".fg(Color::Red).to_string()
                    }
                ))
                .with_label(
                    Label::new(unclosed_span.clone())
                        .with_message(format!("This {} is unclosed", unclosed.fg(Color::Red)))
                        .with_color(Color::Red),
                )
                .with_label(
                    Label::new(span.clone())
                        .with_message("Error encountered here")
                        .with_color(Color::Red),
                )
                .finish(),
            _ => todo!(),
        }
    }

    pub fn code(&self) -> u32 {
        // SAFETY: Because `Self` is marked `repr(u32)`, its layout is a `repr(C)` `union`
        //         between `repr(C)` structs, each of which has the `u32` discriminant as its first
        //         field, so we can read the discriminant without offsetting the pointer.
        unsafe { *<*const _>::from(self).cast::<u32>() }
    }
}
