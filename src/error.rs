use ariadne::{Color, Config, Fmt, Label, Report, ReportBuilder, ReportKind, sources};
use chumsky::prelude::*;
use itertools::Itertools;
use std::mem::discriminant;
use std::ops::Range;
use std::sync::Arc;

/// The stream compiler Error
#[derive(Debug, Clone, PartialEq)]
#[repr(u64)]
#[rustfmt::skip]
pub enum Error {
    During { label: String, error: Box<Error> } = 0,
    ExpectedInputFound { span: (Arc<str>, Range<usize>), expected: Vec<String>, found: Option<char> } = 1,
    UnexpectedKeyword { span: (Arc<str>, Range<usize>), expected: String, found:String } = 2,
    ExpectedKeyword { span: (Arc<str>, Range<usize>), expected: &'static str, found: Option<String> } = 3,
}

impl chumsky::Error<char> for Error {
    type Span = (Arc<str>, Range<usize>);
    type Label = String;

    fn expected_input_found<Iter: IntoIterator<Item = Option<char>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<char>,
    ) -> Self {
        Error::ExpectedInputFound {
            span,
            expected: expected
                .into_iter()
                .filter_map(|c| c)
                .map(|c| c.to_string())
                .collect(),
            found,
        }
    }

    fn with_label(self, label: Self::Label) -> Self {
        Error::During {
            label,
            error: Box::new(self),
        }
        .compact_while()
        .compact_expected_input()
    }

    fn merge(self, other: Self) -> Self {
        if let Some(e) = self.clone().combine(other.clone()) {
            e
        } else if let Some(e) = other.clone().combine(self.clone()) {
            e
        } else {
            Error::During {
                label: "multiple errors".to_string(),
                error: Box::new(self),
            }
        }
        .compact_while()
        .compact_expected_input()
    }
}

impl Error {
    fn combine(self, other: Self) -> Option<Self> {
        if let Error::During { label, error } = self {
            other.combine_while(label, *error)
        } else if let Error::ExpectedInputFound {
            span,
            expected,
            found,
        } = self
        {
            other.combine_expected_input_found(span, expected, found)
        } else {
            None
        }
    }

    fn combine_while(self, label: String, error: Error) -> Option<Self> {
        Some(match self {
            Self::During { label: l, error: e } => Self::During {
                label: format!("{} and {}", l, label),
                error: Box::new(e.merge(error)),
            },
            _ => Self::During {
                label,
                error: Box::new(error),
            },
        })
    }

    pub fn compact_while(self) -> Self {
        if let Self::During { label, error } = self {
            if let Self::During { label: l, error: e } = *error {
                Self::During {
                    label: format!("{} -> {}", label, l),
                    error: e,
                }
                .compact_while()
            } else {
                Self::During { label, error }
            }
        } else {
            self
        }
    }

    pub fn compact_expected_input(self) -> Self {
        if let Self::ExpectedInputFound {
            span,
            expected,
            found,
        } = self
        {
            Self::ExpectedInputFound {
                span,
                expected: expected.into_iter().unique().collect(),
                found,
            }
        } else if let Self::During { label, error } = self {
            Self::During {
                label,
                error: Box::new(error.compact_expected_input()),
            }
        } else {
            self
        }
    }

    fn combine_expected_input_found(
        self,
        span: (Arc<str>, Range<usize>),
        expected: Vec<String>,
        found: Option<char>,
    ) -> Option<Self> {
        Some(match self {
            Self::ExpectedInputFound {
                span: s,
                expected: e,
                found: f,
            } => Self::ExpectedInputFound {
                span: s,
                expected: e.into_iter().chain(expected).collect(),
                found: found.or(f),
            },
            _ => Self::ExpectedInputFound {
                span,
                expected,
                found,
            },
        })
    }

    fn discriminant(&self) -> u64 {
        // SAFETY: Because `Self` is marked `repr(u8)`, its layout is a `repr(C)` `union`
        // between `repr(C)` structs, each of which has the `u8` discriminant as its first
        // field, so we can read the discriminant without offsetting the pointer.
        unsafe { *<*const _>::from(self).cast::<u64>() }
    }

    pub fn report_builder(&self) -> ReportBuilder<'_, (Arc<str>, Range<usize>)> {
        match self {
            Error::ExpectedInputFound {
                span,
                expected,
                found,
            } => Report::build(ReportKind::Error, span.clone())
                .with_message(format!(
                    "expected one of {}, found '{}'",
                    expected
                        .iter()
                        .map(|s| format!("'{}'", s.fg(Color::Green)))
                        .intersperse(", ".to_string())
                        .collect::<String>(),
                    found.unwrap_or(' ').fg(Color::Red)
                ))
                .with_label(Label::new(span.clone()).with_color(Color::Red))
                .with_code(self.discriminant()),
            Error::UnexpectedKeyword {
                span,
                expected,
                found,
            } => Report::build(ReportKind::Error, span.clone())
                .with_message(format!(
                    "expected {}, found keyword '{}'",
                    expected.fg(Color::Green),
                    found.fg(Color::Red)
                ))
                .with_label(Label::new(span.clone()).with_color(Color::Red))
                .with_code(self.discriminant()),
            Error::ExpectedKeyword {
                span,
                expected,
                found,
            } => Report::build(ReportKind::Error, span.clone())
                .with_message(format!(
                    "expected keyword '{}', found {}",
                    expected.fg(Color::Green),
                    found
                        .as_ref()
                        .map(|s| format!("'{}'", s.fg(Color::Red)))
                        .unwrap_or("end of input".to_string())
                ))
                .with_label(Label::new(span.clone()).with_color(Color::Red))
                .with_code(self.discriminant()),
            Error::During { label, error } => error
                .report_builder()
                .with_note(format!("during {}", label)),
        }
    }

    pub fn report(&self) -> Report<(Arc<str>, Range<usize>)> {
        self.report_builder().finish()
    }
}
