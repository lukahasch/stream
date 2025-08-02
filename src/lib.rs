#![feature(
    impl_trait_in_bindings,
    stmt_expr_attributes,
    decl_macro,
    impl_trait_in_fn_trait_return,
    try_trait_v2,
    never_type
)]
#![allow(invalid_type_param_default)]

use crate::error::{Error, Warning};

pub mod error;
pub mod parser;

#[derive(Debug, Clone, PartialEq)]
pub enum Result<T, W = Warning, E = Error, F = Error> {
    Ok(T),
    Warning(T, Vec<W>),
    Error(E),
    Fatal(F),
}

impl<T, W, E, F> Result<T, W, E, F> {
    pub fn is_ok(&self) -> bool {
        matches!(self, Result::Ok(_))
    }

    pub fn is_warning(&self) -> bool {
        matches!(self, Result::Warning(_, _))
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Result::Error(_))
    }

    pub fn is_fatal(&self) -> bool {
        matches!(self, Result::Fatal(_))
    }

    pub fn map<O>(self, f: impl FnOnce(T) -> O) -> Result<O, W, E, F> {
        match self {
            Result::Ok(value) => Result::Ok(f(value)),
            Result::Warning(value, warnings) => Result::Warning(f(value), warnings),
            Result::Error(err) => Result::Error(err),
            Result::Fatal(fatal) => Result::Fatal(fatal),
        }
    }

    pub fn with_warnings(self, w: Vec<W>) -> Self {
        if w.len() > 0 {
            match self {
                Result::Ok(value) => Result::Warning(value, w),
                Result::Warning(value, mut warnings) => {
                    warnings.extend(w);
                    Result::Warning(value, warnings)
                }
                err @ Result::Error(_) | err @ Result::Fatal(_) => err,
            }
        } else {
            self
        }
    }

    pub fn fatal<F2>(self) -> Result<T, W, E, F2>
    where
        E: Into<F2>,
        F: Into<F2>,
    {
        match self {
            Result::Ok(value) => Result::Ok(value),
            Result::Warning(value, warnings) => Result::Warning(value, warnings),
            Result::Error(err) => Result::Fatal(err.into()),
            Result::Fatal(fatal) => Result::Fatal(fatal.into()),
        }
    }
}

impl<T> std::ops::Try for Result<T> {
    type Output = (T, Option<Vec<Warning>>);
    type Residual = Result<!>;

    fn from_output(output: Self::Output) -> Self {
        let (value, warnings) = output;
        if let Some(warnings) = warnings {
            Result::Warning(value, warnings)
        } else {
            Result::Ok(value)
        }
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self {
            Result::Ok(value) => std::ops::ControlFlow::Continue((value, None)),
            Result::Warning(value, warnings) => {
                std::ops::ControlFlow::Continue((value, Some(warnings)))
            }
            Result::Error(err) => std::ops::ControlFlow::Break(Result::Error(err)),
            Result::Fatal(fatal) => std::ops::ControlFlow::Break(Result::Fatal(fatal)),
        }
    }
}

impl<T> std::ops::FromResidual for Result<T> {
    fn from_residual(residual: <Self as std::ops::Try>::Residual) -> Self {
        match residual {
            Result::Error(err) => Result::Error(err),
            Result::Fatal(fatal) => Result::Fatal(fatal),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    location: &'static str,
    range: std::ops::Range<usize>,
}

impl std::ops::Add for Span {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        if self.location != other.location {
            panic!("Cannot add spans from different locations");
        }
        Span {
            location: self.location,
            range: self.range.start..other.range.end,
        }
    }
}

impl ariadne::Span for Span {
    type SourceId = &'static str;

    fn source(&self) -> &Self::SourceId {
        &self.location
    }

    fn start(&self) -> usize {
        self.range.start
    }

    fn end(&self) -> usize {
        self.range.end
    }
}
