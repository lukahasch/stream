use crate::{
    Result, Span,
    error::{Error, Expected, Found, Warning},
};
use std::marker::PhantomData;

pub trait Parser<'a, W, E, F, Output> {
    /// Parses the input string starting at the given index and returns the new index and output.
    fn parse(
        &self,
        input: &'a str,
        index: usize,
        location: &'static str,
    ) -> Result<(usize, Output), W, E, F>;
}

pub trait Parsable<'a, W = Warning, E = Error, F = Error>: Sized {
    fn parse(
        input: &'a str,
        index: usize,
        location: &'static str,
    ) -> Result<(usize, Self), W, E, F>;
}

pub struct Type<T>(pub PhantomData<fn(T)>);

impl<'a, T, W, E, F> Parser<'a, W, E, F, T> for Type<T>
where
    T: Parsable<'a, W, E, F>,
{
    fn parse(
        &self,
        input: &'a str,
        index: usize,
        location: &'static str,
    ) -> Result<(usize, T), W, E, F> {
        T::parse(input, index, location)
    }
}

impl<'a, Func, W, E, F, Output> Parser<'a, W, E, F, Output> for Func
where
    Func: Fn(&'a str, usize, &'static str) -> Result<(usize, Output), W, E, F>,
{
    fn parse(
        &self,
        input: &'a str,
        index: usize,
        location: &'static str,
    ) -> Result<(usize, Output), W, E, F> {
        self(input, index, location)
    }
}

pub fn with_ctx<'a, W, E, F, T>(
    parser: impl Parser<'a, W, E, F, T>,
    input: &'a str,
    index: &mut usize,
    location: &'static str,
) -> Result<T, W, E, F> {
    parser
        .parse(input, *index, location)
        .map(|(new_index, output)| {
            *index = new_index;
            output
        })
}

pub fn whitespace(input: &str, index: &mut usize) {
    while *index < input.len() && input[*index..].starts_with(char::is_whitespace) {
        *index += 1;
    }
}

pub fn identity<T>(val: T) -> T {
    val
}

pub fn fatal<'a, T, W, E, F, P>(p: P) -> impl Parser<'a, W, E, F, T>
where
    P: Parser<'a, W, E, F, T>,
    F: From<E>,
{
    move |input: &'a str, index: usize, location: &'static str| {
        p.parse(input, index, location).fatal()
    }
}

/// TODO: fatal modifier, fatal action, optional, repeat, intersperse
pub macro parser {
    (@modifiers($wrapper:expr, {$($field:ident => $parser:expr,)*}, [$($before:expr,)*], [$($modifier:expr,)*]) {! $($rest:tt)*} $($outside:tt)*) => {
        parser!(@modifiers($wrapper, {$($field => $parser,)*}, [$($before,)*], [$($modifier,)* fatal,]) {$($rest)*} $($outside)*)
    },
    (@modifiers($wrapper:expr, {$($field:ident => $parser:expr,)*}, [$($before:expr,)*], [$($modifier:expr,)*]) {? $($rest:tt)*} $($outside:tt)*) => {
        parser!(@modifiers($wrapper, {$($field => $parser,)*}, [$($before,)*], [$($modifier,)* optional,]) {$($rest)*} $($outside)*)
    },
    (@modifiers($wrapper:expr, {$($field:ident => $parser:expr,)*}, [$($before:expr,)*], [$($modifier:expr,)*]) {* $($rest:tt)*} $($outside:tt)*) => {
        parser!(@modifiers($wrapper, {$($field => $parser,)*}, [$($before,)*], [$($modifier,)* repeat,]) {$($rest)*} $($outside)*)
    },
    (@modifiers($wrapper:expr, {$($field:ident => $parser:expr,)*}, [$($before:expr,)*], [$($modifier:expr,)*]) {$($rest:tt)*} $($outside:tt)*) => {
        parser!(@parser($wrapper, {$($field=> $parser,)*}, [$($before,)*], [$($modifier,)*]) {$($rest)*} $($outside)*)
    },

    (@parser($wrapper:expr, {$($field:ident => $parser:expr,)*}, [$($before:expr,)*], [$($modifier:expr,)*]) {#($($tt:tt)*) $($rest:tt)*} $($outside:tt)*) => {
        parser!(@finish($wrapper, {$($field => $parser,)*}, [$($before,)*], [$($modifier,)*], parser!($($tt)*)) {$($rest)*} $($outside)*)
    },
    (@parser($wrapper:expr, {$($field:ident => $parser:expr,)*}, [$($before:expr,)*], [$($modifier:expr,)*]) {$lit:literal $($rest:tt)*} $($outside:tt)*) => {
        parser!(@finish($wrapper, {$($field => $parser,)*}, [$($before,)*], [$($modifier,)*], $lit) {$($rest)*} $($outside)*)
    },
    (@parser($wrapper:expr, {$($field:ident => $parser:expr,)*}, [$($before:expr,)*], [$($modifier:expr,)*]) {:$ty:ty} $($outside:tt)*) => {
        parser!(@finish($wrapper, {$($field => $parser,)*}, [$($before,)*], [$($modifier,)*], Type::<$ty>(PhantomData)) {} $($outside)*)
    },
    (@parser($wrapper:expr, {$($field:ident => $parser:expr,)*}, [$($before:expr,)*], [$($modifier:expr,)*]) {$expr:expr} $($outside:tt)*) => {
        parser!(@finish($wrapper, {$($field => $parser,)*}, [$($before,)*], [$($modifier,)*], $expr) {} $($outside)*)
    },
    (@parser($wrapper:expr, {$($field:ident => $parser:expr,)*}, [$($before:expr,)*], [$($modifier:expr,)*]) {:$ty:ty => $($rest:tt)*} $($outside:tt)*) => {
        parser!(@finish($wrapper, {$($field => $parser,)*}, [$($before,)*], [$($modifier,)*], (Type::<$ty>(PhantomData))) {=> $($rest)*} $($outside)*)
    },
    (@parser($wrapper:expr, {$($field:ident => $parser:expr,)*}, [$($before:expr,)*], [$($modifier:expr,)*]) {$expr:expr => $($rest:tt)*} $($outside:tt)*) => {
        parser!(@finish($wrapper, {$($field => $parser,)*}, [$($before,)*], [$($modifier,)*], $expr) {=> $($rest)*} $($outside)*)
    },

    (@finish($wrapper:expr, {$($field:ident => $parser:expr,)*}, [$($before:expr,)*], [$($modifier:expr,)*], $to_finish:expr) {} $($outside:tt)*) => {
        parser!(@step($wrapper, {$($field=> $parser,)*}, [$($before,)* {
            let mut fatal = false;
            let parser = $to_finish;
            $(
                let parser = $modifier(parser);
            )*
            let parser = $wrapper(parser);
            move |input, mut index, location| {
                whitespace(input, &mut index);
                parser.parse(input, index, location).cond_fatal(fatal)
            }
        },]) $($outside)*)
    },
    (@finish($wrapper:expr, {$($field:ident => $parser:expr,)*}, [$($before:expr,)*], [$($modifier:expr,)*], $to_finish:expr) {=> $ident:ident} $($outside:tt)*) => {
        parser!(@step($wrapper, {
            $($field=> $parser,)*
            $ident => {
                let parser = $to_finish;
                $(
                    let parser = $modifier(parser);
                )*
                let parser = $wrapper(parser);
                move |input, mut index, location| {
                    let mut warnings = Vec::new();
                    $(
                        _ = match with_ctx(
                            $before,
                            input,
                            &mut index,
                            location,
                        ){
                            Result::Ok(_) => {},
                            Result::Warning(_, warning) => {
                                warnings.extend(warning);
                            },
                            Result::Error(err) => return Result::Error(err),
                            Result::Fatal(fatal) => return Result::Fatal(fatal),
                        };
                    )*
                    whitespace(input, &mut index);
                    <_ as Parser<'_, _, _, _, _>>::parse(&parser, input, index, location).with_warnings(warnings)
                }
            },
        }, []) $($outside)*)
    },

    (@step($wrapper:expr, {$($field:ident => $parser:expr,)*}, [$($before:expr,)*]) {!} $($outside:tt)*) => {
        parser!(@step(fatal, {$($field=> $parser,)*}, [$($before,)*]) $($outside)*)
    },
    (@step($wrapper:expr, {$($field:ident => $parser:expr,)*}, [$($before:expr,)*]) {$($rest:tt)*} $($outside:tt)*) => {
        parser!(@modifiers($wrapper, {$($field=> $parser,)*}, [$($before,)*], []) {$($rest)*} $($outside)*)
    },
    (@step($wrapper:expr, {$($field:ident => $parser:expr,)*}, [$($before:expr,)*]) $lit:literal $($outside:tt)*) => {
        parser!(@step($wrapper, {$($field=> $parser,)*}, [$($before,)* $wrapper($lit),]) $($outside)*)
    },
    (@step($wrapper:expr, {$($field:ident => $parser:expr,)*}, [$($before:expr,)*]) $tt:tt $($outside:tt)*) => {
        parser!(@step($wrapper, {$($field=> $parser,)*}, [$($before,)* $wrapper(stringify!($tt)),]) $($outside)*)
    },

    (@step($wrapper:expr, {$($field:ident => $parser:expr,)*}, [$($before:expr,)*])) => {
        #[allow(non_camel_case_types, unreachable_code)]
        {
            #[derive(Debug, Clone, PartialEq, Eq)]
            struct Output<$($field,)*> {
            $(
                $field: $field,
            )*
            }

            move |input: &str, mut index: usize, location: &'static str| {
                let mut warnings = Vec::new();
                $(
                    _ = match with_ctx(
                        $before,
                        input,
                        &mut index,
                        location,
                    ) {
                        Result::Ok(_) => {},
                        Result::Warning(_, warning) => {
                            warnings.extend(warning);
                        },
                        Result::Error(err) => return Result::Error(err),
                        Result::Fatal(fatal) => return Result::Fatal(fatal),
                    };
                )*
                let output = Output {
                    $(
                        $field: {
                            whitespace(input, &mut index);
                            match with_ctx($parser, input, &mut index, location) {
                                Result::Ok(value) => value,
                                Result::Warning(value, warning) => {
                                    warnings.extend(warning);
                                    value
                                },
                                Result::Error(err) => return Result::Error(err),
                                Result::Fatal(fatal) => return Result::Fatal(fatal.into()),
                            }
                        },
                    )*
                };
                Result::Ok((index, output)).with_warnings(warnings)
            }
        }
    },

    ($($tt:tt)*) => {
        parser!(@step(identity, {}, [])  $($tt)*)
    },
}

impl<'a, 'b> Parser<'a, Warning, Error, Error, &'a str> for &'b str {
    fn parse(
        &self,
        input: &'a str,
        index: usize,
        location: &'static str,
    ) -> Result<(usize, &'a str), Warning, Error, Error> {
        if input.get(index..).is_some_and(|s| s.starts_with(self)) {
            Result::Ok((index + self.len(), &input[index..index + self.len()]))
        } else {
            let found = Found::parse(input, index, location)?;
            let expeted = Expected::Thing(self.to_string());
            todo!()
        }
    }
}

pub fn index(_: &str, index: usize, location: &'static str) -> Result<Span, Warning, Error, Error> {
    Result::Ok(Span {
        location: location,
        range: index..index,
    })
}
