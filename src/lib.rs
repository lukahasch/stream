#![feature(impl_trait_in_bindings, never_type, stmt_expr_attributes)]

use std::marker::PhantomData;

pub trait Parser<'a> {
    type Error;
    type Location;
    type Output;

    /// Parses the input string starting at the given index and returns the new index and output.
    fn parse(
        &self,
        input: &'a str,
        index: usize,
        location: Self::Location,
    ) -> Result<(usize, Self::Output), Self::Error>;
}

pub struct FunctionParser<Error, Location, Output, F> {
    function: F,
    _marker: PhantomData<(Error, Location, Output)>,
}

impl<'a, Error, Location, Output, F> Parser<'a> for FunctionParser<Error, Location, Output, F>
where
    F: Fn(&'a str, usize, Location) -> Result<(usize, Output), Error>,
{
    type Error = Error;
    type Location = Location;
    type Output = Output;

    fn parse(
        &self,
        input: &'a str,
        index: usize,
        location: Self::Location,
    ) -> Result<(usize, Self::Output), Self::Error> {
        (self.function)(input, index, location)
    }
}

pub trait Parsable<'a>: Sized {
    type Error;
    type Location;

    fn parse(
        input: &'a str,
        index: usize,
        location: Self::Location,
    ) -> Result<(usize, Self), Self::Error>;
}

pub struct Type<T>(PhantomData<fn(T)>);

impl<'a, T: Parsable<'a>> Parser<'a> for Type<T> {
    type Error = T::Error;
    type Location = T::Location;
    type Output = T;

    fn parse(
        &self,
        input: &'a str,
        index: usize,
        location: Self::Location,
    ) -> Result<(usize, Self::Output), Self::Error> {
        T::parse(input, index, location)
    }
}

pub fn with_ctx<'a, P: Parser<'a>>(
    input: &'a str,
    index: &mut usize,
    location: P::Location,
    parser: P,
) -> Result<P::Output, P::Error> {
    let (new_index, output) = parser.parse(input, *index, location)?;
    *index = new_index;
    Ok(output)
}

pub fn cast<'a, Error, Location, Output>(
    f: impl Fn(&'a str, usize, Location) -> Result<Output, Error>,
) -> impl Fn(&'a str, usize, Location) -> Result<Output, Error> {
    move |input, index, location| f(input, index, location)
}

pub fn whitespace(input: &str, index: &mut usize) {
    while let Some(c) = input.get(*index..).and_then(|s| s.chars().next()) {
        if c == ' ' || c == '\t' || c == '\r' {
            *index += c.len_utf8();
        } else {
            break;
        }
    }
}

#[macro_export]
macro_rules! parser {
    (@step({$($field:ident : $parser:expr,)*}, [$($before:expr,)*]) $lit:literal $($rest:tt)*) => {
        parser!(
            @step(
                {$($field : $parser,)*},
                [$($before,)* $lit,]
            )
            $($rest)*
        )
    };
    (@step({$($field:ident : $parser:expr,)*}, [$($before:expr,)*]) #(:$ty:ty) $($rest:tt)*) => {
        parser!(
            @step(
                {$($field : $parser,)*},
                [$($before,)* Type::<$ty>(PhantomData),]
            )
            $($rest)*
        )
    };
    (@step({$($field:ident : $parser:expr,)*}, [$($before:expr,)*]) #($expr:expr) $($rest:tt)*) => {
        parser!(
            @step(
                {$($field : $parser,)*},
                [$($before,)* $expr,]
            )
            $($rest)*
        )
    };
    (@step({$($field:ident : $parser:expr,)*}, [$($before:expr,)*]) {$name:ident => $expr:expr} $($rest:tt)*) => {
        parser!(
            @step(
                {
                    $($field : $parser,)*
                    $name : FunctionParser { function: cast(move |input: &str, mut index: usize, location| {
                        $(
                            whitespace(input, &mut index);
                            _ = with_ctx(input, &mut index, location, $before)?;
                        )*
                        whitespace(input, &mut index);
                        <_ as Parser<'_>>::parse(&$expr, input, index, location)
                    }), _marker: PhantomData},
                },
                []
            )
            $($rest)*
        )
    };
    (@step({$($field:ident : $parser:expr,)*}, [$($before:expr,)*]) {$name:ident : $type:ty} $($rest:tt)*) => {
        parser!(
            @step(
                {
                    $($field : $parser,)*
                    $name : FunctionParser { function: cast(move |input: &str, mut index: usize, location| {
                        $(
                            whitespace(input, &mut index);
                            _ = with_ctx(input, &mut index, location, $before)?;
                        )*
                        whitespace(input, &mut index);
                        Type::<$type>(PhantomData).parse(input, index, location)
                    }), _marker: PhantomData},
                },
                []
            )
            $($rest)*
        )
    };
    (@step({$($field:ident : $parser:expr,)*}, [$($before:expr,)*]) $tt:tt $($rest:tt)*) => {
        parser!(
            @step(
                {$($field : $parser,)*},
                [$($before,)* stringify!($tt),]
            )
            $($rest)*
        )
    };
    (@step({$($field:ident : $parser:expr,)*}, [$($before:expr,)*])) => {
        #[allow(non_camel_case_types, unused_mut)]
        {
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            struct ParserOutput<$($field,)*> {
                $($field : $field,)*
            }

            FunctionParser {
                function: cast(move |input: &str, mut index: usize, location| {
                    whitespace(input, &mut index);
                    let output = ParserOutput {
                        $($field: with_ctx(input, &mut index, location, $parser)?,)*
                    };
                    $(
                        whitespace(input, &mut index);
                        _ = with_ctx(input, &mut index, location, $before)?;
                    )*
                    Ok((
                        index,
                        output
                    ))
                }),
                _marker: PhantomData,
            }
        }
    };
    ($($tt:tt)*) => {
        parser!(@step({}, []) $($tt)*)
    };
}

impl<'a> Parser<'a> for &'a str {
    type Error = ();
    type Location = ();
    type Output = &'a str;

    fn parse(
        &self,
        input: &'a str,
        index: usize,
        _location: Self::Location,
    ) -> Result<(usize, Self::Output), Self::Error> {
        if input.get(index..).is_some_and(|s| s.starts_with(self)) {
            Ok((index + self.len(), &input[index..index + self.len()]))
        } else {
            Err(())
        }
    }
}

#[derive(Debug)]
pub struct Ident<'a>(pub &'a str);

impl<'a> Parsable<'a> for Ident<'a> {
    type Error = ();
    type Location = ();

    fn parse(
        input: &'a str,
        mut index: usize,
        _location: Self::Location,
    ) -> Result<(usize, Self), Self::Error> {
        let start = index;
        while let Some(c) = input.get(index..).and_then(|s| s.chars().next()) {
            if c.is_alphanumeric() || c == '_' {
                index += c.len_utf8();
            } else {
                break;
            }
        }
        if start == index {
            Err(())
        } else {
            Ok((index, Ident(&input[start..index])))
        }
    }
}

pub fn test() {
    let parser: FunctionParser<(), _, _, _> = parser!(let {name: Ident} = {value: Ident});

    let input = "let x = 42";
    let mut index = 0;
    let location = ();
    match parser.parse(input, index, location) {
        Ok((new_index, output)) => {
            index = new_index;
            println!("Parsed successfully: {output:?}, new index: {index}",);
        }
        Err(_) => {
            println!("Failed to parse input");
        }
    }
}
