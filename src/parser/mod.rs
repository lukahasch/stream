use crate::{Error, Expression, Span};

pub struct Input<'source> {
    pub input: &'source str,
    pub source: &'source str,
    pub position: usize,
}

pub trait Parser<'source> {
    type Output: std::fmt::Debug;
    fn parse(&self, input: &'source mut Input<'source>) -> Result<Self::Output, Error<'source>>;
}

impl<'source, T> Parser<'source> for T
where
    T: Fn<(&'source mut Input<'source>,)>,
    T::Output: IsResult,
    <<T as FnOnce<(&'source mut Input<'source>,)>>::Output as IsResult>::Err: Into<Error<'source>>,
    <<T as FnOnce<(&'source mut Input<'source>,)>>::Output as IsResult>::Ok: std::fmt::Debug,
{
    type Output = <<T as FnOnce<(&'source mut Input<'source>,)>>::Output as IsResult>::Ok;

    fn parse(&self, input: &'source mut Input<'source>) -> Result<Self::Output, Error<'source>> {
        let result: Result<Self::Output, _> = self.call((input,)).into();
        match result {
            Ok(value) => Ok(value),
            Err(err) => Err(err.into()),
        }
    }
}

pub trait IsResult: Into<Result<Self::Ok, Self::Err>> {
    type Ok;
    type Err;
}

impl<T, E> IsResult for Result<T, E> {
    type Ok = T;
    type Err = E;
}

pub fn text<'source>(text: &'source str) -> impl Parser<'source> {
    move |input: &mut Input<'source>| {
        let start = input.position;
        let end = start + text.len();
        if input.input.get(start..end) == Some(text) {
            input.position = end;
            Ok(())
        } else {
            Err(Error::Expected {
                expected: text,
                found: &input.input[start..end],
                span: Span {
                    start,
                    end,
                    source: input.source,
                },
            })
        }
    }
}

#[macro_export]
macro_rules! parser {
    ($lit:literal) => {
        crate::parser::text($lit)
    };
    ($($tt:tt)*) => {
        (|input: &mut Input| -> Result<_, Error> {Ok(($(parser!($tt).parse(input)?,)*))})
    }
}

pub fn test() {
    let mut input = Input {
        input: "Hello, world!",
        source: "<test>",
        position: 0,
    };
    dbg!(parser!("H" "E" "L").parse(&mut input));
}
