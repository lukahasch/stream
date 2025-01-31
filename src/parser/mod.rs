use std::sync::Arc;

use nom::branch::alt;
use nom::bytes::{is_not, tag};
use nom::character::complete::char;
use nom::combinator::fail;
use nom::sequence::{self, delimited};
use nom::{IResult, Parser};
use nom_language::precedence::{Assoc, Operation, binary_op, precedence, unary_op};
use spanned::Spanned;

use crate::error::Error;
use crate::{Expression, Node};

pub mod spanned;

pub fn ws<'a, P>(
    parser: P,
) -> impl Parser<
    Spanned<'a>,
    Output = <P as Parser<Spanned<'a>>>::Output,
    Error = <P as Parser<Spanned<'a>>>::Error,
>
where
    P: Parser<Spanned<'a>>,
{
    delimited(
        nom::character::complete::multispace0,
        parser,
        nom::character::complete::multispace0,
    )
}

pub fn integer(input: Spanned) -> IResult<Spanned, Node<()>, Error> {
    let s = input.span();
    nom::character::complete::digit1(input).map(|(next_input, digits)| {
        let value: i64 = digits.as_str().parse().unwrap();
        let node = Node::new(Expression::Integer(value), s + next_input.span());
        (next_input, node)
    })
}

pub fn float(input: Spanned) -> IResult<Spanned, Node<()>, Error> {
    let s = input.span();
    (
        nom::character::complete::digit1,
        nom::character::complete::char('.'),
        nom::character::complete::digit1,
    )
        .parse(input)
        .map(|(next_input, (int_part, _, frac_part))| {
            let value: f64 = format!("{}.{}", int_part.as_str(), frac_part.as_str())
                .parse()
                .unwrap();
            let node = Node::new(Expression::Float(value), s + next_input.span());
            (next_input, node)
        })
}

pub fn string(input: Spanned) -> IResult<Spanned, Node<()>, Error> {
    let s = input.span();
    let (input, _) = char('"')(input)?;
    let (input, content) = is_not("\"").parse(input)?;
    let (input, _) = char('"')(input)?;
    Ok((
        input,
        Node::new(
            Expression::String(content.as_str().into()),
            s + input.span(),
        ),
    ))
}

pub fn ident(input: Spanned) -> IResult<Spanned, Spanned, Error> {
    nom::character::complete::alpha1(input).map(|(next_input, ident)| (next_input, ident))
}

pub fn variable(input: Spanned) -> IResult<Spanned, Node<()>, Error> {
    let s = input.span();
    let (input, name) = nom::character::complete::alpha1(input)?;
    Ok((
        input,
        Node::new(Expression::Variable(name.as_str().into()), s + input.span()),
    ))
}

/// seperated by ,
pub fn tuple(input: Spanned) -> IResult<Spanned, Node<()>, Error> {
    let s = input.span();
    let (input, _) = char('(')(input)?;
    let (input, _) = nom::character::complete::multispace0(input)?;
    let (input, elements) = nom::multi::separated_list0(ws(char(',')), complex).parse(input)?;
    let (input, _) = nom::character::complete::multispace0(input)?;
    let (input, _) = char(')')(input)?;
    Ok((
        input,
        Node::new(Expression::Tuple(elements), s + input.span()),
    ))
}

pub fn primitive(input: Spanned) -> IResult<Spanned, Node<()>, Error> {
    let (input, _) = nom::character::complete::multispace0(input)?;
    alt((
        float,
        integer,
        string,
        variable,
        tuple,
        delimited(ws(char('(')), complex, ws(char(')'))),
    ))
    .parse(input)
}

pub fn complex(input: Spanned) -> IResult<Spanned, Node<()>, Error> {
    let (input, _) = nom::character::complete::multispace0(input)?;

    pub enum Pre {
        Neg,
    }

    pub enum Post {
        Access(Arc<str>),
        Call(Node<()>),
    }

    precedence(
        fail(),
        alt((
            unary_op(
                6,
                sequence::pair(char('.'), ident)
                    .map(|(_, name)| Post::Access(name.as_str().into())),
            ),
            unary_op(7, primitive.map(|arg| Post::Call(arg))),
        )),
        alt((
            binary_op(9, Assoc::Left, ws(char('*'))),
            binary_op(9, Assoc::Left, ws(char('/'))),
            binary_op(10, Assoc::Left, ws(char('+'))),
            binary_op(10, Assoc::Left, ws(char('-'))),
        )),
        primitive,
        |op: Operation<Pre, Post, char, Node<()>>| -> Result<Node<()>, Error> {
            use nom_language::precedence::Operation::*;
            match op {
                Postfix(value, Post::Access(name)) => Ok(Node::new(
                    Expression::Access {
                        value: Box::new(value.clone()),
                        field: name,
                    },
                    value.span,
                )),
                Postfix(value, Post::Call(arg)) => Ok(Node::new(
                    Expression::Call {
                        f: Box::new(value.clone()),
                        arg: Box::new(arg.clone()),
                    },
                    value.span + arg.span,
                )),
                Binary(lhs, '*', rhs) => Ok(Node::new(
                    Expression::Call {
                        f: Box::new(Node::call(Expression::Variable("*".into()), rhs.clone())),
                        arg: Box::new(lhs.clone()),
                    },
                    lhs.span + rhs.span,
                )),
                Binary(lhs, '/', rhs) => Ok(Node::new(
                    Expression::Call {
                        f: Box::new(Node::call(Expression::Variable("/".into()), rhs.clone())),
                        arg: Box::new(lhs.clone()),
                    },
                    lhs.span + rhs.span,
                )),
                Binary(lhs, '+', rhs) => Ok(Node::new(
                    Expression::Call {
                        f: Box::new(Node::call(Expression::Variable("+".into()), rhs.clone())),
                        arg: Box::new(lhs.clone()),
                    },
                    lhs.span + rhs.span,
                )),
                Binary(lhs, '-', rhs) => Ok(Node::new(
                    Expression::Call {
                        f: Box::new(Node::call(Expression::Variable("-".into()), rhs.clone())),
                        arg: Box::new(lhs.clone()),
                    },
                    lhs.span + rhs.span,
                )),
                _ => unreachable!(),
            }
        },
    )
    .or(primitive)
    .parse(input)
}
