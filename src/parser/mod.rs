use crate::{error::Error, *};
use chumsky::prelude::*;

pub mod lexer;

use lexer::Token;

pub fn parser() -> impl Parser<Token, Node<()>, Error = Error> {
    recursive(|expr| {
        let basic = select! {
            Token::Integer(i) => Expression::Literal(Literal::Integer(i)),
            Token::Float(f) => Expression::Literal(Literal::Float(f)),
            Token::String(s) => Expression::Literal(Literal::String(s)),
            Token::Boolean(b) => Expression::Literal(Literal::Boolean(b)),
            Token::Identifier(s) => Expression::Literal(Literal::Variable(s)),
        }
        .map_with_span(|expr, span| Node::new(expr, span));

        let list = just(Token::Paren('['))
            .ignore_then(expr.clone().separated_by(just(Token::Control(','))))
            .then_ignore(just(Token::Paren(']')))
            .map_with_span(|exprs, span| {
                Node::new(Expression::Literal(Literal::List(exprs)), span)
            });

        let tuple = just(Token::Paren('('))
            .ignore_then(expr.clone().separated_by(just(Token::Control(','))))
            .then_ignore(just(Token::Paren(')')))
            .map_with_span(|exprs, span| {
                Node::new(Expression::Literal(Literal::Tuple(exprs)), span)
            });

        let kv_pair = select! { Token::Identifier(s) => s}
            .then(just(Token::Symbol(['=', '\0'])).ignore_then(expr.clone()));

        let record = just(Token::Paren('{'))
            .ignore_then(kv_pair.clone().separated_by(just(Token::Control(','))))
            .then_ignore(just(Token::Paren('}')))
            .map_with_span(|fields, span| {
                Node::new(
                    Expression::Literal(Literal::Record(
                        fields.into_iter().map(|(k, v)| (k, v)).collect(),
                    )),
                    span,
                )
            });

        let parenthesised = just(Token::Paren('('))
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Paren(')')));

        let literal = parenthesised.or(basic).or(list).or(tuple).or(record);

        let block = just(Token::Paren('{'))
            .ignore_then(expr.clone().separated_by(just(Token::Control(';'))))
            .then_ignore(just(Token::Paren('}')))
            .map_with_span(|exprs, span| Node::new(Expression::Block(exprs), span));

        let capture = select! { Token::Identifier(s) => s }
            .then(just(Token::Control(':')).ignore_then(expr.clone()).or_not())
            .map(|(name, r#type)| Pattern::Capture { name, r#type });

        let pattern = capture;

        let generic_for = just(Token::For)
            .ignore_then(just(Token::Symbol(['<', '\0'])))
            .ignore_then(pattern.clone())
            .then_ignore(just(Token::Symbol(['>', '\0'])));

        let generic =
            generic_for
                .clone()
                .then(expr.clone())
                .map_with_span(|(generics, value), span| {
                    Node::new(Expression::Generic { generics, value }, span)
                });

        let function = pattern
            .clone()
            .then_ignore(just(Token::Symbol(['-', '>'])))
            .then(expr.clone())
            .map_with_span(|(param, body), span| {
                Node::new(Expression::Function { param, body }, span)
            });

        let r#let = just(Token::Let)
            .ignore_then(pattern.clone())
            .then_ignore(just(Token::Symbol(['=', '\0'])))
            .then(expr.clone())
            .map_with_span(|(pattern, value), span| {
                Node::new(Expression::Let { pattern, value }, span)
            });

        let generic_let = just(Token::Let)
            .ignore_then(generic_for)
            .then(pattern.clone())
            .then_ignore(just(Token::Symbol(['=', '\0'])))
            .then(expr.clone())
            .map_with_span(|((generics, pattern), value), span| {
                Node::new(
                    Expression::GenericLet {
                        pattern,
                        generics,
                        value,
                    },
                    span,
                )
            });

        let r#if = just(Token::If)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Then))
            .then(expr.clone())
            .then(just(Token::Else).ignore_then(expr.clone()).or_not())
            .map_with_span(|((condition, then), r#else), span| {
                Node::new(
                    Expression::If {
                        condition,
                        then,
                        r#else,
                    },
                    span,
                )
            });

        let match_arm = just(Token::Symbol(['|', '\0']))
            .ignore_then(pattern.clone())
            .then_ignore(just(Token::Symbol(['-', '>'])))
            .then(expr.clone());

        let r#match = just(Token::Match)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::With))
            .then(match_arm.repeated().at_least(1))
            .map_with_span(|(value, arms), span| {
                Node::new(Expression::Match { value, arms }, span)
            });

        let module = just(Token::Module)
            .ignore_then(expr.clone())
            .map_with_span(|body, span| Node::new(Expression::Module { body }, span));

        let primary = choice((
            literal,
            block,
            generic,
            function,
            r#let,
            generic_let,
            r#if,
            r#match,
            module,
        ));

        let implementation = primary
            .clone()
            .then_ignore(just(Token::Symbol([':', ':'])))
            .then(expr.clone())
            .map_with_span(|(value, implementation), span| {
                Node::new(
                    Expression::Implementation {
                        value,
                        implementation,
                    },
                    span,
                )
            })
            .or(primary);

        let access = implementation
            .clone()
            .then(
                just(Token::Symbol(['.', '\0'])).ignore_then(select! { Token::Identifier(s) => s }),
            )
            .map_with_span(|(value, field), span| {
                Node::new(Expression::Access { value, field }, span)
            })
            .or(implementation);

        let apply = access
            .clone()
            .then(access.clone())
            .then(access.clone().repeated())
            .map_with_span(|((f, arg), args), span: Span| {
                args.into_iter().fold(
                    Node::new(Expression::Apply { f, arg }, span.clone()),
                    move |acc, arg| Node::new(Expression::Apply { f: acc, arg }, span.clone()),
                )
            })
            .or(access);

        apply
    })
}

pub fn parse(s: &str, origin: Arc<str>) -> Result<Node<()>, Vec<Error>> {
    parser().parse(lexer::lex(s, origin)?)
}
