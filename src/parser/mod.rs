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

        let list = expr
            .clone()
            .separated_by(just(Token::Control(',')))
            .delimited_by(just(Token::Paren('[')), just(Token::Paren(']')))
            .map_with_span(|nodes, span| {
                Node::new(Expression::Literal(Literal::List(nodes)), span)
            });

        let tuple = expr
            .clone()
            .separated_by(just(Token::Control(',')))
            .delimited_by(just(Token::Paren('(')), just(Token::Paren(')')))
            .map_with_span(|nodes, span| {
                Node::new(Expression::Literal(Literal::Tuple(nodes)), span)
            });

        let kv_pair = expr
            .clone()
            .then_ignore(just(Token::Control(':')))
            .then(expr.clone());

        let map = kv_pair
            .clone()
            .separated_by(just(Token::Control(',')))
            .delimited_by(just(Token::Paren('{')), just(Token::Paren('}')))
            .map_with_span(|pairs, span| Node::new(Expression::Literal(Literal::Map(pairs)), span));

        let r#type = just(Token::Control(':')).ignore_then(expr.clone()).or_not();

        let capture = select! {
            Token::Identifier(s) => s,
        }
        .then(r#type.clone())
        .map(|(name, r#type)| Pattern::Capture { name, r#type });

        let pattern = capture;

        let generic = just(Token::For).ignore_then(pattern.clone().delimited_by(
            just(Token::Symbol(['<', '\0'])),
            just(Token::Symbol(['>', '\0'])),
        ));

        let r#let = just(Token::Let)
            .ignore_then(pattern.clone())
            .then_ignore(just(Token::Symbol(['=', '\0'])))
            .then(expr.clone())
            .then(just(Token::Control(';')).ignore_then(expr.clone()).or_not())
            .map_with_span(|((pattern, value), then), span| {
                Node::new(
                    Expression::Let {
                        pattern,
                        value,
                        then,
                    },
                    span,
                )
            });

        let generic_let = just(Token::Let)
            .ignore_then(generic.clone())
            .then(pattern.clone())
            .then_ignore(just(Token::Symbol(['=', '\0'])))
            .then(expr.clone())
            .then(just(Token::Control(';')).ignore_then(expr.clone()).or_not())
            .map_with_span(|(((generics, pattern), value), then), span| {
                Node::new(
                    Expression::GenericLet {
                        pattern,
                        generics,
                        value,
                        then,
                    },
                    span,
                )
            });

        let generic_value =
            generic
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

        let parens = expr
            .clone()
            .delimited_by(just(Token::Paren('(')), just(Token::Paren(')')));

        let primary = choice((
            function,
            basic,
            parens,
            r#match,
            r#if,
            generic_value,
            generic_let,
            r#let,
            list,
            tuple,
            map,
            module,
        ));

        fn create(
            p: impl Parser<Token, Node<()>, Error = Error> + Clone,
            expr: impl Parser<Token, Node<()>, Error = Error> + Clone,
        ) -> impl Parser<Token, Node<()>, Error = Error> + Clone {
            expr.clone()
                .then(p.then(expr.clone()).repeated().at_least(1).or_not())
                .map_with_span(|(init, transformations), span| {
                    if let Some(transformations) = transformations {
                        transformations.into_iter().fold(init, |acc, (f, arg)| {
                            Node::new(
                                Expression::Apply {
                                    f: Node::new(Expression::Apply { f, arg }, span.clone()),
                                    arg: acc,
                                },
                                span.clone(),
                            )
                        })
                    } else {
                        init
                    }
                })
        }

        let implementation = primary
            .clone()
            .then_ignore(just(Token::Symbol(['@', '\0'])))
            .then(primary.clone())
            .map_with_span(|(value, implementation), span| {
                Node::new(
                    Expression::Implementation {
                        value,
                        implementation,
                    },
                    span,
                )
            });

        let access = primary
            .clone()
            .then_ignore(just(Token::Symbol(['.', '\0'])))
            .then(select! {
                Token::Identifier(s) => s,
            })
            .map_with_span(|(value, field), span| {
                Node::new(Expression::Access { value, field }, span)
            });

        let primary = choice((implementation, access, primary));

        let apply = primary
            .clone()
            .then(primary.clone().repeated().at_least(1).or_not())
            .map_with_span(|(init, args), span: Span| {
                if let Some(args) = args {
                    args.into_iter().fold(init, |acc, arg| {
                        Node::new(Expression::Apply { f: acc, arg }, span.clone())
                    })
                } else {
                    init
                }
            });

        let factors = create(
            select! {
                Token::Symbol(['*', '\0']) => Expression::Literal(Literal::Variable("*".into())),
                Token::Symbol(['/', '\0']) => Expression::Literal(Literal::Variable("/".into())),
            }
            .map_with_span(|expr, span| Node::new(expr, span)),
            apply,
        );

        let sums = create(
            select! {
                Token::Symbol(['+', '\0']) => Expression::Literal(Literal::Variable("+".into())),
                Token::Symbol(['-', '\0']) => Expression::Literal(Literal::Variable("-".into())),
            }
            .map_with_span(|expr, span| Node::new(expr, span)),
            factors,
        );

        let comparisons = create(
            select! {
                Token::Symbol(['<', '=']) => Expression::Literal(Literal::Variable("<=".into())),
                Token::Symbol(['>', '=']) => Expression::Literal(Literal::Variable(">=".into())),
                Token::Symbol(['<', '\0']) => Expression::Literal(Literal::Variable("<".into())),
                Token::Symbol(['>', '\0']) => Expression::Literal(Literal::Variable(">".into())),
                Token::Symbol(['=', '=']) => Expression::Literal(Literal::Variable("==".into())),
                Token::Symbol(['!', '=']) => Expression::Literal(Literal::Variable("!=".into())),
            }
            .map_with_span(|expr, span| Node::new(expr, span)),
            sums,
        );

        let logic = create(
            select! {
                Token::Symbol(['&', '&']) => Expression::Literal(Literal::Variable("&&".into())),
                Token::Symbol(['|', '|']) => Expression::Literal(Literal::Variable("||".into())),
            }
            .map_with_span(|expr, span| Node::new(expr, span)),
            comparisons,
        );

        let pipe = logic
            .clone()
            .then_ignore(just(Token::Symbol(['|', '>'])))
            .then(expr.clone())
            .map_with_span(|(arg, f), span| Node::new(Expression::Apply { f, arg }, span));

        choice((pipe, logic))
    })
}

pub fn parse(s: &str, origin: Arc<str>) -> Result<Node<()>, Vec<Error>> {
    parser().parse(lexer::lex(s, origin)?)
}
