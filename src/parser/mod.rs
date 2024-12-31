use super::*;
use chumsky::{
    BoxStream, Stream,
    prelude::*,
    text::{self, TextParser},
};

fn node(expression: Expression<()>, span: Span) -> Node<()> {
    (expression, span).into()
}

const KEYWORDS: [&str; 6] = ["for", "let", "if", "match", "with", "else"];

/// TODO: float, there is some kinda recursion bug where its going weird places but still works,
/// performace concern
pub fn parser() -> impl Parser<char, Node<()>, Error = Error> {
    recursive(|expr| {
        let identifier = text::ident::<char, Error>()
            .try_map(|s, span| {
                if KEYWORDS.contains(&s.as_str()) {
                    Err(Error::UnexpectedKeyword {
                        span,
                        expected: "identifier".to_string(),
                        found: s,
                    })
                } else {
                    Ok(s)
                }
            })
            .labelled("identifier");

        let int = text::int::<char, Error>(10)
            .map_with_span(|n, span| node(Expression::Int(n.parse().unwrap()), span))
            .padded()
            .labelled("int");

        let string = just::<char, _, Error>('"')
            .ignore_then(take_until(just('"')))
            .map_with_span(|(s, _), span| node(Expression::Str(s.into_iter().collect()), span))
            .padded()
            .labelled("string");

        let bool = just::<char, _, Error>("true")
            .or(just("false"))
            .map_with_span(|b, span| node(Expression::Bool(b == "true"), span))
            .padded()
            .labelled("bool");

        let var = identifier
            .map_with_span(|s, span| node(Expression::Var(s), span))
            .padded()
            .labelled("variable");

        let wildcard = just::<char, _, Error>('_')
            .map_with_span(|_, span| Pattern::<()>::Wildcard(span))
            .padded()
            .labelled("wildcard");

        let capture = identifier
            .then(just(':').ignore_then(expr.clone()).or_not())
            .map_with_span(|(name, r#type), span| Pattern::Capture { name, r#type, span })
            .padded()
            .labelled("capture");

        let pattern: impl Parser<char, Pattern<()>, Error = Error> =
            choice((wildcard, capture)).labelled("pattern");

        let generic = just("for")
            .ignore_then(pattern.clone().delimited_by(just('('), just(')')))
            .then(expr.clone())
            .map_with_span(|(pattern, value), span| node(Expression::Generic(pattern, value), span))
            .padded()
            .labelled("generic");

        let list = expr
            .clone()
            .separated_by(just(',').padded())
            .delimited_by(just('['), just(']'))
            .map_with_span(|values, span| node(Expression::List(values), span))
            .padded();

        let tuple = expr
            .clone()
            .separated_by(just(',').padded())
            .delimited_by(just('('), just(')'))
            .map_with_span(|values, span| node(Expression::Tuple(values), span))
            .padded();

        let key_value_pair = identifier
            .then(just('=').ignore_then(expr.clone()))
            .map(|(key, value)| (key, value))
            .padded()
            .labelled("key-value pair");

        let record = key_value_pair
            .separated_by(just(',').padded())
            .delimited_by(just('{'), just('}'))
            .map_with_span(|pairs, span| node(Expression::Record(pairs), span))
            .padded();

        let r#let = just("let")
            .ignore_then(pattern.clone())
            .then(just('=').ignore_then(expr.clone()))
            .map_with_span(|(pattern, value), span| node(Expression::Let { pattern, value }, span))
            .padded()
            .labelled("let");

        let generic_let = just("let for(")
            .padded()
            .ignore_then(pattern.clone())
            .then_ignore(just(')'))
            .then(pattern.clone())
            .then(just('=').ignore_then(expr.clone()))
            .map_with_span(|((generics, pattern), value), span| {
                node(
                    Expression::GenericLet {
                        generics,
                        pattern,
                        value,
                    },
                    span,
                )
            });

        let function = pattern
            .clone()
            .then_ignore(just("->"))
            .then(expr.clone())
            .map_with_span(|(arg, body), span| node(Expression::Function { arg, body }, span));

        let method = just("self")
            .padded()
            .then_ignore(just("->"))
            .ignore_then(expr.clone())
            .map_with_span(|body, span| {
                node(
                    Expression::Method {
                        arg: Pattern::Capture {
                            name: "self".to_string(),
                            r#type: None,
                            span: span.clone(),
                        },
                        body,
                    },
                    span,
                )
            });

        let block = expr
            .clone()
            .separated_by(just(';').or(just('\n')).padded())
            .delimited_by(just('{'), just('}'))
            .map_with_span(|expressions, span| node(Expression::Block(expressions), span))
            .padded();

        let r#if = just("if")
            .ignore_then(expr.clone())
            .then_ignore(just("then"))
            .then(expr.clone())
            .then(just("else").ignore_then(expr.clone()).or_not())
            .map_with_span(|((condition, then), else_), span| {
                node(
                    Expression::If {
                        condition,
                        then,
                        else_,
                    },
                    span,
                )
            });

        let match_arm = just('|')
            .ignore_then(pattern.clone())
            .then_ignore(just("->"))
            .then(expr.clone())
            .map(|(pattern, value)| (pattern, value))
            .padded()
            .labelled("match arm");

        let r#match = just("match")
            .ignore_then(expr.clone())
            .then_ignore(just("with"))
            .then(match_arm.clone().repeated())
            .map_with_span(|(value, arms), span| node(Expression::Match { value, arms }, span));

        let primary = choice((
            r#match,
            r#if,
            block,
            method,
            function,
            generic_let,
            r#let,
            list,
            tuple,
            record,
            generic,
            int,
            string,
            bool,
            var,
        ))
        .padded()
        .labelled("primary");

        let implementation = primary
            .clone()
            .then_ignore(just("::"))
            .then(expr.clone())
            .map_with_span(|(r#trait, r#type), span| {
                node(Expression::Implementation { r#trait, r#type }, span)
            })
            .or(primary.clone());

        let access = implementation
            .clone()
            .then(just('.').ignore_then(identifier))
            .map_with_span(|(value, field), span| node(Expression::Access { value, field }, span))
            .padded()
            .or(implementation.clone());

        choice((access,)).padded()
    })
}

pub fn stream(
    file: &Arc<str>,
    contents: impl ToString,
) -> BoxStream<'_, char, (std::sync::Arc<str>, std::ops::Range<usize>)> {
    let contents = contents.to_string();
    BoxStream::from_iter(
        (file.clone(), contents.len()..contents.len()),
        Box::new(
            contents
                .chars()
                .enumerate()
                .map(|(i, c)| (c, (file.clone(), i..i + 1)))
                .collect::<Vec<(_, _)>>()
                .into_iter(),
        ),
    )
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_implementation() {
        let name = Arc::from("test");
        let code = stream(&name, "1 :: 2");
        let result = parser().parse(code);
        assert_eq!(
            result,
            Ok(node(
                Expression::Implementation {
                    r#trait: node(Expression::Int(1), (Arc::from("test"), 0..1)),
                    r#type: node(Expression::Int(2), (Arc::from("test"), 5..6))
                },
                (Arc::from("test"), 0..6)
            ))
        );
    }

    #[test]
    fn test_match() {
        let name = Arc::from("test");
        let code = stream(&name, "match 1 with | x -> 1 | _ -> 2");
        let result = parser().parse(code);
        assert_eq!(
            result,
            Ok(node(
                Expression::Match {
                    value: node(Expression::Int(1), (Arc::from("test"), 6..7)),
                    arms: vec![
                        (
                            Pattern::Capture {
                                name: "x".to_string(),
                                r#type: None,
                                span: (Arc::from("test"), 15..16)
                            },
                            node(Expression::Int(1), (Arc::from("test"), 20..21))
                        ),
                        (
                            Pattern::Wildcard((Arc::from("test"), 24..25)),
                            node(Expression::Int(2), (Arc::from("test"), 29..30))
                        )
                    ]
                },
                (Arc::from("test"), 0..30)
            ))
        );
    }

    #[test]
    fn test_if() {
        let name = Arc::from("test");
        let code = stream(&name, "if true then 1 else 2");
        let result = parser().parse(code);
        assert_eq!(
            result,
            Ok(node(
                Expression::If {
                    condition: node(Expression::Bool(true), (Arc::from("test"), 3..7)),
                    then: node(Expression::Int(1), (Arc::from("test"), 13..14)),
                    else_: Some(node(Expression::Int(2), (Arc::from("test"), 20..21))),
                },
                (Arc::from("test"), 0..21)
            ))
        );
    }

    #[test]
    fn parse_block() {
        let name = Arc::from("test");
        let code = stream(&name, "{1; 2}");
        let result = parser().parse(code);
        assert_eq!(
            result,
            Ok(node(
                Expression::Block(vec![
                    node(Expression::Int(1), (Arc::from("test"), 1..2)),
                    node(Expression::Int(2), (Arc::from("test"), 4..5)),
                ]),
                (Arc::from("test"), 0..6)
            ))
        );
    }

    #[test]
    fn parse_method() {
        let name = Arc::from("test");
        let code = stream(&name, "self -> x -> y");
        let result = parser().parse(code);
        assert_eq!(
            result,
            Ok(node(
                Expression::Method {
                    arg: Pattern::Capture {
                        name: "self".to_string(),
                        r#type: None,
                        span: (Arc::from("test"), 0..14)
                    },
                    body: node(
                        Expression::Function {
                            arg: Pattern::Capture {
                                name: "x".to_string(),
                                r#type: None,
                                span: (Arc::from("test"), 8..9)
                            },
                            body: node(
                                Expression::Var("y".to_string()),
                                (Arc::from("test"), 13..14)
                            )
                        },
                        (Arc::from("test"), 8..14)
                    )
                },
                (Arc::from("test"), 0..14)
            ))
        );
    }

    #[test]
    fn parse_function() {
        let name = Arc::from("test");
        let code = stream(&name, "x -> y");
        let result = parser().parse(code);
        assert_eq!(
            result,
            Ok(node(
                Expression::Function {
                    arg: Pattern::Capture {
                        name: "x".to_string(),
                        r#type: None,
                        span: (Arc::from("test"), 0..1)
                    },
                    body: node(Expression::Var("y".to_string()), (Arc::from("test"), 5..6))
                },
                (Arc::from("test"), 0..6)
            ))
        );
    }

    #[test]
    fn parse_generic_let() {
        let name = Arc::from("test");
        let code = stream(&name, "let for(y) x = y");
        let result = parser().parse(code);
        assert_eq!(
            result,
            Ok(node(
                Expression::GenericLet {
                    generics: Pattern::Capture {
                        name: "y".to_string(),
                        r#type: None,
                        span: (Arc::from("test"), 8..9)
                    },
                    pattern: Pattern::Capture {
                        name: "x".to_string(),
                        r#type: None,
                        span: (Arc::from("test"), 11..12)
                    },
                    value: node(
                        Expression::Var("y".to_string()),
                        (Arc::from("test"), 15..16)
                    )
                },
                (Arc::from("test"), 0..16)
            ))
        );
    }

    #[test]
    fn parse_let() {
        let name = Arc::from("test");
        let code = stream(&name, "let x: 10 = 1");
        let result = parser().parse(code);
        assert_eq!(
            result,
            Ok(node(
                Expression::Let {
                    pattern: Pattern::Capture {
                        name: "x".to_string(),
                        r#type: Some(node(Expression::Int(10), (Arc::from("test"), 7..9))),
                        span: (Arc::from("test"), 4..10)
                    },
                    value: node(Expression::Int(1), (Arc::from("test"), 12..13))
                },
                (Arc::from("test"), 0..13)
            ))
        );
    }

    #[test]
    fn parse_int() {
        let name = Arc::from("test");
        let code = stream(&name, "123");
        let result = parser().parse(code);
        assert_eq!(
            result,
            Ok(node(Expression::Int(123), (Arc::from("test"), 0..3)))
        );
    }

    #[test]
    fn parse_string() {
        let name = Arc::from("test");
        let code = stream(&name, "\"hello\"");
        let result = parser().parse(code);
        assert_eq!(
            result,
            Ok(node(
                Expression::Str("hello".to_string()),
                (Arc::from("test"), 0..7)
            ))
        );
    }

    #[test]
    fn parse_bool() {
        let name = Arc::from("test");
        let code = stream(&name, "true");
        let result = parser().parse(code);
        assert_eq!(
            result,
            Ok(node(Expression::Bool(true), (Arc::from("test"), 0..4)))
        );
    }

    #[test]
    fn parse_var() {
        let name = Arc::from("test");
        let code = stream(&name, "hello");
        let result = parser().parse(code);
        assert_eq!(
            result,
            Ok(node(
                Expression::Var("hello".to_string()),
                (Arc::from("test"), 0..5)
            ))
        );
    }

    #[test]
    fn parse_wildcard() {
        let name = Arc::from("test");
        let code = stream(&name, "_");
        let result = parser().parse(code);
        assert_eq!(
            result,
            Ok(node(
                Expression::Var("_".to_string()),
                (Arc::from("test"), 0..1)
            ))
        );
    }

    #[test]
    #[rustfmt::skip]
    fn parse_generic() {
        let name = Arc::from("test");
        let code = stream(&name, "for(x: int) x");
        let result = parser().parse(code);
        assert!(matches!(result, Ok(Node { expression: (box Expression::Generic(Pattern::Capture { name: _, r#type: Some(Node { expression: (box Expression::Var(_), _), tag: _, }), span: _, }, Node { expression: (box Expression::Var(_), _), tag: _, }), _), tag: _, })))
    }

    #[test]
    fn parse_tuple() {
        let name = Arc::from("test");
        let code = stream(&name, "(1, 2)");
        let result = parser().parse(code);
        assert_eq!(
            result,
            Ok(node(
                Expression::Tuple(vec![
                    node(Expression::Int(1), (Arc::from("test"), 1..2)),
                    node(Expression::Int(2), (Arc::from("test"), 4..5)),
                ]),
                (Arc::from("test"), 0..6)
            ))
        );
    }

    #[test]
    fn parse_list() {
        let name = Arc::from("test");
        let code = stream(&name, "[1, 2]");
        let result = parser().parse(code);
        assert_eq!(
            result,
            Ok(node(
                Expression::List(vec![
                    node(Expression::Int(1), (Arc::from("test"), 1..2)),
                    node(Expression::Int(2), (Arc::from("test"), 4..5)),
                ]),
                (Arc::from("test"), 0..6)
            ))
        );
    }

    #[test]
    fn parse_record() {
        let name = Arc::from("test");
        let code = stream(&name, "{a=1, b=2}");
        let result = parser().parse(code);
        assert_eq!(
            result,
            Ok(node(
                Expression::Record(vec![
                    (
                        "a".to_string(),
                        node(Expression::Int(1), (Arc::from("test"), 3..4))
                    ),
                    (
                        "b".to_string(),
                        node(Expression::Int(2), (Arc::from("test"), 8..9))
                    ),
                ]),
                (Arc::from("test"), 0..10)
            ))
        );
    }
}
