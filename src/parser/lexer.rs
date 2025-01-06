use ariadne::{Cache, sources};
use chumsky::{BoxStream, Stream, prelude::*};
use std::sync::Arc;

use crate::{Span, error::Error};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Integer(i64),
    Float(f64),
    String(Arc<str>),
    Boolean(bool),

    Symbol([char; 2]),
    Identifier(Arc<str>),

    If,
    Else,
    Let,
    Match,
    Module,
    For,
    Then,
    With,

    Paren(char),
    Control(char),
}

pub fn parser() -> impl Parser<char, Vec<(Token, Span)>, Error = Error> {
    let symbols = "+-*/#*!$%&<>=?@^|~.";
    let parens = "[]{}()";
    let control = ";,:";

    let integer = text::digits(10)
        .map_with_span(|s: String, span| (Token::Integer(s.parse().unwrap()), span));

    let float = text::digits(10)
        .then(just('.').ignore_then(text::digits(10)))
        .map_with_span(|(a, b): (String, String), span| {
            (Token::Float(format!("{}.{}", a, b).parse().unwrap()), span)
        });

    let string = just('"')
        .ignore_then(
            filter(|c| *c != '"' && *c != '\\')
                .or(just('\\').ignore_then(
                    just('\\')
                        .or(just('"'))
                        .or(just('n').to('\n'))
                        .or(just('r').to('\r'))
                        .or(just('t').to('\t')),
                ))
                .repeated(),
        )
        .then_ignore(just('"'))
        .map_with_span(|chars: Vec<char>, span| {
            (
                Token::String(chars.into_iter().collect::<String>().into()),
                span,
            )
        });

    let boolean = just("true")
        .or(just("false"))
        .map_with_span(|s: &str, span| (Token::Boolean(s == "true"), span));

    let symbol = one_of(symbols)
        .then(one_of(symbols).or_not())
        .map_with_span(|(a, b), span| (Token::Symbol([a, b.unwrap_or('\0')]), span));

    let r#if = just("if").map_with_span(|_, span| (Token::If, span));
    let r#else = just("else").map_with_span(|_, span| (Token::Else, span));
    let r#let = just("let").map_with_span(|_, span| (Token::Let, span));
    let r#match = just("match").map_with_span(|_, span| (Token::Match, span));
    let r#module = just("module").map_with_span(|_, span| (Token::Module, span));
    let r#for = just("for").map_with_span(|_, span| (Token::For, span));
    let r#then = just("then").map_with_span(|_, span| (Token::Then, span));
    let r#with = just("with").map_with_span(|_, span| (Token::With, span));

    let identifier =
        text::ident().map_with_span(|s: String, span| (Token::Identifier(s.into()), span));

    let paren = one_of(parens).map_with_span(|c, span| (Token::Paren(c), span));

    let control = one_of(control).map_with_span(|c, span| (Token::Control(c), span));

    choice((
        float, integer, string, boolean, symbol, r#if, r#else, r#let, r#match, r#module, r#for,
        r#then, r#with, identifier, paren, control,
    ))
    .padded()
    .repeated()
    .collect()
    .then_ignore(end())
}

pub fn lex<'a>(
    input: &'a str,
    source: Arc<str>,
) -> Result<
    Stream<'static, Token, Span, std::vec::IntoIter<(Token, (Arc<str>, std::ops::Range<usize>))>>,
    Vec<Error>,
> {
    let stream = Stream::from_iter(
        (source.clone(), input.len()..input.len()),
        input
            .chars()
            .enumerate()
            .map(|(i, c)| (c, (source.clone(), i..i)))
            .collect::<Vec<_>>()
            .into_iter(),
    );
    let tokens = parser().parse(stream)?;
    Ok(Stream::from_iter(
        (source.clone(), input.len()..input.len()),
        tokens.into_iter(),
    ))
}

pub struct Loader {
    sources: Vec<(Arc<str>, String)>,
}

impl Loader {
    pub fn new() -> Self {
        Self {
            sources: Vec::new(),
        }
    }

    pub fn load(&mut self, path: &str) -> Result<String, std::io::Error> {
        if let Some((_, content)) = self.sources.iter().find(|(p, _)| p.as_str() == path) {
            return Ok(content.clone());
        }
        let content = std::fs::read_to_string(path)?;
        self.sources.push((path.into(), content.clone()));
        Ok(content)
    }

    pub fn cache(&self) -> impl Cache<Arc<str>> {
        sources(self.sources.clone().into_iter())
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Integer(i) => write!(f, "{}", i),
            Token::Float(i) => write!(f, "{}", i),
            Token::String(s) => write!(f, "{:?}", s),
            Token::Boolean(b) => write!(f, "{}", b),
            Token::Symbol([a, '\0']) => write!(f, "{}", a),
            Token::Symbol([a, b]) => write!(f, "{}{}", a, b),
            Token::Identifier(s) => write!(f, "{}", s),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Let => write!(f, "let"),
            Token::Match => write!(f, "match"),
            Token::Module => write!(f, "module"),
            Token::For => write!(f, "for"),
            Token::Then => write!(f, "then"),
            Token::With => write!(f, "with"),
            Token::Paren(c) => write!(f, "'{}'", c),
            Token::Control(c) => write!(f, "'{}'", c),
        }
    }
}

impl Token {
    pub fn is_symbol(&self) -> bool {
        matches!(self, Token::Symbol(_))
    }

    pub fn is_identifier(&self) -> bool {
        matches!(self, Token::Identifier(_))
    }

    pub fn is_control(&self) -> bool {
        matches!(self, Token::Control(_))
    }

    pub fn is_paren(&self) -> bool {
        matches!(self, Token::Paren(_))
    }

    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            Token::If | Token::Else | Token::Let | Token::Match | Token::Module | Token::For
        )
    }

    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            Token::Integer(_) | Token::Float(_) | Token::String(_) | Token::Boolean(_)
        )
    }

    pub fn is_if(&self) -> bool {
        matches!(self, Token::If)
    }

    pub fn is_else(&self) -> bool {
        matches!(self, Token::Else)
    }

    pub fn is_let(&self) -> bool {
        matches!(self, Token::Let)
    }

    pub fn is_match(&self) -> bool {
        matches!(self, Token::Match)
    }

    pub fn is_module(&self) -> bool {
        matches!(self, Token::Module)
    }

    pub fn is_for(&self) -> bool {
        matches!(self, Token::For)
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, Token::Integer(_))
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Token::Float(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Token::String(_))
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Token::Boolean(_))
    }
}
