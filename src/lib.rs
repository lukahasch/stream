#![feature(unboxed_closures, fn_traits)]

pub mod parser;

#[derive(Debug, PartialEq, Clone)]
pub struct Span<'source> {
    source: &'source str,
    start: usize,
    end: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression<'source> {
    Pattern(Pattern<'source>, Span<'source>),
    Let(Pattern<'source>, Box<Expression<'source>>, Span<'source>),
    Define(Pattern<'source>, Box<Expression<'source>>, Span<'source>),

    Match {
        expression: Box<Expression<'source>>,
        branches: Vec<(Pattern<'source>, Expression<'source>, Span<'source>)>,
        source_id: Span<'source>,
    },
    Function {
        input: Pattern<'source>,
        body: Box<Expression<'source>>,
        return_type: Option<Box<Type<'source>>>,
        source_id: Span<'source>,
    },

    Type(Box<Type<'source>>, Span<'source>),

    Binary(
        Binary,
        Box<Expression<'source>>,
        Box<Expression<'source>>,
        Span<'source>,
    ),
    Not(Box<Expression<'source>>, Span<'source>),

    Sequence(Vec<Expression<'source>>, Span<'source>),

    /// This is an invalid expression only produced by the compiler to represent an error
    /// while still being able to continue parsing.
    Error(Error<'source>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type<'source> {
    Tuple(Vec<Type<'source>>, Span<'source>),
    Enum(Vec<Type<'source>>, Span<'source>),
    Struct(Vec<(&'source str, Type<'source>)>, Span<'source>),
    Generic(&'source str, Box<Expression<'source>>, Span<'source>),

    Integer(Span<'source>),
    Float(Span<'source>),
    Character(Span<'source>),
    String(Span<'source>),
    Boolean(Span<'source>),

    Function(Pattern<'source>, Box<Type<'source>>, Span<'source>),

    Expression(Expression<'source>, Span<'source>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern<'source> {
    Variable(&'source str, Option<Box<Type<'source>>>, Span<'source>),
    Literal(&'source str, Span<'source>),
    Tuple(Vec<Pattern<'source>>, Span<'source>),

    Wildcard(Span<'source>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Binary {
    Add,
    Sub,
    Mul,
    Div,

    Or,
    And,
    Xor,

    Call,

    Equal,
    Greater,
    Less,

    Satisfies,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Context {
    Let,
    Define,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Error<'source> {
    Expected {
        expected: &'source str,
        found: &'source str,
        span: Span<'source>,
    },
}

impl<'source> ariadne::Span for Span<'source> {
    type SourceId = &'source str;

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }

    fn len(&self) -> usize {
        self.end.saturating_sub(self.start)
    }

    fn is_empty(&self) -> bool {
        self.start == self.end
    }

    fn source(&self) -> &Self::SourceId {
        &self.source
    }

    fn contains(&self, offset: usize) -> bool {
        offset < self.end && offset > self.start
    }
}
