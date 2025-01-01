#![feature(impl_trait_in_bindings, let_chains, box_patterns)]

use chumsky::prelude::*;
use std::ops::Range;
use std::sync::Arc;

pub mod error;
pub mod parser;

pub use error::Error;

pub type Span = (Arc<str>, Range<usize>);
pub type Spanned<T> = (T, Span);

#[derive(Debug, Clone, PartialEq)]
pub struct Node<Tag> {
    pub expression: Spanned<Box<Expression<Tag>>>,
    pub tag: Tag,
}

/// TODO: loops
#[derive(Debug, Clone, PartialEq)]
#[rustfmt::skip]
pub enum Expression<Tag> {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Var(String),

    Generic(Pattern<Tag>, Node<Tag>),
    Tuple(Vec<Node<Tag>>),
    List(Vec<Node<Tag>>),
    Record(Vec<(String, Node<Tag>)>),

    Apply { f: Node<Tag>, arg: Node<Tag> },
    Access { value: Node<Tag>, field: String },
    Implementation { r#trait: Node<Tag>, r#type: Node<Tag> },

    Let { pattern: Pattern<Tag>, value: Node<Tag> },

    Mutate { target: Node<Tag>, value: Node<Tag> },

    If { condition: Node<Tag>, then: Node<Tag>, else_: Option<Node<Tag>> },
    Match { value: Node<Tag>, arms: Vec<(Pattern<Tag>, Node<Tag>)> },

    Function { arg: Pattern<Tag>, body: Node<Tag> },
    Method { arg: Pattern<Tag>, body: Node<Tag> },
    Block(Vec<Node<Tag>>),

    Struct { name: String, fields: Vec<(String, Node<Tag>)> },
    Enum { name: String, variants: Vec<(String, Vec<Node<Tag>>)> },
    Trait { name: String, fields: Vec<(String, Node<Tag>)> },
}

#[derive(Debug, Clone, PartialEq)]
#[rustfmt::skip]
pub enum Pattern<Tag> {
    Wildcard(Span),
    Capture { name: String, r#type: Option<Node<Tag>>, span: Span },
}

impl From<(Expression<()>, Span)> for Node<()> {
    fn from((expression, span): (Expression<()>, Span)) -> Self {
        Node {
            expression: (Box::new(expression), span),
            tag: (),
        }
    }
}
