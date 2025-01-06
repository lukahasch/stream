#![feature(str_as_str, impl_trait_in_assoc_type, impl_trait_in_bindings)]

use std::{collections::BTreeMap, ops::Range, sync::Arc};

/// The Span type used by the stream compiler
/// (source, start..end)
pub type Span = (Arc<str>, Range<usize>);

pub mod error;
pub mod parser;

/// A node in the AST
#[derive(Debug, Clone, PartialEq)]
pub struct Node<Tag> {
    expr: Box<Expression<Tag>>,
    span: Span,
    tag: Tag,
}

/// All stream expressions
#[derive(Debug, Clone, PartialEq)]
#[rustfmt::skip]
pub enum Expression<Tag> {
    Literal(Literal<Tag>),

    Block(Vec<Node<Tag>>),
    Generic { generics: Pattern<Tag>, value: Node<Tag> },
    Function { param: Pattern<Tag>, body: Node<Tag> },
    Let { pattern: Pattern<Tag>, value: Node<Tag> },
    GenericLet { pattern: Pattern<Tag>, generics: Pattern<Tag>, value: Node<Tag> },
    If { condition: Node<Tag>, then: Node<Tag>, r#else: Option<Node<Tag>> },
    Match { value: Node<Tag>, arms: Vec<(Pattern<Tag>, Node<Tag>)> },

    Implementation { value: Node<Tag>, implementation: Node<Tag> },
    Access { value: Node<Tag>, field: Arc<str> },
    Apply { f: Node<Tag>, arg: Node<Tag> },

    Struct { fields: BTreeMap<Arc<str>, Node<Tag>> },
    Enum { variants: BTreeMap<Arc<str>, Option<Node<Tag>>> },

    Module { body: Node<Tag> },
}

/// All stream literals
#[derive(Debug, Clone, PartialEq)]
#[rustfmt::skip]
pub enum Literal<Tag> {
    Integer(i64),
    Float(f64),
    String(Arc<str>),
    Boolean(bool),
    Variable(Arc<str>),

    List(Vec<Node<Tag>>),
    Tuple(Vec<Node<Tag>>),
    Record(BTreeMap<Arc<str>, Node<Tag>>),
}

/// All stream patterns
#[derive(Debug, Clone, PartialEq)]
#[rustfmt::skip]
pub enum Pattern<Tag> {
    Capture { name: Arc<str>, r#type: Option<Node<Tag>> },
}

impl Node<()> {
    pub fn new(expr: Expression<()>, span: Span) -> Self {
        Self {
            expr: Box::new(expr),
            span,
            tag: (),
        }
    }
}
