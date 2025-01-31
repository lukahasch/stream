#![feature(impl_trait_in_assoc_type)]
use std::ops::{Add, Range};
use std::sync::Arc;

pub mod error;
pub mod parser;
pub mod types;

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub offset: Range<usize>,
    pub source: &'static str,
}

impl Add<Span> for Span {
    type Output = Span;

    fn add(self, other: Span) -> Span {
        Span {
            offset: self.offset.start..other.offset.end,
            source: self.source,
        }
    }
}

impl ariadne::Span for Span {
    type SourceId = str;

    fn is_empty(&self) -> bool {
        false
    }

    fn start(&self) -> usize {
        self.offset.start
    }

    fn end(&self) -> usize {
        self.offset.end
    }

    fn source(&self) -> &Self::SourceId {
        self.source
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node<Ctx> {
    pub span: Span,
    pub ctx: Ctx,
    pub expr: Expression<Ctx>,
}

impl<Ctx> Node<Ctx>
where
    Ctx: Default,
{
    pub fn new(expr: Expression<Ctx>, span: Span) -> Self {
        Self {
            span,
            ctx: Ctx::default(),
            expr,
        }
    }

    pub fn call(f: Expression<Ctx>, a: Node<Ctx>) -> Node<Ctx> {
        let span = a.span.clone();
        Node::new(
            Expression::Call {
                f: Box::new(Node::new(f, span.clone())),
                arg: Box::new(a),
            },
            span,
        )
    }

    pub fn transform<C>(self, f: impl FnOnce(Self) -> Node<C>) -> Node<C> {
        f(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
#[rustfmt::skip]
pub enum Expression<Ctx> {
    Float(f64),
    Integer(i64),
    String(Arc<str>),
    Variable(Arc<str>),

    Tuple(Vec<Node<Ctx>>),

    Implementation { value: Box<Node<Ctx>>, r#trait: Box<Node<Ctx>> },
    Access { value: Box<Node<Ctx>>, field: Arc<str> },

    Call { f: Box<Node<Ctx>>, arg: Box<Node<Ctx>> },
}
