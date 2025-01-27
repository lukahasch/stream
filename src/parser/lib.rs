use crate::*;

pub trait Stream {
    type Item;
    fn next(&mut self) -> Option<(Self::Item, Span, usize)>;
    fn rollback(&mut self, n: usize);
}

pub struct StreamIter<Item, I: Iterator<Item = (Item, Span)>> {
    stream: I,
    buffer: Vec<(Item, Span)>,
}

impl<I: Iterator> StreamIter<I> {
    pub fn new(stream: I) -> Self {
        Self {
            stream,
            buffer: Vec::new(),
        }
    }
}

impl<I: Iterator> Stream for
