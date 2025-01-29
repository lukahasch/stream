use nom::Input;
use std::ops::{Add, Range};

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub offset: Range<usize>,
    pub origin: &'static str,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<'a> {
    input: &'a str,
    offset: usize,
    origin: &'static str,
}

impl<'a> Spanned<'a> {
    pub fn new(input: &'a str, origin: &'static str) -> Self {
        Self {
            input,
            offset: 0,
            origin,
        }
    }

    pub fn span(&self) -> Span {
        Span {
            offset: self.offset..self.offset,
            origin: self.origin,
        }
    }
}

impl Add<Span> for Span {
    type Output = Span;

    fn add(self, other: Span) -> Span {
        Span {
            offset: self.offset.start..other.offset.end,
            origin: self.origin,
        }
    }
}

impl<'a> Input for Spanned<'a> {
    type Item = char;
    type Iter = impl Iterator<Item = char>;
    type IterIndices = impl Iterator<Item = (usize, Self::Item)>;

    fn input_len(&self) -> usize {
        self.input.input_len()
    }

    fn take(&self, index: usize) -> Self {
        Self {
            input: &self.input[..index],
            offset: self.offset,
            origin: self.origin,
        }
    }

    fn take_from(&self, index: usize) -> Self {
        Self {
            input: &self.input[index..],
            offset: self.offset + index,
            origin: self.origin,
        }
    }

    fn take_split(&self, index: usize) -> (Self, Self) {
        let (prefix, suffix) = self.input.take_split(index);
        (
            Self {
                input: prefix,
                offset: self.offset,
                origin: self.origin,
            },
            Self {
                input: suffix,
                offset: self.offset + index,
                origin: self.origin,
            },
        )
    }

    fn iter_elements(&self) -> Self::Iter {
        self.input.iter_elements()
    }

    fn iter_indices(&self) -> Self::IterIndices {
        self.input.iter_indices()
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        self.input.slice_index(count)
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.input.position(predicate)
    }
}
