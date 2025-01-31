use crate::Span;
use nom::{Compare, IResult, Input};

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct Spanned<'a> {
    input: &'a str,
    original: &'a str,
    source: &'static str,
}

impl<'a> Spanned<'a> {
    pub fn new(source: &'static str, input: &'a str) -> Self {
        Self {
            input,
            original: input,
            source,
        }
    }

    pub fn offset(&self) -> usize {
        self.input.as_ptr() as usize - self.original.as_ptr() as usize
    }

    pub fn span(&self) -> Span {
        Span {
            offset: self.offset()..self.offset(),
            source: self.source,
        }
    }

    pub fn as_str(&self) -> &'a str {
        self.input
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
            original: self.original,
            source: self.source,
        }
    }

    fn take_from(&self, index: usize) -> Self {
        Self {
            input: &self.input[index..],
            original: self.original,
            source: self.source,
        }
    }

    fn take_split(&self, index: usize) -> (Self, Self) {
        let (prefix, suffix) = self.input.take_split(index);
        (
            Self {
                input: prefix,
                original: self.original,
                source: self.source,
            },
            Self {
                input: suffix,
                original: self.original,
                source: self.source,
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

impl Compare<&str> for Spanned<'_> {
    fn compare(&self, t: &str) -> nom::CompareResult {
        self.input.compare(t)
    }

    fn compare_no_case(&self, t: &str) -> nom::CompareResult {
        self.input.compare_no_case(t)
    }
}

impl<'a> Compare<Spanned<'a>> for Spanned<'a> {
    fn compare(&self, t: Spanned<'a>) -> nom::CompareResult {
        self.input.compare(t.input)
    }

    fn compare_no_case(&self, t: Spanned<'a>) -> nom::CompareResult {
        self.input.compare_no_case(t.input)
    }
}
