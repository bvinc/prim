use std::ops::Range;

/// A span representing a range of bytes in the source code
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    range: Range<usize>,
}

impl Span {
    /// Create a new span from a byte range
    pub fn new(start: usize, end: usize) -> Self {
        Self { range: start..end }
    }

    /// Create a span from a Range
    pub fn from_range(range: Range<usize>) -> Self {
        Self { range }
    }

    /// Get the start byte offset
    pub fn start(&self) -> usize {
        self.range.start
    }

    /// Get the end byte offset
    pub fn end(&self) -> usize {
        self.range.end
    }

    /// Get the length in bytes
    pub fn len(&self) -> usize {
        self.range.len()
    }

    /// Check if the span is empty
    pub fn is_empty(&self) -> bool {
        self.range.is_empty()
    }

    /// Get the text this span represents from the source
    pub fn text<'a>(&self, source: &'a str) -> &'a str {
        &source[self.range.clone()]
    }

    /// Get the underlying range
    pub fn range(&self) -> Range<usize> {
        self.range.clone()
    }

    /// Combine two spans into one that covers both
    pub fn to(&self, other: &Span) -> Span {
        Span::new(self.start().min(other.start()), self.end().max(other.end()))
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self::from_range(range)
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.range
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.range.start, self.range.end)
    }
}
