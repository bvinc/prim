use std::ops::Range;

/// Byte range inside a source string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    /// Construct a span; in debug builds we panic if `start > end`, in release we swap.
    pub fn new(start: usize, end: usize) -> Self {
        if cfg!(debug_assertions) {
            assert!(
                start <= end,
                "Span::new expects start <= end (start: {}, end: {})",
                start,
                end
            );
            Self { start, end }
        } else if start <= end {
            Self { start, end }
        } else {
            Self {
                start: end,
                end: start,
            }
        }
    }

    /// Empty span at a single byte offset.
    pub fn empty_at(pos: usize) -> Self {
        Self::new(pos, pos)
    }

    /// Construct from a range.
    pub fn from_range(range: Range<usize>) -> Self {
        Self::new(range.start, range.end)
    }

    /// Convert back into a range.
    pub fn to_range(self) -> Range<usize> {
        self.start..self.end
    }

    /// Start byte offset.
    pub fn start(self) -> usize {
        self.start
    }

    /// End byte offset (exclusive).
    pub fn end(self) -> usize {
        self.end
    }

    /// Length in bytes.
    pub fn len(self) -> usize {
        self.end.saturating_sub(self.start)
    }

    /// Whether the span is empty.
    pub fn is_empty(self) -> bool {
        self.start == self.end
    }

    /// Slice the provided source string with this span.
    pub fn text<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start..self.end]
    }

    /// Checked variant of [`Span::text`]; returns `None` on invalid bounds.
    pub fn checked_text<'a>(&self, source: &'a str) -> Option<&'a str> {
        source.get(self.start..self.end)
    }

    /// Span covering both `self` and `other`.
    pub fn cover(self, other: Span) -> Span {
        Span::new(self.start.min(other.start), self.end.max(other.end))
    }

    /// Extend the end of this span.
    pub fn extend_to(&mut self, end: usize) {
        if cfg!(debug_assertions) {
            assert!(
                end >= self.start,
                "Span::extend_to expects end >= start (start: {}, end: {})",
                self.start,
                end
            );
            self.end = end;
        } else if end >= self.start {
            self.end = end;
        } else {
            self.end = self.start;
        }
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Span::from_range(range)
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.to_range()
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

#[cfg(test)]
impl Span {
    /// Convenience constructor for tests.
    pub fn dummy() -> Self {
        Span::new(0, 0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_span {
        ($span:expr, $start:expr, $end:expr) => {{
            let span = $span;
            assert_eq!(span.start(), $start);
            assert_eq!(span.end(), $end);
        }};
    }

    #[test]
    fn span_construction_and_accessors() {
        let span = Span::new(2, 6);
        assert_span!(span, 2, 6);
        assert_eq!(span.len(), 4);
        assert!(!span.is_empty());
        assert_eq!(Span::empty_at(3).len(), 0);
    }

    #[test]
    fn span_cover_and_extend() {
        let span_a = Span::new(0, 4);
        let span_b = Span::new(3, 10);
        let covered = span_a.cover(span_b);
        assert_span!(covered, 0, 10);

        let mut extendable = Span::new(5, 7);
        extendable.extend_to(12);
        assert_span!(extendable, 5, 12);
    }

    #[test]
    fn span_text_helpers() {
        let src = "hello world";
        let span = Span::new(0, 5);
        assert_eq!(span.text(src), "hello");
        assert_eq!(span.checked_text(src), Some("hello"));
        let bad_span = Span::new(0, src.len() + 1);
        assert_eq!(bad_span.checked_text(src), None);
    }
}
