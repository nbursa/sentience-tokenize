use crate::Span;

pub struct LineMap {
    starts: Vec<usize>,
}

impl LineMap {
    pub fn new(src: &str) -> Self {
        let mut starts = Vec::with_capacity(128);
        starts.push(0);
        for (i, b) in src.bytes().enumerate() {
            if b == b'\n' {
                starts.push(i + 1);
            }
        }
        Self { starts }
    }

    pub fn to_line_col(&self, byte: usize) -> (usize, usize) {
        let i = match self.starts.binary_search(&byte) {
            Ok(k) => k,
            Err(k) => k.saturating_sub(1),
        };
        (i + 1, byte.saturating_sub(self.starts[i]) + 1)
    }

    pub fn span_lines(&self, span: Span) -> ((usize, usize), (usize, usize)) {
        (self.to_line_col(span.start), self.to_line_col(span.end))
    }

    pub fn line_count(&self) -> usize {
        self.starts.len()
    }
}
