use crate::common::NonEmpty;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub struct SourcePosition {
    pub span: logos::Span,
    pub slice: String,
}

#[derive(Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("(at position {}): unexpected token; found {:?}, expected {}", .pos.span.start, .pos.slice, .expected.fmt_expected())]
    UnexpectedToken {
        pos: SourcePosition,
        expected: NonEmpty<String>,
    },
    #[error("(at position {}): {:?} can only operate on dice", .0.span.start, .0.slice)]
    InvalidSetOp(SourcePosition),
    #[error("(at position {}): {:?} must have at least one selector", .0.span.start, .0.slice)]
    NoSelector(SourcePosition),
    #[error("(at position {}): {:?}: is not a valid selector for 'mi' and 'ma'; use an integer instead", .0.span.start, .0.slice)]
    InvalidMinMaxSelector(SourcePosition),
}

trait FormatExpected {
    fn fmt_expected(&self) -> String;
}

impl FormatExpected for [String] {
    fn fmt_expected(&self) -> String {
        match self {
            [] => unreachable!("NonEmpty cannot be empty"),
            [a] => a.to_owned(),
            [a, b] => format!("{} or {}", a, b),
            s => format!("{}, or {}", s[..s.len() - 1].join(", "), &s[s.len() - 1]),
        }
    }
}
