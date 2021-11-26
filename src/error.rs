#[derive(thiserror::Error, Debug, PartialEq)]
pub enum RollError {
    #[error("unexpected input at position {position}: expected {expected:?}, found {found:?}")]
    SyntaxError {
        position: usize,
        found: String,
        expected: Vec<String>,
    },
    #[error("{0}")]
    ValueError(String),
    #[error("too many dice rolled")]
    TooManyRolls,
    #[error("{0}")]
    ParseError(#[from] crate::parse::ParseError),
}

impl RollError {
    pub fn value_error(msg: impl ToString) -> Self {
        Self::ValueError(msg.to_string())
    }
}
