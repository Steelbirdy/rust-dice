mod common;
mod parse;
mod roll;

use error::Error;
use roll::Roll;

pub mod error {
    use super::{parse::ParseError, roll::RollError};

    #[derive(thiserror::Error, Debug, PartialEq)]
    pub enum Error {
        #[error(transparent)]
        ParseError(#[from] ParseError),
        #[error(transparent)]
        RollError(#[from] RollError),
    }
}

pub use parse::parse;
pub use roll::{eval, RollContext, Roller};

pub fn roll(s: &str) -> Result<Roll, Error> {
    let ast = parse(s)?;
    let mut ctx = RollContext::default();
    ctx.eval(ast).map_err(Into::into)
}
