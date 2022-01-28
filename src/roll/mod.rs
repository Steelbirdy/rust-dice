mod ctx;
mod error;
mod num;
mod roller;
mod stringify;
mod tree;
mod visit;

use crate::parse::ast;

type RResult<T> = Result<T, RollError>;

pub use ctx::{DefaultRoller, RollContext};
pub use error::RollError;
pub use roller::Roller;
pub use tree::Roll;
pub use stringify::{MarkdownStringifier, SimpleStringifier};

pub fn eval<R: Roller>(expr: ast::Expression, roller: R, max_rolls: usize) -> RResult<Roll> {
    let mut ctx = RollContext::new(max_rolls, roller);
    ctx.eval(expr)
}
