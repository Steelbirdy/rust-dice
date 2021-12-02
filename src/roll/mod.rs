mod ctx;
mod error;
mod num;
mod roller;
mod tree;
mod visit;
mod stringify;

use crate::parse::ast;

type RResult<T> = Result<T, RollError>;

pub use ctx::{DefaultRoller, RollContext};
pub use error::RollError;
pub use roller::Roller;
pub use tree::Roll;

pub fn eval<R: Roller>(
    expr: ast::Expression,
    roller: R,
    max_rolls: Option<usize>,
) -> RResult<Roll> {
    let mut ctx = RollContext::new(max_rolls, roller);
    ctx.eval(expr)
}
