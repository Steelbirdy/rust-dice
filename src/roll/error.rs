use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum RollError {
    #[error("too many dice rolled")]
    TooManyRolls,
    #[error("cannot divide by zero")]
    ZeroDivision,
    #[error("cannot take modulus by zero")]
    ZeroModulo,
}
