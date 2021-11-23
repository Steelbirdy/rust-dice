use crate::{eval::RollResult, roll::Roller, DSize};
use crate::ops::DiceOperator;
use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum CritType {
    None = 0,
    Crit = 1,
    Fail = 2,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum AdvType {
    None = 0,
    Adv = 1,
    Dis = -1,
}

pub struct RollContext {
    max_rolls: usize,
    rolls: usize,
}

impl RollContext {
    pub fn new(max_rolls: usize) -> Self {
        Self { max_rolls, rolls: 0 }
    }

    pub fn reset(&mut self) {
        self.rolls = 0;
    }

    pub fn count_roll(&mut self) -> Result<(), RollError> {
        self.rolls += 1;
        todo!("Check")
    }

    pub fn count_rolls(&mut self, n: usize) -> Result<(), RollError> {
        self.rolls += n;
        todo!("Check")
    }
}

impl Default for RollContext {
    fn default() -> Self {
        Self::new(1000)
    }
}

pub struct RollResult {

}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Dice {
    pub num: usize,
    pub size: DSize,
    pub ops: Vec<DiceOperator>,
}

impl Dice {
    pub const fn new(num: usize, size: DSize) -> Self {
        Self { num, size, ops: Vec::new() }
    }

    pub fn add_op(&mut self, op: impl Into<DiceOperator>) -> &mut Self {
        self.ops.push(op.into());
        self
    }

    pub fn roll<R: rand::Rng>(self, roller: &mut Roller<R>) -> RollResult {
        RollResult::new(self.num, self.size, self.ops, roller)
    }
}

impl fmt::Display for Dice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}d{}", self.num, self.size)?;
        for op in &self.ops {
            write!(f, "{}", op)?;
        }
        Ok(())
    }
}

impl std::str::FromStr for Dice {
    type Err = ParseDiceError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (num, size) = s.split_once('d').ok_or(ParseDiceError::NoDelimiter)?;
        let num = if num.is_empty() {
            1
        } else {
            num.parse().map_err(ParseDiceError::InvalidNum)?
        };
        let size = size.parse().map_err(ParseDiceError::InvalidSize)?;
        Ok(Self::new(num, size))
    }
}

#[derive(thiserror::Error, Debug, Clone, Eq, PartialEq)]
pub enum ParseDiceError {
    #[error("cannot parse string as dice without 'd' delimiter")]
    NoDelimiter,
    #[error("{0}")]
    InvalidNum(std::num::ParseIntError),
    #[error("{0}")]
    InvalidSize(std::num::ParseIntError),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dice_from_str() {
        assert_eq!("1d20".parse::<Dice>().unwrap(), Dice::new(1, 20));
        assert_eq!("d20".parse::<Dice>().unwrap(), Dice::new(1, 20));
        assert_eq!("14d4".parse::<Dice>().unwrap(), Dice::new(14, 4));
        assert_eq!("1".parse::<Dice>(), Err(ParseDiceError::NoDelimiter));
        assert_eq!("hd2".parse::<Dice>(), Err(ParseDiceError::InvalidNum("h".parse::<usize>().unwrap_err())));
        assert_eq!("2dx".parse::<Dice>(), Err(ParseDiceError::InvalidSize("x".parse::<DSize>().unwrap_err())));
    }
}
