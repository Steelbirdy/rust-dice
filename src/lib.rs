pub mod dice;
pub mod eval;
pub mod ops;
pub mod roll;
mod markdown;

/// The type used for the number of faces on [Dice].
pub type DSize = usize;

pub(crate) type DefaultRng = rand::prelude::ThreadRng;

pub mod prelude {
    pub use crate::dice::Dice;
    pub use crate::ops::{Drop, Explode, ExplodeOnce, Keep, Maximum, Minimum, Reroll, RerollOnce};
    pub use crate::roll::Roller;
    pub use crate::DSize;
}

#[cfg(test)]
pub(crate) mod test_utils {
    use rand::SeedableRng;
    pub use super::prelude::*;
    pub use super::eval::*;

    const SEED: [u8; 32] = [
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
        26, 27, 28, 29, 30, 31, 32,
    ];

    pub fn roller() -> Roller<rand_pcg::Pcg64> {
        Roller::new(rand_pcg::Pcg64::from_seed(SEED))
    }

    pub fn dice_roll(size: DSize, rolls: Vec<DSize>) -> RollResult {
        let num = rolls.len();
        let dice = rolls.into_iter().map(|r| Die::new(size, r)).collect();
        RollResult { num, size, ops: Vec::new(), dice }
    }
}
