pub mod dice;
pub mod error;
pub mod expr;
pub mod parse;
pub mod stringifiers;

pub fn roll(s: &str) -> Result<dice::RollResult> {
    let mut roller = dice::Roller::new(Default::default(), rand::thread_rng());
    roller.roll(s)
}

type Result<T> = std::result::Result<T, error::RollError>;

pub(crate) type DefaultRng = rand::prelude::ThreadRng;

#[cfg(test)]
pub(crate) mod test_utils {
    use super::*;
    use rand::SeedableRng;

    pub const SEED: u64 = 10353;

    pub fn roller() -> dice::Roller<impl rand::Rng> {
        let rng = rand_pcg::Pcg64::seed_from_u64(SEED);
        dice::Roller::new(Default::default(), rng)
    }
}
