use crate::{DSize, DefaultRng};

#[derive(Default, Debug, Copy, Clone)]
pub struct Roller<R: rand::Rng = DefaultRng> {
    rng: R,
}

impl<R: rand::Rng> Roller<R> {
    pub fn new(rng: R) -> Self {
        Self { rng }
    }

    pub fn roll(&mut self, size: DSize) -> DSize {
        self.rng.gen_range(1..size)
    }

    pub fn roll_n(&mut self, num: usize, size: DSize) -> impl Iterator<Item = DSize> + '_ {
        let distr = rand::distributions::Uniform::new_inclusive(1, size);
        rand::Rng::sample_iter(&mut self.rng, distr).take(num)
    }
}
