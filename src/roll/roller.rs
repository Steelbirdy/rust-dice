use crate::common::{NonZeroUInt, UInt};
use rand::{
    distributions::{DistIter, Distribution, Uniform},
    Rng,
};

pub trait Roller {
    type RollIter<'a>: Iterator<Item = UInt> + 'a
    where
        Self: 'a;

    fn roll(&mut self, sides: NonZeroUInt) -> UInt;

    fn roll_iter(&mut self, num: usize, sides: NonZeroUInt) -> Self::RollIter<'_>;
}

impl<R: Rng> Roller for R {
    type RollIter<'a>
    where
        Self: 'a,
    = std::iter::Take<DistIter<Uniform<UInt>, &'a mut Self, UInt>>;

    fn roll(&mut self, sides: NonZeroUInt) -> UInt {
        self.gen_range(1..=sides.get())
    }

    fn roll_iter(&mut self, num: usize, sides: NonZeroUInt) -> Self::RollIter<'_> {
        Uniform::new_inclusive(1, sides.get())
            .sample_iter(self)
            .take(num)
    }
}

#[cfg(test)]
pub(crate) use step::StepRoller;

#[cfg(test)]
mod step {
    use super::*;

    pub(crate) struct StepRoller {
        current: UInt,
        step: UInt,
    }

    impl StepRoller {
        pub fn new(initial: NonZeroUInt, step: UInt) -> Self {
            Self {
                current: initial.get(),
                step,
            }
        }
    }

    impl Roller for StepRoller {
        type RollIter<'a> = StepIter<'a>;

        fn roll(&mut self, sides: NonZeroUInt) -> UInt {
            let ret = (self.current - 1) % sides.get() + 1;
            self.current += self.step;
            ret
        }

        fn roll_iter(&mut self, num: usize, sides: NonZeroUInt) -> Self::RollIter<'_> {
            StepIter {
                roller: self,
                num,
                sides,
            }
        }
    }

    pub(crate) struct StepIter<'a> {
        roller: &'a mut StepRoller,
        num: usize,
        sides: NonZeroUInt,
    }

    impl Iterator for StepIter<'_> {
        type Item = UInt;

        fn next(&mut self) -> Option<Self::Item> {
            if self.num == 0 {
                None
            } else {
                self.num -= 1;
                Some(self.roller.roll(self.sides))
            }
        }
    }
}
