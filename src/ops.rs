use crate::{eval::RollResult, roll::Roller, DSize};
use std::collections::HashSet;
use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[enum_dispatch::enum_dispatch(DiceOperate)]
pub enum DiceOperator {
    Keep(Keep),
    Drop(Drop),
    Reroll(Reroll),
    RerollOnce(RerollOnce),
    Explode(Explode),
    ExplodeOnce(ExplodeOnce),
    Minimum(Minimum),
    Maximum(Maximum),
}

impl fmt::Display for DiceOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Keep(x) => x.fmt(f),
            Self::Drop(x) => x.fmt(f),
            Self::Reroll(x) => x.fmt(f),
            Self::RerollOnce(x) => x.fmt(f),
            Self::Explode(x) => x.fmt(f),
            Self::ExplodeOnce(x) => x.fmt(f),
            Self::Minimum(x) => x.fmt(f),
            Self::Maximum(x) => x.fmt(f),
        }
    }
}

#[enum_dispatch::enum_dispatch]
pub trait DiceOperate {
    fn selector(&self) -> Option<&DiceSelector>;

    fn select(&self, target: &RollResult, max_targets: Option<usize>) -> HashSet<usize> {
        match self.selector() {
            Some(x) => x.select(target, max_targets),
            None => HashSet::new(),
        }
    }

    fn operate<R: rand::Rng>(&self, target: &mut RollResult, roller: &mut Roller<R>);
}

macro_rules! dice_op_impl {
    ([($first_name:ident, $first_disp:literal) $(, ($name:ident, $disp:literal))* $(,)?] [$(($sel_name:ident, $sel_fn:ident($var:ident: $var_ty:ty))),+ $(,)?]) => {
        dice_op_impl!(@impl ($first_name, $first_disp) [$(($sel_name, $sel_fn($var: $var_ty))),+]);
        dice_op_impl!([$(($name, $disp)),*] [$(($sel_name, $sel_fn($var: $var_ty))),+]);
    };
    ([] [$(($sel_name:ident, $sel_fn:ident($var:ident: $var_ty:ty))),+ $(,)?]) => {};
    (@impl ($name:ident, $disp:literal) [$(($sel_name:ident, $sel_fn:ident($var:ident: $var_ty:ty))),+]) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
        pub struct $name(DiceSelector);

        impl $name {
            $(pub fn $sel_fn($var: $var_ty) -> Self {
                Self(DiceSelector::$sel_name($var))
            })+
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}{}", $disp, &self.0)
            }
        }
    };
}

dice_op_impl!(
[(Keep, "k"), (Drop, "p"), (Reroll, "rr"), (RerollOnce, "ro"), (Explode, "e"), (ExplodeOnce, "ra")]
[
    (Highest, highest(n: usize)),
    (Lowest, lowest(n: usize)),
    (LessThan, less_than(x: DSize)),
    (GreaterThan, greater_than(x: DSize)),
    (EqualTo, equal_to(x: DSize)),
]);

impl DiceOperate for Keep {
    fn selector(&self) -> Option<&DiceSelector> {
        Some(&self.0)
    }

    fn operate<R: rand::Rng>(&self, target: &mut RollResult, _: &mut Roller<R>) {
        let keptset: Vec<_> = target.kept_set().collect();
        for i in keptset {
            if !<Self as DiceOperate>::select(self, target, None).contains(&i) {
                target.dice[i].drop();
            }
        }
    }
}

impl DiceOperate for Drop {
    fn selector(&self) -> Option<&DiceSelector> {
        Some(&self.0)
    }

    fn operate<R: rand::Rng>(&self, target: &mut RollResult, _: &mut Roller<R>) {
        for i in <Self as DiceOperate>::select(self, target, None) {
            target.dice[i].drop();
        }
    }
}

impl DiceOperate for Reroll {
    fn selector(&self) -> Option<&DiceSelector> {
        Some(&self.0)
    }

    fn operate<R: rand::Rng>(&self, target: &mut RollResult, roller: &mut Roller<R>) {
        let mut to_reroll = <Self as DiceOperate>::select(self, target, None);
        while !to_reroll.is_empty() {
            for i in to_reroll {
                target.dice[i].reroll(roller);
            }

            to_reroll = <Self as DiceOperate>::select(self, target, None);
        }
    }
}

impl DiceOperate for RerollOnce {
    fn selector(&self) -> Option<&DiceSelector> {
        Some(&self.0)
    }

    fn operate<R: rand::Rng>(&self, target: &mut RollResult, roller: &mut Roller<R>) {
        for i in <Self as DiceOperate>::select(self, target, None) {
            target.dice[i].reroll(roller);
        }
    }
}

impl DiceOperate for Explode {
    fn selector(&self) -> Option<&DiceSelector> {
        Some(&self.0)
    }

    fn operate<R: rand::Rng>(&self, target: &mut RollResult, roller: &mut Roller<R>) {
        let mut to_explode = <Self as DiceOperate>::select(self, target, None);
        let mut already_exploded = HashSet::new();

        while !to_explode.is_empty() {
            for &i in &to_explode {
                let size = target.dice[i].size;
                target.dice[i].explode();
                target.roll_another(size, roller);
            }

            already_exploded.extend(to_explode);
            to_explode = <Self as DiceOperate>::select(self, target, None)
                .difference(&already_exploded)
                .copied()
                .collect();
        }
    }
}

impl DiceOperate for ExplodeOnce {
    fn selector(&self) -> Option<&DiceSelector> {
        Some(&self.0)
    }

    fn operate<R: rand::Rng>(&self, target: &mut RollResult, roller: &mut Roller<R>) {
        for i in <Self as DiceOperate>::select(self, target, Some(1)) {
            let size = target.dice[i].size;
            target.dice[i].explode();
            target.roll_another(size, roller);
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Minimum(pub(crate) DSize);

impl Minimum {
    pub fn new(x: DSize) -> Self {
        Self(x)
    }
}

impl DiceOperate for Minimum {
    fn selector(&self) -> Option<&DiceSelector> {
        None
    }

    fn operate<R: rand::Rng>(&self, target: &mut RollResult, _: &mut Roller<R>) {
        let keptset: Vec<_> = target.kept_set().collect();
        for i in keptset {
            if target.dice[i].total() < self.0 {
                target.dice[i].force_value(self.0);
            }
        }
    }
}

impl fmt::Display for Minimum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "mi{}", &self.0)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Maximum(pub(crate) DSize);

impl Maximum {
    pub fn new(x: DSize) -> Self {
        Self(x)
    }
}

impl DiceOperate for Maximum {
    fn selector(&self) -> Option<&DiceSelector> {
        None
    }

    fn operate<R: rand::Rng>(&self, target: &mut RollResult, _: &mut Roller<R>) {
        let keptset: Vec<_> = target.kept_set().collect();
        for i in keptset {
            if target.dice[i].total() > self.0 {
                target.dice[i].force_value(self.0);
            }
        }
    }
}

impl fmt::Display for Maximum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ma{}", &self.0)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum DiceSelector {
    Lowest(usize),
    Highest(usize),
    LessThan(DSize),
    GreaterThan(DSize),
    EqualTo(DSize),
}

impl DiceSelector {
    fn select(
        &self,
        target: &RollResult,
        max_targets: Option<usize>,
    ) -> HashSet<usize> {
        let selected = match self {
            Self::Lowest(n) => Self::lowest_n(*n, target),
            Self::Highest(n) => Self::highest_n(*n, target),
            Self::LessThan(x) => Self::less_than(*x, target),
            Self::GreaterThan(x) => Self::greater_than(*x, target),
            Self::EqualTo(x) => Self::equal_to(*x, target),
        };

        match max_targets {
            Some(x) if x < selected.len() => {
                selected.into_iter().take(max_targets.unwrap()).collect()
            }
            _ => selected,
        }
    }

    fn lowest_n(n: usize, target: &RollResult) -> HashSet<usize> {
        let mut keptset: Vec<_> = target.kept_set().collect();
        keptset.sort_by_key(|&i| target.dice[i].total());
        keptset.into_iter().take(n).collect()
    }

    fn highest_n(n: usize, target: &RollResult) -> HashSet<usize> {
        let mut keptset: Vec<_> = target.kept_set().collect();
        keptset.sort_by_key(|&i| target.dice[i].total());
        let len = keptset.len();
        keptset.into_iter().skip(len - n).collect()
    }

    fn less_than(x: DSize, target: &RollResult) -> HashSet<usize> {
        target
            .kept_set()
            .filter(|&i| target.dice[i].total() < x)
            .collect()
    }

    fn greater_than(x: DSize, target: &RollResult) -> HashSet<usize> {
        target
            .kept_set()
            .filter(|&i| target.dice[i].total() > x)
            .collect()
    }

    fn equal_to(x: DSize, target: &RollResult) -> HashSet<usize> {
        target
            .kept_set()
            .filter(|&i| target.dice[i].total() == x)
            .collect()
    }
}

impl fmt::Display for DiceSelector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Highest(n) => write!(f, "h{}", n),
            Self::Lowest(n) => write!(f, "l{}", n),
            Self::LessThan(x) => write!(f, "<{}", x),
            Self::GreaterThan(x) => write!(f, ">{}", x),
            Self::EqualTo(x) => write!(f, "{}", x),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use rand::SeedableRng;


    #[test]
    fn test_keep_highest() {
        let mut dice = dice_roll(4, vec![1, 2, 3, 4]);

        // let op = DiceOperator::new(DiceOperatorType::Keep, vec![DiceSelector::Highest(2)]);
        let op = Keep::highest(2);
        op.operate(&mut dice, &mut roller());

        let mut expected = dice_roll(4, vec![1, 2, 3, 4]);
        expected.dice[0].drop();
        expected.dice[1].drop();

        assert_eq!(dice, expected);
    }

    #[test]
    fn test_reroll_greater_than() {
        let mut dice = dice_roll(10, vec![4, 8, 5]);

        // let op = DiceOperator::new(DiceOperatorType::Reroll, vec![DiceSelector::GreaterThan(5)]);
        let op = Reroll::greater_than(5);
        op.operate(&mut dice, &mut roller());

        let mut expected = dice_roll(10, vec![4, 8, 5]);
        expected.dice[1].reroll(&mut roller());

        assert_eq!(dice, expected);
    }

    #[test]
    fn test_minimum() {
        let mut dice = dice_roll(12, vec![7, 11, 2, 6, 3]);

        let op = Minimum::new(6);
        op.operate(&mut dice, &mut roller());

        let mut expected = dice_roll(12, vec![7, 11, 2, 6, 3]);
        expected.dice[2].force_value(6);
        expected.dice[4].force_value(6);

        assert_eq!(dice, expected);
    }
}
