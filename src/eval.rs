use crate::{
    ops::DiceOperate,
    roll::Roller,
    DSize,
    dice::Dice,
};
use crate::ops::DiceOperator;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RollResult {
    pub num: usize,
    pub size: DSize,
    pub ops: Vec<DiceOperator>,
    pub dice: Vec<Die>,
}

impl RollResult {
    pub fn new<R: rand::Rng>(num: usize, size: DSize, ops: Vec<DiceOperator>, roller: &mut Roller<R>) -> Self {
        let dice = roller
            .roll_n(num, size)
            .map(|x| Die::new(size, x))
            .collect();
        let mut this = Self { num, size, ops: Vec::new(), dice };
        for op in &ops {
            op.operate(&mut this, roller);
        }
        this.ops = ops;
        this
    }

    pub fn total(&self) -> DSize {
        self.dice.iter().map(|r| r.total()).sum()
    }

    pub(crate) fn roll_another<R: rand::Rng>(&mut self, size: DSize, roller: &mut Roller<R>) -> &mut Self {
        self.dice.push(Die::roll_new(size, roller));
        self
    }

    pub(crate) fn kept_set(&self) -> impl Iterator<Item = usize> + '_ {
        (0..self.dice.len()).filter(|&i| self.dice[i].is_kept())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Die {
    pub values: Vec<DieValue>,
    pub size: DSize,
    pub kept: bool,
}

impl Die {
    fn __new(size: DSize, values: Vec<DieValue>) -> Self {
        Self {
            values,
            size,
            kept: true,
        }
    }

    pub(crate) fn roll_new<R: rand::Rng>(size: DSize, roller: &mut Roller<R>) -> Self {
        let mut ret = Self::__new(size, Vec::with_capacity(1));
        ret.add_roll(roller);
        ret
    }

    pub(crate) fn new(size: DSize, value: DSize) -> Self {
        Self::__new(size, vec![DieValue::new(value)])
    }

    pub(crate) fn is_kept(&self) -> bool {
        self.kept
    }

    pub(crate) fn value(&self) -> DSize {
        self.values.last().map_or(0, |d| d.value())
    }

    pub(crate) fn total(&self) -> DSize {
        if self.kept {
            self.value()
        } else {
            0
        }
    }

    pub(crate) fn drop(&mut self) {
        self.kept = false;
    }

    pub(crate) fn add_roll<R: rand::Rng>(&mut self, roller: &mut Roller<R>) {
        let value = roller.roll(self.size);
        self.values.push(DieValue::new(value));
    }

    pub(crate) fn reroll<R: rand::Rng>(&mut self, roller: &mut Roller<R>) {
        if let Some(last) = self.values.last_mut() {
            last.drop();
        }
        self.add_roll(roller);
    }

    pub(crate) fn explode(&mut self) {
        if let Some(last) = self.values.last_mut() {
            last.explode();
        }
    }

    pub(crate) fn force_value(&mut self, new_value: DSize) {
        if let Some(last) = self.values.last_mut() {
            last.update(new_value);
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DieValue {
    pub history: Vec<DSize>,
    pub kept: bool,
    pub exploded: bool,
}

impl DieValue {
    fn new(value: DSize) -> Self {
        Self {
            history: vec![value],
            kept: true,
            exploded: false,
        }
    }

    pub(crate) fn value(&self) -> DSize {
        self.history.last().copied().unwrap_or(0)
    }

    pub(crate) fn total(&self) -> DSize {
        if self.kept {
            self.value()
        } else {
            0
        }
    }

    fn drop(&mut self) {
        self.kept = false;
    }

    fn explode(&mut self) {
        self.exploded = true;
    }

    fn update(&mut self, value: DSize) {
        self.history.push(value);
    }
}
