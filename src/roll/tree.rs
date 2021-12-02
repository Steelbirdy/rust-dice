use super::{error::RollError, num::Number, roller::Roller, RResult, RollContext};
use crate::common::*;
use std::collections::HashSet;
use std::fmt;

#[enum_dispatch::enum_dispatch]
pub trait Eval {
    fn kept(&self) -> bool {
        true
    }

    fn drop(&mut self) {}

    fn number(&self) -> RResult<Number>;

    fn total(&self) -> RResult<Number> {
        if self.kept() {
            self.number()
        } else {
            Ok(Number::ZERO)
        }
    }
}

pub trait RollSet {
    type Item: Eval;

    fn set(&self) -> &[Self::Item];

    fn set_mut(&mut self) -> &mut [Self::Item];

    fn kept_set(&self) -> Vec<&Self::Item> {
        self.set().iter().filter(|x| x.kept()).collect()
    }

    fn kept_set_mut(&mut self) -> Vec<&mut Self::Item> {
        self.set_mut().iter_mut().filter(|x| x.kept()).collect()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Roll<'a> {
    pub(crate) tree: RollTree<'a>,
    pub(crate) comment: Option<&'a str>,
}

impl<'a> Roll<'a> {
    pub(crate) fn new(tree: RollTree<'a>, comment: Option<&'a str>) -> Self {
        Self { tree, comment }
    }

    pub fn total(&self) -> Result<Number, RollError> {
        self.tree.total()
    }
}

impl Eval for Roll<'_> {
    fn number(&self) -> RResult<Number> {
        self.tree.number()
    }

    fn total(&self) -> RResult<Number> {
        self.total()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RollTree<'a> {
    pub roll: RollNode<'a>,
    pub annotations: Vec<&'a str>,
    kept: bool,
}

impl<'a> RollTree<'a> {
    pub fn new(roll: RollNode<'a>) -> Self {
        Self {
            roll,
            annotations: vec![],
            kept: true,
        }
    }
}

impl Eval for RollTree<'_> {
    fn kept(&self) -> bool {
        self.kept
    }

    fn drop(&mut self) {
        self.kept = false;
    }

    fn number(&self) -> RResult<Number> {
        self.roll.total()
    }
}

#[derive(Debug, Clone, PartialEq)]
#[enum_dispatch::enum_dispatch(Eval, AcceptRoll)]
pub enum RollNode<'a> {
    Literal(Literal<Number>),
    Set(Set<'a>),
    Dice(Dice),
    Grouping(Grouping<'a>),
    Unary(Unary<'a>),
    Binary(Binary<'a>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Literal<T: Copy + Into<Number>> {
    pub values: NonEmpty<T>,
    pub kept: bool,
    pub exploded: bool,
}

impl<T: Copy + Into<Number>> Literal<T> {
    pub fn new(value: T) -> Self {
        Self {
            values: vec1![value],
            kept: true,
            exploded: false,
        }
    }

    pub fn explode(&mut self) {
        self.exploded = true;
    }

    pub fn update(&mut self, value: T) {
        self.values.push(value);
    }
}

impl<T: Copy + Into<Number>> Eval for Literal<T> {
    fn kept(&self) -> bool {
        self.kept
    }

    fn drop(&mut self) {
        self.kept = false;
    }

    fn number(&self) -> RResult<Number> {
        Ok((*self.values.last()).into())
    }
}

impl<T: Copy + Into<Number>> fmt::Display for Literal<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.values.last(), f)
    }
}

impl<T: Copy + Into<Number>> From<T> for Literal<T> {
    fn from(t: T) -> Self {
        Self::new(t)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Set<'a> {
    pub values: Vec<RollTree<'a>>,
    pub ops: Vec<SetOperator>,
}

impl<'a> Set<'a> {
    pub fn new(values: Vec<RollTree<'a>>, ops: Vec<SetOperator>) -> Self {
        Self { values, ops }
    }
}

impl Eval for Set<'_> {
    fn number(&self) -> RResult<Number> {
        self.values
            .iter()
            .map(Eval::total)
            .try_fold(Number::ZERO, |a, b| b.map(|b| a + b))
    }
}

impl<'a> RollSet for Set<'a> {
    type Item = RollTree<'a>;

    fn set(&self) -> &[Self::Item] {
        &self.values
    }

    fn set_mut(&mut self) -> &mut [Self::Item] {
        &mut self.values
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Dice {
    pub num: Num,
    pub sides: Sides,
    pub values: Vec<Die>,
    pub ops: Vec<DiceOperator>,
}

impl Dice {
    pub fn new(num: Num, sides: Sides, values: Vec<Die>, ops: Vec<DiceOperator>) -> Self {
        Self {
            num,
            sides,
            values,
            ops,
        }
    }

    pub fn roll_new<R: Roller>(
        ctx: &mut RollContext<R>,
        num: Num,
        sides: Sides,
        ops: Vec<DiceOperator>,
    ) -> RResult<Self> {
        let count = num.get() as usize;

        let mut ret = Self::new(num, sides, Vec::with_capacity(count), ops);
        ret.roll_more(ctx, count)?;
        Ok(ret)
    }

    pub fn roll_another<R: Roller>(&mut self, ctx: &mut RollContext<R>) -> RResult<()> {
        let die = Die::roll_new(ctx, self.sides)?;
        self.values.push(die);
        Ok(())
    }

    pub fn roll_more<R: Roller>(&mut self, ctx: &mut RollContext<R>, num: usize) -> RResult<()> {
        self.values.extend(
            ctx.roll(num, self.sides)?
                .map(|x| Die::new(self.sides, x.get() as Int)),
        );
        Ok(())
    }
}

impl Eval for Dice {
    fn number(&self) -> RResult<Number> {
        self.values
            .iter()
            .map(Eval::total)
            .try_fold(Number::ZERO, |a, b| b.map(|b| a + b))
    }
}

impl RollSet for Dice {
    type Item = Die;

    fn set(&self) -> &[Self::Item] {
        &self.values
    }

    fn set_mut(&mut self) -> &mut [Self::Item] {
        &mut self.values
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Die {
    pub sides: Sides,
    pub values: NonEmpty<Literal<Int>>,
    pub kept: bool,
}

impl Die {
    fn __new(sides: Sides, values: NonEmpty<Literal<Int>>) -> Self {
        Self {
            sides,
            values,
            kept: true,
        }
    }

    pub fn new(sides: Sides, value: Int) -> Self {
        Self::__new(sides, vec1![value.into()])
    }

    pub fn roll_new<R: Roller>(ctx: &mut RollContext<R>, sides: Sides) -> RResult<Self> {
        Ok(Self::new(sides, ctx.roll_one(sides)?.get() as Int))
    }

    pub fn add_roll<R: Roller>(&mut self, ctx: &mut RollContext<R>) -> RResult<()> {
        let x = ctx.roll_one(self.sides)?;
        self.values.push((x.get() as Int).into());
        Ok(())
    }

    pub fn reroll<R: Roller>(&mut self, ctx: &mut RollContext<R>) -> RResult<()> {
        self.value_mut().drop();
        self.add_roll(ctx)
    }

    pub fn explode(&mut self) {
        self.value_mut().explode();
    }

    pub fn force_value(&mut self, new_value: Int) {
        self.value_mut().update(new_value);
    }

    fn value(&self) -> &Literal<Int> {
        self.values.last()
    }

    fn value_mut(&mut self) -> &mut Literal<Int> {
        self.values.last_mut()
    }
}

impl Eval for Die {
    fn kept(&self) -> bool {
        self.kept
    }

    fn drop(&mut self) {
        self.kept = false;
    }

    fn number(&self) -> RResult<Number> {
        self.value().total()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Grouping<'a>(pub(crate) Box<RollTree<'a>>);

impl<'a> Grouping<'a> {
    pub fn new(value: RollTree<'a>) -> Self {
        Self(Box::new(value))
    }
}

impl Eval for Grouping<'_> {
    fn kept(&self) -> bool {
        self.0.kept()
    }

    fn drop(&mut self) {
        <RollTree<'_> as Eval>::drop(&mut *self.0)
    }

    fn number(&self) -> RResult<Number> {
        self.0.total()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary<'a> {
    pub op: UnaryOperator,
    pub value: Box<RollTree<'a>>,
}

impl<'a> Unary<'a> {
    pub fn new(op: UnaryOperator, value: RollTree<'a>) -> Self {
        Self {
            op,
            value: Box::new(value),
        }
    }
}

impl Eval for Unary<'_> {
    fn number(&self) -> RResult<Number> {
        let value = self.value.total()?;
        Ok(match self.op {
            UnaryOperator::Pos => value,
            UnaryOperator::Neg => -value,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary<'a> {
    pub left: Box<RollTree<'a>>,
    pub op: BinaryOperator,
    pub right: Box<RollTree<'a>>,
}

impl<'a> Binary<'a> {
    pub fn new(left: RollTree<'a>, op: BinaryOperator, right: RollTree<'a>) -> Self {
        Self {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    }
}

impl Eval for Binary<'_> {
    fn number(&self) -> RResult<Number> {
        use BinaryOperator::*;

        let left = self.left.total()?;
        let right = self.right.total()?;
        Ok(match self.op {
            Add => left + right,
            Sub => left - right,
            Mul => left * right,
            Div => {
                if right == Number::ZERO {
                    return Err(RollError::ZeroDivision);
                } else {
                    left / right
                }
            }
            Flr => {
                if right == Number::ZERO {
                    return Err(RollError::ZeroDivision);
                } else {
                    (left / right).floor()
                }
            }
            Rem => {
                if right == Number::ZERO {
                    return Err(RollError::ZeroModulo);
                } else {
                    left % right
                }
            }
            Lt => ((left < right) as Int).into(),
            Gt => ((left > right) as Int).into(),
            Le => ((left <= right) as Int).into(),
            Ge => ((left >= right) as Int).into(),
            Eq => ((left == right) as Int).into(),
            Ne => ((left != right) as Int).into(),
        })
    }
}

impl SetOperator {
    pub(crate) fn operate(&self, target: &mut Set<'_>) -> RResult<()> {
        match self {
            Self::Keep(sels) => keep(target, sels),
            Self::Drop(sels) => drop(target, sels),
        }
    }
}

impl DiceOperator {
    pub(crate) fn operate<R: Roller>(
        &self,
        ctx: &mut RollContext<R>,
        target: &mut Dice,
    ) -> RResult<()> {
        match self {
            Self::Keep(sels) => keep(target, sels),
            Self::Drop(sels) => drop(target, sels),
            Self::Reroll(sels) => Self::reroll(ctx, target, sels),
            Self::RerollOnce(sels) => Self::reroll_once(ctx, target, sels),
            Self::Explode(sels) => Self::explode(ctx, target, sels),
            Self::ExplodeOnce(sels) => Self::explode_once(ctx, target, sels),
            Self::Minimum(x) => Self::minimum(*x, target),
            Self::Maximum(x) => Self::maximum(*x, target),
        }
    }

    fn reroll<R: Roller>(
        ctx: &mut RollContext<R>,
        target: &mut Dice,
        selectors: &[Selector],
    ) -> RResult<()> {
        let mut to_reroll = select(target, selectors, None)?;
        while !to_reroll.is_empty() {
            for i in to_reroll {
                target.values[i].reroll(ctx)?;
            }

            to_reroll = select(target, selectors, None)?;
        }
        Ok(())
    }

    fn reroll_once<R: Roller>(
        ctx: &mut RollContext<R>,
        target: &mut Dice,
        selectors: &[Selector],
    ) -> RResult<()> {
        for i in select(target, selectors, None)? {
            target.values[i].reroll(ctx)?;
        }
        Ok(())
    }

    fn explode<R: Roller>(
        ctx: &mut RollContext<R>,
        target: &mut Dice,
        selectors: &[Selector],
    ) -> RResult<()> {
        let mut to_explode = select(target, selectors, None)?;
        let mut already_exploded = HashSet::new();

        while !to_explode.is_empty() {
            already_exploded.extend(to_explode.iter().copied());

            let count = to_explode.len();
            for i in to_explode.drain() {
                target.values[i].explode();
            }
            target.roll_more(ctx, count)?;

            to_explode.extend(select(target, selectors, None)?.difference(&already_exploded));
        }
        Ok(())
    }

    fn explode_once<R: Roller>(
        ctx: &mut RollContext<R>,
        target: &mut Dice,
        selectors: &[Selector],
    ) -> RResult<()> {
        for i in select(target, selectors, Some(1))? {
            target.values[i].explode();
            target.roll_another(ctx)?;
        }
        Ok(())
    }

    fn minimum(min: Int, target: &mut Dice) -> RResult<()> {
        let min_val: Number = min.into();
        for die in target.kept_set_mut() {
            if die.number()? < min_val {
                die.force_value(min);
            }
        }
        Ok(())
    }

    fn maximum(max: Int, target: &mut Dice) -> RResult<()> {
        let max_val: Number = max.into();
        for die in target.kept_set_mut() {
            if die.number()? > max_val {
                die.force_value(max);
            }
        }
        Ok(())
    }
}

fn select(
    target: &impl RollSet,
    selectors: &[Selector],
    max_targets: Option<usize>,
) -> RResult<HashSet<usize>> {
    let mut ret = HashSet::new();
    for selector in selectors {
        let batch_max = max_targets.map(|x| x - ret.len());
        if batch_max == Some(0) {
            break;
        }

        ret.extend(selector.select(target, batch_max)?)
    }
    Ok(ret)
}

fn keep(target: &mut impl RollSet, selectors: &[Selector]) -> RResult<()> {
    let to_keep = select(target, selectors, None)?;
    for (i, value) in target.kept_set_mut().into_iter().enumerate() {
        if !to_keep.contains(&i) {
            value.drop();
        }
    }
    Ok(())
}

fn drop(target: &mut impl RollSet, selectors: &[Selector]) -> RResult<()> {
    let to_drop = select(target, selectors, None)?;
    for (i, value) in target.kept_set_mut().into_iter().enumerate() {
        if to_drop.contains(&i) {
            value.drop();
        }
    }
    Ok(())
}

impl Selector {
    pub(crate) fn select(
        &self,
        target: &impl RollSet,
        max_targets: Option<usize>,
    ) -> RResult<HashSet<usize>> {
        let mut target: Vec<_> = target
            .kept_set()
            .into_iter()
            .enumerate()
            .map(|(i, x)| x.total().map(|x| (i, x)))
            .collect::<RResult<_>>()?;

        match *self {
            Self::Highest(n) => Self::highest(n, &mut target),
            Self::Lowest(n) => Self::lowest(n, &mut target),
            Self::Less(x) => Self::less_than(x, &mut target),
            Self::Greater(x) => Self::greater_than(x, &mut target),
            Self::Equal(x) => Self::equal_to(x, &mut target),
        };
        if let Some(max_targets) = max_targets {
            target.truncate(max_targets);
        }

        Ok(target.into_iter().map(|(i, _)| i).collect())
    }

    fn highest(n: usize, target: &mut Vec<(usize, Number)>) {
        target.sort_by(|(_, x), (_, y)| y.partial_cmp(x).unwrap());
        target.truncate(n);
    }

    fn lowest(n: usize, target: &mut Vec<(usize, Number)>) {
        target.sort_by(|(_, x), (_, y)| x.partial_cmp(y).unwrap());
        target.truncate(n);
    }

    fn less_than(x: Int, target: &mut Vec<(usize, Number)>) {
        let x = Number::from(x);
        target.retain(|(_, v)| *v < x);
    }

    fn greater_than(x: Int, target: &mut Vec<(usize, Number)>) {
        let x = Number::from(x);
        target.retain(|(_, v)| *v > x);
    }

    fn equal_to(x: Int, target: &mut Vec<(usize, Number)>) {
        let x = Number::from(x);
        target.retain(|(_, v)| *v == x);
    }
}
