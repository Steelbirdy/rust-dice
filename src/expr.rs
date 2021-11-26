use crate::dice::Roller;
use crate::error::RollError;
use crate::parse::ast::{
    self, BinaryOperator, DiceSize, Float, Int, SetOperatorKind, UnaryOperator,
};
use std::cmp::Ordering;
use std::collections::HashSet;
use std::fmt;
use std::fmt::Write;

#[enum_dispatch::enum_dispatch]
pub trait NumberTrait {
    fn kept(&self) -> bool {
        true
    }

    fn drop(&mut self) {}

    fn number(&self) -> crate::Result<Val>;

    fn total(&self) -> crate::Result<Val> {
        if self.kept() {
            self.number()
        } else {
            Ok(Val::ZERO)
        }
    }
}

pub trait KeptSet {
    type Item: NumberTrait;

    fn kept_set(&self) -> &[Self::Item];

    fn kept_set_mut(&mut self) -> &mut [Self::Item];
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub roll: Number,
}

impl Expression {
    pub fn new(roll: Number) -> Self {
        Self { roll }
    }
}

impl NumberTrait for Expression {
    fn kept(&self) -> bool {
        self.roll.kept()
    }

    fn drop(&mut self) {
        self.roll.drop()
    }

    fn number(&self) -> crate::Result<Val> {
        self.roll.number()
    }

    fn total(&self) -> crate::Result<Val> {
        self.roll.total()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Number {
    pub kind: NumberKind,
    pub kept: bool,
    pub annotation: Option<String>,
}

impl Number {
    pub fn new(kind: NumberKind) -> Self {
        Self {
            kind,
            kept: true,
            annotation: None,
        }
    }

    pub fn children(&self) -> Vec<&Number> {
        match &self.kind {
            NumberKind::Literal(_) => vec![],
            NumberKind::UnOp(x) => vec![&*x.value],
            NumberKind::BinOp(x) => vec![&*x.left, &*x.right],
            NumberKind::Parenthetical(x) => vec![&*x.value],
            NumberKind::Set(x) => x.values.iter().collect(),
            NumberKind::Dice(_) => vec![],
        }
    }
}

impl NumberTrait for Number {
    fn kept(&self) -> bool {
        self.kept
    }

    fn drop(&mut self) {
        self.kept = false;
    }

    fn number(&self) -> crate::Result<Val> {
        self.kind.number()
    }
}

#[derive(Debug, Clone, PartialEq)]
#[enum_dispatch::enum_dispatch(NumberTrait, VisitStringify)]
pub enum NumberKind {
    Literal(Literal<Val>),
    UnOp(UnOp),
    BinOp(BinOp),
    Parenthetical(Parenthetical),
    Set(Set),
    Dice(Dice),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Literal<T> {
    pub values: Vec<T>,
    pub kept: bool,
    pub exploded: bool,
}

impl<T> Literal<T> {
    pub fn new(value: T) -> Self {
        Self {
            values: vec![value],
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

impl<T> NumberTrait for Literal<T>
where
    T: Copy + Into<Val>,
{
    fn kept(&self) -> bool {
        self.kept
    }

    fn drop(&mut self) {
        self.kept = false;
    }

    fn number(&self) -> crate::Result<Val> {
        Ok(self.values[self.values.len() - 1].into())
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Val {
    Int(Int),
    Float(Float),
}

impl Val {
    pub const ZERO: Self = Val::Int(0);

    pub fn is_zero(&self) -> bool {
        self.as_float().abs() < Float::EPSILON
    }

    pub fn as_int(&self) -> Int {
        match self {
            Self::Int(x) => *x,
            Self::Float(x) => *x as Int,
        }
    }

    pub fn as_float(&self) -> Float {
        match self {
            Self::Int(x) => *x as Float,
            Self::Float(x) => *x,
        }
    }
}

impl std::ops::Neg for Val {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::Float(-self.as_float())
    }
}

macro_rules! val_impl_bin_op {
    ($Name:ident, $fn_name:ident) => {
        impl std::ops::$Name for Val {
            type Output = Self;

            fn $fn_name(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Self::Int(x), Self::Int(y)) => Self::Int(x.$fn_name(y)),
                    (x, y) => Self::Float(x.as_float().$fn_name(y.as_float())),
                }
            }
        }
    };
}

val_impl_bin_op!(Add, add);
val_impl_bin_op!(Sub, sub);
val_impl_bin_op!(Mul, mul);
val_impl_bin_op!(Div, div);
val_impl_bin_op!(Rem, rem);

impl PartialEq for Val {
    fn eq(&self, other: &Self) -> bool {
        self.as_float().eq(&other.as_float())
    }
}

impl PartialOrd for Val {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.as_float().partial_cmp(&other.as_float())
    }
}

impl From<Int> for Val {
    fn from(x: Int) -> Self {
        Val::Int(x)
    }
}

impl From<Float> for Val {
    fn from(x: Float) -> Self {
        Val::Float(x)
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(x) => fmt::Display::fmt(x, f),
            Self::Float(x) => fmt::Debug::fmt(x, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnOp {
    pub op: UnaryOperator,
    pub value: Box<Number>,
}

impl UnOp {
    pub fn new(op: UnaryOperator, value: Number) -> Self {
        Self {
            op,
            value: Box::new(value),
        }
    }
}

impl NumberTrait for UnOp {
    fn number(&self) -> crate::Result<Val> {
        let value = self.value.total()?;
        Ok(match self.op {
            UnaryOperator::Plus => value,
            UnaryOperator::Minus => -value,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinOp {
    pub op: BinaryOperator,
    pub left: Box<Number>,
    pub right: Box<Number>,
}

impl BinOp {
    pub fn new(op: BinaryOperator, left: Number, right: Number) -> Self {
        Self {
            op,
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

impl NumberTrait for BinOp {
    fn number(&self) -> crate::Result<Val> {
        use BinaryOperator::*;

        let left = self.left.total()?;
        let right = self.right.total()?;
        Ok(match self.op {
            Add => left + right,
            Sub => left - right,
            Mul => left * right,
            Div => {
                if right.is_zero() {
                    return Err(RollError::value_error("cannot divide by zero"));
                } else {
                    left / right
                }
            }
            FloorDiv => {
                if right.is_zero() {
                    return Err(RollError::value_error("cannot divide by zero"));
                } else {
                    match left / right {
                        Val::Float(x) => Val::Int(x.floor() as Int),
                        x @ Val::Int(_) => x,
                    }
                }
            }
            Mod => {
                if right.is_zero() {
                    return Err(RollError::value_error("cannot modulo by zero"));
                } else {
                    left % right
                }
            }
            Eq => ((left == right) as Int).into(),
            Neq => ((left != right) as Int).into(),
            Lt => ((left < right) as Int).into(),
            Gt => ((left > right) as Int).into(),
            Leq => ((left <= right) as Int).into(),
            Geq => ((left >= right) as Int).into(),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parenthetical {
    pub value: Box<Number>,
    pub operations: Vec<SetOperator>,
    pub kept: bool,
}

impl Parenthetical {
    pub fn new(value: Number, ops: Vec<SetOperator>) -> Self {
        Self {
            value: Box::new(value),
            operations: ops,
            kept: true,
        }
    }
}

impl NumberTrait for Parenthetical {
    fn kept(&self) -> bool {
        self.kept
    }

    fn drop(&mut self) {
        self.kept = false;
    }

    fn number(&self) -> crate::Result<Val> {
        self.value.total()
    }
}

impl KeptSet for Parenthetical {
    type Item = Number;

    fn kept_set(&self) -> &[Self::Item] {
        std::slice::from_ref(&*self.value)
    }

    fn kept_set_mut(&mut self) -> &mut [Self::Item] {
        std::slice::from_mut(&mut *self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Set {
    pub values: Vec<Number>,
    pub operations: Vec<SetOperator>,
}

impl Set {
    pub fn new(values: Vec<Number>) -> Self {
        Self {
            values,
            operations: Vec::new(),
        }
    }
}

impl NumberTrait for Set {
    fn number(&self) -> crate::Result<Val> {
        self.values
            .iter()
            .map(NumberTrait::total)
            .try_fold(Val::ZERO, |a, b| b.map(|b| a + b))
    }
}

impl KeptSet for Set {
    type Item = Number;

    fn kept_set(&self) -> &[Self::Item] {
        &self.values
    }

    fn kept_set_mut(&mut self) -> &mut [Self::Item] {
        &mut self.values
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Dice {
    pub num: Int,
    pub size: DiceSize,
    pub values: Vec<Die>,
    pub operations: Vec<SetOperator>,
}

impl Dice {
    fn __new(num: Int, size: DiceSize, values: Vec<Die>, ops: Vec<SetOperator>) -> Self {
        Self {
            num,
            size,
            values,
            operations: ops,
        }
    }

    pub fn roll_new<R: rand::Rng>(
        num: Int,
        size: DiceSize,
        context: &mut Roller<R>,
    ) -> crate::Result<Self> {
        let values = context
            .roll_n(num as usize, size)?
            .map(|x| Die::new(size, x))
            .collect();
        Ok(Self::__new(num, size, values, Vec::new()))
    }

    pub fn roll_another<R: rand::Rng>(&mut self, context: &mut Roller<R>) -> crate::Result<()> {
        let die = Die::roll_new(self.size, context)?;
        self.values.push(die);
        Ok(())
    }
}

impl NumberTrait for Dice {
    fn number(&self) -> crate::Result<Val> {
        self.values
            .iter()
            .map(NumberTrait::total)
            .try_fold(Val::ZERO, |a, b| b.map(|b| a + b))
    }
}

impl KeptSet for Dice {
    type Item = Die;

    fn kept_set(&self) -> &[Self::Item] {
        &self.values
    }

    fn kept_set_mut(&mut self) -> &mut [Self::Item] {
        &mut self.values
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Die {
    pub size: DiceSize,
    pub values: Vec<Literal<Int>>,
    pub kept: bool,
}

impl Die {
    fn __new(size: DiceSize, values: Vec<Literal<Int>>) -> Self {
        Self {
            size,
            values,
            kept: true,
        }
    }

    pub fn new(size: DiceSize, value: Int) -> Self {
        Self::__new(size, vec![Literal::new(value)])
    }

    pub fn roll_new<R: rand::Rng>(size: DiceSize, context: &mut Roller<R>) -> crate::Result<Self> {
        let mut ret = Self::__new(size, Vec::new());
        ret.add_roll(context)?;
        Ok(ret)
    }

    pub(crate) fn add_roll<R: rand::Rng>(&mut self, context: &mut Roller<R>) -> crate::Result<()> {
        let n = context.roll_one(self.size)?;
        self.values.push(Literal::new(n));
        Ok(())
    }

    pub fn reroll<R: rand::Rng>(&mut self, context: &mut Roller<R>) -> crate::Result<()> {
        if let Some(last_value) = self.values.last_mut() {
            last_value.drop();
        }
        self.add_roll(context)
    }

    pub fn explode(&mut self) {
        if let Some(last_value) = self.values.last_mut() {
            last_value.explode();
        }
    }

    pub fn force_value(&mut self, new_value: Int) {
        if let Some(last_value) = self.values.last_mut() {
            last_value.update(new_value);
        }
    }
}

impl NumberTrait for Die {
    fn kept(&self) -> bool {
        self.kept
    }

    fn drop(&mut self) {
        self.kept = false;
    }

    fn number(&self) -> crate::Result<Val> {
        self.values.last().map_or(Ok(Val::ZERO), |d| d.total())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SetOperator {
    pub op: SetOperatorKind,
    pub sels: Vec<SetSelector>,
}

impl SetOperator {
    pub fn new(op: SetOperatorKind, sels: Vec<SetSelector>) -> Self {
        Self { op, sels }
    }

    pub(crate) fn from_ast(op: &ast::SetOperator) -> Self {
        let my_op = op.kind;
        let sels = op.sels.iter().map(SetSelector::from_ast).collect();
        Self::new(my_op, sels)
    }

    pub fn operate_on_set(&self, target: &mut Set) -> crate::Result<()> {
        match self.op {
            SetOperatorKind::Keep => self.keep(target),
            SetOperatorKind::Drop => self.drop(target),
            _ => unreachable!(),
        }
    }

    pub fn operate_on_dice<R: rand::Rng>(
        &self,
        target: &mut Dice,
        context: &mut Roller<R>,
    ) -> crate::Result<()> {
        match self.op {
            SetOperatorKind::Keep => self.keep(target),
            SetOperatorKind::Drop => self.drop(target),
            SetOperatorKind::Reroll => self.reroll(target, context),
            SetOperatorKind::RerollOnce => self.reroll_once(target, context),
            SetOperatorKind::Explode => self.explode(target, context),
            SetOperatorKind::ExplodeOnce => self.explode_once(target, context),
            SetOperatorKind::Minimum => self.minimum(target),
            SetOperatorKind::Maximum => self.maximum(target),
        }
    }

    fn select(
        &self,
        target: &impl KeptSet,
        max_targets: Option<usize>,
    ) -> crate::Result<HashSet<usize>> {
        let mut ret = HashSet::new();
        for selector in &self.sels {
            let batch_max = max_targets.map(|x| x - ret.len());
            if batch_max == Some(0) {
                break;
            }

            ret.extend(selector.select(target.kept_set(), batch_max)?);
        }
        Ok(ret)
    }

    fn keep(&self, target: &mut impl KeptSet) -> crate::Result<()> {
        let to_keep = self.select(target, None)?;
        let kept_set = target.kept_set_mut();

        let to_drop = (0..kept_set.len()).filter(|i| !to_keep.contains(i));
        for i in to_drop {
            kept_set[i].drop();
        }
        Ok(())
    }

    fn drop(&self, target: &mut impl KeptSet) -> crate::Result<()> {
        let to_drop = self.select(target, None)?;
        let kept_set = target.kept_set_mut();
        for i in to_drop {
            kept_set[i].drop();
        }
        Ok(())
    }

    fn reroll<R: rand::Rng>(
        &self,
        target: &mut Dice,
        context: &mut Roller<R>,
    ) -> crate::Result<()> {
        let mut to_reroll = self.select(target, None)?;
        while !to_reroll.is_empty() {
            for i in to_reroll {
                target.values[i].reroll(context)?;
            }

            to_reroll = self.select(target, None)?;
        }
        Ok(())
    }

    fn reroll_once<R: rand::Rng>(
        &self,
        target: &mut Dice,
        context: &mut Roller<R>,
    ) -> crate::Result<()> {
        for i in self.select(target, None)? {
            target.values[i].reroll(context)?;
        }
        Ok(())
    }

    fn explode<R: rand::Rng>(
        &self,
        target: &mut Dice,
        context: &mut Roller<R>,
    ) -> crate::Result<()> {
        let mut to_explode = self.select(target, None)?;
        let mut already_exploded = HashSet::new();

        while !to_explode.is_empty() {
            for &i in &to_explode {
                target.values[i].explode();
                target.roll_another(context)?;
            }
            already_exploded.extend(to_explode);
            to_explode = self
                .select(target, None)?
                .difference(&already_exploded)
                .copied()
                .collect();
        }
        Ok(())
    }

    fn explode_once<R: rand::Rng>(
        &self,
        target: &mut Dice,
        context: &mut Roller<R>,
    ) -> crate::Result<()> {
        for i in self.select(target, None)? {
            target.values[i].explode();
            target.roll_another(context)?;
        }
        Ok(())
    }

    fn minimum(&self, target: &mut Dice) -> crate::Result<()> {
        let sel = &self.sels[self.sels.len() - 1];
        // Only EqualTo can be used with Minimum and Maximum.
        // Any cases of this should be caught during parsing.
        debug_assert_eq!(sel.cat, SetSelectorKind::EqualTo);

        let the_min = sel.num;
        for die in target.kept_set_mut() {
            if die.number()?.as_int() < the_min {
                die.force_value(the_min);
            }
        }
        Ok(())
    }

    fn maximum(&self, target: &mut Dice) -> crate::Result<()> {
        let sel = &self.sels[self.sels.len() - 1];
        // Only EqualTo can be used with Minimum and Maximum.
        // Any cases of this should be caught during parsing.
        debug_assert_eq!(sel.cat, SetSelectorKind::EqualTo);

        let the_max = sel.num;
        for die in target.kept_set_mut() {
            if die.number()?.as_int() > the_max {
                die.force_value(the_max);
            }
        }
        Ok(())
    }
}

impl fmt::Display for SetOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.op.to_str())?;
        for sel in &self.sels {
            fmt::Display::fmt(sel, f)?;
        }

        Ok(())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct SetSelector {
    pub cat: SetSelectorKind,
    pub num: Int,
}

impl SetSelector {
    pub fn new(cat: SetSelectorKind, num: Int) -> Self {
        Self { cat, num }
    }

    pub fn select(
        &self,
        kept_set: &[impl NumberTrait],
        max_targets: Option<usize>,
    ) -> crate::Result<Vec<usize>> {
        let mut selected = self.cat.select(self.num, kept_set)?;
        if let Some(max_targets) = max_targets {
            selected.truncate(max_targets);
        }
        Ok(selected)
    }

    pub(crate) fn from_ast(sel: &ast::SetSelector) -> Self {
        use SetSelectorKind::*;
        let (cat, num) = match sel {
            ast::SetSelector::Highest(x) => (Highest, x),
            ast::SetSelector::Lowest(x) => (Lowest, x),
            ast::SetSelector::LessThan(x) => (LessThan, x),
            ast::SetSelector::GreaterThan(x) => (GreaterThan, x),
            ast::SetSelector::EqualTo(x) => (EqualTo, x),
        };
        Self::new(cat, *num)
    }
}

impl fmt::Display for SetSelector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.cat, self.num)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SetSelectorKind {
    Highest,
    Lowest,
    LessThan,
    GreaterThan,
    EqualTo,
}

impl SetSelectorKind {
    fn select(&self, n: Int, kept_set: &[impl NumberTrait]) -> crate::Result<Vec<usize>> {
        match self {
            Self::Highest => Self::highest(n, kept_set),
            Self::Lowest => Self::lowest(n, kept_set),
            Self::LessThan => Self::less_than(n, kept_set),
            Self::GreaterThan => Self::greater_than(n, kept_set),
            Self::EqualTo => Self::equal_to(n, kept_set),
        }
    }

    fn highest(n: Int, kept_set: &[impl NumberTrait]) -> crate::Result<Vec<usize>> {
        let mut kept_set: Vec<_> = kept_set
            .iter()
            .enumerate()
            .map(|(i, x)| x.total().map(|x| (i, x)))
            .collect::<crate::Result<_>>()?;
        kept_set.sort_by(|(_, x), (_, y)| y.partial_cmp(x).unwrap());
        Ok(kept_set
            .into_iter()
            .map(|(i, _)| i)
            .take(n as usize)
            .collect())
    }

    fn lowest(n: Int, kept_set: &[impl NumberTrait]) -> crate::Result<Vec<usize>> {
        let mut kept_set: Vec<_> = kept_set
            .iter()
            .enumerate()
            .map(|(i, x)| x.total().map(|x| (i, x)))
            .collect::<crate::Result<_>>()?;
        kept_set.sort_by(|(_, x), (_, y)| x.partial_cmp(y).unwrap());
        Ok(kept_set
            .into_iter()
            .map(|(i, _)| i)
            .take(n as usize)
            .collect())
    }

    fn less_than(n: Int, kept_set: &[impl NumberTrait]) -> crate::Result<Vec<usize>> {
        let n = Val::from(n);
        kept_set
            .iter()
            .enumerate()
            .filter_map(|(i, x)| match x.total() {
                Err(e) => Some(Err(e)),
                Ok(x) if x < n => Some(Ok(i)),
                Ok(_) => None,
            })
            .collect()
    }

    fn greater_than(n: Int, kept_set: &[impl NumberTrait]) -> crate::Result<Vec<usize>> {
        let n = Val::from(n);
        kept_set
            .iter()
            .enumerate()
            .filter_map(|(i, x)| match x.total() {
                Err(e) => Some(Err(e)),
                Ok(x) if x > n => Some(Ok(i)),
                Ok(_) => None,
            })
            .collect()
    }

    fn equal_to(n: Int, kept_set: &[impl NumberTrait]) -> crate::Result<Vec<usize>> {
        let n = Val::from(n);
        kept_set
            .iter()
            .enumerate()
            .filter_map(|(i, x)| match x.total() {
                Err(e) => Some(Err(e)),
                Ok(x) if x == n => Some(Ok(i)),
                Ok(_) => None,
            })
            .collect()
    }
}

impl fmt::Display for SetSelectorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lowest => f.write_char('l'),
            Self::Highest => f.write_char('h'),
            Self::LessThan => f.write_char('<'),
            Self::GreaterThan => f.write_char('>'),
            Self::EqualTo => Ok(()),
        }
    }
}
