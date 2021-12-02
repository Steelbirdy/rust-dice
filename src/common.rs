use std::fmt::{self, Write};
use std::num::{NonZeroU32, NonZeroUsize};
use std::str::FromStr;
pub use vec1::vec1;

pub type Int = i32;
pub type UInt = u32;
pub type NonZeroUInt = NonZeroU32;

pub type Float = f64;

pub type Num = NonZeroUsize;

pub type NonEmpty<T> = vec1::Vec1<T>;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Sides {
    Poly(NonZeroUInt),
    Percentile,
}

impl PartialOrd for Sides {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Poly(x), Self::Poly(y)) => x.partial_cmp(y),
            (Self::Percentile, Self::Percentile) => Some(std::cmp::Ordering::Equal),
            _ => None,
        }
    }
}

impl fmt::Display for Sides {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Poly(x) => fmt::Display::fmt(x, f),
            Self::Percentile => f.write_char('%'),
        }
    }
}

impl From<NonZeroUInt> for Sides {
    fn from(x: NonZeroUInt) -> Self {
        Self::Poly(x)
    }
}

impl TryFrom<Int> for Sides {
    type Error = <NonZeroUInt as TryFrom<UInt>>::Error;

    fn try_from(value: Int) -> Result<Self, Self::Error> {
        NonZeroUInt::try_from(value as UInt).map(Self::Poly)
    }
}

impl FromStr for Sides {
    type Err = <NonZeroUInt as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "%" {
            Ok(Self::Percentile)
        } else {
            s.parse().map(Self::Poly)
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum UnaryOperator {
    Pos,
    Neg,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let c = match self {
            Self::Pos => '+',
            Self::Neg => '-',
        };
        f.write_char(c)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Flr,
    Rem,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Flr => "//",
            Self::Rem => "%",
            Self::Lt => "<",
            Self::Gt => ">",
            Self::Le => "<=",
            Self::Ge => ">=",
            Self::Eq => "==",
            Self::Ne => "!=",
        };
        f.write_str(s)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DiceOperator {
    Keep(NonEmpty<Selector>),
    Drop(NonEmpty<Selector>),
    Reroll(NonEmpty<Selector>),
    RerollOnce(NonEmpty<Selector>),
    Explode(NonEmpty<Selector>),
    ExplodeOnce(NonEmpty<Selector>),
    Minimum(Int),
    Maximum(Int),
}

impl DiceOperator {
    pub const fn is_immediate(&self) -> bool {
        matches!(self, Self::Minimum(_) | Self::Maximum(_))
    }

    pub(crate) const fn kind(&self) -> OperatorKind {
        match self {
            Self::Keep(_) => OperatorKind::Keep,
            Self::Drop(_) => OperatorKind::Drop,
            Self::Reroll(_) => OperatorKind::Reroll,
            Self::RerollOnce(_) => OperatorKind::RerollOnce,
            Self::Explode(_) => OperatorKind::Explode,
            Self::ExplodeOnce(_) => OperatorKind::ExplodeOnce,
            Self::Minimum(_) => OperatorKind::Minimum,
            Self::Maximum(_) => OperatorKind::Maximum,
        }
    }

    pub fn sels_mut(&mut self) -> Option<&mut NonEmpty<Selector>> {
        Some(match self {
            Self::Keep(s) => s,
            Self::Drop(s) => s,
            Self::Reroll(s) => s,
            Self::RerollOnce(s) => s,
            Self::Explode(s) => s,
            Self::ExplodeOnce(s) => s,
            Self::Minimum(_) | Self::Maximum(_) => return None,
        })
    }

    pub fn into_sels(self) -> Option<NonEmpty<Selector>> {
        Some(match self {
            Self::Keep(s) => s,
            Self::Drop(s) => s,
            Self::Reroll(s) => s,
            Self::RerollOnce(s) => s,
            Self::Explode(s) => s,
            Self::ExplodeOnce(s) => s,
            Self::Minimum(_) | Self::Maximum(_) => return None,
        })
    }

    pub fn add_sels(&mut self, sels: &mut Vec<Selector>) {
        if let Some(my_sels) = self.sels_mut() {
            my_sels.append(sels)
        }
    }
}

impl fmt::Display for DiceOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let sels = match self {
            Self::Keep(sels) => {
                f.write_char('k')?;
                sels
            }
            Self::Drop(sels) => {
                f.write_char('p')?;
                sels
            }
            Self::Reroll(sels) => {
                f.write_str("rr")?;
                sels
            }
            Self::RerollOnce(sels) => {
                f.write_str("ro")?;
                sels
            }
            Self::Explode(sels) => {
                f.write_char('e')?;
                sels
            }
            Self::ExplodeOnce(sels) => {
                f.write_str("ra")?;
                sels
            }
            Self::Minimum(n) => return write!(f, "mi{}", n),
            Self::Maximum(n) => return write!(f, "ma{}", n),
        };
        for sel in sels {
            write!(f, "{}", sel)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SetOperator {
    Keep(NonEmpty<Selector>),
    Drop(NonEmpty<Selector>),
}

impl SetOperator {
    pub(crate) const fn kind(&self) -> OperatorKind {
        match self {
            Self::Keep(_) => OperatorKind::Keep,
            Self::Drop(_) => OperatorKind::Drop,
        }
    }

    pub fn sels_mut(&mut self) -> &mut NonEmpty<Selector> {
        match self {
            Self::Keep(s) => s,
            Self::Drop(s) => s,
        }
    }

    pub fn into_sels(self) -> NonEmpty<Selector> {
        match self {
            Self::Keep(s) => s,
            Self::Drop(s) => s,
        }
    }

    pub fn add_sels(&mut self, sels: &mut Vec<Selector>) {
        self.sels_mut().append(sels);
    }
}

impl fmt::Display for SetOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let sels = match self {
            Self::Keep(sels) => {
                f.write_char('k')?;
                sels
            }
            Self::Drop(sels) => {
                f.write_char('p')?;
                sels
            }
        };
        for sel in sels {
            write!(f, "{}", sel)?;
        }
        Ok(())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Selector {
    Highest(usize),
    Lowest(usize),
    Less(Int),
    Greater(Int),
    Equal(Int),
}

impl fmt::Display for Selector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Highest(n) => write!(f, "h{}", n),
            Self::Lowest(n) => write!(f, "l{}", n),
            Self::Less(x) => write!(f, "<{}", x),
            Self::Greater(x) => write!(f, ">{}", x),
            Self::Equal(x) => write!(f, "{}", x),
        }
    }
}

#[derive(Eq, PartialEq)]
pub(crate) enum OperatorKind {
    Keep,
    Drop,
    Reroll,
    RerollOnce,
    Explode,
    ExplodeOnce,
    Minimum,
    Maximum,
}

#[cfg(test)]
pub(crate) mod test_utils {
    pub use super::*;
    pub(crate) use crate::parse::ast;
    pub use BinaryOperator::*;
    pub use UnaryOperator::*;

    pub trait NodeExt<'a>: Sized {
        fn annot(x: Self, a: Vec<&'a str>) -> Self;

        fn int(x: Int) -> Self;

        fn float(x: Float) -> Self;

        fn set(x: Vec<Self>) -> Self;

        fn op_set(x: Vec<Self>, ops: Vec<SetOperator>) -> Self;

        fn dice(num: impl TryInto<Num>, sides: impl TryInto<Sides>) -> Self;

        fn op_dice(
            num: impl TryInto<Num>,
            sides: impl TryInto<Sides>,
            ops: Vec<DiceOperator>,
        ) -> Self;

        fn parens(x: Self) -> Self;

        fn un(op: UnaryOperator, x: Self) -> Self;

        fn bin(l: Self, op: BinaryOperator, r: Self) -> Self;
    }

    impl<'a> NodeExt<'a> for ast::Node<'a> {
        fn annot(x: Self, a: Vec<&'a str>) -> Self {
            Self::Annotated(Box::new(x), a)
        }

        fn int(x: Int) -> Self {
            Self::LiteralInt(x)
        }

        fn float(x: Float) -> Self {
            Self::LiteralFloat(x)
        }

        fn set(x: Vec<Self>) -> Self {
            Self::op_set(x, vec![])
        }

        fn op_set(x: Vec<Self>, ops: Vec<SetOperator>) -> Self {
            Self::Set(ast::Set::new(x, ops))
        }

        fn dice(num: impl TryInto<Num>, sides: impl TryInto<Sides>) -> Self {
            Self::op_dice(num, sides, vec![])
        }

        fn op_dice(
            num: impl TryInto<Num>,
            sides: impl TryInto<Sides>,
            ops: Vec<DiceOperator>,
        ) -> Self {
            let num = num.try_into().ok().unwrap();
            let sides = sides.try_into().ok().unwrap();
            Self::Dice(ast::OperatedDice::new(num, sides, ops))
        }

        fn parens(x: Self) -> Self {
            Self::Parenthetical(Box::new(x))
        }

        fn un(op: UnaryOperator, x: Self) -> Self {
            Self::Unary(op, Box::new(x))
        }

        fn bin(l: Self, op: BinaryOperator, r: Self) -> Self {
            Self::Binary(Box::new(l), op, Box::new(r))
        }
    }
}
