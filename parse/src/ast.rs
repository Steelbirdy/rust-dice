use std::convert::Infallible;
use std::str::FromStr;

pub(crate) type UInt = u64;
pub(crate) type Int = i64;
pub(crate) type Dec = f64;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Expression<'a> {
    pub expr: Expr<'a>,
    pub comment: Option<&'a str>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expr<'a> {
    Binary(BinaryOp, Box<Expr<'a>>, Box<Expr<'a>>),
    Unary(UnaryOp, Box<Expr<'a>>),
    Grouping(Box<Expr<'a>>),
    Annotated(Box<Expr<'a>>, Vec<&'a str>),
    Set(Set<'a>),
    Integer(Int),
    Decimal(Dec),
    Dice(Dice),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Dice {
    pub(crate) num: UInt,
    pub(crate) size: DiceSize,
    pub(crate) ops: Vec<DiceOp>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Set<'a> {
    pub(crate) items: Vec<Expr<'a>>,
    pub(crate) ops: Vec<SetOp>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum DiceSize {
    Int(UInt),
    Percentile,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    FloorDiv,
    Mod,
    Lt,
    Gt,
    Leq,
    Geq,
    Eq,
    Neq,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum UnaryOp {
    Plus,
    Minus,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum SetOp {
    Keep(SetSelector),
    Drop(SetSelector),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum DiceOp {
    Keep(SetSelector),
    Drop(SetSelector),
    Reroll(SetSelector),
    RerollOnce(SetSelector),
    Explode(SetSelector),
    ExplodeOnce(SetSelector),
    Minimum(Int),
    Maximum(Int),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum SetSelector {
    Lowest(Int),
    Highest(Int),
    LessThan(Int),
    GreaterThan(Int),
    EqualTo(Int),
}

impl FromStr for Dice {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (num, size) = s.split_once('d').unwrap();
        let num = if num.is_empty() {
            1
        } else {
            num.parse().unwrap()
        };
        let size = if size == "%" {
            DiceSize::Percentile
        } else {
            DiceSize::Int(size.parse().unwrap())
        };

        Ok(Dice {
            num,
            size,
            ops: Vec::new(),
        })
    }
}

impl FromStr for BinaryOp {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "+" => Self::Add,
            "-" => Self::Sub,
            "*" => Self::Mul,
            "/" => Self::Div,
            "//" => Self::FloorDiv,
            "%" => Self::Mod,
            "<" => Self::Lt,
            ">" => Self::Gt,
            "<=" => Self::Leq,
            ">=" => Self::Geq,
            "==" => Self::Eq,
            "!=" => Self::Neq,
            _ => unreachable!(),
        })
    }
}

impl FromStr for UnaryOp {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "+" => Self::Plus,
            "-" => Self::Minus,
            _ => unreachable!(),
        })
    }
}
