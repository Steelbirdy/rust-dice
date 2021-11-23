use std::convert::Infallible;
use std::fmt;
use std::fmt::Write;
use std::str::FromStr;

pub type Int = u64;
pub type Float = f64;

#[derive(Debug, Clone, PartialEq)]
pub struct Expression<'a> {
    pub roll: Node<'a>,
    pub comment: Option<&'a str>,
}

impl<'a> Expression<'a> {
    pub(crate) fn new(roll: Node<'a>) -> Self {
        Self {
            roll,
            comment: None,
        }
    }

    pub(crate) fn new_commented(roll: Node<'a>, comment: &'a str) -> Self {
        Self {
            roll,
            comment: Some(comment),
        }
    }
}

impl fmt::Display for Expression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.comment {
            Some(comment) => write!(f, "{} {}", &self.roll, comment),
            None => write!(f, "{}", &self.roll),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node<'a> {
    Annotated(Box<Node<'a>>, Vec<&'a str>),
    Literal(Literal),
    Parenthetical(Box<Node<'a>>),
    Unary(UnaryOperator, Box<Node<'a>>),
    Binary(BinaryOperator, Box<Node<'a>>, Box<Node<'a>>),
    Set(OperatedSet<'a>),
}

impl<'a> Node<'a> {
    pub(crate) fn annotated(
        expr: Node<'a>,
        annotations: impl IntoIterator<Item = &'a str>,
    ) -> Self {
        Self::Annotated(Box::new(expr), annotations.into_iter().collect())
    }

    pub(crate) fn literal(x: impl Into<Literal>) -> Self {
        Self::Literal(x.into())
    }

    pub(crate) fn parenthetical(expr: Node<'a>) -> Self {
        Self::Parenthetical(Box::new(expr))
    }

    pub(crate) fn unary(op: UnaryOperator, right: Node<'a>) -> Self {
        Self::Unary(op, Box::new(right))
    }

    pub(crate) fn binary(op: BinaryOperator, left: Node<'a>, right: Node<'a>) -> Self {
        Self::Binary(op, Box::new(left), Box::new(right))
    }

    pub(crate) fn set(set: impl Into<Set<'a>>, ops: impl IntoIterator<Item = SetOperator>) -> Self {
        Self::Set(OperatedSet::new(set, ops))
    }
}

impl fmt::Display for Node<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Annotated(value, annotations) => {
                fmt::Display::fmt(&*value, f)?;

                if !annotations.is_empty() {
                    f.write_char(' ')?;
                    for annot in annotations {
                        write!(f, "[{}]", annot)?;
                    }
                }

                Ok(())
            }
            Self::Literal(x) => fmt::Display::fmt(x, f),
            Self::Parenthetical(x) => write!(f, "({})", &*x),
            Self::Unary(op, right) => write!(f, "{}{}", op, &*right),
            Self::Binary(op, left, right) => write!(f, "{} {} {}", &*left, op, &*right),
            Self::Set(set) => fmt::Display::fmt(set, f),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Literal {
    Int(Int),
    Float(Float),
}

impl From<Int> for Literal {
    fn from(x: Int) -> Self {
        Literal::Int(x)
    }
}

impl From<Float> for Literal {
    fn from(x: Float) -> Self {
        Literal::Float(x)
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(x) => fmt::Display::fmt(x, f),
            Self::Float(x) => fmt::Display::fmt(x, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct OperatedSet<'a> {
    pub set: Set<'a>,
    pub ops: Vec<SetOperator>,
}

impl<'a> OperatedSet<'a> {
    pub(crate) fn new(set: impl Into<Set<'a>>, ops: impl IntoIterator<Item = SetOperator>) -> Self {
        let mut ret = Self {
            set: set.into(),
            ops: ops.into_iter().collect(),
        };
        ret.simplify_operations();
        ret
    }

    fn simplify_operations(&mut self) {
        let mut new_ops = Vec::with_capacity(self.ops.len());

        for op in self.ops.drain(..) {
            if new_ops.is_empty() || SetOperatorKind::IMMEDIATE.contains(&op.kind) {
                new_ops.push(op);
            } else {
                let last_op = new_ops.last_mut().expect("new_ops is not empty");
                if op.kind == last_op.kind {
                    last_op.add_sels(op.sels);
                } else {
                    new_ops.push(op);
                }
            }
        }

        new_ops.shrink_to_fit();
        self.ops = new_ops;
    }
}

impl fmt::Display for OperatedSet<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.set, f)?;
        for op in &self.ops {
            fmt::Display::fmt(op, f)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Set<'a> {
    NumberSet(Vec<Node<'a>>),
    Dice(Dice),
}

impl From<Dice> for Set<'_> {
    fn from(dice: Dice) -> Self {
        Set::Dice(dice)
    }
}

impl fmt::Display for Set<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NumberSet(items) => {
                f.write_char('(')?;
                for item in &items[..items.len() - 1] {
                    write!(f, "{}, ", item)?;
                }
                if items.len() == 1 {
                    write!(f, "{},)", &items[0])
                } else {
                    write!(f, "{})", &items[items.len() - 1])
                }
            }
            Self::Dice(x) => fmt::Display::fmt(x, f),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Dice {
    pub num: Int,
    pub size: DiceSize,
}

impl Dice {
    pub(crate) fn new(num: Int, size: impl Into<DiceSize>) -> Self {
        Self {
            num,
            size: size.into(),
        }
    }
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

        Ok(Dice::new(num, size))
    }
}

impl fmt::Display for Dice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}d{}", &self.num, &self.size)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum DiceSize {
    Int(Int),
    Percentile,
}

impl fmt::Display for DiceSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(x) => fmt::Display::fmt(x, f),
            Self::Percentile => f.write_char('%'),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinaryOperator {
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

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::FloorDiv => "//",
            Self::Mod => "%",
            Self::Lt => "<",
            Self::Gt => ">",
            Self::Leq => "<=",
            Self::Geq => ">=",
            Self::Eq => "==",
            Self::Neq => "!=",
        };
        f.write_str(s)
    }
}

impl FromStr for BinaryOperator {
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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnaryOperator {
    Plus,
    Minus,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Plus => "+",
            Self::Minus => "-",
        };
        f.write_str(s)
    }
}

impl FromStr for UnaryOperator {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "+" => Self::Plus,
            "-" => Self::Minus,
            _ => unreachable!(),
        })
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SetOperator {
    pub kind: SetOperatorKind,
    pub sels: Vec<SetSelector>,
}

impl SetOperator {
    pub(crate) fn new(kind: SetOperatorKind, sels: impl IntoIterator<Item = SetSelector>) -> Self {
        Self {
            kind,
            sels: sels.into_iter().collect(),
        }
    }

    pub fn add_sels(&mut self, sels: impl IntoIterator<Item = SetSelector>) {
        self.sels.extend(sels);
    }
}

impl fmt::Display for SetOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.kind.to_str())?;
        for sel in &self.sels {
            fmt::Display::fmt(sel, f)?;
        }

        Ok(())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SetOperatorKind {
    Keep,
    Drop,
    Reroll,
    RerollOnce,
    Explode,
    ExplodeOnce,
    Minimum,
    Maximum,
}

impl SetOperatorKind {
    pub(crate) const IMMEDIATE: &'static [SetOperatorKind] = &[Self::Minimum, Self::Maximum];

    pub(crate) fn to_str(self) -> &'static str {
        match self {
            Self::Keep => "k",
            Self::Drop => "p",
            Self::Reroll => "rr",
            Self::RerollOnce => "ro",
            Self::Explode => "e",
            Self::ExplodeOnce => "ra",
            Self::Minimum => "mi",
            Self::Maximum => "ma",
        }
    }
}

impl fmt::Display for SetOperatorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.to_str())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SetSelector {
    Lowest(Int),
    Highest(Int),
    LessThan(Int),
    GreaterThan(Int),
    EqualTo(Int),
}

impl fmt::Display for SetSelector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lowest(x) => write!(f, "l{}", x),
            Self::Highest(x) => write!(f, "h{}", x),
            Self::LessThan(x) => write!(f, "<{}", x),
            Self::GreaterThan(x) => write!(f, ">{}", x),
            Self::EqualTo(x) => write!(f, "{}", x),
        }
    }
}
