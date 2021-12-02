use crate::common::*;
use std::fmt;

#[derive(Debug, Copy, Clone)]
pub enum Number {
    Int(Int),
    Float(Float),
}

impl Number {
    pub(crate) const ZERO: Self = Self::Int(0);

    pub fn as_int(self) -> Int {
        match self {
            Self::Int(x) => x,
            Self::Float(x) => x as Int,
        }
    }

    pub fn as_float(self) -> Float {
        match self {
            Self::Int(x) => x as Float,
            Self::Float(x) => x,
        }
    }

    pub(crate) fn floor(self) -> Self {
        match self {
            Self::Int(_) => self,
            Self::Float(x) => Self::Int(x as Int),
        }
    }
}

impl std::ops::Neg for Number {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::Float(-self.as_float())
    }
}

macro_rules! val_impl_bin_op {
    ($Name:ident, $fn_name:ident) => {
        impl std::ops::$Name for Number {
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

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.as_float().eq(&other.as_float())
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_float().partial_cmp(&other.as_float())
    }
}

impl From<Int> for Number {
    fn from(x: Int) -> Self {
        Self::Int(x)
    }
}

impl From<Float> for Number {
    fn from(x: Float) -> Self {
        Self::Float(x)
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(x) => fmt::Display::fmt(x, f),
            Self::Float(x) => fmt::Debug::fmt(x, f),
        }
    }
}
