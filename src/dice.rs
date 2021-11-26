use crate::error::RollError;
use crate::expr::{
    BinOp, Dice, Expression, KeptSet, Literal, Number, NumberKind, NumberTrait, Parenthetical, Set,
    SetOperator, UnOp,
};
use crate::parse::ast::{self, DiceSize, Int};
use crate::parse::ParseError;
use crate::stringifiers::Stringify;
use rand::distributions::{Distribution, Uniform};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum CritType {
    Crit,
    Fail,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum AdvType {
    Adv,
    Dis,
}

pub struct RollContext {
    max_rolls: usize,
    rolls: usize,
}

impl RollContext {
    pub fn new(max_rolls: usize) -> Self {
        Self {
            max_rolls,
            rolls: 0,
        }
    }

    pub fn reset(&mut self) {
        self.rolls = 0;
    }

    pub fn count_roll(&mut self) -> crate::Result<()> {
        self.count_rolls(1)
    }

    pub fn count_rolls(&mut self, n: usize) -> crate::Result<()> {
        self.rolls += n;
        if self.rolls > self.max_rolls {
            Err(RollError::TooManyRolls)
        } else {
            Ok(())
        }
    }
}

impl Default for RollContext {
    fn default() -> Self {
        Self::new(1000)
    }
}

#[derive(Clone)]
pub struct RollResult<'a> {
    pub ast: ast::Node<'a>,
    pub expr: Expression,
}

impl<'a> RollResult<'a> {
    pub fn new(ast: ast::Expression<'a>, roll: Expression) -> Self {
        Self {
            ast: ast.roll,
            expr: roll,
        }
    }

    pub fn total(&self) -> Result<Int, RollError> {
        self.expr.total().map(|x| x.as_int())
    }

    pub fn result<S: Stringify>(&self) -> Result<String, RollError> {
        S::default().stringify(&self.expr)
    }

    pub fn crit(&self) -> Option<CritType> {
        let mut left = &self.expr.roll;
        let left = loop {
            let ch = left.children();
            if ch.is_empty() {
                break left;
            } else {
                left = ch[0];
            }
        };

        let dice = match &left.kind {
            NumberKind::Dice(dice) => dice,
            _ => return None,
        };

        if dice.size != DiceSize::Int(20) || dice.kept_set().len() != 1 {
            return None;
        }

        match dice.total() {
            Ok(x) if x.as_int() == 1 => Some(CritType::Fail),
            Ok(x) if x.as_int() == 20 => Some(CritType::Crit),
            _ => None,
        }
    }
}

pub struct Roller<R: rand::Rng = crate::DefaultRng> {
    context: RollContext,
    rng: R,
}

pub trait RollInput<'a> {
    fn try_into_ast(self) -> std::result::Result<ast::Expression<'a>, ParseError>;
}

impl<'a> RollInput<'a> for &'a str {
    fn try_into_ast(self) -> Result<ast::Expression<'a>, ParseError> {
        crate::parse::parse(self)
    }
}

impl<'a> RollInput<'a> for ast::Expression<'a> {
    fn try_into_ast(self) -> Result<ast::Expression<'a>, ParseError> {
        Ok(self)
    }
}

impl<R: rand::Rng> Roller<R> {
    pub fn new(context: RollContext, rng: R) -> Self {
        Self { context, rng }
    }

    pub fn roll<'a>(&mut self, expr: impl RollInput<'a>) -> crate::Result<RollResult<'a>> {
        self.context.reset();
        let dice_tree = expr.try_into_ast()?;
        let dice_expr = self.eval(dice_tree.clone())?;
        Ok(RollResult::new(dice_tree, dice_expr))
    }

    pub(crate) fn roll_one(&mut self, size: DiceSize) -> crate::Result<Int> {
        self.context.count_roll()?;
        Ok(match size {
            DiceSize::Percentile => 10 * (&mut self.rng).gen_range(1..=10),
            DiceSize::Int(x) => (&mut self.rng).gen_range(1..=x),
        })
    }

    pub(crate) fn roll_n(
        &mut self,
        n: usize,
        size: DiceSize,
    ) -> crate::Result<impl Iterator<Item = Int> + '_> {
        self.context.count_rolls(n)?;
        let sides = match size {
            DiceSize::Percentile => 10,
            DiceSize::Int(x) => x,
        };
        Ok(Uniform::new_inclusive(1, sides)
            .sample_iter(&mut self.rng)
            .take(n)
            .map(move |x| match size {
                DiceSize::Percentile => 10 * x,
                _ => x,
            }))
    }

    fn eval(&mut self, node: ast::Expression) -> crate::Result<Expression> {
        let roll = node.roll.accept(self)?;
        Ok(Expression::new(roll))
    }

    fn eval_annotated(&mut self, value: &ast::Node, annotations: &[&str]) -> crate::Result<Number> {
        let mut ret = value.accept(self)?;
        let annotation = annotations
            .iter()
            .map(|s| format!("[{}]", s))
            .collect::<Vec<_>>()
            .join(" ");
        ret.annotation = Some(annotation);
        Ok(ret)
    }

    fn eval_literal(&mut self, value: &ast::Literal) -> crate::Result<Number> {
        let val = match value {
            ast::Literal::Int(x) => (*x).into(),
            ast::Literal::Float(x) => (*x).into(),
        };
        Ok(Number::new(NumberKind::Literal(Literal::new(val))))
    }

    fn eval_unary(&mut self, op: ast::UnaryOperator, right: &ast::Node) -> crate::Result<Number> {
        let value = right.accept(self)?;
        Ok(Number::new(NumberKind::UnOp(UnOp::new(op, value))))
    }

    fn eval_binary(
        &mut self,
        op: ast::BinaryOperator,
        left: &ast::Node,
        right: &ast::Node,
    ) -> crate::Result<Number> {
        let left = left.accept(self)?;
        let right = right.accept(self)?;
        Ok(Number::new(NumberKind::BinOp(BinOp::new(op, left, right))))
    }

    fn eval_parenthetical(&mut self, value: &ast::Node) -> crate::Result<Number> {
        let value = value.accept(self)?;
        Ok(Number::new(NumberKind::Parenthetical(Parenthetical::new(
            value,
            vec![],
        ))))
    }

    fn eval_operated_set(&mut self, set: &ast::OperatedSet) -> crate::Result<Number> {
        match &set.set {
            ast::Set::NumberSet(values) => {
                let mut target = self.eval_number_set(values)?;
                for op in &set.ops {
                    let the_op = SetOperator::from_ast(op);
                    the_op.operate_on_set(&mut target)?;
                    target.operations.push(the_op);
                }
                Ok(Number::new(NumberKind::Set(target)))
            }
            ast::Set::Dice(dice) => {
                let mut target = self.eval_dice(dice)?;
                for op in &set.ops {
                    let the_op = SetOperator::from_ast(op);
                    the_op.operate_on_dice(&mut target, self)?;
                    target.operations.push(the_op);
                }
                Ok(Number::new(NumberKind::Dice(target)))
            }
        }
    }

    fn eval_number_set(&mut self, values: &[ast::Node]) -> crate::Result<Set> {
        Ok(Set::new(
            values
                .iter()
                .map(|n| n.accept(self))
                .collect::<crate::Result<_>>()?,
        ))
    }

    fn eval_dice(&mut self, dice: &ast::Dice) -> crate::Result<Dice> {
        Dice::roll_new(dice.num, dice.size, self)
    }
}

pub(crate) trait VisitEval {
    fn accept<R: rand::Rng>(&self, v: &mut Roller<R>) -> crate::Result<Number>;
}

impl VisitEval for ast::Node<'_> {
    fn accept<R: rand::Rng>(&self, v: &mut Roller<R>) -> crate::Result<Number> {
        match self {
            Self::Annotated(x, a) => v.eval_annotated(&*x, a),
            Self::Literal(x) => v.eval_literal(x),
            Self::Unary(op, x) => v.eval_unary(*op, &*x),
            Self::Binary(op, l, r) => v.eval_binary(*op, &*l, &*r),
            Self::Parenthetical(x) => v.eval_parenthetical(&*x),
            Self::Set(x) => v.eval_operated_set(x),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;

    macro_rules! roll {
        ($(($n:literal, $size:expr $(, $keep:literal)?)),+ $(,)?) => {
            {
                let mut roller = roller();
                [$(roll!(@fmt $n, $size $(, $keep)?)),+].into_iter()
                    .map(|(n, size, keep)| {
                        let total = roller.roll_n(n, size.into()).unwrap().sum::<Int>();
                        if keep { total } else { 0 }
                    })
                    .sum()
            }
        };
        (@fmt $n:literal, $size:expr, $keep:literal) => {
            ($n, $size, $keep)
        };
        (@fmt $n:literal, $size:expr) => {
            ($n, $size, true)
        };
    }

    fn check(s: &str, expected: Int) {
        let mut roller = roller();
        let actual = roller.roll(s).unwrap().total().unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_eval_number() {
        check("2", 2);
        check("2.0", 2);
        check("1.5 + 2.5", 4);
        check("1.2 * (3.5 + 1.5)", 6);
    }

    #[test]
    fn test_eval_dice() {
        check("2d4", roll!((2, 4)));
        check("1d20 + 1d6", roll!((1, 20), (1, 6)));
        check("2d20kh1", {
            let mut roller = roller();
            let mut rolls = roller.roll_n(2, DiceSize::Int(20)).unwrap();
            let (first, second) = (rolls.next().unwrap(), rolls.next().unwrap());
            if first >= second {
                first
            } else {
                second
            }
        })
    }
}
