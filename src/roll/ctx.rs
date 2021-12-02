use super::{error::RollError, num::Number, roller::Roller, tree::*, RResult};
use crate::common::*;
use crate::parse::{
    ast,
    visit::{self, Accept},
};

pub type DefaultRoller = rand::prelude::ThreadRng;

pub struct RollContext<R = DefaultRoller> {
    max_rolls: Option<usize>,
    rolls: usize,
    roller: R,
}

impl<R: Roller> RollContext<R> {
    pub fn new(max_rolls: Option<usize>, roller: R) -> Self {
        Self {
            max_rolls,
            rolls: 0,
            roller,
        }
    }

    pub fn new_bounded(max_rolls: usize, roller: R) -> Self {
        Self::new(Some(max_rolls), roller)
    }

    pub fn new_unbounded(roller: R) -> Self {
        Self::new(None, roller)
    }

    fn count_rolls(&mut self, n: usize) -> RResult<()> {
        self.rolls += n;
        if self.max_rolls.map_or(false, |max| self.rolls > max) {
            Err(RollError::TooManyRolls)
        } else {
            Ok(())
        }
    }

    pub fn roll(
        &mut self,
        num: usize,
        sides: Sides,
    ) -> RResult<impl Iterator<Item = NonZeroUInt> + '_> {
        self.count_rolls(num)?;

        let (sides, multiplier) = match sides {
            Sides::Poly(x) => (x, 1),
            Sides::Percentile => (NonZeroUInt::new(10).unwrap(), 10),
        };

        Ok((&mut self.roller)
            .roll_iter(num, sides)
            .map(move |x| NonZeroUInt::new(x * multiplier).unwrap()))
    }

    pub fn roll_one(&mut self, sides: Sides) -> RResult<NonZeroUInt> {
        self.count_rolls(1)?;
        let value = match sides {
            Sides::Poly(x) => (&mut self.roller).roll(x),
            Sides::Percentile => (&mut self.roller).roll(NonZeroUInt::new(10).unwrap()) * 10,
        };
        Ok(NonZeroUInt::new(value).unwrap())
    }

    pub fn eval<'a>(&mut self, expr: ast::Expression<'a>) -> RResult<Roll<'a>> {
        let tree = expr.roll.accept(self)?;
        let comment = expr.comment;
        Ok(Roll::new(tree, comment))
    }
}

impl Default for RollContext {
    fn default() -> Self {
        Self::new(Some(1000), rand::thread_rng())
    }
}

impl<'a, R: Roller> visit::AstVisitor<'a> for RollContext<R> {
    type Output = RResult<RollTree<'a>>;

    fn visit_annotated(&mut self, value: &ast::Node<'a>, annotations: &[&'a str]) -> Self::Output {
        let mut ret = value.accept(self)?;
        ret.annotations = annotations.to_vec();
        Ok(ret)
    }

    fn visit_int(&mut self, x: &Int) -> Self::Output {
        let val: Number = (*x).into();
        Ok(RollTree::new(RollNode::Literal(val.into())))
    }

    fn visit_float(&mut self, x: &Float) -> Self::Output {
        let val: Number = (*x).into();
        Ok(RollTree::new(RollNode::Literal(val.into())))
    }

    fn visit_set(&mut self, set: &ast::Set<'a>) -> Self::Output {
        let values: Vec<_> = set
            .values
            .iter()
            .map(|node| node.accept(self))
            .collect::<RResult<_>>()?;

        let mut ret = Set::new(values, set.ops.clone());
        for op in &set.ops {
            op.operate(&mut ret)?;
        }
        Ok(RollTree::new(RollNode::Set(ret)))
    }

    fn visit_dice(&mut self, dice: &ast::OperatedDice) -> Self::Output {
        let mut ret = Dice::roll_new(self, dice.num, dice.sides, dice.ops.clone())?;
        for op in &dice.ops {
            op.operate(self, &mut ret)?;
        }
        Ok(RollTree::new(RollNode::Dice(ret)))
    }

    fn visit_parenthetical(&mut self, p: &ast::Node<'a>) -> Self::Output {
        let value = p.accept(self)?;
        Ok(RollTree::new(RollNode::Grouping(Grouping::new(value))))
    }

    fn visit_unary(&mut self, op: &UnaryOperator, r: &ast::Node<'a>) -> Self::Output {
        let value = r.accept(self)?;
        Ok(RollTree::new(RollNode::Unary(Unary::new(*op, value))))
    }

    fn visit_binary(
        &mut self,
        l: &ast::Node<'a>,
        op: &BinaryOperator,
        r: &ast::Node<'a>,
    ) -> Self::Output {
        let left = l.accept(self)?;
        let right = r.accept(self)?;
        Ok(RollTree::new(RollNode::Binary(Binary::new(
            left, *op, right,
        ))))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::roll::roller::StepRoller;

    fn mock_roller() -> StepRoller {
        StepRoller::new(NonZeroUInt::new(10).unwrap(), 1)
    }

    fn check(s: &str, expected: impl Into<Number>) {
        let mut ctx = RollContext::new_bounded(1000, mock_roller());
        let ast = crate::parse::parse(s).unwrap();
        let actual = ctx.eval(ast).unwrap();
        assert_eq!(expected.into(), actual.total().unwrap());
    }

    fn check_err(s: &str, expected: RollError) {
        let mut ctx = RollContext::new_bounded(1000, mock_roller());
        let ast = crate::parse::parse(s).unwrap();
        let actual = ctx.eval(ast);
        assert_eq!(expected, actual.unwrap_err());
    }

    #[test]
    fn test_eval_number() {
        check("2", 2);
        check("2.0", 2.0);
    }

    #[test]
    fn test_eval_unary() {
        check("-2", -2);
        check("--2", 2);
        check("---2", -2);
    }

    #[test]
    fn test_eval_binary() {
        check("2 + 3", 5);
        check("3.5 % 2", 1.5);
        check("2 * (1 - 3)", -4);
    }

    #[test]
    fn test_eval_dice() {
        check("1d20 + 4", 10 + 4);
        check("2d4", 2 + 3);
        check("8d6", 4 + 5 + 6 + 1 + 2 + 3 + 4 + 5);
    }

    #[test]
    fn test_eval_set() {
        check("(1, 2, 3)", 6);
        check("(2d4, 1d8)", 2 + 3 + 4);
        check("(2d4kl1, 1d8rr<5)ph1", 2)
    }

    #[test]
    fn test_eval_op_dice() {
        check("2d20kh1", 11);
        check("8d6rr1", 4 + 5 + 6 + 2 + 3 + 4 + 5 + 6);
        check("8d6e6", 4 + 5 + 6 + 1 + 2 + 3 + 4 + 5 + 6 + 1);
        check("8d6mi3", 4 + 5 + 6 + 3 + 3 + 3 + 4 + 5);
        check("8d6ph1pl1", 4 + 5 + 2 + 3 + 4 + 5);
        check("1d6 + 2 * 8d6ro>3", 4 + 2 * (1 + 2 + 3 + 1 + 2 + 3 + 4 + 5));
    }

    #[test]
    fn test_err_too_many_rolls() {
        check_err("2d20rrh1", RollError::TooManyRolls);
    }
}
