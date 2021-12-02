use crate::common::{DiceOperator, Int, SetOperator, Sides};
use crate::roll::num::Number;
use crate::roll::{Roll, RResult};
use crate::roll::tree::{Binary, Dice, Die, Eval, Grouping, Literal, RollTree, Set, Unary};
use super::visit::{VisitRoll, AcceptRoll};

pub trait Stringify {
    fn stringify<A: AcceptRoll>(&mut self, a: &A) -> RResult<String> {
        a.accept(self)
    }

    fn str_roll(&mut self, roll: &Roll<'_>) -> RResult<String> {
        let tree = self.stringify(&roll.tree)?;
        let total = roll.total()?;
        Ok(format!("{} = {}", tree, total))
    }

    fn str_roll_tree(&mut self, tree: &RollTree<'_>) -> RResult<String> {
        let roll = self.stringify(&tree.roll)?;
        Ok(if tree.annotations.is_empty() {
            roll
        } else {
            let annotations = tree.annotations.iter()
                .map(|s| format!("[{}]", s))
                .collect::<Vec<_>>()
                .join(" ");
            format!("{} {}", roll, annotations)
        })
    }

    fn str_literal<T: Copy + Into<Number>>(&mut self, lit: &Literal<T>) -> RResult<String> {
        let mut ret = lit.values.iter()
            .copied()
            .map(|x| x.into().to_string())
            .collect::<Vec<_>>()
            .join(" -> ");
        if lit.exploded {
            ret.push('!');
        }
        Ok(ret)
    }

    fn str_set(&mut self, set: &Set<'_>) -> RResult<String> {
        let mut ret = set.values.iter()
            .try_fold(String::from("("), |a, b| {
                self.stringify(b).map(|b| format!("{}{}, ", a, b))
            })?;

        let len = set.values.len();
        if len == 1 {
            // Keep the comma, but not the space
            ret.truncate(ret.len() - 1);
        } else if len != 0 {
            // Remove both the space and the comma
            ret.truncate(ret.len() - 2);
        }
        ret.push(')');

        let ops = self.str_set_ops(&set.ops)?;

        Ok(format!("{}{}", ret, ops))
    }

    fn str_dice(&mut self, dice: &Dice) -> RResult<String> {
        let the_dice = dice.values.iter()
            .map(|die| self.stringify(die))
            .collect::<RResult<Vec<_>>>()?
            .join(", ");
        let the_ops = self.str_dice_ops(&dice.ops)?;
        Ok(format!("{}d{}{} ({})", dice.num, dice.sides, the_ops, the_dice))
    }

    fn str_die(&mut self, die: &Die) -> RResult<String> {
        die.values.iter()
            .map(|v| self.stringify(v))
            .collect::<RResult<Vec<_>>>()
            .map(|s| s.join(", "))
    }

    fn str_grouping(&mut self, g: &Grouping<'_>) -> RResult<String> {
        let inner = self.stringify(&*g.0)?;
        Ok(format!("({})", inner))
    }

    fn str_unary(&mut self, un: &Unary<'_>) -> RResult<String> {
        let r = self.stringify(&*un.value)?;
        Ok(format!("{}{}", &un.op, r))
    }

    fn str_binary(&mut self, bin: &Binary<'_>) -> RResult<String> {
        let l = self.stringify(&*bin.left)?;
        let r = self.stringify(&*bin.right)?;
        Ok(format!("{} {} {}", l, &bin.op, r))
    }

    fn str_set_ops(&mut self, ops: &[SetOperator]) -> RResult<String> {
        Ok(ops.iter().map(ToString::to_string).collect())
    }

    fn str_dice_ops(&mut self, ops: &[DiceOperator]) -> RResult<String> {
        Ok(ops.iter().map(ToString::to_string).collect())
    }
}

#[derive(Default)]
pub struct SimpleStringifier;

impl SimpleStringifier {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn stringify<A: AcceptRoll>(&mut self, roll: &A) -> RResult<String> {
        Stringify::stringify(self, roll)
    }
}

impl Stringify for SimpleStringifier {}

#[derive(Default)]
pub struct MarkdownStringifier {
    in_dropped: bool,
}

impl MarkdownStringifier {
    pub fn new() -> Self {
        Self::default()
    }

    fn reset(&mut self) {
        self.in_dropped = false;
    }

    pub fn stringify<A: AcceptRoll>(&mut self, roll: &A) -> RResult<String> {
        self.reset();
        Stringify::stringify(self, roll)
    }

    fn fmt_die_value(&mut self, v: &Literal<Int>, sides: Sides) -> RResult<String> {
        let number = v.number()?;
        let bold_last = number == Number::Int(1) || matches!(sides, Sides::Poly(n) if number == Number::Int(n.get() as Int));
        self.fmt_literal(v, bold_last)
    }

    fn fmt_literal<T: Copy + Into<Number>>(&mut self, lit: &Literal<T>, bold_last: bool) -> RResult<String> {
        let mut ret: String = lit.values[..lit.values.len() - 1].iter()
            .copied()
            .map(|x| format!("{} -> ", x.into()))
            .collect();

        let last = *lit.values.last();
        let last = last.into();
        if bold_last {
            ret = format!("{}**{}", ret, last);
            if lit.exploded {
                ret.push('!');
            }
            ret.push_str("**");
        } else {
            ret = format!("{}{}", ret, last);
            if lit.exploded {
                ret.push('!');
            }
        }

        if !lit.kept() && !self.in_dropped {
            ret = format!("~~{}~~", ret);
        }

        Ok(ret)
    }
}

impl Stringify for MarkdownStringifier {
    fn stringify<A: AcceptRoll>(&mut self, a: &A) -> RResult<String> {
        if !a.kept() && !self.in_dropped {
            self.in_dropped = true;
            let inside = a.accept(self)?;
            self.in_dropped = false;
            Ok(format!("~~{}~~", inside))
        } else {
            a.accept(self)
        }
    }

    fn str_roll(&mut self, roll: &Roll<'_>) -> RResult<String> {
        let tree = Stringify::stringify(self, &roll.tree)?;
        let total = roll.total()?;
        Ok(format!("{} = `{}`", tree, total))
    }

    fn str_literal<T: Copy + Into<Number>>(&mut self, lit: &Literal<T>) -> RResult<String> {
        self.fmt_literal(lit, false)
    }

    fn str_die(&mut self, die: &Die) -> RResult<String> {
        let sides = die.sides;
        die.values.iter()
            .map(|v| self.fmt_die_value(v, sides))
            .collect::<RResult<Vec<_>>>()
            .map(|s| s.join(", "))
    }
}

impl<S: ?Sized> VisitRoll for S
where
    S: Stringify,
{
    type Output = RResult<String>;

    fn visit_roll(&mut self, x: &Roll<'_>) -> Self::Output {
        self.str_roll(x)
    }

    fn visit_roll_tree(&mut self, x: &RollTree<'_>) -> Self::Output {
        self.str_roll_tree(x)
    }

    fn visit_literal<T: Copy + Into<Number>>(&mut self, x: &Literal<T>) -> Self::Output {
        self.str_literal(x)
    }

    fn visit_set(&mut self, x: &Set<'_>) -> Self::Output {
        self.str_set(x)
    }

    fn visit_dice(&mut self, x: &Dice) -> Self::Output {
        self.str_dice(x)
    }

    fn visit_die(&mut self, x: &Die) -> Self::Output {
        self.str_die(x)
    }

    fn visit_grouping(&mut self, x: &Grouping<'_>) -> Self::Output {
        self.str_grouping(x)
    }

    fn visit_unary(&mut self, op: &Unary<'_>) -> Self::Output {
        self.str_unary(op)
    }

    fn visit_binary(&mut self, op: &Binary<'_>) -> Self::Output {
        self.str_binary(op)
    }
}

#[cfg(test)]
mod tests {
    use crate::common::NonZeroUInt;
    use super::*;

    macro_rules! check {
        ($cls:ident, $input:expr, $expected:expr) => {
            let mut str = $cls::default();
            let ast = crate::parse($input).unwrap();
            let roller = crate::roll::roller::StepRoller::new(NonZeroUInt::new(10).unwrap(), 1);
            let roll = crate::eval(ast, roller, Some(1000))
                .unwrap();
            let actual = $cls::stringify(&mut str, &roll).unwrap();
            assert_eq!(&actual, $expected);
        };
    }

    #[test]
    fn test_simple_stringify() {
        check!(SimpleStringifier, "2 + 3", "2 + 3 = 5");
        check!(SimpleStringifier, "2d20", "2d20 (10, 11) = 21");
        check!(SimpleStringifier, "2d20kh1", "2d20kh1 (10, 11) = 11");
        check!(SimpleStringifier, "4d4rr1 + 2d6e3 + 3", "4d4rr1 (2, 3, 4, 1, 2) + 2d6e3 (3!, 4, 5) + 3 = 26");
        check!(SimpleStringifier, "8d6mi2mi3 + 5", "8d6mi2mi3 (4, 5, 6, 1 -> 2 -> 3, 2 -> 3, 3, 4, 5) + 5 = 38");
    }

    #[test]
    fn test_markdown_stringify() {
        check!(MarkdownStringifier, "2 + 3", "2 + 3 = `5`");
        check!(MarkdownStringifier, "2d20", "2d20 (10, 11) = `21`");
        check!(MarkdownStringifier, "2d20kh1", "2d20kh1 (~~10~~, 11) = `11`");
        check!(MarkdownStringifier, "4d4rr1 + 2d6e3 + 3", "4d4rr1 (2, 3, **4**, ~~**1**~~, 2) + 2d6e3 (3!, 4, 5) + 3 = `26`");
        check!(MarkdownStringifier, "8d6mi2mi3 + 5", "8d6mi2mi3 (4, 5, **6**, 1 -> 2 -> 3, 2 -> 3, 3, 4, 5) + 5 = `38`");
        check!(MarkdownStringifier, "1d12mi12", "1d12mi12 (10 -> **12**) = `12`");
        check!(MarkdownStringifier, "(2d4rr<3, 1d20e20, 3d3rah1)kh1", "(~~2d4rr<3 (2, **4**, 3)~~, 1d20e20 (13), ~~3d3rah1 (2, **3!**, **1**, 2)~~)kh1 = `13`");
    }
}