use crate::expr::*;
use crate::parse::ast::{DiceSize, Int};

pub trait Stringify: Sized + Default {
    fn stringify(&mut self, the_roll: &impl VisitStringify) -> crate::Result<String> {
        self._stringify(the_roll)
    }

    fn _stringify(&mut self, node: &impl VisitStringify) -> crate::Result<String> {
        node.accept(self)
    }

    fn str_number(&mut self, node: &Number) -> crate::Result<String> {
        let inside = self._stringify(&node.kind)?;
        Ok(if let Some(s) = node.annotation.as_ref() {
            format!("{} {}", inside, s)
        } else {
            inside
        })
    }

    fn str_expression(&mut self, node: &Expression) -> crate::Result<String> {
        let roll = self._stringify(&node.roll)?;
        Ok(format!("{} = {}", roll, node.total()?.as_int()))
    }

    fn str_literal(&mut self, node: &Literal<impl Copy + Into<Val>>) -> crate::Result<String> {
        let history = node
            .values
            .iter()
            .map(|x| (*x).into().to_string())
            .reduce(|a, b| format!("{} -> {}", a, b))
            .unwrap();
        Ok(if node.exploded {
            format!("{}!", history)
        } else {
            history
        })
    }

    fn str_unop(&mut self, node: &UnOp) -> crate::Result<String> {
        Ok(format!("{}{}", node.op, self._stringify(&*node.value)?))
    }

    fn str_binop(&mut self, node: &BinOp) -> crate::Result<String> {
        Ok(format!(
            "{} {} {}",
            self._stringify(&*node.left)?,
            node.op,
            self._stringify(&*node.right)?
        ))
    }

    fn str_parenthetical(&mut self, node: &Parenthetical) -> crate::Result<String> {
        Ok(format!(
            "({}{})",
            self._stringify(&*node.value)?,
            self.str_ops(&node.operations)?
        ))
    }

    fn str_set(&mut self, node: &Set) -> crate::Result<String> {
        let out = node
            .values
            .iter()
            .map(|v| self._stringify(v))
            .collect::<crate::Result<Vec<_>>>()?
            .join(", ");
        let ops = self.str_ops(&node.operations)?;
        Ok(if node.values.len() == 1 {
            format!("({},){}", out, ops)
        } else {
            format!("({}){}", out, ops)
        })
    }

    fn str_dice(&mut self, node: &Dice) -> crate::Result<String> {
        let the_dice = node
            .values
            .iter()
            .map(|die| self._stringify(die))
            .collect::<crate::Result<Vec<_>>>()?
            .join(", ");
        Ok(format!(
            "{}d{}{} ({})",
            node.num,
            node.size,
            self.str_ops(&node.operations)?,
            the_dice
        ))
    }

    fn str_die(&mut self, node: &Die) -> crate::Result<String> {
        node.values
            .iter()
            .map(|val| self._stringify(val))
            .collect::<crate::Result<Vec<_>>>()
            .map(|v| v.join(", "))
    }

    fn str_ops(&mut self, ops: &[SetOperator]) -> crate::Result<String> {
        Ok(ops.iter().map(ToString::to_string).collect())
    }
}

#[enum_dispatch::enum_dispatch]
pub trait VisitStringify: NumberTrait {
    fn accept(&self, v: &mut impl Stringify) -> crate::Result<String>;
}

impl VisitStringify for Expression {
    fn accept(&self, v: &mut impl Stringify) -> crate::Result<String> {
        v.str_expression(self)
    }
}

impl VisitStringify for Number {
    fn accept(&self, v: &mut impl Stringify) -> crate::Result<String> {
        v.str_number(self)
    }
}

impl<T: Copy + Into<Val>> VisitStringify for Literal<T> {
    fn accept(&self, v: &mut impl Stringify) -> crate::Result<String> {
        v.str_literal(self)
    }
}

impl VisitStringify for UnOp {
    fn accept(&self, v: &mut impl Stringify) -> crate::Result<String> {
        v.str_unop(self)
    }
}

impl VisitStringify for BinOp {
    fn accept(&self, v: &mut impl Stringify) -> crate::Result<String> {
        v.str_binop(self)
    }
}

impl VisitStringify for Parenthetical {
    fn accept(&self, v: &mut impl Stringify) -> crate::Result<String> {
        v.str_parenthetical(self)
    }
}

impl VisitStringify for Set {
    fn accept(&self, v: &mut impl Stringify) -> crate::Result<String> {
        v.str_set(self)
    }
}

impl VisitStringify for Dice {
    fn accept(&self, v: &mut impl Stringify) -> crate::Result<String> {
        v.str_dice(self)
    }
}

impl VisitStringify for Die {
    fn accept(&self, v: &mut impl Stringify) -> crate::Result<String> {
        v.str_die(self)
    }
}

#[derive(Default, Debug)]
pub struct SimpleStringifier;

impl Stringify for SimpleStringifier {}

#[derive(Default)]
pub struct MarkdownStringifier {
    in_dropped: bool,
}

impl Stringify for MarkdownStringifier {
    fn stringify(&mut self, the_roll: &impl VisitStringify) -> crate::Result<String> {
        self.in_dropped = false;
        self._stringify(the_roll)
    }

    fn _stringify(&mut self, node: &impl VisitStringify) -> crate::Result<String> {
        if !node.kept() && !self.in_dropped {
            self.in_dropped = true;
            let inside = node.accept(self)?;
            self.in_dropped = false;
            Ok(format!("~~{}~~", inside))
        } else {
            node.accept(self)
        }
    }

    fn str_expression(&mut self, node: &Expression) -> crate::Result<String> {
        Ok(format!(
            "{} = `{}`",
            self._stringify(&node.roll)?,
            node.total()?.as_int()
        ))
    }

    fn str_die(&mut self, node: &Die) -> crate::Result<String> {
        let size = node.size;
        node.values
            .iter()
            .map(|val| fmt_die_value_mkd(self, val, size))
            .collect::<crate::Result<Vec<_>>>()
            .map(|v| v.join(", "))
    }
}

fn fmt_die_value_mkd(
    this: &mut MarkdownStringifier,
    val: &Literal<Int>,
    size: DiceSize,
) -> crate::Result<String> {
    let inside = this._stringify(val)?;
    let number = val.number()?.as_int();
    Ok(if number == 1 || size == DiceSize::Int(number) {
        format!("**{}**", inside)
    } else {
        inside
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;

    fn check(input: &str, expected: &str) {
        let mut roller = roller();
        let expr = roller.roll(input).unwrap();
        assert_eq!(expected, expr.result::<SimpleStringifier>().unwrap())
    }

    #[test]
    fn test_stringify_number() {
        check("2", "2 = 2");
        check("2.0", "2.0 = 2");
    }

    #[test]
    fn test_stringify_dice() {
        let mut roller = roller();
        let mut rolls = roller.roll_n(2, DiceSize::Int(4)).unwrap();
        let (first, second) = (rolls.next().unwrap(), rolls.next().unwrap());
        check(
            "2d4",
            &format!("2d4 ({}, {}) = {}", first, second, first + second),
        );
    }
}
