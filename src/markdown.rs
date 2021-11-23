use crate::{eval::*, dice::*};

#[derive(Default)]
pub struct MarkdownStringifier {
    in_dropped: bool,
}

impl MarkdownStringifier {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn mkd(&mut self, x: &impl Markdown) -> String {
        self.in_dropped = false;
        self._mkd(x)
    }

    fn _mkd(&mut self, x: &impl Markdown) -> String {
        if !x.is_kept() && !self.in_dropped {
            self.in_dropped = true;
            let inner = self._mkd(x);
            self.in_dropped = false;
            format!("~~{}~~", inner)
        } else {
            x.fmt(self)
        }
    }
}

pub trait Markdown {
    fn is_kept(&self) -> bool;

    fn fmt(&self, s: &mut MarkdownStringifier) -> String;
}

impl Markdown for Dice {
    fn is_kept(&self) -> bool {
        true
    }

    fn fmt(&self, _: &mut MarkdownStringifier) -> String {
        self.to_string()
    }
}

impl Markdown for RollResult {
    fn is_kept(&self) -> bool {
        true
    }

    fn fmt(&self, s: &mut MarkdownStringifier) -> String {
        let ops = self.ops.iter().map(ToString::to_string).reduce(|a, b| format!("{}{}", a, b));
        let dice = self.dice.iter().map(|d| s._mkd(d)).reduce(|a, b| format!("{}, {}", a, b));
        format!("{}d{}{} ({})", self.num, self.size, ops.as_deref().unwrap_or(""), dice.as_deref().unwrap_or(""))
    }
}

impl Markdown for Die {
    fn is_kept(&self) -> bool {
        self.kept
    }

    fn fmt(&self, s: &mut MarkdownStringifier) -> String {
        self.values.iter()
            .map(|val| {
                if val.value() == 1 || val.value() == self.size {
                    format!("**{}**", s._mkd(val))
                } else {
                    s._mkd(val)
                }
            })
            .reduce(|a, b| format!("{}, {}", a, b))
            .unwrap()
    }
}

impl Markdown for DieValue {
    fn is_kept(&self) -> bool {
        self.kept
    }

    fn fmt(&self, _: &mut MarkdownStringifier) -> String {
        let history: String = self.history.iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(" -> ");
        if self.exploded {
            format!("{}!", history)
        } else {
            history
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::*;
    use super::*;

    #[test]
    fn test_markdown() {
        let mut s = MarkdownStringifier::new();

        assert_eq!(&s.mkd(&Dice::new(1, 20)), "1d20");

        let mut dice = Dice::new(2, 20);
        dice.add_op(Keep::highest(1));
        assert_eq!(&s.mkd(&dice), "2d20kh1");

        let mut dice = Dice::new(2, 20);
        dice.add_op(RerollOnce::equal_to(1))
            .add_op(Keep::greater_than(2));
        let roll = RollResult::new(10, 4, vec![RerollOnce::equal_to(1).into(), Keep::greater_than(2).into()], &mut roller());

        println!("{}", s.mkd(&roll))
    }
}
