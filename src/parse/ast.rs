use crate::common::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Expression<'a> {
    pub(crate) roll: Node<'a>,
    pub(crate) comment: Option<&'a str>,
}

impl<'a> Expression<'a> {
    pub(crate) fn new(roll: Node<'a>, comment: Option<&'a str>) -> Self {
        Self { roll, comment }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node<'a> {
    Annotated(Box<Node<'a>>, Vec<&'a str>),
    LiteralInt(Int),
    LiteralFloat(Float),
    Set(Set<'a>),
    Dice(OperatedDice),
    Parenthetical(Box<Node<'a>>),
    Unary(UnaryOperator, Box<Node<'a>>),
    Binary(Box<Node<'a>>, BinaryOperator, Box<Node<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Set<'a> {
    pub values: Vec<Node<'a>>,
    pub ops: Vec<SetOperator>,
}

impl<'a> Set<'a> {
    pub fn new(values: Vec<Node<'a>>, ops: Vec<SetOperator>) -> Self {
        let mut ret = Self { values, ops };
        ret.simplify_ops();
        ret
    }

    fn simplify_ops(&mut self) {
        if self.ops.is_empty() {
            return;
        }

        let mut new_ops = Vec::with_capacity(self.ops.len());

        for op in self.ops.drain(..) {
            if new_ops.is_empty() {
                new_ops.push(op);
            } else {
                let last_op = new_ops.last_mut().expect("new_ops is not empty");
                if op.kind() == last_op.kind() {
                    last_op.add_sels(&mut op.into_sels().into_vec());
                } else {
                    new_ops.push(op);
                }
            }
        }

        new_ops.shrink_to_fit();
        self.ops = new_ops;
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct OperatedDice {
    pub num: Num,
    pub sides: Sides,
    pub ops: Vec<DiceOperator>,
}

impl OperatedDice {
    pub fn new(num: Num, sides: Sides, ops: Vec<DiceOperator>) -> Self {
        let mut ret = Self { num, sides, ops };
        ret.simplify_ops();
        ret
    }

    fn simplify_ops(&mut self) {
        if self.ops.is_empty() {
            return;
        }

        let mut new_ops = Vec::with_capacity(self.ops.len());

        for op in self.ops.drain(..) {
            if new_ops.is_empty() || op.is_immediate() {
                new_ops.push(op);
            } else {
                let last_op = new_ops.last_mut().expect("new_ops is not empty");
                if op.kind() == last_op.kind() {
                    if let Some(sels) = op.into_sels() {
                        last_op.add_sels(&mut sels.into_vec());
                    }
                } else {
                    new_ops.push(op);
                }
            }
        }

        new_ops.shrink_to_fit();
        self.ops = new_ops;
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Dice {
    pub num: Num,
    pub sides: Sides,
}

impl Dice {
    pub fn new(num: Num, sides: Sides) -> Self {
        Self { num, sides }
    }

    pub fn with_ops(self, ops: Vec<DiceOperator>) -> OperatedDice {
        OperatedDice::new(self.num, self.sides, ops)
    }
}
