use crate::common::*;
use crate::parse::ast;

pub trait AstVisitor<'a> {
    type Output;

    fn visit<T: ?Sized>(&mut self, node: &T) -> Self::Output
    where
        T: Accept<'a, Self>,
    {
        node.accept(self)
    }

    fn visit_annotated(&mut self, value: &ast::Node<'a>, annotations: &[&'a str]) -> Self::Output;

    fn visit_int(&mut self, x: &Int) -> Self::Output;

    fn visit_float(&mut self, x: &Float) -> Self::Output;

    fn visit_set(&mut self, set: &ast::Set<'a>) -> Self::Output;

    fn visit_dice(&mut self, dice: &ast::OperatedDice) -> Self::Output;

    fn visit_parenthetical(&mut self, p: &ast::Node<'a>) -> Self::Output;

    fn visit_unary(&mut self, op: &UnaryOperator, r: &ast::Node<'a>) -> Self::Output;

    fn visit_binary(
        &mut self,
        l: &ast::Node<'a>,
        op: &BinaryOperator,
        r: &ast::Node<'a>,
    ) -> Self::Output;
}

pub trait Accept<'a, V: AstVisitor<'a> + ?Sized> {
    fn accept(&self, v: &mut V) -> V::Output;
}

impl<'a, V: AstVisitor<'a> + ?Sized> Accept<'a, V> for ast::Expression<'a> {
    fn accept(&self, v: &mut V) -> V::Output {
        v.visit(&self.roll)
    }
}

impl<'a, V: AstVisitor<'a> + ?Sized> Accept<'a, V> for ast::Node<'a> {
    fn accept(&self, v: &mut V) -> V::Output {
        match self {
            Self::Annotated(x, a) => v.visit_annotated(&*x, a),
            Self::LiteralInt(x) => v.visit_int(x),
            Self::LiteralFloat(x) => v.visit_float(x),
            Self::Set(x) => v.visit_set(x),
            Self::Dice(x) => v.visit_dice(x),
            Self::Parenthetical(x) => v.visit_parenthetical(&*x),
            Self::Unary(op, x) => v.visit_unary(op, &*x),
            Self::Binary(l, op, r) => v.visit_binary(&*l, op, &*r),
        }
    }
}
