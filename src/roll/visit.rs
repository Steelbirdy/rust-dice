use super::num::Number;
use super::tree::*;

pub trait VisitRoll {
    type Output;

    fn visit<A: AcceptRoll>(&mut self, a: &A) -> Self::Output {
        a.accept(self)
    }

    fn visit_roll(&mut self, x: &Roll<'_>) -> Self::Output;

    fn visit_roll_tree(&mut self, x: &RollTree<'_>) -> Self::Output;

    fn visit_literal<T: Copy + Into<Number>>(&mut self, x: &Literal<T>) -> Self::Output;

    fn visit_set(&mut self, x: &Set<'_>) -> Self::Output;

    fn visit_dice(&mut self, x: &Dice) -> Self::Output;

    fn visit_die(&mut self, x: &Die) -> Self::Output;

    fn visit_grouping(&mut self, x: &Grouping<'_>) -> Self::Output;

    fn visit_unary(&mut self, op: &Unary<'_>) -> Self::Output;

    fn visit_binary(&mut self, op: &Binary<'_>) -> Self::Output;
}

#[enum_dispatch::enum_dispatch]
pub trait AcceptRoll: Sized + Eval {
    fn accept<V: VisitRoll + ?Sized>(&self, v: &mut V) -> V::Output;
}

impl AcceptRoll for Roll<'_> {
    fn accept<V: VisitRoll + ?Sized>(&self, v: &mut V) -> V::Output {
        v.visit_roll(self)
    }
}

impl AcceptRoll for RollTree<'_> {
    fn accept<V: VisitRoll + ?Sized>(&self, v: &mut V) -> V::Output {
        v.visit_roll_tree(self)
    }
}

impl<T: Copy + Into<Number>> AcceptRoll for Literal<T> {
    fn accept<V: VisitRoll + ?Sized>(&self, v: &mut V) -> V::Output {
        v.visit_literal(self)
    }
}

impl AcceptRoll for Set<'_> {
    fn accept<V: VisitRoll + ?Sized>(&self, v: &mut V) -> V::Output {
        v.visit_set(self)
    }
}

impl AcceptRoll for Dice {
    fn accept<V: VisitRoll + ?Sized>(&self, v: &mut V) -> V::Output {
        v.visit_dice(self)
    }
}

impl AcceptRoll for Die {
    fn accept<V: VisitRoll + ?Sized>(&self, v: &mut V) -> V::Output {
        v.visit_die(self)
    }
}

impl AcceptRoll for Grouping<'_> {
    fn accept<V: VisitRoll + ?Sized>(&self, v: &mut V) -> V::Output {
        v.visit_grouping(self)
    }
}

impl AcceptRoll for Unary<'_> {
    fn accept<V: VisitRoll + ?Sized>(&self, v: &mut V) -> V::Output {
        v.visit_unary(self)
    }
}

impl AcceptRoll for Binary<'_> {
    fn accept<V: VisitRoll + ?Sized>(&self, v: &mut V) -> V::Output {
        v.visit_binary(self)
    }
}
