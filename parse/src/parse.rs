use crate::{
    ast::{Dice, DiceOp, Expr, Expression, Set, SetOp, SetSelector},
    lex::{lexer, Lexer, TokenKind},
};
use logos_iter::LogosIter;

type PResult<'a, T = Expr<'a>> = Result<T, ParseError>;

#[derive(Debug, PartialEq)]
pub(crate) struct ParseError {
    kind: ParseErrorKind,
    span: logos::Span,
    slice: String,
}

#[derive(Debug, PartialEq)]
pub(crate) enum ParseErrorKind {
    UnexpectedToken {
        found: Option<TokenKind>,
        expected: Vec<TokenKind>,
    },
    InvalidMinMaxSelector(SetSelector),
}

// TODO: Generic over int, decimal, and dice size types?
pub(crate) struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    const COMPARISON_OPS: &'static [TokenKind] = &[
        TokenKind::LessThan,
        TokenKind::GreaterThan,
        TokenKind::LessEqual,
        TokenKind::GreaterEqual,
        TokenKind::EqualEqual,
        TokenKind::BangEqual,
    ];

    const ADDITION_OPS: &'static [TokenKind] = &[TokenKind::Plus, TokenKind::Minus];

    const MULTIPLICATION_OPS: &'static [TokenKind] = &[
        TokenKind::Star,
        TokenKind::Slash,
        TokenKind::SlashSlash,
        TokenKind::Percent,
    ];

    const UNARY_PREFIX_OPS: &'static [TokenKind] = &[TokenKind::Plus, TokenKind::Minus];

    const SET_OPS: &'static [TokenKind] = &[TokenKind::SetOpKeep, TokenKind::SetOpDrop];

    const DICE_OPS: &'static [TokenKind] = &[
        TokenKind::SetOpKeep,
        TokenKind::SetOpDrop,
        TokenKind::DiceOpReroll,
        TokenKind::DiceOpRerollOnce,
        TokenKind::DiceOpExplode,
        TokenKind::DiceOpRerollAdd,
        TokenKind::DiceOpMinimum,
        TokenKind::DiceOpMaximum,
    ];

    const SELECTORS: &'static [TokenKind] = &[
        TokenKind::SelHighest,
        TokenKind::SelLowest,
        TokenKind::LessThan,
        TokenKind::GreaterThan,
    ];

    pub fn new(s: &'a str) -> Self {
        Self { lexer: lexer(s) }
    }

    pub fn parse(mut self) -> Result<Expr<'a>, ParseError> {
        self.parse_expression()
    }

    pub fn parse_commented(mut self) -> Result<Expression<'a>, ParseError> {
        self.parse_commented_expression()
    }

    fn advance(&mut self) -> Option<TokenKind> {
        self.lexer.next()
    }

    fn advance_as<T: std::str::FromStr>(&mut self) -> Option<T>
    where
        T::Err: std::fmt::Debug,
    {
        self.advance()?;
        Some(self.lexer.slice().parse().unwrap())
    }

    fn matches(&mut self, kind: TokenKind) -> bool {
        self.lexer.peek().map_or(false, |&peeked| peeked == kind)
    }

    fn matches_any(&mut self, options: &[TokenKind]) -> bool {
        self.lexer
            .peek()
            .map_or(false, |peeked| options.contains(peeked))
    }

    fn consume(&mut self, expected: TokenKind) -> PResult<()> {
        if self.matches(expected) {
            self.lexer.next();
            Ok(())
        } else {
            self.unexpected_token(vec![expected])
        }
    }

    fn consume_as<T: std::str::FromStr>(
        &mut self,
        expected: TokenKind,
    ) -> PResult<Result<T, T::Err>> {
        self.consume(expected)?;
        Ok(self.lexer.slice().parse())
    }

    fn error<T>(&mut self, kind: ParseErrorKind) -> PResult<T> {
        Err(ParseError {
            kind,
            span: self.lexer.span(),
            slice: self.lexer.slice().to_string(),
        })
    }

    fn unexpected_token<T>(&mut self, expected: Vec<TokenKind>) -> PResult<T> {
        let found = self.lexer.next();
        self.error(ParseErrorKind::UnexpectedToken { found, expected })
    }

    fn parse_commented_expression(&mut self) -> PResult<'a, Expression<'a>> {
        let expr = self.parse_expression()?;

        let comment = self.lexer.remainder().trim();
        let comment = if comment.is_empty() {
            None
        } else {
            Some(comment)
        };

        Ok(Expression { expr, comment })
    }

    fn parse_expression(&mut self) -> PResult<'a> {
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> PResult<'a> {
        let mut lhs = self.parse_addition()?;

        while self.matches_any(Self::COMPARISON_OPS) {
            let op = self.advance_as().unwrap();
            let rhs = self.parse_comparison()?;

            lhs = Expr::Binary(op, Box::new(lhs), Box::new(rhs))
        }

        Ok(lhs)
    }

    fn parse_addition(&mut self) -> PResult<'a> {
        let mut lhs = self.parse_multiplication()?;

        while self.matches_any(Self::ADDITION_OPS) {
            let op = self.advance_as().unwrap();
            let rhs = self.parse_addition()?;

            lhs = Expr::Binary(op, Box::new(lhs), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_multiplication(&mut self) -> PResult<'a> {
        let mut lhs = self.parse_unary_prefix()?;

        while self.matches_any(Self::MULTIPLICATION_OPS) {
            let op = self.advance_as().unwrap();
            let rhs = self.parse_multiplication()?;

            lhs = Expr::Binary(op, Box::new(lhs), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_unary_prefix(&mut self) -> PResult<'a> {
        if self.matches_any(Self::UNARY_PREFIX_OPS) {
            let op = self.advance_as().unwrap();
            let rhs = self.parse_unary_prefix()?;

            Ok(Expr::Unary(op, Box::new(rhs)))
        } else {
            self.parse_atom()
        }
    }

    fn parse_atom(&mut self) -> PResult<'a> {
        let atom = match self.lexer.peek() {
            Some(TokenKind::LeftParen) => self.parse_set_or_parens(),
            Some(TokenKind::Decimal) => self.parse_decimal(),
            Some(TokenKind::Integer) => self.parse_integer(),
            Some(TokenKind::Dice) => self.parse_dice(),
            _ => self.unexpected_token(vec![
                TokenKind::LeftParen,
                TokenKind::Decimal,
                TokenKind::Integer,
            ]),
        }?;

        let mut annotations = Vec::new();
        while self.matches(TokenKind::Annotation) {
            annotations.push(self.parse_annotation()?);
        }

        Ok(if annotations.is_empty() {
            atom
        } else {
            Expr::Annotated(Box::new(atom), annotations)
        })
    }

    fn parse_annotation(&mut self) -> PResult<&'a str> {
        self.consume(TokenKind::Annotation)?;
        let slice = self.lexer.slice();
        Ok(&slice[1..slice.len() - 1])
    }

    fn parse_set_or_parens(&mut self) -> PResult<'a> {
        self.consume(TokenKind::LeftParen)?;

        if self.matches(TokenKind::RightParen) {
            self.parse_set(None)
        } else {
            let first_item = self.parse_expression()?;
            if self.matches(TokenKind::Comma) {
                self.parse_set(Some(first_item))
            } else {
                self.consume(TokenKind::RightParen)?;
                Ok(Expr::Grouping(Box::new(first_item)))
            }
        }
    }

    fn parse_set(&mut self, first_item: Option<Expr<'a>>) -> PResult<'a> {
        let mut items: Vec<_> = first_item.into_iter().collect();
        while self.matches(TokenKind::Comma) {
            self.advance();
            if self.matches(TokenKind::RightParen) {
                break;
            }
            items.push(self.parse_expression()?);
        }
        self.consume(TokenKind::RightParen)?;

        let ops = self.parse_set_ops()?;
        Ok(Expr::Set(Set { items, ops }))
    }

    fn parse_decimal(&mut self) -> PResult<'a> {
        let x = self.consume_as::<f64>(TokenKind::Decimal)?.unwrap();
        Ok(Expr::Decimal(x))
    }

    fn parse_integer(&mut self) -> PResult<'a> {
        self.consume_as(TokenKind::Integer)
            .map(|x| Expr::Integer(x.unwrap()))
    }

    fn parse_dice(&mut self) -> PResult<'a> {
        let mut dice = self.consume_as::<Dice>(TokenKind::Dice)?.unwrap();
        let ops = self.parse_dice_ops()?;
        dice.ops = ops;
        Ok(Expr::Dice(dice))
    }

    fn parse_set_ops(&mut self) -> PResult<Vec<SetOp>> {
        let mut ops = Vec::new();
        while self.matches_any(Self::SET_OPS) {
            ops.push(self.parse_set_op()?);
        }
        Ok(ops)
    }

    fn parse_set_op(&mut self) -> PResult<SetOp> {
        let op = self.lexer.next().unwrap();
        let sel = self.parse_selector()?;
        Ok(match op {
            TokenKind::SetOpKeep => SetOp::Keep(sel),
            TokenKind::SetOpDrop => SetOp::Drop(sel),
            _ => unreachable!(),
        })
    }

    fn parse_dice_ops(&mut self) -> PResult<Vec<DiceOp>> {
        let mut ops = Vec::new();
        while self.matches_any(Self::DICE_OPS) {
            ops.push(self.parse_dice_op()?);
        }
        Ok(ops)
    }

    fn parse_dice_op(&mut self) -> PResult<DiceOp> {
        let op = self.lexer.next().unwrap();
        let sel = self.parse_selector()?;
        Ok(match op {
            TokenKind::SetOpKeep => DiceOp::Keep(sel),
            TokenKind::SetOpDrop => DiceOp::Drop(sel),
            TokenKind::DiceOpReroll => DiceOp::Reroll(sel),
            TokenKind::DiceOpRerollOnce => DiceOp::RerollOnce(sel),
            TokenKind::DiceOpExplode => DiceOp::Explode(sel),
            TokenKind::DiceOpRerollAdd => DiceOp::ExplodeOnce(sel),
            TokenKind::DiceOpMinimum => {
                if let SetSelector::EqualTo(x) = sel {
                    DiceOp::Minimum(x)
                } else {
                    return self.error(ParseErrorKind::InvalidMinMaxSelector(sel));
                }
            }
            TokenKind::DiceOpMaximum => {
                if let SetSelector::EqualTo(x) = sel {
                    DiceOp::Maximum(x)
                } else {
                    return self.error(ParseErrorKind::InvalidMinMaxSelector(sel));
                }
            }
            _ => unreachable!(),
        })
    }

    fn parse_selector(&mut self) -> PResult<SetSelector> {
        let kind = if self.matches_any(Self::SELECTORS) {
            self.lexer.next().unwrap()
        } else if self.matches(TokenKind::Integer) {
            let x = self.advance_as().unwrap();
            return Ok(SetSelector::EqualTo(x));
        } else {
            return self.unexpected_token(vec![TokenKind::Integer]);
        };

        let x = self.advance_as().unwrap();
        Ok(match kind {
            TokenKind::SelLowest => SetSelector::Lowest(x),
            TokenKind::SelHighest => SetSelector::Highest(x),
            TokenKind::LessThan => SetSelector::LessThan(x),
            TokenKind::GreaterThan => SetSelector::GreaterThan(x),
            _ => unreachable!(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;

    impl From<UInt> for DiceSize {
        fn from(x: UInt) -> Self {
            DiceSize::Int(x)
        }
    }

    macro_rules! dice {
        ($num:literal, $size:literal $(; $($op:expr),+)?) => {
            Expr::Dice(Dice { num: $num, size: DiceSize::Int($size), ops: vec![$($($op),+)?] })
        };
        ($num:literal, $size:expr $(; $($op:expr),+)?) => {
            Expr::Dice(Dice { num: $num, size: $size, ops: vec![$($($op),+)?] })
        };
    }

    macro_rules! set {
        ($($item:expr),* $(,)? $(; $($op:expr),+ $(,)?)?) => {
            Expr::Set(Set { items: vec![$($item),*], ops: vec![$($($op),+)?] })
        };
    }

    fn parse(s: &str) -> PResult<'_> {
        Parser::new(s).parse()
    }

    fn check(s: &str, expected: Expr) {
        let parsed = parse(s).unwrap();
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_parse_nums() {
        check("32", Expr::Integer(32));
        check("3.2", Expr::Decimal(3.2));
        check(".67", Expr::Decimal(0.67));
    }

    #[test]
    fn test_parse_dice() {
        check("1d20", dice!(1, 20));
        check("d4", dice!(1, 4));
        check("2d%", dice!(2, DiceSize::Percentile));
        check(
            "2d20kh1",
            dice!(2, 20; DiceOp::Keep(SetSelector::Highest(1))),
        );
        check(
            "10d4rol2mi5e5",
            dice!(10, 4;
            DiceOp::RerollOnce(SetSelector::Lowest(2)),
            DiceOp::Minimum(5),
            DiceOp::Explode(SetSelector::EqualTo(5))),
        );
    }

    #[test]
    fn test_parse_set() {
        check(
            "(1, 2, 3)",
            set!(Expr::Integer(1), Expr::Integer(2), Expr::Integer(3)),
        );
        check(
            "(3d4, 1d12, 2d6)",
            set!(dice!(3, 4), dice!(1, 12), dice!(2, 6)),
        );
        check(
            "(1d4, 1d4e4)k>2",
            set!(dice!(1, 4), dice!(1, 4; DiceOp::Explode(SetSelector::EqualTo(4))); SetOp::Keep(SetSelector::GreaterThan(2))),
        );
    }

    #[test]
    fn test_parse_unary() {
        check(
            "-2",
            Expr::Unary(UnaryOp::Minus, Box::new(Expr::Integer(2))),
        );
        check(
            "-2.0",
            Expr::Unary(UnaryOp::Minus, Box::new(Expr::Decimal(2.0))),
        );
        check("-1d20", Expr::Unary(UnaryOp::Minus, Box::new(dice!(1, 20))));
        check(
            "- -- - -2d4",
            Expr::Unary(
                UnaryOp::Minus,
                Box::new(Expr::Unary(
                    UnaryOp::Minus,
                    Box::new(Expr::Unary(
                        UnaryOp::Minus,
                        Box::new(Expr::Unary(
                            UnaryOp::Minus,
                            Box::new(Expr::Unary(UnaryOp::Minus, Box::new(dice!(2, 4)))),
                        )),
                    )),
                )),
            ),
        );
    }

    #[test]
    fn test_parse_binary() {
        check(
            "2 + 2",
            Expr::Binary(
                BinaryOp::Add,
                Box::new(Expr::Integer(2)),
                Box::new(Expr::Integer(2)),
            ),
        );
        check(
            "2.5 // 1",
            Expr::Binary(
                BinaryOp::FloorDiv,
                Box::new(Expr::Decimal(2.5)),
                Box::new(Expr::Integer(1)),
            ),
        );
        check(
            "1 + 2 * 3",
            Expr::Binary(
                BinaryOp::Add,
                Box::new(Expr::Integer(1)),
                Box::new(Expr::Binary(
                    BinaryOp::Mul,
                    Box::new(Expr::Integer(2)),
                    Box::new(Expr::Integer(3)),
                )),
            ),
        );
        check(
            "(3d2 - 2d3) % 2 == 0",
            Expr::Binary(
                BinaryOp::Eq,
                Box::new(Expr::Binary(
                    BinaryOp::Mod,
                    Box::new(Expr::Grouping(Box::new(Expr::Binary(
                        BinaryOp::Sub,
                        Box::new(dice!(3, 2)),
                        Box::new(dice!(2, 3)),
                    )))),
                    Box::new(Expr::Integer(2)),
                )),
                Box::new(Expr::Integer(0)),
            ),
        );
    }

    #[test]
    fn test_parse_annotations() {
        check(
            "1d20 [d20]",
            Expr::Annotated(Box::new(dice!(1, 20)), vec!["d20"]),
        );
        check(
            "2d20kh1 [Adv.] [d20]",
            Expr::Annotated(
                Box::new(dice!(2, 20; DiceOp::Keep(SetSelector::Highest(1)))),
                vec!["Adv.", "d20"],
            ),
        );
    }
    #[test]
    fn test_parse_commented() {
        let parsed = Parser::new("2d6 [piercing] + 1d10 [cold] Ice Knife")
            .parse_commented()
            .unwrap();
        assert_eq!(
            parsed,
            Expression {
                expr: Expr::Binary(
                    BinaryOp::Add,
                    Box::new(Expr::Annotated(Box::new(dice!(2, 6)), vec!["piercing"],)),
                    Box::new(Expr::Annotated(Box::new(dice!(1, 10)), vec!["cold"],)),
                ),
                comment: Some("Ice Knife"),
            }
        );

        let parsed = Parser::new("2d6 [piercing] + 1d10 [cold] ")
            .parse_commented()
            .unwrap();
        assert_eq!(
            parsed,
            Expression {
                expr: Expr::Binary(
                    BinaryOp::Add,
                    Box::new(Expr::Annotated(Box::new(dice!(2, 6)), vec!["piercing"],)),
                    Box::new(Expr::Annotated(Box::new(dice!(1, 10)), vec!["cold"],)),
                ),
                comment: None,
            }
        );
    }
}
