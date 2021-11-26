use super::{ast::*, lexer::*};
use logos_iter::LogosIter;
use std::fmt;
use std::ops::Range;

type PResult<'a, T = Node<'a>> = Result<T, ParseError>;

#[derive(thiserror::Error, Debug, PartialEq)]
#[error("error at position {} ({slice:?}): {kind}", .span.start)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Range<usize>,
    pub slice: String,
}

#[derive(Debug, PartialEq)]
pub enum ParseErrorKind {
    UnexpectedToken {
        found: Option<TokenKind>,
        expected: Vec<TokenKind>,
    },
    UnexpectedString {
        expected: Vec<TokenKind>,
    },
    LexError(TokenKind),
    InvalidMinMaxSelector(SetSelector),
    InvalidSetOp(SetOperatorKind),
}

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedToken { found, expected } => {
                write!(f, "unexpected token: found {:?}, expected ", found)?;
                fmt_expected(expected, f)
            }
            Self::UnexpectedString { expected } => {
                write!(f, "expected ")?;
                fmt_expected(expected, f)
            }
            Self::LexError(TokenKind::ErrBadDice) => {
                write!(f, "invalid dice literal")
            }
            Self::LexError(TokenKind::ErrEmptyAnnotation) => {
                write!(f, "annotations cannot be empty")
            }
            Self::InvalidMinMaxSelector(sel) => {
                write!(
                    f,
                    "{} cannot be used for set operator 'mi' or 'ma'; only a number is allowed",
                    sel
                )
            }
            Self::InvalidSetOp(kind) => {
                write!(f, "{} can only be used with dice", kind)
            }
            _ => unreachable!(),
        }
    }
}

fn fmt_expected(expected: &[TokenKind], f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let len = expected.len();

    if expected.is_empty() {
        Ok(())
    } else if len == 1 {
        f.write_str(expected[0].to_str())
    } else if len == 2 {
        write!(f, "{} or {}", expected[0].to_str(), expected[1].to_str())
    } else {
        for exp in &expected[..len - 1] {
            write!(f, "{}, ", exp.to_str())?;
        }
        write!(f, "or {}", expected[len - 1].to_str())
    }
}

pub struct Parser<'a> {
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

    pub fn parse(mut self) -> Result<Expression<'a>, ParseError> {
        self.parse_expression()
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
        if matches!(
            found,
            Some(TokenKind::ErrBadDice | TokenKind::ErrEmptyAnnotation)
        ) {
            self.error(ParseErrorKind::LexError(found.unwrap()))
        } else if matches!(found, Some(TokenKind::Error)) {
            self.error(ParseErrorKind::UnexpectedString { expected })
        } else {
            self.error(ParseErrorKind::UnexpectedToken { found, expected })
        }
    }

    fn parse_expression(&mut self) -> PResult<'a, Expression<'a>> {
        let roll = self.parse_node()?;
        Ok(Expression::new(roll))
    }

    fn parse_node(&mut self) -> PResult<'a> {
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> PResult<'a> {
        let mut lhs = self.parse_addition()?;

        while self.matches_any(Self::COMPARISON_OPS) {
            let op = self.advance_as().unwrap();
            let rhs = self.parse_comparison()?;

            lhs = Node::new_binary(op, lhs, rhs)
        }

        Ok(lhs)
    }

    fn parse_addition(&mut self) -> PResult<'a> {
        let mut lhs = self.parse_multiplication()?;

        while self.matches_any(Self::ADDITION_OPS) {
            let op = self.advance_as().unwrap();
            let rhs = self.parse_addition()?;

            lhs = Node::new_binary(op, lhs, rhs);
        }

        Ok(lhs)
    }

    fn parse_multiplication(&mut self) -> PResult<'a> {
        let mut lhs = self.parse_unary_prefix()?;

        while self.matches_any(Self::MULTIPLICATION_OPS) {
            let op = self.advance_as().unwrap();
            let rhs = self.parse_multiplication()?;

            lhs = Node::new_binary(op, lhs, rhs);
        }

        Ok(lhs)
    }

    fn parse_unary_prefix(&mut self) -> PResult<'a> {
        if self.matches_any(Self::UNARY_PREFIX_OPS) {
            let op = self.advance_as().unwrap();
            let rhs = self.parse_unary_prefix()?;

            Ok(Node::new_unary(op, rhs))
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
            Node::new_annotated(atom, annotations)
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
            let first_item = self.parse_node()?;
            if self.matches(TokenKind::Comma) {
                self.parse_set(Some(first_item))
            } else {
                self.consume(TokenKind::RightParen)?;
                Ok(Node::new_parenthetical(first_item))
            }
        }
    }

    fn parse_set(&mut self, first_item: Option<Node<'a>>) -> PResult<'a> {
        let mut items: Vec<_> = first_item.into_iter().collect();
        while self.matches(TokenKind::Comma) {
            self.advance();
            if self.matches(TokenKind::RightParen) {
                break;
            }
            items.push(self.parse_node()?);
        }
        self.consume(TokenKind::RightParen)?;

        let ops = self.parse_set_ops()?;
        Ok(Node::new_set(Set::NumberSet(items), ops))
    }

    fn parse_decimal(&mut self) -> PResult<'a> {
        let x = self.consume_as::<Float>(TokenKind::Decimal)?.unwrap();
        Ok(Node::new_literal(x))
    }

    fn parse_integer(&mut self) -> PResult<'a> {
        self.consume_as::<Int>(TokenKind::Integer)
            .map(|x| Node::new_literal(x.unwrap()))
    }

    fn parse_dice(&mut self) -> PResult<'a> {
        let dice = self.consume_as::<Dice>(TokenKind::Dice)?.unwrap();
        let ops = self.parse_dice_ops()?;
        Ok(Node::new_set(dice, ops))
    }

    fn parse_set_ops(&mut self) -> PResult<Vec<SetOperator>> {
        let mut ops = Vec::new();
        while self.matches_any(Self::SET_OPS) {
            ops.push(self.parse_set_op()?);
        }
        if self.matches_any(Self::DICE_OPS) {
            let op = self.parse_dice_op()?;
            return self.error(ParseErrorKind::InvalidSetOp(op.kind));
        }

        Ok(ops)
    }

    fn parse_set_op(&mut self) -> PResult<SetOperator> {
        let op = self.lexer.next().unwrap();
        let sel = self.parse_selector()?;

        use SetOperatorKind::*;
        Ok(match op {
            TokenKind::SetOpKeep => SetOperator::new(Keep, Some(sel)),
            TokenKind::SetOpDrop => SetOperator::new(Drop, Some(sel)),
            _ => unreachable!(),
        })
    }

    fn parse_dice_ops(&mut self) -> PResult<Vec<SetOperator>> {
        let mut ops = Vec::new();
        while self.matches_any(Self::DICE_OPS) {
            ops.push(self.parse_dice_op()?);
        }
        Ok(ops)
    }

    fn parse_dice_op(&mut self) -> PResult<SetOperator> {
        let op = self.lexer.next().unwrap();
        let sel = self.parse_selector()?;

        use SetOperatorKind::*;
        Ok(match op {
            TokenKind::SetOpKeep => SetOperator::new(Keep, Some(sel)),
            TokenKind::SetOpDrop => SetOperator::new(Drop, Some(sel)),
            TokenKind::DiceOpReroll => SetOperator::new(Reroll, Some(sel)),
            TokenKind::DiceOpRerollOnce => SetOperator::new(RerollOnce, Some(sel)),
            TokenKind::DiceOpExplode => SetOperator::new(Explode, Some(sel)),
            TokenKind::DiceOpRerollAdd => SetOperator::new(ExplodeOnce, Some(sel)),
            TokenKind::DiceOpMinimum => {
                if matches!(sel, SetSelector::EqualTo(_)) {
                    SetOperator::new(Minimum, Some(sel))
                } else {
                    return self.error(ParseErrorKind::InvalidMinMaxSelector(sel));
                }
            }
            TokenKind::DiceOpMaximum => {
                if matches!(sel, SetSelector::EqualTo(_)) {
                    SetOperator::new(Maximum, Some(sel))
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

    impl From<Int> for DiceSize {
        fn from(x: Int) -> Self {
            DiceSize::Int(x)
        }
    }

    macro_rules! dice {
        ($num:literal, $size:expr $(; $($op:expr),+)?) => {
            Node::new_set(Dice::new($num, $size), vec![$($($op),+)?])
        };
    }

    macro_rules! set {
        ($($item:expr),* $(,)? $(; $($op:expr),+ $(,)?)?) => {
            Node::new_set(Set::NumberSet(vec![$($item),*]), vec![$($($op),+)?])
        };
    }

    fn parse(s: &str) -> PResult<Expression> {
        Parser::new(s).parse()
    }

    fn check(s: &str, expected: Node) {
        let parsed = parse(s).unwrap();
        assert_eq!(parsed.roll, expected);
    }

    #[test]
    fn test_parse_nums() {
        check("32", Node::new_literal(32));
        check("3.2", Node::new_literal(3.2));
        check(".67", Node::new_literal(0.67));
    }

    #[test]
    fn test_parse_dice() {
        check("1d20", dice!(1, 20));
        check("d4", dice!(1, 4));
        check("2d%", dice!(2, DiceSize::Percentile));
        check(
            "2d20kh1",
            dice!(2, 20; SetOperator::new(SetOperatorKind::Keep, Some(SetSelector::Highest(1)))),
        );
        check(
            "10d4rol2mi5e5",
            dice!(10, 4;
            SetOperator::new(SetOperatorKind::RerollOnce, Some(SetSelector::Lowest(2))),
            SetOperator::new(SetOperatorKind::Minimum, Some(SetSelector::EqualTo(5))),
            SetOperator::new(SetOperatorKind::Explode, Some(SetSelector::EqualTo(5)))),
        );
    }

    #[test]
    fn test_parse_set() {
        check(
            "(1, 2, 3)",
            set!(
                Node::new_literal(1),
                Node::new_literal(2),
                Node::new_literal(3)
            ),
        );
        check(
            "(3d4, 1d12, 2d6)",
            set!(dice!(3, 4), dice!(1, 12), dice!(2, 6)),
        );
        check(
            "(1d4, 1d4e4)k>2",
            set!(dice!(1, 4), dice!(1, 4; SetOperator::new(SetOperatorKind::Explode, Some(SetSelector::EqualTo(4)))); SetOperator::new(SetOperatorKind::Keep, Some(SetSelector::GreaterThan(2)))),
        );
    }

    #[test]
    fn test_parse_unary() {
        check(
            "-2",
            Node::new_unary(UnaryOperator::Minus, Node::new_literal(2)),
        );
        check(
            "-2.0",
            Node::new_unary(UnaryOperator::Minus, Node::new_literal(2.0)),
        );
        check("-1d20", Node::new_unary(UnaryOperator::Minus, dice!(1, 20)));
        check(
            "- -- - -2d4",
            Node::new_unary(
                UnaryOperator::Minus,
                Node::new_unary(
                    UnaryOperator::Minus,
                    Node::new_unary(
                        UnaryOperator::Minus,
                        Node::new_unary(
                            UnaryOperator::Minus,
                            Node::new_unary(UnaryOperator::Minus, dice!(2, 4)),
                        ),
                    ),
                ),
            ),
        );
    }

    #[test]
    fn test_parse_binary() {
        check(
            "2 + 2",
            Node::new_binary(
                BinaryOperator::Add,
                Node::new_literal(2),
                Node::new_literal(2),
            ),
        );
        check(
            "2.5 // 1",
            Node::new_binary(
                BinaryOperator::FloorDiv,
                Node::new_literal(2.5),
                Node::new_literal(1),
            ),
        );
        check(
            "1 + 2 * 3",
            Node::new_binary(
                BinaryOperator::Add,
                Node::new_literal(1),
                Node::new_binary(
                    BinaryOperator::Mul,
                    Node::new_literal(2),
                    Node::new_literal(3),
                ),
            ),
        );
        check(
            "(3d2 - 2d3) % 2 == 0",
            Node::new_binary(
                BinaryOperator::Eq,
                Node::new_binary(
                    BinaryOperator::Mod,
                    Node::new_parenthetical(Node::new_binary(
                        BinaryOperator::Sub,
                        dice!(3, 2),
                        dice!(2, 3),
                    )),
                    Node::new_literal(2),
                ),
                Node::new_literal(0),
            ),
        );
    }

    #[test]
    fn test_parse_annotations() {
        check("1d20 [d20]", Node::new_annotated(dice!(1, 20), vec!["d20"]));
        check(
            "2d20kh1 [Adv.] [d20]",
            Node::new_annotated(
                dice!(2, 20; SetOperator::new(SetOperatorKind::Keep, Some(SetSelector::Highest(1)))),
                vec!["Adv.", "d20"],
            ),
        );
    }

    #[test]
    fn test_simplify_ops() {
        check(
            "2d20kh1kl1",
            dice!(2, 20; SetOperator::new(SetOperatorKind::Keep, vec![SetSelector::Highest(1), SetSelector::Lowest(1)])),
        );
    }
}
