use super::{
    ast::*,
    error::{ParseError, SourcePosition},
    lexer::{lexer, Lexer, TokenKind},
};
use crate::common::*;
use logos_iter::LogosIter;

pub fn parse(s: &str) -> PResult<Expression> {
    Parser::new(s).parse()
}

pub type PResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

macro_rules! consume {
    (? $self:ident, $($pat:pat_param)|+) => {
        if matches!($self.peek(), Some($($pat)|+)) {
            $self.advance();
            true
        } else {
            false
        }
    };
    ($self:ident, $($pat:pat_param)|+) => {
        if matches!($self.peek(), Some($($pat)|+)) {
            Ok($self.advance().unwrap())
        } else {
            Err(())
        }
    };
    ($self:ident, $pat:pat => $map:expr) => {
        if matches!($self.peek(), Some($pat)) {
            match $self.advance() {
                Some($pat) => Ok($map),
                _ => unreachable!(),
            }
        } else {
            Err(())
        }
    };
}

impl<'a> Parser<'a> {
    pub fn new(s: &'a str) -> Self {
        Self { lexer: lexer(s) }
    }

    pub fn parse(mut self) -> PResult<Expression<'a>> {
        self.parse_expression()
    }

    fn advance(&mut self) -> Option<TokenKind> {
        self.lexer.next()
    }

    fn peek(&mut self) -> Option<&TokenKind> {
        self.lexer.peek()
    }

    fn matches_any(&mut self, kinds: &[TokenKind]) -> bool {
        self.peek().map_or(false, |p| kinds.contains(p))
    }

    fn position(&self) -> SourcePosition {
        SourcePosition {
            span: self.lexer.span(),
            slice: self.lexer.slice().to_string(),
        }
    }

    fn peek_position(&mut self) -> SourcePosition {
        SourcePosition {
            span: self.lexer.peek_span(),
            slice: self.lexer.peek_slice().to_string(),
        }
    }

    fn unexpected_token(&mut self, expected: NonEmpty<TokenKind>, extras: Vec<&str>) -> ParseError {
        let expected = expected
            .into_iter()
            .map(|tk| tk.to_string())
            .chain(extras.into_iter().map(ToString::to_string))
            .collect();
        let expected = NonEmpty::try_from_vec(expected).unwrap();

        ParseError::UnexpectedToken {
            pos: self.peek_position(),
            expected,
        }
    }

    fn parse_expression(&mut self) -> PResult<Expression<'a>> {
        let roll = self.parse_node()?;
        let comment = if consume!(?self, TokenKind::Comment) {
            let slice = self.lexer.slice();
            const TRIM_CHARS: &[char] = &[' ', '\t', '#'];
            Some(slice.trim_matches(TRIM_CHARS))
        } else if self.peek().is_some() {
            // TODO: automatically handle expected
            return Err(self.unexpected_token(vec1![TokenKind::Comment], vec![]));
        } else {
            None
        };
        Ok(Expression::new(roll, comment))
    }

    fn parse_node(&mut self) -> PResult<Node<'a>> {
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> PResult<Node<'a>> {
        let mut lhs = self.parse_addition()?;

        while self.matches_any(TokenKind::COMPARISON_OPS) {
            let op = self
                .advance()
                .and_then(|tk| tk.as_binary_op())
                .expect("the token is a comparison operator");
            let rhs = self.parse_comparison()?;

            lhs = Node::Binary(Box::new(lhs), op, Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_addition(&mut self) -> PResult<Node<'a>> {
        let mut lhs = self.parse_multiplication()?;

        while self.matches_any(TokenKind::ADDITION_OPS) {
            let op = self
                .advance()
                .and_then(|tk| tk.as_binary_op())
                .expect("the token is an addition operator");
            let rhs = self.parse_addition()?;

            lhs = Node::Binary(Box::new(lhs), op, Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_multiplication(&mut self) -> PResult<Node<'a>> {
        let mut lhs = self.parse_unary()?;

        while self.matches_any(TokenKind::MULTIPLICATION_OPS) {
            let op = self
                .advance()
                .and_then(|tk| tk.as_binary_op())
                .expect("the token is a multiplication operator");
            let rhs = self.parse_multiplication()?;

            lhs = Node::Binary(Box::new(lhs), op, Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_unary(&mut self) -> PResult<Node<'a>> {
        if self.matches_any(TokenKind::UNARY_OPS) {
            let op = self.advance().and_then(|tk| tk.as_unary_op()).unwrap();
            let rhs = self.parse_unary()?;

            Ok(Node::Unary(op, Box::new(rhs)))
        } else {
            self.parse_atom()
        }
    }

    fn parse_atom(&mut self) -> PResult<Node<'a>> {
        let atom = match self.peek() {
            Some(TokenKind::LeftParen) => self.parse_parens(),
            Some(TokenKind::Integer(_)) => self.parse_int(),
            Some(TokenKind::Float(_)) => self.parse_float(),
            Some(TokenKind::Dice(_)) => self.parse_dice(),
            _ => {
                return Err(
                    self.unexpected_token(vec1![TokenKind::LeftParen], vec!["<number>", "<dice>"])
                )
            }
        }?;

        let mut annotations = Vec::new();
        while matches!(self.peek(), Some(TokenKind::Annotation)) {
            annotations.push(self.parse_annotation()?);
        }

        Ok(if annotations.is_empty() {
            atom
        } else {
            Node::Annotated(Box::new(atom), annotations)
        })
    }

    fn parse_annotation(&mut self) -> PResult<&'a str> {
        consume!(self, TokenKind::Annotation).expect("bug in parser: failed to parse annotation");
        let slice = self.lexer.slice();
        Ok(&slice[1..slice.len() - 1])
    }

    fn parse_parens(&mut self) -> PResult<Node<'a>> {
        consume!(self, TokenKind::LeftParen)
            .expect("bug in parser: failed to parse opening parenthesis");

        if matches!(self.peek(), Some(TokenKind::RightParen)) {
            self.parse_set(None)
        } else {
            let first = self.parse_node()?;
            if matches!(self.peek(), Some(TokenKind::Comma)) {
                self.parse_set(Some(first))
            } else {
                consume!(self, TokenKind::RightParen)
                    .map_err(|_| self.unexpected_token(vec1![TokenKind::RightParen], vec![]))?;
                Ok(Node::Parenthetical(Box::new(first)))
            }
        }
    }

    fn parse_set(&mut self, first: Option<Node<'a>>) -> PResult<Node<'a>> {
        let mut values: Vec<_> = first.into_iter().collect();
        while consume!(?self, TokenKind::Comma) {
            if matches!(self.peek(), Some(TokenKind::RightParen)) {
                break;
            }
            values.push(self.parse_node()?);
        }
        consume!(self, TokenKind::RightParen)
            .map_err(|_| self.unexpected_token(vec1![TokenKind::RightParen], vec![]))?;

        let ops = self.parse_set_ops()?;
        Ok(Node::Set(Set::new(values, ops)))
    }

    fn parse_int(&mut self) -> PResult<Node<'a>> {
        let x = consume!(self, TokenKind::Integer(_x) => _x)
            .expect("bug in parser: failed to parse int");
        Ok(Node::LiteralInt(x))
    }

    fn parse_float(&mut self) -> PResult<Node<'a>> {
        let x = consume!(self, TokenKind::Float(_x) => _x)
            .expect("bug in parser: failed to parse float");
        Ok(Node::LiteralFloat(x))
    }

    fn parse_dice(&mut self) -> PResult<Node<'a>> {
        let dice =
            consume!(self, TokenKind::Dice(_d) => _d).expect("bug in parser: failed to parse dice");
        let ops = self.parse_dice_ops()?;
        Ok(Node::Dice(dice.with_ops(ops)))
    }

    fn parse_set_ops(&mut self) -> PResult<Vec<SetOperator>> {
        let mut ops = Vec::new();
        while self.matches_any(TokenKind::SET_OPERATORS) {
            ops.push(self.parse_set_op()?);
        }
        if self.matches_any(TokenKind::DICE_OPERATORS) {
            return Err(ParseError::InvalidSetOp(self.peek_position()));
        }

        Ok(ops)
    }

    fn parse_set_op(&mut self) -> PResult<SetOperator> {
        let op_kind = consume!(self, TokenKind::Keep | TokenKind::Drop)
            .expect("bug in parser: failed to parse set operator");
        let sel = vec1![self.parse_selector(self.position())?];

        Ok(match op_kind {
            TokenKind::Keep => SetOperator::Keep(sel),
            TokenKind::Drop => SetOperator::Drop(sel),
            _ => unreachable!("bug in parser: unhandled set operator"),
        })
    }

    fn parse_dice_ops(&mut self) -> PResult<Vec<DiceOperator>> {
        let mut ops = Vec::new();
        while self.matches_any(TokenKind::DICE_OPERATORS) {
            ops.push(self.parse_dice_op()?);
        }
        Ok(ops)
    }

    fn parse_dice_op(&mut self) -> PResult<DiceOperator> {
        let op_kind = consume!(
            self,
            TokenKind::Keep
                | TokenKind::Drop
                | TokenKind::Reroll
                | TokenKind::RerollOnce
                | TokenKind::RerollAdd
                | TokenKind::Explode
                | TokenKind::Minimum
                | TokenKind::Maximum
        )
        .expect("bug in parser: failed to parse dice operator");
        let op_position = self.position();
        let sel_position = self.peek_position();
        let sel = vec1![self.parse_selector(op_position)?];

        Ok(match op_kind {
            TokenKind::Keep => DiceOperator::Keep(sel),
            TokenKind::Drop => DiceOperator::Drop(sel),
            TokenKind::Reroll => DiceOperator::Reroll(sel),
            TokenKind::RerollOnce => DiceOperator::RerollOnce(sel),
            TokenKind::Explode => DiceOperator::Explode(sel),
            TokenKind::RerollAdd => DiceOperator::ExplodeOnce(sel),
            TokenKind::Minimum => match &sel[0] {
                &Selector::Equal(n) => DiceOperator::Minimum(n),
                _ => return Err(ParseError::InvalidMinMaxSelector(sel_position)),
            },
            TokenKind::Maximum => match &sel[0] {
                &Selector::Equal(n) => DiceOperator::Maximum(n),
                _ => return Err(ParseError::InvalidMinMaxSelector(sel_position)),
            },
            _ => unreachable!("bug in parser: unhandled dice operator"),
        })
    }

    fn parse_selector(&mut self, op_position: SourcePosition) -> PResult<Selector> {
        let kind = if self.matches_any(TokenKind::SELECTORS) {
            self.advance()
        } else if matches!(self.peek(), Some(TokenKind::Integer(_))) {
            None
        } else {
            return Err(ParseError::NoSelector(op_position));
        };

        let n = consume!(self, TokenKind::Integer(_x) => _x)
            .map_err(|_| self.unexpected_token(vec1![TokenKind::Integer(0)], vec![]))?;
        Ok(match kind {
            Some(TokenKind::Highest) => Selector::Highest(n as usize),
            Some(TokenKind::Lowest) => Selector::Lowest(n as usize),
            Some(TokenKind::LessThan) => Selector::Less(n),
            Some(TokenKind::GreaterThan) => Selector::Greater(n),
            None => Selector::Equal(n),
            _ => unreachable!("bug in parser: unhandled selector"),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::test_utils::*;

    fn check(s: &str, expected: PResult<Node>) {
        let actual = parse(s);
        assert_eq!(expected.map(|x| Expression::new(x, None)), actual);
    }

    fn check_commented(s: &str, expected: PResult<Node>, comment: &str) {
        let actual = parse(s);
        assert_eq!(expected.map(|x| Expression::new(x, Some(comment))), actual);
    }

    fn pos(span: logos::Span, slice: &str) -> SourcePosition {
        SourcePosition {
            span,
            slice: slice.to_string(),
        }
    }

    fn unex_tok(span: logos::Span, slice: &str, expected: Vec<&str>) -> ParseError {
        let position = pos(span, slice);
        let expected =
            NonEmpty::try_from_vec(expected.into_iter().map(ToString::to_string).collect())
                .unwrap();
        ParseError::UnexpectedToken {
            pos: position,
            expected,
        }
    }

    #[test]
    fn test_parse_number() {
        check("2", Ok(Node::int(2)));
        check("2.0", Ok(Node::float(2.0)));
        check("0", Ok(Node::int(0)));
    }

    #[test]
    fn test_parse_dice() {
        check("2d4", Ok(Node::dice(2, 4)));
        check("d20", Ok(Node::dice(1, 20)));
        check("d%", Ok(Node::dice(1, Sides::Percentile)));
    }

    #[test]
    fn test_parse_unary() {
        check("-2", Ok(Node::un(Neg, Node::int(2))));
        check("+3d6", Ok(Node::un(Pos, Node::dice(3, 6))));
        check(
            "---3.5",
            Ok(Node::un(
                Neg,
                Node::un(Neg, Node::un(Neg, Node::float(3.5))),
            )),
        );
    }

    #[test]
    fn test_parse_binary() {
        check("2+2", Ok(Node::bin(Node::int(2), Add, Node::int(2))));
        check(
            "1 * 2 // 3",
            Ok(Node::bin(
                Node::int(1),
                Mul,
                Node::bin(Node::int(2), Flr, Node::int(3)),
            )),
        );
        check(
            "1 % 2 - 3",
            Ok(Node::bin(
                Node::bin(Node::int(1), Rem, Node::int(2)),
                Sub,
                Node::int(3),
            )),
        );
        check(
            "-1 --2 -3",
            Ok(Node::bin(
                Node::un(Neg, Node::int(1)),
                Sub,
                Node::bin(Node::un(Neg, Node::int(2)), Sub, Node::int(3)),
            )),
        );
        check(
            "1 + 2 < 3 * 4",
            Ok(Node::bin(
                Node::bin(Node::int(1), Add, Node::int(2)),
                Lt,
                Node::bin(Node::int(3), Mul, Node::int(4)),
            )),
        );
    }

    #[test]
    fn test_parse_parens() {
        check(
            "(1 / 2) == 3",
            Ok(Node::bin(
                Node::parens(Node::bin(Node::int(1), Div, Node::int(2))),
                Eq,
                Node::int(3),
            )),
        );
        check(
            "1 / (2 == 3)",
            Ok(Node::bin(
                Node::int(1),
                Div,
                Node::parens(Node::bin(Node::int(2), Eq, Node::int(3))),
            )),
        );
        check(
            "(((1) + 2) + 3) + 4",
            Ok(Node::bin(
                Node::parens(Node::bin(
                    Node::parens(Node::bin(Node::parens(Node::int(1)), Add, Node::int(2))),
                    Add,
                    Node::int(3),
                )),
                Add,
                Node::int(4),
            )),
        );
    }

    #[test]
    fn test_parse_set() {
        check("(1, 2)", Ok(Node::set(vec![Node::int(1), Node::int(2)])));
        check(
            "(1 + 2, 2d4)",
            Ok(Node::set(vec![
                Node::bin(Node::int(1), Add, Node::int(2)),
                Node::dice(2, 4),
            ])),
        );
        check("(1,)", Ok(Node::set(vec![Node::int(1)])));
        check(
            "(1, 2, 3,)",
            Ok(Node::set(vec![Node::int(1), Node::int(2), Node::int(3)])),
        );
    }

    #[test]
    fn test_parse_set_dice_ops() {
        check(
            "2d20kh1",
            Ok(Node::op_dice(
                2,
                20,
                vec![DiceOperator::Keep(vec1![Selector::Highest(1)])],
            )),
        );
        check(
            "2d20rr1pl1",
            Ok(Node::op_dice(
                2,
                20,
                vec![
                    DiceOperator::Reroll(vec1![Selector::Equal(1)]),
                    DiceOperator::Drop(vec1![Selector::Lowest(1)]),
                ],
            )),
        );
        check(
            "4d4mi2",
            Ok(Node::op_dice(4, 4, vec![DiceOperator::Minimum(2)])),
        );

        check(
            "(1, 2, 3)k>2",
            Ok(Node::op_set(
                vec![Node::int(1), Node::int(2), Node::int(3)],
                vec![SetOperator::Keep(vec1![Selector::Greater(2)])],
            )),
        );
        check(
            "(1d4e4, 2d6ro1)k>4k<4",
            Ok(Node::op_set(
                vec![
                    Node::op_dice(1, 4, vec![DiceOperator::Explode(vec1![Selector::Equal(4)])]),
                    Node::op_dice(
                        2,
                        6,
                        vec![DiceOperator::RerollOnce(vec1![Selector::Equal(1)])],
                    ),
                ],
                vec![SetOperator::Keep(vec1![
                    Selector::Greater(4),
                    Selector::Less(4)
                ])],
            )),
        );
    }

    #[test]
    fn test_parse_annotations() {
        check(
            "2d6 [cold] + 1d10 [piercing] + 4",
            Ok(Node::bin(
                Node::annot(Node::dice(2, 6), vec!["cold"]),
                Add,
                Node::bin(
                    Node::annot(Node::dice(1, 10), vec!["piercing"]),
                    Add,
                    Node::int(4),
                ),
            )),
        );
        check(
            "2d6 [cold] [magical] + 1d10 [piercing] [non-magical] + 4",
            Ok(Node::bin(
                Node::annot(Node::dice(2, 6), vec!["cold", "magical"]),
                Add,
                Node::bin(
                    Node::annot(Node::dice(1, 10), vec!["piercing", "non-magical"]),
                    Add,
                    Node::int(4),
                ),
            )),
        );
    }

    #[test]
    fn test_parse_comment() {
        check_commented(
            "1d20kh1 # Advantage",
            Ok(Node::op_dice(
                1,
                20,
                vec![DiceOperator::Keep(vec1![Selector::Highest(1)])],
            )),
            "Advantage",
        );
    }

    #[test]
    fn test_err_unexpected_token() {
        check("2d4 6", Err(unex_tok(4..5, "6", vec!["<comment>"])));
        check(
            "1 +k",
            Err(unex_tok(3..4, "k", vec!["'('", "<number>", "<dice>"])),
        );
        check("5*(2 + 3 400)", Err(unex_tok(9..12, "400", vec!["')'"])));
        check("(1, 2 400", Err(unex_tok(6..9, "400", vec!["')'"])));
        check("2d20khl1", Err(unex_tok(6..7, "l", vec!["<integer>"])));
    }

    #[test]
    fn test_err_invalid_set_op() {
        check("(1,)rr<2", Err(ParseError::InvalidSetOp(pos(4..6, "rr"))));
        check(
            "(1, 2)kh1e2",
            Err(ParseError::InvalidSetOp(pos(9..10, "e"))),
        );
    }

    #[test]
    fn test_err_no_selector() {
        check("1d20k", Err(ParseError::NoSelector(pos(4..5, "k"))));
        check("d4ma", Err(ParseError::NoSelector(pos(2..4, "ma"))));
    }

    #[test]
    fn test_err_invalid_min_max_selector() {
        check(
            "2d20mi>2",
            Err(ParseError::InvalidMinMaxSelector(pos(6..7, ">"))),
        );
    }
}
