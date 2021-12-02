use super::ast::Dice;
use crate::common::*;
use logos::{Lexer as LogosLexer, Logos};
use logos_iter::{LogosIter, PeekableLexer};
use std::fmt;

pub type Lexer<'a> = PeekableLexer<'a, LogosLexer<'a, TokenKind>, TokenKind>;

pub fn lexer(s: &str) -> Lexer {
    TokenKind::lexer(s).peekable_lexer()
}

#[derive(Logos, Debug, Copy, Clone, PartialEq)]
pub enum TokenKind {
    #[regex(r"[0-9]+", |lex| lex.slice().parse())]
    Integer(Int),
    #[regex(r"([0-9]+\.[0-9]*)|(\.[0-9]+)", |lex| lex.slice().parse())]
    Float(Float),

    #[regex(r"([1-9][0-9]*)?d(%|[1-9][0-9]*)", |lex| parse_dice(lex.slice()))]
    Dice(Dice),

    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token(",")]
    Comma,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("//")]
    SlashSlash,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("==")]
    EqualEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("<=")]
    LessEqual,
    #[token("!=")]
    BangEqual,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,

    #[token("k")]
    Keep,
    #[token("p")]
    Drop,
    #[token("rr")]
    Reroll,
    #[token("ro")]
    RerollOnce,
    #[token("ra")]
    RerollAdd,
    #[token("e")]
    Explode,
    #[token("mi")]
    Minimum,
    #[token("ma")]
    Maximum,

    #[token("l")]
    Lowest,
    #[token("h")]
    Highest,

    #[regex(r"\[[^\]]+\]")]
    Annotation,
    #[regex(r"#.+")]
    Comment,

    #[token("[]")]
    ErrEmptyAnnotation,
    #[regex(r"0d(%|[0-9]+)")]
    #[regex(r"([1-9][0-9]*)?d0")]
    ErrZeroDice,

    #[regex(r"[ \t\r\n]+", logos::skip)]
    #[error]
    Error,
}

impl TokenKind {
    pub const UNARY_OPS: &'static [Self] = &[Self::Plus, Self::Minus];

    pub const COMPARISON_OPS: &'static [Self] = &[
        Self::LessThan,
        Self::GreaterThan,
        Self::LessEqual,
        Self::GreaterEqual,
        Self::EqualEqual,
        Self::BangEqual,
    ];

    pub const ADDITION_OPS: &'static [Self] = &[Self::Plus, Self::Minus];

    pub const MULTIPLICATION_OPS: &'static [Self] =
        &[Self::Star, Self::Slash, Self::SlashSlash, Self::Percent];

    pub const SET_OPERATORS: &'static [Self] = &[Self::Keep, Self::Drop];

    pub const DICE_OPERATORS: &'static [Self] = &[
        Self::Keep,
        Self::Drop,
        Self::Reroll,
        Self::RerollOnce,
        Self::RerollAdd,
        Self::Explode,
        Self::Minimum,
        Self::Maximum,
    ];

    pub const SELECTORS: &'static [Self] = &[
        Self::Highest,
        Self::Lowest,
        Self::LessThan,
        Self::GreaterThan,
        // EqualTo is represented by the empty string
    ];

    pub fn as_str(&self) -> &'static str {
        use TokenKind::*;

        match self {
            Integer(_) => "<integer>",
            Float(_) => "<float>",
            Dice(_) => "<dice>",
            LeftParen => "'('",
            RightParen => "')'",
            Comma => "','",
            Plus => "'+'",
            Minus => "'-'",
            Star => "'*'",
            Slash => "'/'",
            SlashSlash => "'//'",
            Percent => "'%'",
            EqualEqual => "'=='",
            GreaterEqual => "'>='",
            LessEqual => "'<='",
            BangEqual => "'!='",
            LessThan => "'<'",
            GreaterThan => "'>'",
            Keep => "'k'",
            Drop => "'p'",
            Reroll => "'rr'",
            RerollOnce => "'ro'",
            RerollAdd => "'ra'",
            Explode => "'e'",
            Minimum => "'mi'",
            Maximum => "'ma'",
            Highest => "'h'",
            Lowest => "'l'",
            Annotation => "<annotation>",
            Comment => "<comment>",
            ErrEmptyAnnotation | ErrZeroDice | Error => "<error>",
        }
    }

    pub fn as_unary_op(&self) -> Option<UnaryOperator> {
        use UnaryOperator::*;
        Some(match self {
            Self::Plus => Pos,
            Self::Minus => Neg,
            _ => return None,
        })
    }

    pub fn as_binary_op(&self) -> Option<BinaryOperator> {
        use BinaryOperator::*;
        Some(match self {
            Self::Plus => Add,
            Self::Minus => Sub,
            Self::Star => Mul,
            Self::Slash => Div,
            Self::SlashSlash => Flr,
            Self::Percent => Rem,
            Self::LessThan => Lt,
            Self::GreaterThan => Gt,
            Self::LessEqual => Le,
            Self::GreaterEqual => Ge,
            Self::EqualEqual => Eq,
            Self::BangEqual => Ne,
            _ => return None,
        })
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

// `unwrap` can be used because logos has verified that the string is a valid dice literal
fn parse_dice(s: &str) -> Dice {
    let (num, sides) = s.split_once('d').unwrap();
    let num = if num.is_empty() {
        Num::new(1).unwrap()
    } else {
        num.parse().unwrap()
    };
    let sides = sides.parse().unwrap();
    Dice::new(num, sides)
}
