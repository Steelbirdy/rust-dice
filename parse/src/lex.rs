pub(crate) type Lexer<'a> = logos_iter::PeekableLexer<'a, logos::Lexer<'a, TokenKind>, TokenKind>;

pub(crate) fn lexer(s: &str) -> Lexer {
    logos_iter::LogosIter::peekable_lexer(<TokenKind as logos::Logos>::lexer(s))
}

#[derive(logos::Logos, Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum TokenKind {
    #[regex(r"[0-9]+")]
    Integer,
    #[regex(r"([0-9]+\.[0-9]*)|(\.[0-9]+)")]
    Decimal,

    #[regex(r"([1-9][0-9]*)?d(%|[1-9][0-9]*)")]
    Dice,

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
    SetOpKeep,
    #[token("p")]
    SetOpDrop,

    #[token("rr")]
    DiceOpReroll,
    #[token("ro")]
    DiceOpRerollOnce,
    #[token("ra")]
    DiceOpRerollAdd,
    #[token("e")]
    DiceOpExplode,
    #[token("mi")]
    DiceOpMinimum,
    #[token("ma")]
    DiceOpMaximum,

    #[token("l")]
    SelLowest,
    #[token("h")]
    SelHighest,

    // #[regex(".+", priority = 0)]
    // Comment,
    #[regex(r"\[[^\]]+\]")]
    Annotation,

    #[token("[]")]
    ErrEmptyAnnotation,
    #[regex(r"0d(%|0|-?[1-9][0-9]*)")]
    #[regex(r"([1-9][0-9]*)d(0|-[1-9][0-9]*)")]
    ErrBadDice,

    #[regex(r"[ \t\r\n]+", logos::skip)]
    #[error]
    Error,
}
